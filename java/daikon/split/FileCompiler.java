package daikon.split;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.time.Duration;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;
import org.apache.commons.exec.CommandLine;
import org.apache.commons.exec.DefaultExecuteResultHandler;
import org.apache.commons.exec.DefaultExecutor;
import org.apache.commons.exec.ExecuteException;
import org.apache.commons.exec.ExecuteWatchdog;
import org.apache.commons.exec.PumpStreamHandler;
import org.checkerframework.checker.index.qual.Positive;
import org.checkerframework.checker.nullness.qual.NonNull;
import org.checkerframework.checker.regex.qual.Regex;
import org.checkerframework.common.value.qual.MinLen;

/**
 * This class has method {@link #compileFiles(List)} that compiles Java source files. It invokes a
 * user-specified external command, such as {@code javac} or {@code jikes}.
 */
public final class FileCompiler {

  /** The Runtime of the JVM. */
  public static Runtime runtime = java.lang.Runtime.getRuntime();

  /**
   * Matches javac error and warning messages. Match group 1 is the complete filename of the Java
   * source file that contains the error or warning, and match group 2 is "error" or "warning".
   */
  static @Regex(2) Pattern java_filename_pattern;

  /**
   * Matches the first line of a javac error message that is not about any one file, such as an
   * invalid command-line option.
   */
  private static final Pattern javac_file_independent_error_pattern =
      Pattern.compile("(?m)^error: ");

  /**
   * External command used to compile Java files, and command-line arguments. Guaranteed to be
   * non-empty.
   */
  private String @MinLen(1) [] compiler;

  /** Time limit for compilation jobs. */
  private long timeLimit;

  static {
    try {
      @Regex(2) String java_filename_re
          // A javac error message may consist of several lines of output.
          // The first line has the form "FILENAME.java:LINENUMBER: error: MESSAGE";
          // the additional lines of information do not.  A warning has the form
          // "FILENAME.java:LINENUMBER: warning: MESSAGE".
          // (?m) turns on MULTILINE mode so "^" matches the start of each
          // line output by javac.  The filename may contain spaces, but it
          // does not start with whitespace.
          = "(?m)^(\\S.*?\\.java):[0-9]+: (error|warning):";
      java_filename_pattern = Pattern.compile(java_filename_re);
    } catch (PatternSyntaxException me) {
      me.printStackTrace();
      throw new Error("Error in regexp", me);
    }
  }

  /**
   * Creates a new FileCompiler. Compared to {@link #FileCompiler(String,long)}, this constructor
   * permits spaces and other special characters in the command and arguments.
   *
   * @param compiler an array of Strings representing a command that runs a Java compiler (it could
   *     be the full path name or whatever is used on the commandline), plus any command-line
   *     options
   * @param timeLimit the maximum permitted compilation time, in msec
   */
  public FileCompiler(String @MinLen(1) [] compiler, @Positive long timeLimit) {
    if (compiler.length == 0) {
      throw new Error("no compile command was provided");
    }

    this.compiler = compiler;
    this.timeLimit = timeLimit;
  }

  /**
   * Creates a new FileCompiler. Compared to {@link #FileCompiler(String,long)}, this constructor
   * permits spaces and other special characters in the command and arguments.
   *
   * @param compiler a list of Strings representing a command that runs a Java compiler (it could be
   *     the full path name or whatever is used on the commandline), plus any command-line options
   * @param timeLimit the maximum permitted compilation time, in msec
   */
  @SuppressWarnings("value") // no index checker list support
  public FileCompiler(/*(at)MinLen(1)*/ List<String> compiler, @Positive long timeLimit) {
    this(compiler.toArray(new String[0]), timeLimit);
  }

  /**
   * Creates a new FileCompiler.
   *
   * @param compiler a command that runs a Java compiler; for instance, it could be the full path
   *     name or whatever is used on the commandline. It may contain command-line arguments, and is
   *     split on spaces.
   * @param timeLimit the maximum permitted compilation time, in msec
   */
  public FileCompiler(String compiler, @Positive long timeLimit) {
    this(compiler.trim().split(" +"), timeLimit);
  }

  /**
   * Compiles the files given by fileNames. Returns the error output.
   *
   * <p>Every file that the compiler can compile on its own is compiled, even if other files have
   * compilation errors. This assumes that the files do not depend on one another, that the compiler
   * writes each class file next to its source file, and that no class file for any of the files
   * exists before this method is called. (A file whose class file exists is treated as compiled.)
   *
   * <p>Each error is reported once. A warning about a file that is compiled again, because another
   * file had an error, may be reported more than once.
   *
   * @param fileNames paths to the files to be compiled as Strings
   * @return the error output from compiling the files
   * @throws IOException if there is a problem reading a file
   */
  public String compileFiles(List<String> fileNames) throws IOException {

    // System.out.printf("compileFiles: %s%n", fileNames);

    String compile_errors = "";

    // javac compiles no file at all if any of its arguments does not exist.
    List<String> candidates = new ArrayList<>();
    List<String> missingFiles = new ArrayList<>();
    for (String sourceFileName : fileNames) {
      if (fileExists(sourceFileName)) {
        candidates.add(sourceFileName);
      } else {
        missingFiles.add(sourceFileName);
      }
    }

    // javac compiles nothing if any file has an error.  Furthermore, javac reports errors in
    // phases:  for example, after a syntax error in one file, it does not report type errors in
    // other files.  Therefore, repeatedly compile the files that have neither been compiled nor
    // been reported to have errors, until compilation succeeds or makes no progress.
    while (!candidates.isEmpty()) {
      CompileResult result = compile_source(candidates);
      // Even a successful recompilation may report new warnings:  after a syntax error, javac
      // does not run the later phases that report warnings such as unchecked conversions.
      String output = withoutCommonPrefix(compile_errors, result);
      if (!result.failed) {
        compile_errors = appendWithLineSeparator(compile_errors, output);
        break;
      }
      // javac writes its diagnostics to standard error, so only standard error is searched for the
      // names of files with errors.
      Set<String> errorFiles = filesWithErrors(result.stderr);
      List<String> retry = filesToRetry(candidates, errorFiles);
      if (retry.size() == candidates.size()) {
        // No progress:  no file was compiled, and the output did not identify any file that has
        // errors (perhaps the compiler is not javac).  Compile each file on its own, unless the
        // error is not about any one file, in which case it would recur for every file.
        if (candidates.size() > 1
            && !javac_file_independent_error_pattern.matcher(result.stderr).find()) {
          // The compilation of each file on its own reports that file's errors, so discard
          // output, which would duplicate them.
          for (String sourceFileName : candidates) {
            CompileResult singleResult = compile_source(Collections.singletonList(sourceFileName));
            compile_errors =
                appendWithLineSeparator(
                    compile_errors, withoutCommonPrefix(compile_errors, singleResult));
          }
        } else {
          compile_errors = appendWithLineSeparator(compile_errors, output);
        }
        break;
      }
      // If no file was identified as having errors, then every error in output is about a file in
      // retry, which will be compiled again.  Discard output, which would duplicate those errors.
      if (!errorFiles.isEmpty()) {
        compile_errors = appendWithLineSeparator(compile_errors, output);
      }
      candidates = retry;
    }

    for (String missingFile : missingFiles) {
      compile_errors =
          appendWithLineSeparator(compile_errors, "error: file not found: " + missingFile);
    }

    return compile_errors;
  }

  /**
   * Returns the lines of the error output of {@code result}, without any leading lines that also
   * start {@code previousOutput}. Such lines are typically warnings about command-line options,
   * which the compiler issues every time it runs.
   *
   * @param previousOutput the error output of an earlier compilation
   * @param result the result of a later compilation
   * @return the error output of {@code result}, without the leading lines it shares with {@code
   *     previousOutput}
   */
  private static String withoutCommonPrefix(String previousOutput, CompileResult result) {
    String[] previousLines = previousOutput.split("\\R", -1);
    String[] lines = result.errorOutput().split("\\R", -1);
    int common = 0;
    while (common < previousLines.length
        && common < lines.length
        && lines[common].equals(previousLines[common])) {
      common++;
    }
    return String.join(System.lineSeparator(), Arrays.asList(lines).subList(common, lines.length));
  }

  /**
   * Returns the concatenation of {@code text} and {@code addition}, separated by a line separator
   * if {@code text} is non-empty and does not end with one.
   *
   * @param text some text
   * @param addition text to append
   * @return the concatenation of {@code text} and {@code addition}
   */
  private static String appendWithLineSeparator(String text, String addition) {
    if (addition.isEmpty()) {
      return text;
    }
    if (!text.isEmpty() && !text.endsWith("\n")) {
      text += System.lineSeparator();
    }
    return text + addition;
  }

  /** The result of running the compiler once. */
  private static final class CompileResult {
    /** True if the compiler exited with a failure status. */
    final boolean failed;

    /** The standard error of the compiler. */
    final String stderr;

    /** The standard output of the compiler. */
    final String stdout;

    /**
     * Creates a new CompileResult.
     *
     * @param failed true if the compiler exited with a failure status
     * @param stderr the standard error of the compiler
     * @param stdout the standard output of the compiler
     */
    CompileResult(boolean failed, String stderr, String stdout) {
      this.failed = failed;
      this.stderr = stderr;
      this.stdout = stdout;
    }

    /**
     * Returns the error output of the compiler. Some compilers write diagnostics to standard output
     * rather than standard error, so this includes standard output if compilation failed. Standard
     * output is not an error if compilation succeeded; for example, it might be verbose output.
     *
     * @return the error output of the compiler
     */
    String errorOutput() {
      return failed ? appendWithLineSeparator(stderr, stdout) : stderr;
    }
  }

  /**
   * Compiles the given files.
   *
   * @param filenames the paths of the Java source to be compiled as Strings
   * @return the result of compiling the files
   * @throws Error if an empty list of filenames is provided
   */
  private CompileResult compile_source(List<String> filenames) throws IOException {
    /* Apache Commons Exec objects */
    CommandLine cmdLine;
    DefaultExecuteResultHandler resultHandler;
    DefaultExecutor executor;
    ExecuteWatchdog watchdog;
    ByteArrayOutputStream outStream;
    ByteArrayOutputStream errStream;
    PumpStreamHandler streamHandler;
    String compile_errors;
    String compile_output;

    if (filenames.isEmpty()) {
      throw new Error("no files to compile were provided");
    }

    cmdLine = new CommandLine(compiler[0]); // constructor requires executable name
    // add rest of compiler command arguments
    @NonNull String[] args = Arrays.copyOfRange(compiler, 1, compiler.length);
    cmdLine.addArguments(args);
    // Add file name arguments.  Do not quote them:  Commons Exec would pass the quotes to javac as
    // part of a file name that contains a space.
    cmdLine.addArguments(filenames.toArray(new String[0]), false);

    resultHandler = new DefaultExecuteResultHandler();
    executor = DefaultExecutor.builder().get();
    watchdog = ExecuteWatchdog.builder().setTimeout(Duration.ofMillis(timeLimit)).get();
    executor.setWatchdog(watchdog);
    outStream = new ByteArrayOutputStream();
    errStream = new ByteArrayOutputStream();
    streamHandler = new PumpStreamHandler(outStream, errStream);
    executor.setStreamHandler(streamHandler);

    // System.out.println(); System.out.println("executing compile command: " + cmdLine);
    try {
      executor.execute(cmdLine, resultHandler);
    } catch (IOException e) {
      throw new UncheckedIOException("exception starting process: " + cmdLine, e);
    }

    int exitValue = -1;
    try {
      resultHandler.waitFor();
      exitValue = resultHandler.getExitValue();
    } catch (InterruptedException e) {
      // Ignore exception, but watchdog.killedProcess() records that the process timed out.
    }
    boolean timedOut = executor.isFailure(exitValue) && watchdog.killedProcess();

    try {
      @SuppressWarnings("DefaultCharset") // toString(Charset) was introduced in Java 10
      String compile_errors_tmp = errStream.toString();
      compile_errors = compile_errors_tmp;
    } catch (RuntimeException e) {
      throw new Error("Exception getting process error output", e);
    }

    try {
      @SuppressWarnings("DefaultCharset") // toString(Charset) was introduced in Java 10
      String compile_output_tmp = outStream.toString();
      compile_output = compile_output_tmp;
    } catch (RuntimeException e) {
      throw new Error("Exception getting process standard output", e);
    }

    if (timedOut) {
      // Print stderr and stdout if there is an unexpected exception (timeout).
      System.out.println("Compile timed out after " + timeLimit + " msecs");
      // System.out.println ("Compile errors: " + compile_errors);
      // System.out.println ("Compile output: " + compile_output);
      ExecuteException e = resultHandler.getException();
      if (e != null) {
        e.printStackTrace();
      }
      runtime.exit(1);
    }
    return new CompileResult(executor.isFailure(exitValue), compile_errors, compile_output);
  }

  /**
   * Returns the files in which {@code errorString} reports an error. This is necessary when
   * compiling with javac because javac does not compile any of the files supplied to it if some of
   * them contain errors. So some "good" files end up not being compiled.
   *
   * <p>A file with only warnings can be compiled, so it is not returned, unless {@code errorString}
   * reports no error in any file. In that case, the warnings caused the failure (for example,
   * because javac was run with {@code -Werror}), and the files with warnings are returned.
   *
   * <p>Recognizes only javac's messages, and only those that are in English.
   *
   * @param errorString the error output of javac
   * @return the normalized paths of the files with errors
   */
  private static Set<String> filesWithErrors(String errorString) {
    Set<String> filesWithErrors = new HashSet<>();
    Set<String> filesWithWarnings = new HashSet<>();
    Matcher m = java_filename_pattern.matcher(errorString);
    while (m.find()) {
      @SuppressWarnings(
          "nullness") // Regex Checker imprecision: find() guarantees that group 1 exists
      @NonNull String fileName = m.group(1);
      String path = normalizePath(fileName);
      if ("error".equals(m.group(2))) {
        filesWithErrors.add(path);
      } else {
        filesWithWarnings.add(path);
      }
    }
    return filesWithErrors.isEmpty() ? filesWithWarnings : filesWithErrors;
  }

  /**
   * Returns the files that were not compiled and that contain no known error.
   *
   * @param fileNames all the files that were attempted to be compiled
   * @param errorFiles the normalized paths of files with errors
   * @return the elements of {@code fileNames} that should be recompiled
   */
  private static List<String> filesToRetry(List<String> fileNames, Set<String> errorFiles) {
    List<String> retry = new ArrayList<>();
    for (String sourceFileName : fileNames) {
      sourceFileName = sourceFileName.trim();
      String classFilePath = getClassFilePath(sourceFileName);
      if (!fileExists(classFilePath) && !errorFiles.contains(normalizePath(sourceFileName))) {
        retry.add(sourceFileName);
      }
    }
    return retry;
  }

  /**
   * Returns a canonical form of the given path, so that different spellings of the same file
   * compare equal.
   *
   * @param path a file path
   * @return a normalized absolute form of the path, or the path itself if it is malformed
   */
  private static String normalizePath(String path) {
    try {
      return Path.of(path).toAbsolutePath().normalize().toString();
    } catch (InvalidPathException e) {
      return path;
    }
  }

  /**
   * Returns the file path to where a class file for a source file at sourceFilePath would be
   * generated.
   *
   * @param sourceFilePath the path to the .java file
   * @return the path to the corresponding .class file
   */
  private static String getClassFilePath(String sourceFilePath) {
    int index = sourceFilePath.lastIndexOf('.');
    if (index == -1) {
      throw new IllegalArgumentException(
          "sourceFilePath: " + sourceFilePath + " must end with an extension.");
    }
    return sourceFilePath.substring(0, index) + ".class";
  }

  /**
   * Returns true if the given file exists.
   *
   * @param pathName path to check for existence
   * @return true iff the file exists
   */
  private static boolean fileExists(String pathName) {
    return new File(pathName).exists();
  }
}
