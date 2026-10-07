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
   * Matches javac error messages, but not warnings. Match group 1 is the complete filename of the
   * Java source file that contains the error.
   */
  static @Regex(1) Pattern java_filename_pattern;

  /**
   * External command used to compile Java files, and command-line arguments. Guaranteed to be
   * non-empty.
   */
  private String @MinLen(1) [] compiler;

  /** Time limit for compilation jobs. */
  private long timeLimit;

  static {
    try {
      @Regex(1) String java_filename_re
          // A javac error message may consist of several lines of output.
          // The first line has the form "FILENAME.java:LINENUMBER: error: MESSAGE";
          // the additional lines of information do not.  A warning has the form
          // "FILENAME.java:LINENUMBER: warning: MESSAGE" and is not matched, because
          // a file with only warnings can be compiled.
          // (?m) turns on MULTILINE mode so "^" matches the start of each
          // line output by javac.  The filename may contain spaces, but it
          // does not start with whitespace.
          = "(?m)^(\\S.*?\\.java):[0-9]+: error:";
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
   * @param fileNames paths to the files to be compiled as Strings
   * @return the error output from compiling the files
   * @throws IOException if there is a problem reading a file
   */
  public String compileFiles(List<String> fileNames) throws IOException {

    // System.out.printf("compileFiles: %s%n", fileNames);

    // Start a process to compile all of the files (in one command)
    CompileResult result = compile_source(fileNames);
    String compile_errors = result.errorOutput();

    // javac tends to stop without completing the compilation if there
    // is an error in one of the files.  Remove all the erring files
    // and recompile only the good ones.  javac reports errors in phases:  for
    // example, after a syntax error it does not report type errors.  So, a
    // recompilation may reveal errors in other files, and recompilation is
    // repeated until it succeeds or no more files can be excluded.
    if (result.failed && compiler[0].indexOf("javac") != -1) {
      // The files in which javac has reported an error, in normalized form.
      Set<String> errorFiles = new HashSet<>();
      List<String> attempted = fileNames;
      while (result.failed) {
        // javac writes its diagnostics to standard error, so only standard error is searched for
        // the names of files with errors.
        addFilesWithErrors(result.stderr, errorFiles);
        List<String> retry = filesToRetry(attempted, errorFiles);
        // If no file was excluded, recompiling would only repeat the previous compilation.  That
        // happens, for example, if the failure is not attributable to any particular file.
        if (retry.isEmpty() || retry.size() == attempted.size()) {
          break;
        }
        result = compile_source(retry);
        attempted = retry;
        // If the recompilation succeeded, its output contains no errors, only warnings that were
        // already reported by an earlier compilation.
        if (result.failed) {
          compile_errors =
              appendWithLineSeparator(compile_errors, withoutCommonPrefix(compile_errors, result));
        }
      }
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
    // add file name arguments
    cmdLine.addArguments(filenames.toArray(new String[0]));

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
   * Adds to {@code errorFiles} the files in which {@code errorString} reports an error. This is
   * necessary when compiling with javac because javac does not compile any of the files supplied to
   * it if some of them contain errors. So some "good" files end up not being compiled.
   *
   * @param errorString the error output of javac
   * @param errorFiles the set of normalized paths of files with errors; is side-effected
   */
  private static void addFilesWithErrors(String errorString, Set<String> errorFiles) {
    Matcher m = java_filename_pattern.matcher(errorString);
    while (m.find()) {
      @SuppressWarnings(
          "nullness") // Regex Checker imprecision: find() guarantees that group 1 exists
      @NonNull String errorFileName = m.group(1);
      errorFiles.add(normalizePath(errorFileName));
    }
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
