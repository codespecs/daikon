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
   * Matches the first line of a javac error message. Match group 1 is the name of the file that
   * contains the error.
   */
  static @Regex(1) Pattern javac_error_pattern;

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
      @Regex(1) String javac_error_re
          // A javac error message may consist of several lines of output.
          // The first line has the form "FILENAME:LINENUMBER: error: MESSAGE".
          // Warnings, which have the form "FILENAME:LINENUMBER: warning: MESSAGE",
          // must not match, because a file with only warnings compiles.
          // (?m) turns on MULTILINE mode so "^" matches the start of each line.
          = "(?m)^(.+?\\.java):[0-9]+: error:";
      javac_error_pattern = Pattern.compile(javac_error_re);
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
   * @param fileNames paths to the files to be compiled as Strings
   * @return the error output from compiling the files
   * @throws IOException if there is a problem reading a file
   */
  public String compileFiles(List<String> fileNames) throws IOException {

    // System.out.printf("compileFiles: %s%n", fileNames);

    StringBuilder compile_errors = new StringBuilder();

    // A compiler may not produce class files for some good files if other files have errors.  For
    // example, javac stops after parsing if any file has a syntax error, so it does not report type
    // errors and does not produce any class files.  Therefore, repeatedly compile the files that
    // have neither been compiled nor been reported to have errors.

    // The files that have not been compiled and are not known to have errors.
    List<String> candidates = new ArrayList<>();
    for (String sourceFileName : fileNames) {
      if (fileExists(sourceFileName)) {
        candidates.add(sourceFileName);
      } else {
        // javac compiles no file at all if any file does not exist.
        compile_errors.append("error: file not found: " + sourceFileName + System.lineSeparator());
      }
    }
    while (!candidates.isEmpty()) {
      String round_errors = compile_source(candidates);
      Set<String> errorFiles = filesWithErrors(round_errors);
      List<String> remaining = new ArrayList<>();
      for (String sourceFileName : candidates) {
        if (!fileExists(getClassFilePath(sourceFileName))
            && !errorFiles.contains(normalizePath(sourceFileName))) {
          remaining.add(sourceFileName);
        }
      }
      if (remaining.size() == candidates.size()) {
        // No progress:  no file was compiled, and the error output did not identify any file that
        // has errors (perhaps the compiler is not javac).  Compile each file on its own, unless
        // the error is not about any one file, in which case it would recur for every file.
        if (remaining.size() > 1
            && !javac_file_independent_error_pattern.matcher(round_errors).find()) {
          // The compilation of each file on its own reports that file's errors, so discard
          // round_errors, which would duplicate them.
          for (String sourceFileName : remaining) {
            compile_errors.append(compile_source(Collections.singletonList(sourceFileName)));
          }
        } else {
          compile_errors.append(round_errors);
        }
        break;
      }
      // If no file was identified as having errors, then every error in round_errors is about a
      // file in remaining, which will be compiled again.  Discard round_errors, which would
      // duplicate those errors.
      if (!errorFiles.isEmpty()) {
        compile_errors.append(round_errors);
      }
      candidates = remaining;
    }

    return compile_errors.toString();
  }

  /**
   * Returns the error output from compiling the files.
   *
   * @param filenames the paths of the Java source to be compiled as Strings
   * @return the error output from compiling the files
   * @throws Error if an empty list of filenames is provided
   */
  private String compile_source(List<String> filenames) throws IOException {
    /* Apache Commons Exec objects */
    CommandLine cmdLine;
    DefaultExecuteResultHandler resultHandler;
    DefaultExecutor executor;
    ExecuteWatchdog watchdog;
    ByteArrayOutputStream outStream;
    ByteArrayOutputStream errStream;
    PumpStreamHandler streamHandler;
    String compile_errors;
    @SuppressWarnings("UnusedVariable") // for debugging
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
      String compile_output_tmp = errStream.toString();
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
    return compile_errors;
  }

  /**
   * Returns the files that javac reports as having errors. Recognizes only javac's error messages,
   * and only those that are in English.
   *
   * @param errorString the error output of the compiler
   * @return the normalized paths (see {@link #normalizePath}) of the files that have errors
   */
  private static Set<String> filesWithErrors(String errorString) {
    Set<String> result = new HashSet<>();
    Matcher m = javac_error_pattern.matcher(errorString);
    while (m.find()) {
      @SuppressWarnings(
          "nullness" // Regex Checker imprecision: find() guarantees that group 1 exists
      )
      @NonNull String fileName = m.group(1);
      try {
        result.add(normalizePath(fileName));
      } catch (InvalidPathException e) {
        // The match is not a file name.  Ignore it.
      }
    }
    return result;
  }

  /**
   * Returns an absolute, normalized version of the given path, so that different ways of writing
   * the same path compare as equal.
   *
   * @param path a path
   * @return an absolute, normalized version of the path
   * @throws InvalidPathException if the argument is not a valid path
   */
  private static String normalizePath(String path) {
    return Path.of(path).toAbsolutePath().normalize().toString();
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
