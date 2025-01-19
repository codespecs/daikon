package daikon.split;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.time.Duration;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
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

  /** Matches the names of Java source files. Match group 1 is the complete filename. */
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
          // The filename will be found at the beginning of the first line,
          // the additional lines of information will all be indented.
          // (?m) turns on MULTILINE mode so the first "^" matches the
          // start of each error line output by javac. The blank space after
          // the second "^" is intentional; together with the first "^", this
          // says a filename can only be found at the start of a non-indented
          // line as noted above.
          = "(?m)^([^ ]+?\\.java)";
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
    String compile_errors = compile_source(fileNames);

    // javac tends to stop without completing the compilation if there
    // is an error in one of the files.  Remove all the erring files
    // and recompile only the good ones.
    if (compiler[0].indexOf("javac") != -1) {
      recompile_without_errors(fileNames, compile_errors);
    }

    return compile_errors;
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

    if (filenames.size() == 0) {
      throw new Error("no files to compile were provided");
    }

    cmdLine = new CommandLine(compiler[0]); // constructor requires executable name
    // add rest of compiler command arguments
    @SuppressWarnings("nullness") // arguments are in range, so result array contains no nulls
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
   * Examine the errorString to identify the files that cannot compile, then recompile all the other
   * files. This function is necessary when compiling with javac because javac does not compile all
   * the files supplied to it if some of them contain errors. So some "good" files end up not being
   * compiled.
   *
   * @param fileNames all the files that were attempted to be compiled
   * @param errorString the error string that indicates which files could not be compiled
   */
  private void recompile_without_errors(List<String> fileNames, String errorString)
      throws IOException {
    // search the error string and extract the files with errors.
    if (errorString != null) {
      HashSet<String> errorClasses = new HashSet<>();
      Matcher m = java_filename_pattern.matcher(errorString);
      while (m.find()) {
        @SuppressWarnings(
            "nullness") // Regex Checker imprecision: find() guarantees that group 1 exists
        @NonNull String sansExtension = m.group(1);
        errorClasses.add(sansExtension);
      }
      // Collect all the files that were not compiled into retry
      List<String> retry = new ArrayList<>();
      for (String sourceFileName : fileNames) {
        sourceFileName = sourceFileName.trim();
        String classFilePath = getClassFilePath(sourceFileName);
        if (!fileExists(classFilePath)) {
          if (!errorClasses.contains(sourceFileName)) {
            retry.add(sourceFileName);
          }
        }
      }

      if (retry.size() > 0) {
        compile_source(retry);
      }
    }
  }

  /**
   * Return the file path to where a class file for a source file at sourceFilePath would be
   * generated.
   *
   * @param sourceFilePath the path to the .java file
   * @return the path to the corresponding .class file
   */
  private static String getClassFilePath(String sourceFilePath) {
    int index = sourceFilePath.lastIndexOf('.');
    if (index == -1) {
      throw new IllegalArgumentException(
          "sourceFilePath: " + sourceFilePath + " must end with an extention.");
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
