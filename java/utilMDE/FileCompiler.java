package utilMDE;

import java.io.*;
import java.util.*;
import java.util.regex.*;

/**
 * This class has a method compile_source which can be used to compile Java
 * source.  It invokes a user-specified external command, such as
 * <tt>javac</tt> or <tt>jikes</tt>.
 **/
public final class FileCompiler {

  public static Runtime runtime = java.lang.Runtime.getRuntime();
  /**
   * Matches the names of Java source files, without directory name.
   * Match group 1 is the class name:  the basse filename without the
   * ".java" extension..
   **/
  static Pattern java_filename_pattern;
  /** External command used to compile Java files. **/
  private String compiler;
  private long timeLimit;

  static {
    try {
      java_filename_pattern
        = Pattern.compile("([^" + UtilMDE.escapeNonJava(File.separator)
                          + "]+)\\.java");
    } catch (PatternSyntaxException me) {
      me.printStackTrace();
      throw new Error("Error in regexp", me);
    }
  }


  /**
   * Creates a new FileCompiler.  Equivalent to FileCompiler("javac", 6000).
   * @see #FileCompiler(String, long)
   **/
  public FileCompiler() {
    this("javac", 6000);
  }

  /**
   * Creates a new FileCompiler.
   * @param compiler A command that runs a Java compiler; for instance, it
   * could be the full path name or whatever is used on the commandline.
   * @param timeLimit The maximum permitted compilation time, in msec.
   **/
  public FileCompiler(String compiler, long timeLimit) {
    this.compiler = compiler;
    this.timeLimit = timeLimit;
  }

  /**
   * Compiles the files given by fileNames.
   * @param fileNames pathes to the files to be compiled as Strings.
   */
  public void compileFiles(List<String> fileNames) throws IOException {

    // Start a process to compile all of the files (in one command)
    TimeLimitProcess p = compile_source (fileNames);

    String compile_errors = "";
    String compile_output = "";

    // Read stderr and stdout (if any) while waiting for the process to
    // complete.  Print both if there is an unexpected exception (timeout)
    try {
      compile_errors = UtilMDE.streamString (p.getErrorStream());
      compile_output = UtilMDE.streamString (p.getInputStream());
      int result = p.waitFor();
    } catch (Throwable e) {
      System.out.println ("Unexpected exception while compiling " + e);
      if (p.timed_out())
        System.out.println ("Compile timed out after " + p.timeout_msecs()
                            + " msecs");
      System.out.println ("Compile errors: " + compile_errors);
      System.out.println ("Compile output: " + compile_output);
      e.printStackTrace();
      runtime.exit (1);
    }

    // javac tends to stop without completing the compilation if there
    // is an error in one of the files.  Remove all the erring files
    // and recompile only the good ones.
    if (compiler.equals("javac")) {
      recompile_without_errors (fileNames, compile_errors);
    }

  }

  /**
   * @param filename the path of the Java source to be compiled
   **/
  private TimeLimitProcess compile_source(String filename) throws IOException {
    String command = compiler + " " + filename;
    // System.out.println ("\nexecuting compile command: " + command);

    return new TimeLimitProcess(runtime.exec(command), timeLimit);
  }

  /**
   * @param filenames the paths of the java source to be compiled as Strings.
   * @return The process that executed the external compile command.
   * @throws Error if an empty list of filenames is provided.
   **/
  private TimeLimitProcess compile_source(List<String> filenames) throws IOException {
    int num_files = filenames.size();

    if (num_files == 0) {
      throw new Error("no files to compile were provided");
    }

    String to_compile = filenames.get(0);
    for (int i = 1; i < num_files; i++) {
      to_compile += (" " + filenames.get(i));
    }

    String command = compiler + " " + to_compile;
    // System.out.println ("\nexecuting compile command: " + command);
    return new TimeLimitProcess(runtime.exec(command), timeLimit);
  }

  /**
   * Examine the errorString to identify the files that cannot
   * compile, then recompile all the other files. This function is
   * necessary when compiling with javac because javac does not
   * compile all the files supplied to it if some of them contain
   * errors. So some "good" files end up not being compiled.
   */
  private void recompile_without_errors (List<String> fileNames, String errorString) throws IOException {
    // search the error string and extract the files with errors.
    if (errorString != null) {
      HashSet<String> errorClasses = new HashSet<String>();
      Matcher m = java_filename_pattern.matcher(errorString);
      while (m.find()) {
        @SuppressWarnings("nullness") // group 1 always matches in regexp
        /*@NonNull*/ String sansExtension = m.group(1);
        errorClasses.add(sansExtension);
      }
      // Collect all the files that were not compiled into retry
      List<String> retry = new ArrayList<String>();
      String filenames = "";
      for (String sourceFileName : fileNames) {
        sourceFileName = sourceFileName.trim();
        String classFilePath = getClassFilePath(sourceFileName);
        if (! fileExists(classFilePath)) {
          if (! errorClasses.contains(getClassName(sourceFileName))) {
            retry.add(sourceFileName);
            filenames += " " + sourceFileName;
          }
        }
      }

      if (retry.size() > 0) {
        TimeLimitProcess tp = compile_source(retry);

        try {
          tp.waitFor();
        } catch (InterruptedException e) {
          System.out.println ("Compile of " + filenames + " interrupted: "
                              + e);
        }
      }
    }
  }


  /**
   * Return the file path to where a class file for a source
   * file at sourceFilePath would be generated.
   */
  private static String getClassFilePath(String sourceFilePath) {
    int index = sourceFilePath.lastIndexOf('.');
    if (index == -1) {
      throw new IllegalArgumentException("sourceFilePath: "
                                         + sourceFilePath +
                                         " must end with an extention.");
    }
    String classFilePath = sourceFilePath.substring(0, index);
    classFilePath = classFilePath + ".class";
    return classFilePath;
  }

  /**
   * Returns the name of the class that sourceFilePath is
   * a path for.
   */
  private static String getClassName(String sourceFilePath) {
    int dotIndex = sourceFilePath.lastIndexOf('.');
    int slashIndex = sourceFilePath.lastIndexOf(File.separator);
    if (dotIndex == -1) {
        throw new IllegalArgumentException("sourceFilePath: " +
                                           sourceFilePath +
                                           " must end with an extention.");
    } else if (slashIndex == -1) {
      slashIndex = 0;
    }
    if (dotIndex < slashIndex) {
      throw new IllegalArgumentException("sourceFilePath: " +
                                         sourceFilePath +
                                         " must end with an extention.");
    }
    return sourceFilePath.substring(slashIndex + 1, dotIndex);
  }

  private static boolean fileExists(String pathName) {
    return (new File(pathName)).exists();
  }

}
