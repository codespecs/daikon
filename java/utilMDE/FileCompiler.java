package utilMDE;

import java.io.*;
import java.util.*;
import java.util.regex.*;

/**
 * This class has a method compile_source which can be used to compile Java
 * source.  It invokes the external command javac/jikes.
 **/
public final class FileCompiler {

  public static Runtime runtime = java.lang.Runtime.getRuntime();
  /** Matches the names of Java source files, without directory name. **/
  static Pattern java_filename_pattern;
  private static String lineSep = System.getProperty("line.separator");
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
      throw new Error("Error in regexp: " + me.toString());
    }
  }


  /**
   * Creates a new FileCompiler.  Equivalent to FileCompiler("javac", 6000).
   * @see #FileCompiler(String)
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
  public void compileFiles(List fileNames) {
    List /*TimeLimitProcess*/ processes = new ArrayList();
    processes.add(compile_source(fileNames));
    StringBuffer errorString = new StringBuffer(); // stores the error messages

    // Wait for all the compilation processes to terminate.
    for (int i = 0; i < processes.size(); i++) {
      TimeLimitProcess tp = (TimeLimitProcess) processes.get(i);
      String theseErrors = "";
      try {
        theseErrors = UtilMDE.streamString(tp.getErrorStream());
        tp.waitFor();
      } catch (InterruptedException e) {
        // nothing to do
      }
      errorString.append(lineSep);
      errorString.append(theseErrors);
    }

    // javac tends to stop without completing the compilation if there
    // is an error in one of the files.  Remove all the erring files
    // and recompile only the good ones.
    if (compiler.equals("javac")) {
      recompile_without_errors (fileNames, errorString.toString());
    }

  }

  /**
   * @param filename the path of the java source to be compiled
   * @return The process which executed the external compile command
   **/
  private TimeLimitProcess compile_source(String filename) {
    String command = compiler + " " + filename;
    try {
      return new TimeLimitProcess(runtime.exec(command), timeLimit);
    } catch (IOException e) {
      System.err.println("IOException while compiling " + filename);
      System.err.println(e.toString());
    }
    return null;
  }

  /**
   * @param filenames the paths of the java source to be compiled as Strings.
   * @return The process that executed the external compile command
   **/
  private TimeLimitProcess compile_source(List filenames) {
    int num_files = filenames.size();

    if (num_files > 0) {
      String to_compile = (String) filenames.get(0);
      for (int i = 1; i < num_files; i++) {
        to_compile += (" " + (String) filenames.get(i));
      }

      String command = compiler + " " + to_compile;
      try {
        return new TimeLimitProcess(runtime.exec(command), timeLimit);
      } catch (IOException e) {
        System.err.println("IOException while compiling files");
        System.err.println(e.toString());
      }
    }
    return null;
  }

  /**
   * Examine the errorString to identify the files that cannot
   * compile, then recompile all the other files. This function is
   * necessary when compiling with javac because javac does not
   * compile all the files supplied to it if some of them contain
   * errors. So some "good" files end up not being compiled.
   */
  private void recompile_without_errors (List fileNames, String errorString) {
    // search the error string and extract the files with errors.
    if (errorString != null) {
      HashSet errors = new HashSet();
      Matcher m = java_filename_pattern.matcher(errorString);
      while (m.find()) {
        errors.add(m.group(1));
      }
      List /*String*/ retry = new ArrayList();
      // Collect all the files that were not compiled
        for (int i = 0; i < fileNames.size(); i++) {
          String sourceFileName = ((String) fileNames.get(i)).trim();
          String classFilePath = getClassFilePath(sourceFileName);
          if (! fileExists(classFilePath)) {
            if (! errors.contains(getClassName(sourceFileName))) {
              retry.add(sourceFileName);
            }
          }
        }

      TimeLimitProcess tp = compile_source(retry);

      try {
        tp.waitFor();
      } catch (InterruptedException e) {
        // nothing to do (?)
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
