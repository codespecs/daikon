package daikon.split;

import java.io.*;
import java.util.*;
import org.apache.oro.text.regex.*;
import utilMDE.*;

/**
 * This class has a method compile_source which can be used to compile Java source.
 * It invokes the external command javac/jikes
 **/
public final class FileCompiler {

  public static Runtime commander = java.lang.Runtime.getRuntime();
  private static Perl5Matcher re_matcher = new Perl5Matcher();
  private static Perl5Compiler re_compiler = new Perl5Compiler();
  /** Matches the names of Java source files, without directory name. **/
  static Pattern splitter_classname_pattern;
  private static String lineSep = System.getProperty("line.separator");

  static {
    try {
      splitter_classname_pattern
        = re_compiler.compile("([^" + UtilMDE.escapeNonJava(File.separator)
                              + "]+)\\.java");
    } catch (MalformedPatternException me) {
      me.printStackTrace();
      throw new Error("Error in regexp: " + me.toString());
    }
  }

  /**
   * String.  Specifies which Java compiler is used to compile
   * Splitters.  This can be the full path name or whatever is used on
   * the commandline.
   **/
  public static String dkconfig_compiler = "jikes";


  /**
   * Compiles the files given by fileNames.
   * @param fileNames pathes to the files to be compiled as Strings.
   */
  public static void compileFiles(List fileNames) {
    List /*TimedProcess*/ processes = new ArrayList();
    processes.add(compile_source(fileNames));
    StringBuffer errorString = new StringBuffer(); // stores the error messages

    // Wait for all the compilation processes to terminate.
    for (int i = 0; i < processes.size(); i++) {
      TimedProcess tp = (TimedProcess) processes.get(i);
      errorString.append(lineSep);
      errorString.append(tp.getErrorMessage());
      if (!tp.finished()) {
        tp.waitFor();
      }
    }

    // javac tends to stop without completing the compilation if there
    // is an error in one of the files. Remove all the erring files
    // and recompile only the good ones.
    if (dkconfig_compiler.equals("javac")) {
      recompile_without_errors (fileNames, errorString.toString());
    }

  }

  /**
   * @param filename the path of the java source to be compiled
   * @return The process which executed the external compile command
   **/
  private static TimedProcess compile_source(String filename) {
    String command = dkconfig_compiler + " " + filename;
    try {
      return new TimedProcess(commander.exec(command), command);
    } catch (IOException e) {
      System.err.println("IOException while compiling " + filename + lineSep
                         + e.toString());
    }
    return null;
  }

  /**
   * @param filenames the paths of the java source to be compiled as Strings.
   * @return The process which executed the external compile command
   **/
  private static TimedProcess compile_source(List filenames) {
    int num_files = filenames.size();

    if (num_files > 0) {
      String to_compile = (String) filenames.get(0);
      for (int i = 1; i < num_files; i++) {
        to_compile += (" " + (String) filenames.get(i));
      }

      String command = dkconfig_compiler + " " + to_compile;
      try {
        return new TimedProcess( commander.exec(command), command);
      } catch (IOException e) {
        System.err.println("IOException while compiling files" + lineSep
                           + e.toString());
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
  private static void recompile_without_errors (List fileNames, String errorString) {
    // search the error string and extract the files with errors.
    if (errorString != null) {
      HashSet errors = new HashSet();
      PatternMatcherInput input = new PatternMatcherInput(errorString);
      while (re_matcher.contains(input, splitter_classname_pattern)) {
        MatchResult result = re_matcher.getMatch();
        errors.add(result.group(1));
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

      TimedProcess tp = FileCompiler.compile_source(retry);

      try {
        Thread.sleep(3000);
      } catch (InterruptedException ie) {
        ie.printStackTrace();
      }

      // We don't want to wait for the old process for too long. We
      // wait for a short time, kill the process and recompile the set
      // of files, removing the leading file
      if (tp != null && !tp.finished()) {
        tp.waitFor();
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
