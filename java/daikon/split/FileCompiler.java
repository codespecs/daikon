package daikon.split;

import java.io.*;
import java.util.*;

/**
 * This class has a method compile_source which can be used to compile Java source.
 * It invokes the external command javac/jikes
 **/

public final class FileCompiler {

  public static Runtime commander = java.lang.Runtime.getRuntime();

  /**
   * String.  Specifies which Java compiler is used to compile
   * Splitters.  This can be the full path name or whatever is used on
   * the commandline.
   **/
  public static String dkconfig_compiler = "jikes";

  /**
   * @param The path of the java source to be compiled
   * @return The process which executed the external compile command
   **/
  public static TimedProcess compile_source(String filename){
    String command = dkconfig_compiler + " " + filename;
    try {
      return new TimedProcess(commander.exec(command), command);
    } catch (IOException e) {
      System.err.println("IOException while compiling " + filename + "\n" + e.toString());
    }
    return null;
  }

  /**
   * @param The path of the java source to be compiled
   * @return The process which executed the external compile command
   **/
  static TimedProcess compile_source(List filenames) {
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
        System.err.println("IOException while compiling files \n" + e.toString());
      }
    }
    return null;
  }
}
