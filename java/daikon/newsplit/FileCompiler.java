package daikon.newsplit;

import java.lang.*;
import java.io.*;

/**
 * This class has a method compile_source which can be used to compile Java source.
 * It invokes the external command javac
 **/
public final class FileCompiler{
  
  static Runtime commander = Runtime.getRuntime();
  /**
   * @param The path of the java source to be compiled
   * @return The process which executed the external compile command
   **/
  static Process compile_source(String filename){
    try {
      return commander.exec("javac " + filename);
    } catch (IOException e){
      System.err.println("IOException while compiling " + filename + "\n" + e.toString());
    }
    return null;
  }
}
