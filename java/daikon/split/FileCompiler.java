package daikon.split;

import java.lang.*;
import java.io.*;
import java.util.*;

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
  
  /**
   * @param The path of the java source to be compiled
   * @return The process which executed the external compile command
   **/
  static Process compile_source(Vector filenames){
    int num_files = filenames.size();
    if (num_files > 0) {
      String to_compile = (String) filenames.elementAt(0);
      for(int i = 1; i < num_files; i++) {
	to_compile += (" " + (String) filenames.elementAt(i));
      }
      try {
	return commander.exec("jikes " + to_compile);
      } catch (IOException e){
	System.err.println("IOException while compiling files \n" + e.toString());
      }
    }
    return null;
  }
}
