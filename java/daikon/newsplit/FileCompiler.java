package daikon.newsplit;

import java.lang.*;
import java.io.*;

public final class FileCompiler{
  //compiles a java file called filename.
  //note: does not return any error yet, if the compile failed.
  //need to use the proc.getErrorStream() method to get errors, However
  //this will slow down everything if we are to wait for every compile
  //to finish before we carry on with the next one.
  
  static Runtime commander;
  static Process proc;
  
  public FileCompiler(){
    commander = Runtime.getRuntime();
  }
    
  public static Process compile_Class(String filename){
    try {
      proc = commander.exec("javac " + filename);
    } catch (IOException e){
      System.err.println("IOException while compiling " + filename);
    }
    return proc;
  }
}
