package daikon.newsplit;

import java.io.*;
import java.util.*;
import java.lang.reflect.*;

public class SplitterLoader extends ClassLoader{
  
  
  private byte[] read_Class_Data(String fileName) {
    
    try {
      FileInputStream fi = new FileInputStream(fileName);
      byte[] classBytes= new byte[fi.available()];
      fi.read(classBytes);
      return classBytes;
    }catch(FileNotFoundException e){
      System.out.println("Error reading splitter class data: " + fileName + " not found");
    }catch(IOException ioe){
      System.out.println("IO Error while reading class data " + fileName);
    }
    return null;
  }
  
  protected Class load_Class(String className, String full_pathname) {
    Class return_class;  
    try {
      //find the class if it's a system class
      return_class = super.findSystemClass(className);
    }catch(ClassNotFoundException cnf){ 
      byte[] classData = read_Class_Data(full_pathname);
      if (classData == null){
	return null;
      }else{
	className = "daikon.split." + className;
	return_class = defineClass(className, classData, 0, classData.length);
	resolveClass(return_class);
      }  
    } 
    return return_class;
  }
  
}










