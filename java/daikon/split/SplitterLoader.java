package daikon.split;

import java.io.*;
import java.util.*;
import java.lang.reflect.*;

/**
 * Reads in and loads compiled Java source and returns a Java Object
 **/
public class SplitterLoader extends ClassLoader{

  /**
   * read in the bytes of the .class file
   **/
  private byte[] read_Class_Data(String fileName) {

    try {
      FileInputStream fi = new FileInputStream(fileName);
      byte[] classBytes= new byte[fi.available()];
      fi.read(classBytes);
      return classBytes;
    } catch (FileNotFoundException e){
      //do nothing. did not compile
    } catch (IOException ioe){
      System.out.println("IO Error while reading class data " + fileName);
    }
    return null;
  }

  /**
   * @param the pathname of a .class file
   * @return a Java Object corresponding to the .class file
   **/
  protected Class load_Class(String className, String full_pathname) {
    Class return_class;
    byte[] classData = read_Class_Data(full_pathname);
    if (classData == null){
      return null;
    } else {
      return_class = defineClass(className, classData, 0, classData.length);
      resolveClass(return_class);
    }
    return return_class;
  }

}
