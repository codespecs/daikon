package daikon.split;

import daikon.*;
import daikon.split.*;
import java.io.*;
import java.util.*;


/**
 * A SplitterObject is a representation of a Splitter. It holds all the information about a splitter. 
 **/
public class SplitterObject {
  
  private Splitter splitter;
  private String condition; //the condition
  private String className = "Unassigned"; //the Java classname of this Splitter
  private String directory; //the directory where it resides
  private String pptName; //the program point with which it is associated
  private boolean compiled = false; 
  private String testString = "Unassigned";
  private String errorMessage = "Splitter for " + this.condition + " valid";
  
  /**
   * @param condition The splitting condition of this splitter
   * @param directory The directory where the source of this splitter is located.
   */
  public SplitterObject (String pptName, String condition, String directory) {
    this.condition = condition;
    this.pptName = pptName;
    this.directory = directory;
  }
  
  /**
   * @param loader The SplitterLoader used to load the compiled source.
   * @requires loader is not null.
   */
  public void load (SplitterLoader loader) {
    Class tempClass = loader.load_Class(className, directory + className + ".class");
    if (tempClass != null) {
      try {
	splitter = (Splitter) tempClass.newInstance();
      } catch (ClassFormatError ce) {
	debugPrint(ce.toString());
      } catch (InstantiationException ie) {
	debugPrint(ie.toString());
      } catch (IllegalAccessException iae) {
	debugPrint(iae.toString());
      }
      compiled = true;
    } else {
      errorMessage = "No class data for " + this.toString();
      compiled = false;
    }
  }

  /**
   * @return returns true if a .class file exists for this Splitter.
   * This means that the Splitter successfully compiled. false otherwise.
   */
  public boolean splitterExists() {
    return compiled;
  }
  
  /**
   * @return the Splitter that this SplitterObject represents. Null if 
   * splitterExists() == false
   */
  public Splitter getSplitter() {
    return this.splitter;
  }
  
  /**
   * set the error message of this this SplitterObject. This indicates the status of
   * the Splitter.
   */
  public void setError(String errorString) {
    this.errorMessage = errorString;
  }
  
  /**
   * get the error message of this SplitterObject.
   */
  public String getError () {
    return this.errorMessage;
  }
  
  /**
   * @return the full source of the Splitter.
   */
  public String getFullSourcePath () {
    return (directory + className + ".java");
  }

  /**
   * @return the program point represented by this Splitter.
   */
  public String getPptName () {
    return this.pptName;
  }
  
  /**
   * @return the className of the Splitter
   */
  public String getClassName() {
    return this.className;
  }
  
  /**
   * set the className of this Splitter
   */
  public void setClassName(String className) {
    this.className = className;
  }
  
  public void setDirectory (String directory) {
    this.directory = directory;
  }

  public String getDirectory () {
    return this.directory;
  }
  
  /**
   * @return the condition represented by the Splitter
   */
  public String condition () {
    return this.condition;
  }

  public void setTestString (String testString) {
    this.testString = testString;
  }

  public String getTestString() {
    return this.testString;
  }
  
  public void debugPrint(String s) {
    System.out.println(s);
  }

  public String toString() {
    return (className + ": " + "condition: " + condition + ", testString: " + testString 
	    + ", @ " + pptName);
  }
  
}
  
