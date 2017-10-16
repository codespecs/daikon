package daikon.split;

import daikon.Daikon;
import daikon.inv.*;
import java.io.*;
import java.lang.reflect.InvocationTargetException;
import plume.*;

/*>>>
import org.checkerframework.checker.lock.qual.*;
import org.checkerframework.checker.nullness.qual.*;
import org.checkerframework.checker.signature.qual.*;
import org.checkerframework.dataflow.qual.*;
import typequals.*;
*/

/**
 * A SplitterObject is the starting point for all the information we have about a splitting
 * condition. It is created immediately when the condition is read from the {@code .spinfo} file,
 * and later contains a reference to the compiled "Splitter" object.
 */
public class SplitterObject implements Comparable<SplitterObject> {

  private /*@MonotonicNonNull*/ Splitter splitter;
  private String condition; // the condition
  private /*@BinaryName*/ String className = "Unassigned"; // the Java classname of this Splitter
  private String directory; // the directory where it resides
  private String pptName; // the program point with which it is associated
  private boolean exists = false;
  private String testString = "Unassigned";
  // Not necessarily an error message -- really just a status message.
  private String errorMessage = "Splitter not yet loaded";
  private int guid = -999; // -999 indicates not yet set
  /** class file containing compiled code for this splitter */
  private /*@MonotonicNonNull*/ File classFile;

  public boolean dummyDesired = false;
  public /*@Nullable*/ String daikonFormat = null;
  public /*@Nullable*/ String javaFormat = null;
  public /*@Nullable*/ String escFormat = null;
  public /*@Nullable*/ String simplifyFormat = null;
  public /*@Nullable*/ String jmlFormat = null;
  public /*@Nullable*/ String dbcFormat = null;
  public /*@Nullable*/ String csharpFormat = null;

  /**
   * @param condition the splitting condition of this splitter
   * @param directory the directory where the source of this splitter is located
   */
  public SplitterObject(String pptName, String condition, String directory) {
    this.condition = condition;
    this.pptName = pptName;
    this.directory = directory;
    this.javaFormat = condition;
    this.daikonFormat = condition;
    this.csharpFormat = condition;
  }

  /**
   * @param fileName the pathname of a {@code .class} file
   * @return a Java Class corresponding to the {@code .class} file, or null
   */
  static /*@Nullable*/ Class<?> defineSplitterClass(
      /*@BinaryName*/ String className, String fileName) {
    try {
      return UtilMDE.defineClassFromFile(className, fileName);
    } catch (FileNotFoundException e) {
      if (!PptSplitter.dkconfig_suppressSplitterErrors) {
        System.out.println(
            "File " + fileName.substring(0, fileName.length() - 6) + ".java did not compile");
      }
      return null;
    } catch (IOException ioe) {
      System.out.println("IO Error while reading class data " + fileName);
      return null;
    } catch (UnsupportedClassVersionError ucve) { // should be more general?
      throw new Daikon.TerminationMessage(
          "Wrong Java version while reading file "
              + fileName
              + ": "
              + ucve.getMessage()
              + "\n"
              + "This indicates a possible problem with configuration option\ndaikon.split.SplitterFactory.compiler whose value is: "
              + SplitterFactory.dkconfig_compiler);
    }
  }

  /** Sets the "splitter" field of this object to a newly-instantiated object. */
  public void load() {
    Class<?> tempClass = defineSplitterClass(className, directory + className + ".class");
    if (tempClass != null) {
      try {
        splitter = (Splitter) tempClass.getDeclaredConstructor().newInstance();
      } catch (ClassFormatError
          | IllegalAccessException
          | InstantiationException
          | InvocationTargetException
          | NoSuchMethodException e) {
        e.printStackTrace(System.out);
        throw new Error("Trying to invoke " + tempClass + " constructor", e);
      }
      DummyInvariant dummy =
          new /*@Prototype*/ DummyInvariant(
              daikonFormat,
              javaFormat,
              escFormat,
              simplifyFormat,
              jmlFormat,
              dbcFormat,
              csharpFormat,
              dummyDesired);
      splitter.makeDummyInvariantFactory(dummy);
      errorMessage = "Splitter exists " + this.toString();
      exists = true;
    } else {
      errorMessage =
          "\nNo class data for "
              + this.toString()
              + ", to be loaded from "
              + directory
              + className
              + ".class";
      exists = false;
    }
  }

  /**
   * @return true if the Splitter object exists for this SplitterObject, i.e. whether it
   *     successfully loaded.
   */
  public boolean splitterExists() {
    return exists;
  }

  /**
   * @return true if the {@code .class} file exists for the Splitter represented by this
   *     SplitterObject, false otherwise
   */
  public boolean compiled() {
    if (classFile != null && classFile.exists()) {
      errorMessage = "Splitter exists " + this.toString();
      return true;
    }
    return false;
  }

  /**
   * @return the Splitter that this SplitterObject represents. Null if splitterExists() == false.
   */
  public /*@Nullable*/ Splitter getSplitter() {
    return this.splitter;
  }

  /**
   * Set the error message of this this SplitterObject. This indicates the status of the Splitter.
   */
  public void setError(String errorMessage) {
    this.errorMessage = errorMessage;
  }

  /** Get the error message of this SplitterObject. */
  public String getError() {
    return this.errorMessage;
  }

  /** Set the unique ID of this splitterObject. */
  public void setGUID(int ID) {
    this.guid = ID;
  }

  /** Return the unique ID of this splitterObject. */
  public int getGUID() {
    return this.guid;
  }

  /** @return the full source of the Splitter */
  public String getFullSourcePath() {
    return (directory + className + ".java");
  }

  /** @return the program point represented by this Splitter */
  public String getPptName() {
    return this.pptName;
  }

  /** Set the className of this Splitter. */
  public void setClassName(/*@BinaryName*/ String className) {
    this.className = className;
    classFile = new File(directory + className + ".class");
  }

  /** @return the className of the Splitter */
  public /*@BinaryName*/ String getClassName() {
    return this.className;
  }

  public void setDirectory(String directory) {
    this.directory = directory;
  }

  public String getDirectory() {
    return this.directory;
  }

  /** @return the condition represented by the Splitter */
  public String condition() {
    return this.condition;
  }

  public void setTestString(String testString) {
    this.testString = testString;
  }

  public String getTestString() {
    return this.testString;
  }

  /*@SideEffectFree*/
  @Override
  public String toString(/*>>>@GuardSatisfied SplitterObject this*/) {
    return (className
        + ": "
        + "condition: "
        + condition
        + ", testString: "
        + testString
        + ", @ "
        + pptName);
  }

  /*@Pure*/
  @Override
  public int compareTo(/*>>>@GuardSatisfied SplitterObject this,*/ SplitterObject o) {
    return this.guid - o.getGUID();
  }
}
