package daikon.inv;
import daikon.*;
import utilMDE.Pair;
import java.util.*;

/**
 * Enumeration type for output style.
 * (Should this be somewhere else?)
 **/
public final class OutputFormat
{
  /** The standard, concise Daikon output format */
  public static final OutputFormat DAIKON = new OutputFormat("Daikon");
  /** Design-By-Contract for Java (used by Parasoft JContract) */
  public static final OutputFormat DBCJAVA = new OutputFormat("DBC");
  /** ESC/Java's annotation language */
  public static final OutputFormat ESCJAVA = new OutputFormat("ESC/Java");
    /** IOA language */
  public static final OutputFormat IOA = new OutputFormat("IOA");
  /** IOA language, sans invariant numbering */
  public static final OutputFormat IOATEST = new OutputFormat("IOA_test");
  /** Java boolean expression */
  public static final OutputFormat JAVA = new OutputFormat("Java");
  /** Java Modeling Language */
  public static final OutputFormat JML = new OutputFormat("JML");
  /** Simplify theorem prover */
  public static final OutputFormat SIMPLIFY = new OutputFormat("Simplify");
  /** Data Structure Repair Format */
  public static final OutputFormat REPAIR = new OutputFormat("Repair");

  /** Whole names as single C/Java style indentifiers **/
  public static final OutputFormat IDENTIFIER = new OutputFormat("Identifier");

  private final String name;
  public final String toString() { return "OutputFormat:" + name; }

  public boolean isJavaFamily() {
    return (this == DBCJAVA || this == JML ||  this == JAVA);
  }

  // Nobody should ever construct these
  private OutputFormat(String name) {
    this.name = name;
  }

  /**
   * Return the appropriate OutputFormat for the given name, or null
   * if no such OutputFormat exists.
   **/
  public static OutputFormat get(String name) {
    if (name == null) { return null; }
    if (name.compareToIgnoreCase(DAIKON.name) == 0) { return DAIKON; }
    if (name.compareToIgnoreCase(DBCJAVA.name) == 0) { return DBCJAVA; }
    if (name.compareToIgnoreCase(ESCJAVA.name) == 0) { return ESCJAVA; }
    if (name.compareToIgnoreCase("ESC") == 0) { return ESCJAVA; }
    if (name.compareToIgnoreCase(IOA.name) == 0) { return IOA; }
    if (name.compareToIgnoreCase(IOATEST.name) == 0) { return IOATEST; }
    if (name.compareToIgnoreCase(JAVA.name) == 0) { return JAVA; }
    if (name.compareToIgnoreCase(JML.name) == 0) { return JML; }
    if (name.compareToIgnoreCase(SIMPLIFY.name) == 0) { return SIMPLIFY; }
    if (name.compareToIgnoreCase(REPAIR.name) == 0) { return REPAIR; }
    if (name.compareToIgnoreCase(IDENTIFIER.name) == 0) { return IDENTIFIER; }
    return null;
  }

}
