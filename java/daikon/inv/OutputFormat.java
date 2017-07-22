package daikon.inv;

import daikon.*;
import java.util.*;

/*>>>
import org.checkerframework.checker.lock.qual.*;
import org.checkerframework.dataflow.qual.*;
*/

/** Enumeration type for output style. (Should this be somewhere else?) */
public enum OutputFormat {

  /** The standard, concise Daikon output format. Intended to be read by humans. */
  DAIKON("Daikon"),
  /** Design-By-Contract for Java (used by Parasoft JContract) */
  DBCJAVA("DBC") {
    @Override
    public String ensures_tag() {
      return "@post";
    }

    @Override
    public String requires_tag() {
      return "@pre";
    }
  },
  /** ESC/Java's annotation language -- a variant of JML. */
  ESCJAVA("ESC/Java"),
  /** Java boolean expression */
  JAVA("Java"),
  /** Java Modeling Language */
  JML("JML"),
  /** Simplify theorem prover. First order logical expressions, expressed in Lisp-style parens. */
  SIMPLIFY("Simplify"),
  /** C# Code Contract */
  CSHARPCONTRACT("CSharpContract");

  final String name;

  OutputFormat(String name) {
    this.name = name;
  }

  /*@SideEffectFree*/
  @Override
  public String toString(/*>>>@GuardSatisfied OutputFormat this*/) {
    return "OutputFormat:" + name;
  }

  /*@Pure*/
  public boolean isJavaFamily() {
    return (this == DBCJAVA || this == JML || this == JAVA);
  }

  // We define the get() method instead of using valueOf() because get()
  // can be case-sensitive, can permit alternative names, etc.  An enum
  // cannot override valueOf().
  /**
   * Return the appropriate OutputFormat for the given name, or throw an error if no such
   * OutputFormat exists.
   */
  public static OutputFormat get(String name) {
    // if (name == null) { return null; }
    if (name.compareToIgnoreCase(DAIKON.name) == 0) {
      return DAIKON;
    }
    if (name.compareToIgnoreCase(DBCJAVA.name) == 0) {
      return DBCJAVA;
    }
    if (name.compareToIgnoreCase(ESCJAVA.name) == 0) {
      return ESCJAVA;
    }
    if (name.compareToIgnoreCase("ESC") == 0) {
      return ESCJAVA;
    }
    if (name.compareToIgnoreCase(JAVA.name) == 0) {
      return JAVA;
    }
    if (name.compareToIgnoreCase(JML.name) == 0) {
      return JML;
    }
    if (name.compareToIgnoreCase(SIMPLIFY.name) == 0) {
      return SIMPLIFY;
    }
    if (name.compareToIgnoreCase(CSHARPCONTRACT.name) == 0) {
      return CSHARPCONTRACT;
    }
    // return null;
    throw new Error("Unknown OutputFormat " + name);
  }

  public String ensures_tag() {
    return "ensures";
  }

  public String requires_tag() {
    return "requires";
  }
}
