package daikon.test;

import java.io.*;
import java.util.*;
import junit.framework.*;

import daikon.*;

public class VarInfoNameTest
  extends TestCase
{

  // for convenience
  public static void main(String[] args) {
    junit.textui.TestRunner.run(new TestSuite(VarInfoNameTest.class));
  }

  public VarInfoNameTest(String name) {
    super(name);
  }

  public void testParse() { run("testParse"); }

  private void run(String name) {
    String file = "varInfoNameTest." + name;
    InputStream in = VarInfoNameTest.class.getResourceAsStream(file);
    VarInfoNameDriver.run(in, System.out);
  }

  // parsing
  // interning
  // *name()
  // object methods

  // Simple
  // Size
  // Function
  // TypeOf
  // Prestate
  // Poststate
  // Add
  // Elements
  // Subscript
  // Slice

  // ElementsFinder
  // Replacer
  // InorderFlattener
  // QuantifierVisitor
  // QuantHelper.format_esc
}
