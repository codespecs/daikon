package javautil;

import junit.framework.*;

// This is just like VectorTest, but it calls the tests in a deterministic
// order, so that the traces can be compared.
public class VectorTestDeterministic extends TestCase {

  public static void main(String[] args) {
    VectorTest t = new VectorTest("foo");
    t.testConstructorNoArg();
    t.testConstructorOneArg();
    t.testConstructorTwoArg();
    t.testCopyInto();
    t.testTrimToSize();
    t.testEnsureCapacity();
    t.testEnsureCapacityHelper();
    t.testSetSize();
    t.testObservers();
    t.testSetElementAt();
    t.testRemoveElementAt();
    t.testInsertElementAt();
    t.testRemoveElement();
    t.testRemoveAllElements();
  }

}
