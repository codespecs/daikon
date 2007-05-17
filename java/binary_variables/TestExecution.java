package binary_variables;

import java.util.TreeSet;
import java.io.IOException;

/**
 * A small test case for for finding the control flow structure.
 *
 * Date: 23/02/2007
 */
public final class TestExecution {
  final Execution m;
  private final int thread_id = 42;
  public TestExecution() throws Throwable {
    m = new Execution();
    int[] calls_g = new int[]{11,12,17,18,11,13,17,18,11,12,17,18,19};
    int[] calls_f = new int[]{1,2,4,4,4,5,2,4,4,5,6};
    for(int b : calls_g) {
      callBB(b);
      if (b==17) {
        Address returnAddress = new Address(59, "bla");
        m.callFunction(thread_id, new Address(22,"module"), returnAddress);
        for(int b2 : calls_f) {
          callBB(b2);
        }
        m.returnFunction(thread_id, returnAddress);
      }
    }
    System.out.println(m);
  }
  private void callBB(int addr) throws Throwable {
    TreeSet<BinaryVariable> binaryVariables = new TreeSet<BinaryVariable>();
    binaryVariables.add( new BinaryVariable("bv1", new Address(addr,"module"), "v1"));
    binaryVariables.add( new BinaryVariable("bv2", new Address(addr,"module"), "v2"));
    if (addr==1) {
      binaryVariables.add( new BinaryVariable("bv2", new Address(4,"module"), "v2"));
      binaryVariables.add( new BinaryVariable("bv1", new Address(4,"module"), "v1"));
      binaryVariables.add( new BinaryVariable("bv2", new Address(2,"module"), "v2"));
      binaryVariables.add( new BinaryVariable("bv1", new Address(2,"module"), "v1"));
    }
    if (addr==2) {
      binaryVariables.add( new BinaryVariable("bv2", new Address(4,"module"), "v2"));
      binaryVariables.add( new BinaryVariable("bv1", new Address(4,"module"), "v1"));
    }
    m.addBB(thread_id, new Address(addr,"module"), binaryVariables);
  }
}
