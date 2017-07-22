package daikon.test.diff;

import daikon.*;
import daikon.diff.*;
import daikon.inv.Invariant;
import java.util.*;
import junit.framework.*;

/*>>>
import org.checkerframework.checker.nullness.qual.*;
*/

@SuppressWarnings("interning") // use of == in test code
public class InvMapTester extends TestCase {

  private /*@MonotonicNonNull*/ InvMap map; // initialized by setUp()
  private PptTopLevel pptA = new PptTopLevel("A:::OBJECT", new VarInfo[0]);
  private PptTopLevel pptB = new PptTopLevel("A:::OBJECT", new VarInfo[0]);
  private PptTopLevel pptC = new PptTopLevel("A:::OBJECT", new VarInfo[0]);
  private List<Invariant> invsA = new ArrayList<Invariant>();
  private List<Invariant> invsB = new ArrayList<Invariant>();
  private List<Invariant> invsC = new ArrayList<Invariant>();

  public static void main(String[] args) {
    daikon.LogHelper.setupLogs(LogHelper.INFO);
    junit.textui.TestRunner.run(new TestSuite(InvMapTester.class));
  }

  public InvMapTester(String name) {
    super(name);
  }

  @Override
  public void setUp() throws Exception {
    super.setUp();
    map = new InvMap();
  }

  /*@RequiresNonNull("map")*/
  // implicit flow: setUp was called by JUnit";
  public void testABC() {
    map.put(pptA, invsA);
    map.put(pptB, invsB);
    map.put(pptC, invsC);

    // Test the keys
    Iterator<PptTopLevel> i = map.pptIterator();
    assert pptA == i.next();
    assert pptB == i.next();
    assert pptC == i.next();
    assert !i.hasNext();

    // Test the values
    assert invsA == map.get(pptA);
    assert invsB == map.get(pptB);
    assert invsC == map.get(pptC);
  }

  /*@RequiresNonNull("map")*/
  // implicit flow: setUp was called by JUnit";
  public void testCAB() {
    map.put(pptC, invsC);
    map.put(pptA, invsA);
    map.put(pptB, invsB);

    // Test the keys
    Iterator<PptTopLevel> i = map.pptIterator();
    assert pptC == i.next();
    assert pptA == i.next();
    assert pptB == i.next();
    assert !i.hasNext();

    // Test the values
    assert invsA == map.get(pptA);
    assert invsB == map.get(pptB);
    assert invsC == map.get(pptC);
  }
}
