package daikon.test.diff;

import daikon.*;
import daikon.diff.*;
import daikon.inv.Invariant;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import junit.framework.*;
import org.checkerframework.checker.nullness.qual.MonotonicNonNull;
import org.checkerframework.checker.nullness.qual.RequiresNonNull;

public class InvMapTester extends TestCase {

  private @MonotonicNonNull InvMap map; // initialized by setUp()
  private PptTopLevel pptA = new PptTopLevel("A:::OBJECT", new VarInfo[0]);
  private PptTopLevel pptB = new PptTopLevel("A:::OBJECT", new VarInfo[0]);
  private PptTopLevel pptC = new PptTopLevel("A:::OBJECT", new VarInfo[0]);
  private List<Invariant> invsA = new ArrayList<>();
  private List<Invariant> invsB = new ArrayList<>();
  private List<Invariant> invsC = new ArrayList<>();

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

  @RequiresNonNull("map")
  // implicit flow: setUp was called by JUnit";
  public void testABC() {
    map.put(pptA, invsA);
    map.put(pptB, invsB);
    map.put(pptC, invsC);

    // Test the keys
    Iterator<PptTopLevel> i = map.pptIterator();
    assertSame(i.next(), pptA);
    assertSame(i.next(), pptB);
    assertSame(i.next(), pptC);
    assertFalse(i.hasNext());

    // Test the values
    assertSame(map.get(pptA), invsA);
    assertSame(map.get(pptB), invsB);
    assertSame(map.get(pptC), invsC);
  }

  @RequiresNonNull("map")
  // implicit flow: setUp was called by JUnit";
  public void testCAB() {
    map.put(pptC, invsC);
    map.put(pptA, invsA);
    map.put(pptB, invsB);

    // Test the keys
    Iterator<PptTopLevel> i = map.pptIterator();
    assertSame(i.next(), pptC);
    assertSame(i.next(), pptA);
    assertSame(i.next(), pptB);
    assertFalse(i.hasNext());

    // Test the values
    assertSame(map.get(pptA), invsA);
    assertSame(map.get(pptB), invsB);
    assertSame(map.get(pptC), invsC);
  }
}
