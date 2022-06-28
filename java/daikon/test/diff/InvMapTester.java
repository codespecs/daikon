package daikon.test.diff;

import static java.util.logging.Level.INFO;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertSame;

import daikon.*;
import daikon.diff.*;
import daikon.inv.Invariant;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import junit.framework.*;
import org.checkerframework.checker.nullness.qual.MonotonicNonNull;
import org.checkerframework.checker.nullness.qual.RequiresNonNull;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

public class InvMapTester {

  private @MonotonicNonNull InvMap map; // initialized by setUp()
  private PptTopLevel pptA = new PptTopLevel("A:::OBJECT", new VarInfo[0]);
  private PptTopLevel pptB = new PptTopLevel("A:::OBJECT", new VarInfo[0]);
  private PptTopLevel pptC = new PptTopLevel("A:::OBJECT", new VarInfo[0]);
  private List<Invariant> invsA = new ArrayList<>();
  private List<Invariant> invsB = new ArrayList<>();
  private List<Invariant> invsC = new ArrayList<>();

  /** prepare for tests */
  @BeforeClass
  public static void setUpClass() {
    daikon.LogHelper.setupLogs(INFO);
    FileIO.new_decl_format = true;
  }

  @Before
  public void setUp() {
    map = new InvMap();
  }

  @RequiresNonNull("map")
  // implicit flow: setUp was called by JUnit";
  @Test
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
  @Test
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
