package daikon.test.diff;

import java.util.*;
import junit.framework.*;
import daikon.*;
import daikon.diff.*;
import daikon.inv.*;

public class XorVisitorTester extends TestCase {

  private Diff diff = new Diff(true);

  public static void main(String[] args) {
    daikon.Logger.setupLogs (Logger.INFO);
    junit.textui.TestRunner.run(new TestSuite(XorVisitorTester.class));
  }

  public XorVisitorTester(String name) {
    super(name);
  }

  // X1 and X2 have the same class and vars, but different formula
  // map1: A->{W, X1, Y}, B->{Y}, D->{M, N_unjusitifed, O_unjustified}
  // map2: A->{W, X2, Z}, C->{Z}, D->{M_unjustified, N}
  // map1 xor map2: A->{X1, X2, Y, Z}, B->{Y}, C->{Z}, D->{M, N}
  public void testXor() {
    VarInfo[] vars = { DiffTester.newIntVarInfo("w"),
                       DiffTester.newIntVarInfo("x"),
                       DiffTester.newIntVarInfo("y"),
                       DiffTester.newIntVarInfo("z"),
                       DiffTester.newIntVarInfo("m"),
                       DiffTester.newIntVarInfo("n"),
                       DiffTester.newIntVarInfo("o"),
    };
    PptTopLevel A = new PptTopLevel("A", vars);
    PptTopLevel B = new PptTopLevel("B", vars);
    PptTopLevel C = new PptTopLevel("C", vars);
    PptTopLevel D = new PptTopLevel("D", vars);
    PptSlice slicew = new PptSlice1(A, new VarInfo[] {vars[0]});
    PptSlice slicex = new PptSlice1(A, new VarInfo[] {vars[1]});
    PptSlice slicey = new PptSlice1(A, new VarInfo[] {vars[2]});
    PptSlice slicez = new PptSlice1(A, new VarInfo[] {vars[3]});
    PptSlice slicem = new PptSlice1(A, new VarInfo[] {vars[4]});
    PptSlice slicen = new PptSlice1(A, new VarInfo[] {vars[5]});
    PptSlice sliceo = new PptSlice1(A, new VarInfo[] {vars[6]});
    Invariant W = new DummyInvariant(slicew, "W", true);
    Invariant X1 = new DummyInvariant(slicex, "X1", true);
    Invariant X2 = new DummyInvariant(slicex, "X2", true);
    Invariant Y = new DummyInvariant(slicey, "Y", true);
    Invariant Z = new DummyInvariant(slicez, "Z", true);
    Invariant M = new DummyInvariant(slicem, "M", true);
    Invariant unjM = new DummyInvariant(slicem, "M", false);
    Invariant N = new DummyInvariant(slicen, "N", true);
    Invariant unjN = new DummyInvariant(slicen, "N", false);
    Invariant unjO = new DummyInvariant(sliceo, "O", false);

    InvMap map1 = new InvMap();
    map1.put(A, Arrays.asList(new Object[] {W, X1, Y}));
    map1.put(B, Arrays.asList(new Object[] {Y}));
    map1.put(D, Arrays.asList(new Object[] {M, unjN, unjO}));

    InvMap map2 = new InvMap();
    map2.put(A, Arrays.asList(new Object[] {W, X2, Z}));
    map2.put(C, Arrays.asList(new Object[] {Z}));
    map2.put(D, Arrays.asList(new Object[] {unjM, N}));

    diff.setAllInvComparators(new Invariant.ClassVarnameFormulaComparator());
    RootNode root = diff.diffInvMap(map1, map2);
    XorVisitor v = new XorVisitor();
    root.accept(v);
    InvMap result = v.getResult();

    InvMap expected = new InvMap();
    expected.put(A, Arrays.asList(new Object[] {X1, X2, Y, Z}));
    expected.put(B, Arrays.asList(new Object[] {Y}));
    expected.put(C, Arrays.asList(new Object[] {Z}));
    expected.put(D, Arrays.asList(new Object[] {M, N}));

    assertEquals(expected.toString(), result.toString());
  }

}
