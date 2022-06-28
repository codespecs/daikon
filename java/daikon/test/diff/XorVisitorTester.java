package daikon.test.diff;

import static java.util.logging.Level.INFO;
import static org.junit.Assert.assertEquals;

import daikon.*;
import daikon.diff.*;
import daikon.inv.*;
import daikon.test.*;
import java.util.Arrays;
import junit.framework.*;
import org.junit.BeforeClass;
import org.junit.Test;

public class XorVisitorTester {

  private Diff diff = new Diff(true, new Invariant.ClassVarnameFormulaComparator());

  /** prepare for tests */
  @BeforeClass
  public static void setUpClass() {
    daikon.LogHelper.setupLogs(INFO);
    FileIO.new_decl_format = true;
  }

  // X1 and X2 have the same class and vars, but different formula
  // map1: A->{W, X1, Y}, B->{Y}, D->{M, N_unjusitifed, O_unjustified}
  // map2: A->{W, X2, Z}, C->{Z}, D->{M_unjustified, N}
  // map1 xor map2: A->{X1, X2, Y, Z}, B->{Y}, C->{Z}, D->{M, N}
  @Test
  public void testXor() {
    VarInfo[] vars = {
      DiffTester.newIntVarInfo("w"),
      DiffTester.newIntVarInfo("x"),
      DiffTester.newIntVarInfo("y"),
      DiffTester.newIntVarInfo("z"),
      DiffTester.newIntVarInfo("m"),
      DiffTester.newIntVarInfo("n"),
      DiffTester.newIntVarInfo("o"),
    };
    PptTopLevel A = Common.makePptTopLevel("A:::OBJECT", vars);
    PptTopLevel B = Common.makePptTopLevel("B:::OBJECT", vars);
    PptTopLevel C = Common.makePptTopLevel("C:::OBJECT", vars);
    PptTopLevel D = Common.makePptTopLevel("D:::OBJECT", vars);
    PptSlice slicew = new PptSlice1(A, new VarInfo[] {vars[0]});
    PptSlice slicex = new PptSlice1(A, new VarInfo[] {vars[1]});
    PptSlice slicey = new PptSlice1(A, new VarInfo[] {vars[2]});
    PptSlice slicez = new PptSlice1(A, new VarInfo[] {vars[3]});
    PptSlice slicem = new PptSlice1(A, new VarInfo[] {vars[4]});
    PptSlice slicen = new PptSlice1(A, new VarInfo[] {vars[5]});
    PptSlice sliceo = new PptSlice1(A, new VarInfo[] {vars[6]});
    Invariant W = new DiffDummyInvariant(slicew, "W", true);
    Invariant X1 = new DiffDummyInvariant(slicex, "X1", true);
    Invariant X2 = new DiffDummyInvariant(slicex, "X2", true);
    Invariant Y = new DiffDummyInvariant(slicey, "Y", true);
    Invariant Z = new DiffDummyInvariant(slicez, "Z", true);
    Invariant M = new DiffDummyInvariant(slicem, "M", true);
    Invariant unjM = new DiffDummyInvariant(slicem, "M", false);
    Invariant N = new DiffDummyInvariant(slicen, "N", true);
    Invariant unjN = new DiffDummyInvariant(slicen, "N", false);
    Invariant unjO = new DiffDummyInvariant(sliceo, "O", false);

    InvMap map1 = new InvMap();
    map1.put(A, Arrays.asList(W, X1, Y));
    map1.put(B, Arrays.asList(Y));
    map1.put(D, Arrays.asList(M, unjN, unjO));

    InvMap map2 = new InvMap();
    map2.put(A, Arrays.asList(W, X2, Z));
    map2.put(C, Arrays.asList(Z));
    map2.put(D, Arrays.asList(unjM, N));

    RootNode root = diff.diffInvMap(map1, map2, false);
    XorVisitor v = new XorVisitor();
    root.accept(v);
    InvMap result = v.getResult();

    InvMap expected = new InvMap();
    expected.put(A, Arrays.asList(X1, X2, Y, Z));
    expected.put(B, Arrays.asList(Y));
    expected.put(C, Arrays.asList(Z));
    expected.put(D, Arrays.asList(M, N));

    assertEquals(expected.toString(), result.toString());
  }
}
