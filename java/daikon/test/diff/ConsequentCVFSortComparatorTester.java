package daikon.test.diff;

import java.util.*;
import junit.framework.*;
import daikon.*;
import daikon.diff.*;
import daikon.inv.*;
import daikon.inv.unary.scalar.*;
import daikon.test.*;

@SuppressWarnings("nullness") // testing code
public class ConsequentCVFSortComparatorTester extends TestCase {

  public static void main(String[] args) {
    daikon.LogHelper.setupLogs(LogHelper.INFO);
    junit.textui.TestRunner.run(new TestSuite(ConsequentCVFSortComparatorTester.class));
  }

  public ConsequentCVFSortComparatorTester(String name) {
    super(name);
  }

  public void testCompare() {
    VarInfo[] vars = {
      DiffTester.newIntVarInfo("a"),
      DiffTester.newIntVarInfo("b"),
      DiffTester.newIntVarInfo("c"),
      DiffTester.newIntVarInfo("d"),
    };
    PptTopLevel P = Common.makePptTopLevel("P:::OBJECT", vars);
    PptSlice slicea = new PptSlice1(P, new VarInfo[] {vars[0]});
    PptSlice sliceb = new PptSlice1(P, new VarInfo[] {vars[1]});
    PptSlice slicec = new PptSlice1(P, new VarInfo[] {vars[2]});
    PptSlice sliced = new PptSlice1(P, new VarInfo[] {vars[3]});
    Invariant A = NonZero.get_proto().instantiate(slicea);
    Invariant B = NonZero.get_proto().instantiate(sliceb);
    Invariant C = NonZero.get_proto().instantiate(slicec);
    Invariant D = NonZero.get_proto().instantiate(sliced);
    Invariant A_B = Implication.makeImplication(P, A, B, false, A, B);
    Invariant A_C = Implication.makeImplication(P, A, C, false, A, C);
    Invariant B_C = Implication.makeImplication(P, B, C, false, B, C);

    Comparator<Invariant> c = new ConsequentCVFSortComparator();

    assert c.compare(A, A) == 0;
    assert c.compare(A, B) < 0;
    assert c.compare(A, C) < 0;
    assert c.compare(A, D) < 0;
    assert c.compare(A, A_B) < 0;
    assert c.compare(A, A_C) < 0;
    assert c.compare(A, B_C) < 0;

    assert c.compare(B, B) == 0;
    assert c.compare(B, C) < 0;
    assert c.compare(B, D) < 0;
    assert c.compare(B, A_B) == 0;
    assert c.compare(B, A_C) < 0;
    assert c.compare(B, B_C) < 0;

    assert c.compare(C, C) == 0;
    assert c.compare(C, D) < 0;
    assert c.compare(C, A_B) > 0;
    assert c.compare(C, A_C) == 0;
    assert c.compare(C, B_C) == 0;

    assert c.compare(D, D) == 0;
    assert c.compare(D, A_B) > 0;
    assert c.compare(D, A_C) > 0;
    assert c.compare(D, B_C) > 0;

    assert c.compare(A_B, A_B) == 0;
    assert c.compare(A_B, A_C) < 0;
    assert c.compare(A_B, B_C) < 0;

    assert c.compare(A_C, A_C) == 0;
    assert c.compare(A_C, B_C) < 0;

    assert c.compare(B_C, B_C) == 0;
  }
}
