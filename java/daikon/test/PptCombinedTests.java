package daikon.test;

import java.util.ArrayList;
import java.util.List;

import daikon.PptCombined;
import daikon.PptTopLevel;
import daikon.VarInfo;
import junit.framework.TestCase;

public class PptCombinedTests extends TestCase {

  public void testFindIntermediateBlocks1() {

    PptTopLevel p1 = new PptTopLevel("ppt:::1", new VarInfo[0]);
    PptTopLevel p2 = new PptTopLevel("ppt:::2", new VarInfo[0]);
    PptTopLevel p3 = new PptTopLevel("ppt:::3", new VarInfo[0]);
    PptTopLevel p4 = new PptTopLevel("ppt:::4", new VarInfo[0]);

    setPredecessors(p1);
    setPredecessors(p2, p1);
    setPredecessors(p3, p1);
    setPredecessors(p4, p2, p3, p4);

    List<PptTopLevel> list = PptCombined.findIntermediateBlocks(p4, p1);

    assertListContains(list, p2, p3, p4);
    assertListDoesNotContain(list, p1);

  }

 public void testFindIntermediateBlocks2() {

    PptTopLevel p1 = new PptTopLevel("ppt:::1", new VarInfo[0]);
    PptTopLevel p2 = new PptTopLevel("ppt:::2", new VarInfo[0]);
    PptTopLevel p21 = new PptTopLevel("ppt:::21", new VarInfo[0]);
    PptTopLevel p22 = new PptTopLevel("ppt:::22", new VarInfo[0]);
    PptTopLevel p3 = new PptTopLevel("ppt:::3", new VarInfo[0]);
    PptTopLevel p4 = new PptTopLevel("ppt:::4", new VarInfo[0]);

    setPredecessors(p1);
    setPredecessors(p2, p1);
    setPredecessors(p21, p2);
    setPredecessors(p22, p2);
    setPredecessors(p3, p1);
    setPredecessors(p4, p2, p21, p22, p4);

    List<PptTopLevel> list = PptCombined.findIntermediateBlocks(p4, p1);

    assertListContains(list, p2, p21, p22, p4);
    assertListDoesNotContain(list, p1, p3);

  }

 public void testFindIntermediateBlocks3() {

   PptTopLevel p1 = new PptTopLevel("ppt:::1", new VarInfo[0]);
   PptTopLevel p2 = new PptTopLevel("ppt:::2", new VarInfo[0]);

   setPredecessors(p1);
   setPredecessors(p2, p1, p2);

   List<PptTopLevel> list = PptCombined.findIntermediateBlocks(p2, p1);

   assertListContains(list, p2);
   assertListDoesNotContain(list, p1);
 }

 public void testFindIntermediateBlocks4() {

   PptTopLevel p1 = new PptTopLevel("ppt:::1", new VarInfo[0]);
   PptTopLevel p2 = new PptTopLevel("ppt:::2", new VarInfo[0]);
   PptTopLevel p3 = new PptTopLevel("ppt:::3", new VarInfo[0]);
   PptTopLevel p4 = new PptTopLevel("ppt:::4", new VarInfo[0]);

   setPredecessors(p1);
   setPredecessors(p2, p1);
   setPredecessors(p3, p2);
   setPredecessors(p4, p3, p4);

   List<PptTopLevel> list = PptCombined.findIntermediateBlocks(p4, p1);

   assertListContains(list, p2, p3, p4);
   assertListDoesNotContain(list, p1);

 }


  private void assertListDoesNotContain(List<PptTopLevel> list, PptTopLevel... ps) {
    for (PptTopLevel p : ps)
      assert !list.contains(p) : list + " " + p;
  }

  private void assertListContains(List<PptTopLevel> list, PptTopLevel... ps) {
    for (PptTopLevel p : ps)
      assert list.contains(p) : list + " " + p;
  }

  private void setPredecessors(PptTopLevel... ppts) {
    PptTopLevel main = ppts[0];
    main.predecessors = new ArrayList<PptTopLevel>();
    for (int i = 1 ; i < ppts.length ; i++) {
      main.predecessors.add(ppts[i]);
    }
  }

}
