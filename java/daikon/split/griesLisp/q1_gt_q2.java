package daikon.split.griesLisp;

import daikon.*;
import daikon.split.*;

public class q1_gt_q2 extends Splitter {
  public String condition() { return "q1 > q2"; }
  VarInfo q1_varinfo;
  VarInfo q2_varinfo;
  public q1_gt_q2() { }
  public q1_gt_q2(Ppt ppt) {
    q1_varinfo = ppt.findVar("Q1");
    q2_varinfo = ppt.findVar("Q2");
  }
  public Splitter instantiate(Ppt ppt) { return new q1_gt_q2(ppt); }
  public boolean valid() { return (q1_varinfo != null) && (q2_varinfo != null); }
  public boolean test(ValueTuple vt) {
    return (q1_varinfo.getIntValue(vt) > q2_varinfo.getIntValue(vt));
  }
}
