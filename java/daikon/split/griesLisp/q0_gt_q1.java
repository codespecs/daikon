package daikon.split.griesLisp;

import daikon.*;
import daikon.split.*;

public class q0_gt_q1 extends Splitter {
  public String condition() { return "q0 > q1"; }
  VarInfo q0_varinfo;
  VarInfo q1_varinfo;
  public q0_gt_q1() { }
  public q0_gt_q1(Ppt ppt) {
    q0_varinfo = ppt.findVar("Q0");
    q1_varinfo = ppt.findVar("Q1");
  }
  public Splitter instantiate(Ppt ppt) { return new q0_gt_q1(ppt); }
  public boolean valid() { return (q0_varinfo != null) && (q1_varinfo != null); }
  public boolean test(ValueTuple vt) {
    return (q0_varinfo.getIntValue(vt) > q1_varinfo.getIntValue(vt));
  }
}
