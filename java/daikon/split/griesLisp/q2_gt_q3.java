package daikon.split.griesLisp;

import daikon.*;
import daikon.split.*;

public final class q2_gt_q3 extends Splitter {
  public String condition() { return "q2 > q3"; }
  VarInfo q2_varinfo;
  VarInfo q3_varinfo;
  public q2_gt_q3() { }
  public q2_gt_q3(Ppt ppt) {
    q2_varinfo = ppt.findVar("Q2");
    q3_varinfo = ppt.findVar("Q3");
  }
  public Splitter instantiate(Ppt ppt) { return new q2_gt_q3(ppt); }
  public boolean valid() { return (q2_varinfo != null) && (q3_varinfo != null); }
  public boolean test(ValueTuple vt) {
    return (q2_varinfo.getIntValue(vt) > q3_varinfo.getIntValue(vt));
  }
}
