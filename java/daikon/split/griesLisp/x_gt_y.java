package daikon.split.griesLisp;

import daikon.*;
import daikon.split.*;

public class x_gt_y extends Splitter {
  public String condition() { return "x > y"; }
  VarInfo x_varinfo;
  VarInfo y_varinfo;
  public x_gt_y() { }
  public x_gt_y(Ppt ppt) {
    x_varinfo = ppt.findVar("X");
    y_varinfo = ppt.findVar("Y");
  }
  public Splitter instantiate(Ppt ppt) { return new x_gt_y(ppt); }
  public boolean valid() { return (x_varinfo != null) && (y_varinfo != null); }
  public boolean test(ValueTuple vt) {
    return (x_varinfo.getIntValue(vt) > y_varinfo.getIntValue(vt));
  }
}
