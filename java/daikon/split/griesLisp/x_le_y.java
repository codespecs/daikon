package daikon.split.griesLisp;

import daikon.*;
import daikon.split.*;

public final class x_le_y extends Splitter {
  public String condition() { return "x <= y"; }
  VarInfo x_varinfo;
  VarInfo y_varinfo;
  public x_le_y() { }
  public x_le_y(Ppt ppt) {
    x_varinfo = ppt.findVar("X");
    y_varinfo = ppt.findVar("Y");
  }
  public Splitter instantiate(Ppt ppt) { return new x_le_y(ppt); }
  public boolean valid() { return (x_varinfo != null) && (y_varinfo != null); }
  public boolean test(ValueTuple vt) {
    return (x_varinfo.getIntValue(vt) <= y_varinfo.getIntValue(vt));
  }
}
