package daikon.split.griesLisp;

import daikon.*;
import daikon.split.*;

public final class x_ge_0 extends Splitter {
  public String condition() { return "x >= 0"; }
  VarInfo x_varinfo;
  public x_ge_0() { }
  public x_ge_0(Ppt ppt) {
    x_varinfo = ppt.findVar("X");
  }
  public Splitter instantiate(Ppt ppt) { return new x_ge_0(ppt); }
  public boolean valid() { return (x_varinfo != null); }
  public boolean test(ValueTuple vt) {
    return (x_varinfo.getIntValue(vt) >= 0);
  }
}
