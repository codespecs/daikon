package daikon.split.griesLisp;

import daikon.*;
import daikon.split.*;

public final class x_le_b_sub_i extends Splitter {
  public String condition() { return "x <= b[i]"; }
  VarInfo x_varinfo;
  VarInfo b_varinfo;
  VarInfo i_varinfo;
  public x_le_b_sub_i() { }
  public x_le_b_sub_i(Ppt ppt) {
    x_varinfo = ppt.findVar("X");
    b_varinfo = ppt.findVar("B");
    i_varinfo = ppt.findVar("I");
  }
  public Splitter instantiate(Ppt ppt) { return new x_le_b_sub_i(ppt); }
  public boolean valid() {
    return (x_varinfo != null) && (b_varinfo != null) && (i_varinfo != null);
  }
  public boolean test(ValueTuple vt) {
    long b_sub_i = b_varinfo.getIntArrayValue(vt)[i_varinfo.getIndexValue(vt)];
    return (x_varinfo.getIntValue(vt) <= b_sub_i);
  }
}
