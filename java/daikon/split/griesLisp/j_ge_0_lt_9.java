package daikon.split.griesLisp;

import daikon.*;
import daikon.split.*;

public class j_ge_0_lt_9 extends Splitter {
  public String condition() { return "0 <= j < 9"; }
  VarInfo j_varinfo;
  public j_ge_0_lt_9() { }
  public j_ge_0_lt_9(Ppt ppt) {
    j_varinfo = ppt.findVar("J");
  }
  public Splitter instantiate(Ppt ppt) { return new j_ge_0_lt_9(ppt); }
  public boolean valid() { return (j_varinfo != null); }
  public boolean test(ValueTuple vt) {
    int j = j_varinfo.getIntValue(vt);
    return (0 <= j) && (j < 9);
  }
}
