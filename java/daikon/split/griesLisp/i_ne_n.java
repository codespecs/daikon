package daikon.split.griesLisp;

import daikon.*;
import daikon.split.*;

public class i_ne_n extends Splitter {
  public String condition() { return "i != n"; }
  VarInfo i_varinfo;
  VarInfo n_varinfo;
  public i_ne_n() { }
  public i_ne_n(Ppt ppt) {
    i_varinfo = ppt.findVar("I");
    n_varinfo = ppt.findVar("N");
  }
  public Splitter instantiate(Ppt ppt) { return new i_ne_n(ppt); }
  public boolean valid() { return (i_varinfo != null) && (n_varinfo != null); }
  public boolean test(ValueTuple vt) {
    return (i_varinfo.getIntValue(vt) != n_varinfo.getIntValue(vt));
  }
}
