package daikon.split.weissDsaaMDE;

import daikon.*;
import daikon.split.*;

public class result_ne_0 extends Splitter {
  public String condition() { return "daikon_return_value != 0"; }
  VarInfo result_varinfo;
  public result_ne_0() { }
  public result_ne_0(Ppt ppt) {
    result_varinfo = ppt.findVar("daikon_return_value");
  }
  public Splitter instantiate(Ppt ppt) { return new result_ne_0(ppt); }
  public boolean valid() { return (result_varinfo != null); }
  public boolean test(ValueTuple vt) {
    return (result_varinfo.getIntValue(vt) != 0);
  }
}
