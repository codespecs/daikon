package daikon.split.weissDsaaMDE;

import daikon.*;
import daikon.split.*;

public class result_current_ne_0 extends Splitter {
  public String condition() { return "daikon_return_value.current != 0"; }
  VarInfo result_current_varinfo;
  public result_current_ne_0() { }
  public result_current_ne_0(Ppt ppt) {
    result_current_varinfo = ppt.findVar("daikon_return_value.current");
  }
  public Splitter instantiate(Ppt ppt) { return new result_current_ne_0(ppt); }
  public boolean valid() { return (result_current_varinfo != null); }
  public boolean test(ValueTuple vt) {
    return (result_current_varinfo.getIntValue(vt) != 0);
  }
}
