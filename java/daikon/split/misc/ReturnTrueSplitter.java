package daikon.split.misc;

import daikon.*;
import daikon.split.*;

// This splitter tests the condition "X>0".
public final class ReturnTrueSplitter extends Splitter {

  VarInfo return_varinfo;

  public ReturnTrueSplitter() {
  }

  public ReturnTrueSplitter(Ppt ppt) {
    return_varinfo = ppt.findVar("return");
  }

  public Splitter instantiate(Ppt ppt) {
    return new ReturnTrueSplitter(ppt);
  }

  public boolean valid() {
    return ((return_varinfo != null)
            && (return_varinfo.type == ProglangType.BOOLEAN));
  }

  public boolean test(ValueTuple vt) {
    return (return_varinfo.getIntValue(vt) != 0);
  }

  public String condition() {
    return "return == true";
  }

}
