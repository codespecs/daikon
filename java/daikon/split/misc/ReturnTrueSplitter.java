package daikon.split.misc;

import daikon.*;

// This splitter tests the condition "X>0".
public final class ReturnTrueSplitter extends Splitter {

  VarInfo return_varinfo;

  public SplitterExample() {
  }

  public SplitterExample(Ppt ppt) {
    x_varinfo = ppt.findVar("return");
  }

  public Splitter instantiate(Ppt ppt) {
    return new SplitterExample(ppt);
  }

  public boolean valid() {
    return ((return_varinfo != null)
            && (return_varinfo.type == ProglangType.BOOLEAN));
  }

  public boolean test(ValueTuple vt) {
    return (return_varinfo.getIntValue(vt) != 0);
  }

  public String condition() {
    return "return > 0";
  }

}
