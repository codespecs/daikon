package daikon.split;

import daikon.*;

// This splitter tests the condition "X>0".
public class SplitterExample extends Splitter {

  VarInfo x_varinfo;

  public SplitterExample() {
  }

  public SplitterExample(Ppt ppt) {
    x_varinfo = ppt.findVar("X");
  }

  public Splitter instantiate(Ppt ppt) {
    return new SplitterExample(ppt);
  }

  public boolean test(ValueTuple vt) {
    // Alternately, use vt.getIntArrayValue(x_varinfo);
    return (x_varinfo.getIntValue(vt) > 0);
  }

  public String condition() {
    return "X > 0";
  }

}
