package daikon.split;

import daikon.*;

// This splitter tests the condition "X>0".
public final class SplitterExample
  extends Splitter
{
  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20020122L;

  VarInfo x_varinfo;

  public SplitterExample() {
  }

  public SplitterExample(Ppt ppt) {
    x_varinfo = ppt.findVar("X");
  }

  public Splitter instantiate(Ppt ppt) {
    return new SplitterExample(ppt);
  }

  public boolean valid() {
    return (x_varinfo != null);
  }

  public boolean test(ValueTuple vt) {
    // Alternately, if x represents an array, use
    //   vt.getIntArrayValue(x_varinfo);
    return (x_varinfo.getIntValue(vt) > 0);
  }

  public String condition() {
    return "X > 0";
  }

}
