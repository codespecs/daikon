package daikon.split;

import daikon.*;
import daikon.inv.*;

/*>>>
import org.checkerframework.checker.nullness.qual.*;
*/

// This splitter tests the condition "X>0".
@SuppressWarnings("nullness") // uses private fields, client code not analyzed
public final class SplitterExample extends Splitter {
  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20030218L;

  static /*@Nullable*/ DummyInvariant dummyInvFactory;
  private /*@Nullable*/ DummyInvariant dummyInv;

  private /*@Nullable*/ VarInfo x_varinfo;

  public SplitterExample() {}

  public SplitterExample(Ppt ppt) {
    x_varinfo = ppt.find_var_by_name("X");
    instantiated = true;
  }

  @Override
  public Splitter instantiate(Ppt ppt) {
    return new SplitterExample(ppt);
  }

  /*@EnsuresNonNullIf(result=true, expression="x_varinfo")*/
  @Override
  public boolean valid() {
    return (x_varinfo != null);
  }

  @SuppressWarnings(
      "nullness:contracts.precondition.override.invalid") // application invariant about private variable
  /*@RequiresNonNull("x_varinfo")*/
  @Override
  public boolean test(ValueTuple vt) {
    // Alternately, if x represents an array, use
    //   vt.getIntArrayValue(x_varinfo);
    return (x_varinfo.getIntValue(vt) > 0);
  }

  @Override
  public String condition() {
    return "X > 0";
  }

  /*@EnsuresNonNull("dummyInvFactory")*/
  @Override
  public void makeDummyInvariantFactory(DummyInvariant inv) {
    assert dummyInvFactory == null;
    dummyInvFactory = inv;
  }

  /*@RequiresNonNull("dummyInvFactory")*/
  @Override
  public void instantiateDummy(PptTopLevel ppt) {
    dummyInv = null;
    VarInfo x_vi = ppt.find_var_by_name("X");
    if (x_vi != null) {
      dummyInv = dummyInvFactory.instantiate(ppt, new VarInfo[] {x_vi});
    }
  }

  @Override
  public /*@Nullable*/ DummyInvariant getDummyInvariant() {
    return dummyInv;
  }
}
