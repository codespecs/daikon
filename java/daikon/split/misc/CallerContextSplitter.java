package daikon.split.misc;

import daikon.*;
import daikon.inv.DummyInvariant;
import daikon.split.*;
import plume.ArraysMDE;

/*>>>
import org.checkerframework.checker.lock.qual.*;
import org.checkerframework.checker.nullness.qual.*;
import org.checkerframework.dataflow.qual.*;
*/

/** This splitter tests the condition "$caller one of { some set of integers }". */
public final class CallerContextSplitter extends Splitter {
  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20030112L;

  /** Create a new splitter for the given ppt using this as a prototype. */
  @Override
  public Splitter instantiate(Ppt ppt) {
    return new CallerContextSplitter(ppt, ids, condition);
  }

  /** Name of the variable used by the front end to store caller (callsite) information. */
  public final String CALLER_INDICATOR_NAME_STRING = "daikon_callsite_id";

  private final /*@Nullable*/ VarInfo caller_varinfo;
  private final long[] ids;
  private final String condition;

  protected CallerContextSplitter(Ppt ppt, long[] ids, String condition) {
    caller_varinfo = ppt.find_var_by_name(CALLER_INDICATOR_NAME_STRING);
    this.ids = ids;
    this.condition = condition;
    instantiated = true;
  }

  /** Create a prototype splitter for the given set of ids and condition. */
  public CallerContextSplitter(long[] ids, String condition) {
    this.caller_varinfo = null;
    this.ids = ids.clone();
    this.condition = condition;
  }

  /*@EnsuresNonNullIf(result=true, expression="caller_varinfo")*/
  @Override
  public boolean valid() {
    return (caller_varinfo != null);
  }

  @SuppressWarnings(
      "nullness:contracts.precondition.override.invalid") // application invariant about private variable
  /*@RequiresNonNull("caller_varinfo")*/
  @Override
  public boolean test(ValueTuple vt) {
    long caller = caller_varinfo.getIntValue(vt);
    return (ArraysMDE.indexOf(ids, caller) >= 0);
  }

  @Override
  public String condition() {
    return condition;
  }

  /*@SideEffectFree*/
  @Override
  public String toString(/*>>>@GuardSatisfied CallerContextSplitter this*/) {
    String attach = "(unattached prototype)";
    if (caller_varinfo != null) {
      attach = "attached to " + caller_varinfo.ppt.name();
    }
    return "CallerContextSplitter<" + condition + ", " + attach + ">";
  }

  @Override
  public /*@Nullable*/ DummyInvariant getDummyInvariant() {
    return null;
  }
}
