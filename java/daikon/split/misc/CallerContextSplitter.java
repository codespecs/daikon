package daikon.split.misc;

import daikon.Ppt;
import daikon.ValueTuple;
import daikon.VarInfo;
import daikon.inv.DummyInvariant;
import daikon.split.Splitter;
import org.checkerframework.checker.initialization.qual.UnknownInitialization;
import org.checkerframework.checker.lock.qual.GuardSatisfied;
import org.checkerframework.checker.nullness.qual.EnsuresNonNullIf;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.checkerframework.checker.nullness.qual.RequiresNonNull;
import org.checkerframework.dataflow.qual.SideEffectFree;
import org.plumelib.util.ArraysPlume;

/** This splitter tests the condition "$caller one of { some set of integers }". */
public final class CallerContextSplitter extends Splitter {
  static final long serialVersionUID = 20030112L;

  /** Create a new splitter for the given ppt using this as a prototype. */
  @SuppressWarnings("nullness:return") // why is "new ...Splitter" @UnderInitialization?
  @Override
  public Splitter instantiateSplitter(@UnknownInitialization(Ppt.class) Ppt ppt) {
    return new CallerContextSplitter(ppt, ids, condition);
  }

  /** Name of the variable used by the front end to store caller (callsite) information. */
  public final String CALLER_INDICATOR_NAME_STRING = "daikon_callsite_id";

  private final @Nullable VarInfo caller_varinfo;
  private final long[] ids;
  private final String condition;

  /** Create a new instantiated CallerContextSplitter. */
  CallerContextSplitter(@UnknownInitialization(Ppt.class) Ppt ppt, long[] ids, String condition) {
    caller_varinfo = ppt.find_var_by_name(CALLER_INDICATOR_NAME_STRING);
    this.ids = ids;
    this.condition = condition;
    instantiated = true;
  }

  /** Create a prototype (factory) splitter for the given set of ids and condition. */
  public CallerContextSplitter(long[] ids, String condition) {
    this.caller_varinfo = null;
    this.ids = ids.clone();
    this.condition = condition;
  }

  @EnsuresNonNullIf(result = true, expression = "caller_varinfo")
  @Override
  public boolean valid() {
    return (caller_varinfo != null);
  }

  @SuppressWarnings(
      "nullness:contracts.precondition.override") // application invariant about private
  // variable
  @RequiresNonNull("caller_varinfo")
  @Override
  public boolean test(ValueTuple vt) {
    long caller = caller_varinfo.getIntValue(vt);
    return (ArraysPlume.indexOf(ids, caller) >= 0);
  }

  @Override
  public String condition() {
    return condition;
  }

  @SideEffectFree
  @Override
  public String toString(@GuardSatisfied CallerContextSplitter this) {
    String attach = "(unattached prototype)";
    if (caller_varinfo != null) {
      attach = "attached to " + caller_varinfo.ppt.name();
    }
    return "CallerContextSplitter<" + condition + ", " + attach + ">";
  }

  @Override
  public @Nullable DummyInvariant getDummyInvariant() {
    return null;
  }
}
