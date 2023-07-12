package daikon.split.misc;

import daikon.Ppt;
import daikon.ProglangType;
import daikon.ValueTuple;
import daikon.VarInfo;
import daikon.inv.DummyInvariant;
import daikon.split.Splitter;
import org.checkerframework.checker.initialization.qual.UnknownInitialization;
import org.checkerframework.checker.nullness.qual.EnsuresNonNullIf;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.checkerframework.checker.nullness.qual.RequiresNonNull;

// This splitter tests the condition "return == true".
public final class ReturnTrueSplitter extends Splitter {
  static final long serialVersionUID = 20020122L;

  private @Nullable VarInfo return_varinfo;

  /** Create a prototype (factory) splitter. */
  public ReturnTrueSplitter() {}

  /** Create a new instantiated ReturnTrueSplitter. */
  public ReturnTrueSplitter(@UnknownInitialization(Ppt.class) Ppt ppt) {
    return_varinfo = ppt.find_var_by_name("return");
    instantiated = true;
  }

  @SuppressWarnings("nullness:return") // why is "new ...Splitter" @UnderInitialization?
  @Override
  public Splitter instantiateSplitter(@UnknownInitialization(Ppt.class) Ppt ppt) {
    return new ReturnTrueSplitter(ppt);
  }

  @EnsuresNonNullIf(result = true, expression = "return_varinfo")
  @Override
  public boolean valid() {
    return (return_varinfo != null) && (return_varinfo.type == ProglangType.BOOLEAN);
  }

  @SuppressWarnings(
      "nullness:contracts.precondition.override") // application invariant about private
  // variable
  @RequiresNonNull("return_varinfo")
  @Override
  public boolean test(ValueTuple vt) {
    return (return_varinfo.getIntValue(vt) != 0);
  }

  @Override
  public String condition() {
    return "return == true";
  }

  @Override
  public @Nullable DummyInvariant getDummyInvariant() {
    return null;
  }
}
