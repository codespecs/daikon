package daikon.test.split;

import daikon.Ppt;
import daikon.ValueTuple;
import daikon.inv.DummyInvariant;
import daikon.split.Splitter;
import org.checkerframework.checker.initialization.qual.UnknownInitialization;
import org.checkerframework.checker.nullness.qual.Nullable;

/** A splitter that only has a condition. It is its own instantiation. */
class ConditionSplitter extends Splitter {
  static final long serialVersionUID = 20261006L;

  /** The condition. */
  private final String condition;

  /** True if the splitter is valid at every program point; false if at none. */
  private final boolean valid;

  /**
   * Creates a new ConditionSplitter that is valid at every program point.
   *
   * @param condition the condition
   */
  ConditionSplitter(String condition) {
    this(condition, true);
  }

  /**
   * Creates a new ConditionSplitter.
   *
   * @param condition the condition
   * @param valid true if the splitter is valid at every program point; false if at none
   */
  ConditionSplitter(String condition, boolean valid) {
    this.condition = condition;
    this.valid = valid;
  }

  @Override
  public Splitter instantiateSplitter(@UnknownInitialization(Ppt.class) Ppt ppt) {
    return this;
  }

  @Override
  public boolean valid() {
    return valid;
  }

  @Override
  public boolean test(ValueTuple vt) {
    throw new UnsupportedOperationException();
  }

  @Override
  public String condition() {
    return condition;
  }

  @Override
  public @Nullable DummyInvariant getDummyInvariant() {
    return null;
  }
}
