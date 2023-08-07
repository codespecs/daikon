package daikon.inv.unary.sequence;

import daikon.PptSlice;
import daikon.VarInfo;
import daikon.inv.unary.UnaryInvariant;
import org.checkerframework.checker.initialization.qual.UnknownInitialization;
import org.checkerframework.checker.lock.qual.GuardSatisfied;
import typequals.prototype.qual.Prototype;

/** Invariants on a single sequence (array) variable, such as {@code a[] contains no duplicates}. */
public abstract class SingleSequence extends UnaryInvariant {
  static final long serialVersionUID = 20031024L;

  /**
   * Boolean. Set to true to disable all SeqIndex invariants (SeqIndexIntEqual,
   * SeqIndexFloatLessThan, etc). This overrides the settings of the individual SeqIndex enable
   * configuration options. To disable only some options, the options must be disabled individually.
   */
  public static boolean dkconfig_SeqIndexDisableAll = false;

  protected SingleSequence(PptSlice ppt) {
    super(ppt);
  }

  protected @Prototype SingleSequence() {
    super();
  }

  public VarInfo var(
      @GuardSatisfied @UnknownInitialization(SingleSequence.class) SingleSequence this) {
    return ppt.var_infos[0];
  }
}
