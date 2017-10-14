package daikon.inv.unary.string;

import daikon.*;
import daikon.inv.*;
import java.util.*;
import plume.*;

/*>>>
import org.checkerframework.checker.interning.qual.*;
import org.checkerframework.checker.lock.qual.*;
import org.checkerframework.checker.nullness.qual.*;
import org.checkerframework.dataflow.qual.*;
import typequals.*;
*/

/**
 * Represents a string that contains only printable ascii characters (values 32 through 126 plus 9
 * (tab).
 */
public final class PrintableString extends SingleString {
  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20061016L;

  /** Boolean. True iff PrintableString invariants should be considered. */
  public static boolean dkconfig_enabled = false;

  public PrintableString(PptSlice slice) {
    super(slice);
  }

  public /*@Prototype*/ PrintableString() {
    super();
  }

  private static /*@Prototype*/ PrintableString proto = new /*@Prototype*/ PrintableString();

  /** Returns the prototype invariant for PrintableString */
  public static /*@Prototype*/ PrintableString get_proto() {
    return proto;
  }

  /** returns whether or not this invariant is enabled */
  @Override
  public boolean enabled() {
    return dkconfig_enabled;
  }

  /** instantiate an invariant on the specified slice */
  @Override
  public PrintableString instantiate_dyn(/*>>> @Prototype PrintableString this,*/ PptSlice slice) {
    return new PrintableString(slice);
  }

  /** Return description of invariant. Only Daikon format is implemented. */
  /*@SideEffectFree*/
  @Override
  public String format_using(/*>>>@GuardSatisfied PrintableString this,*/ OutputFormat format) {
    if (format == OutputFormat.DAIKON) {
      return var().name() + " is printable";
    } else {
      return format_unimplemented(format);
    }
  }

  /** Check to see if a only contains printable ascii characters */
  @Override
  public InvariantStatus add_modified(/*@Interned*/ String a, int count) {
    return check_modified(a, count);
  }

  /** Check to see if a only contains printable ascii characters */
  @Override
  public InvariantStatus check_modified(/*@Interned*/ String a, int count) {
    for (int ii = 0; ii < a.length(); ii++) {
      char ch = a.charAt(ii);
      if (ch > 126) {
        return InvariantStatus.FALSIFIED;
      }
      if ((ch < 32) && (ch != 9)) {
        return InvariantStatus.FALSIFIED;
      }
    }
    return InvariantStatus.NO_CHANGE;
  }

  @Override
  protected double computeConfidence() {
    ValueSet vs = ppt.var_infos[0].get_value_set();
    if (vs.size() > 1) {
      return Invariant.CONFIDENCE_JUSTIFIED;
    } else {
      return Invariant.CONFIDENCE_UNJUSTIFIED;
    }
  }

  /**
   * Returns whether or not this is obvious statically. The only check is for static constants which
   * are obviously printable (or not) from their values.
   */
  /*@Pure*/
  @Override
  public /*@Nullable*/ DiscardInfo isObviousStatically(VarInfo[] vis) {
    if (vis[0].isStaticConstant()) {
      return new DiscardInfo(this, DiscardCode.obvious, vis[0].name() + " is a static constant.");
    }
    return super.isObviousStatically(vis);
  }

  /*@Pure*/
  @Override
  public boolean isSameFormula(Invariant o) {
    assert o instanceof PrintableString;
    return true;
  }
}
