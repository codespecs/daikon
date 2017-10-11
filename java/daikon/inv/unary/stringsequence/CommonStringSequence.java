package daikon.inv.unary.stringsequence;

import daikon.*;
import daikon.inv.*;
import plume.*;

/*>>>
import org.checkerframework.checker.interning.qual.*;
import org.checkerframework.checker.lock.qual.*;
import org.checkerframework.checker.nullness.qual.*;
import org.checkerframework.dataflow.qual.*;
import typequals.*;
*/

/**
 * Represents string sequences that contain a common subset. Prints as {@code {s1, s2, s3, ...}
 * subset of x[]}.
 */
public class CommonStringSequence extends SingleStringSequence {
  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20030822L;

  // Variables starting with dkconfig_ should only be set via the
  // daikon.config.Configuration interface.
  /** Boolean. True iff CommonStringSequence invariants should be considered. */
  public static boolean dkconfig_enabled = false;

  private int elts = 0;
  /** Null means no samples have been seen yet. Empty array means intersection is empty. */
  private String /*@MonotonicNonNull*/ [] intersect = null;

  protected CommonStringSequence(PptSlice ppt) {
    super(ppt);
  }

  protected /*@Prototype*/ CommonStringSequence() {
    super();
  }

  private static /*@Prototype*/ CommonStringSequence proto =
      new /*@Prototype*/ CommonStringSequence();

  /** Returns the prototype invariant for CommonStringSequence */
  public static /*@Prototype*/ CommonStringSequence get_proto() {
    return proto;
  }

  /** returns whether or not this invariant is enabled */
  @Override
  public boolean enabled() {
    return dkconfig_enabled;
  }

  /** instantiate an invariant on the specified slice */
  @Override
  protected CommonStringSequence instantiate_dyn(
      /*>>> @Prototype CommonStringSequence this,*/ PptSlice slice) {
    return new CommonStringSequence(slice);
  }

  // Don't write clone, because this.intersect is read-only
  // protected Object clone();

  @Override
  public String repr(/*>>>@GuardSatisfied CommonStringSequence this*/) {
    return "CommonStringSequence " + varNames() + ": " + "elts=\"" + elts;
  }

  private String printIntersect(/*>>>@GuardSatisfied CommonStringSequence this*/) {
    if (intersect == null) return "{}";

    String result = "{";
    for (int i = 0; i < intersect.length; i++) {
      result += intersect[i];
      if (i != intersect.length - 1) result += ", ";
    }
    result += "}";
    return result;
  }

  /*@SideEffectFree*/
  @Override
  public String format_using(
      /*>>>@GuardSatisfied CommonStringSequence this,*/ OutputFormat format) {
    if (format == OutputFormat.DAIKON) return format_daikon();
    if (format == OutputFormat.CSHARPCONTRACT) return format_csharp_contract();

    return format_unimplemented(format);
  }

  public String format_daikon(/*>>>@GuardSatisfied CommonStringSequence this*/) {
    return (printIntersect() + " subset of " + var().name());
  }

  public String format_csharp_contract(/*>>>@GuardSatisfied CommonStringSequence this*/) {
    if (intersect == null) return "()";

    if (intersect.length == 1) {
      return var().csharp_name() + ".Contains(" + intersect[0] + ")";
    }

    String exp = "{";
    for (int i = 0; i < intersect.length; i++) {
      exp += " " + intersect[i] + " ";
      if (i != intersect.length - 1) {
        exp += ",";
      }
    }
    exp += "}";
    String[] split = var().csharp_array_split();
    return "Contract.ForAll(new[] " + exp + " , x => " + var().csharp_name() + ".Contains(x))";
  }

  @Override
  public InvariantStatus check_modified(/*@Interned*/ String /*@Interned*/ [] a, int count) {
    if (a == null) {
      return InvariantStatus.FALSIFIED;
    } else if (intersect == null) {
      return InvariantStatus.NO_CHANGE;
    } else {
      String[] tmp = new String[intersect.length];
      int size = 0;
      for (int i = 1; i < a.length; i++) {
        if ((ArraysMDE.indexOf(intersect, a[i]) != -1)
            && ((size == 0) || (ArraysMDE.indexOf(ArraysMDE.subarray(tmp, 0, size), a[i]) == -1)))
          tmp[size++] = a[i];
      }

      if (size == 0) {
        return InvariantStatus.FALSIFIED;
      }
    }
    return InvariantStatus.NO_CHANGE;
  }

  @Override
  public InvariantStatus add_modified(/*@Interned*/ String /*@Interned*/ [] a, int count) {
    if (a == null) {
      return InvariantStatus.FALSIFIED;
    } else if (intersect == null) {
      intersect = Intern.intern(a);
      return InvariantStatus.NO_CHANGE;
    } else {
      /*@Interned*/ String[] tmp = new /*@Interned*/ String[intersect.length];
      int size = 0;
      for (int i = 1; i < a.length; i++) {
        if ((ArraysMDE.indexOf(intersect, a[i]) != -1)
            && ((size == 0) || (ArraysMDE.indexOf(ArraysMDE.subarray(tmp, 0, size), a[i]) == -1)))
          tmp[size++] = a[i];
      }

      if (size == 0) {
        return InvariantStatus.FALSIFIED;
      }
      intersect = Intern.intern(ArraysMDE.subarray(tmp, 0, size));
    }
    elts++;
    return InvariantStatus.NO_CHANGE;
  }

  @Override
  protected double computeConfidence() {
    throw new Error("Not yet implemented");
  }

  /*@Pure*/
  public /*@Nullable*/ DiscardInfo isObviousImplied() {
    return null;
  }

  /*@Pure*/
  @Override
  public boolean isSameFormula(Invariant other) {
    assert other instanceof CommonStringSequence;
    return true;
  }
}
