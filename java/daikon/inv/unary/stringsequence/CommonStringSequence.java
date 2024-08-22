package daikon.inv.unary.stringsequence;

import daikon.PptSlice;
import daikon.inv.DiscardInfo;
import daikon.inv.Invariant;
import daikon.inv.InvariantStatus;
import daikon.inv.OutputFormat;
import java.util.List;
import java.util.StringJoiner;
import org.checkerframework.checker.interning.qual.Interned;
import org.checkerframework.checker.lock.qual.GuardSatisfied;
import org.checkerframework.checker.nullness.qual.MonotonicNonNull;
import org.checkerframework.checker.nullness.qual.NonNull;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.checkerframework.dataflow.qual.Pure;
import org.checkerframework.dataflow.qual.SideEffectFree;
import org.plumelib.util.ArraysPlume;
import org.plumelib.util.Intern;
import typequals.prototype.qual.NonPrototype;
import typequals.prototype.qual.Prototype;

// TODO: Why isn't this generated from CommonSequence.java.jpp?
/**
 * Represents string sequences that contain a common subset. Prints as {@code {s1, s2, s3, ...}
 * subset of x[]}.
 */
public class CommonStringSequence extends SingleStringSequence {
  static final long serialVersionUID = 20030822L;

  // Variables starting with dkconfig_ should only be set via the
  // daikon.config.Configuration interface.
  /** Boolean. True iff CommonStringSequence invariants should be considered. */
  public static boolean dkconfig_enabled = false;

  /** The number of samples observed. */
  private int count = 0;

  /** Null means no samples have been seen yet. Empty array means intersection is empty. */
  private @Interned String @MonotonicNonNull @Interned [] intersect = null;

  protected CommonStringSequence(PptSlice ppt) {
    super(ppt);
  }

  protected @Prototype CommonStringSequence() {
    super();
  }

  private static @Prototype CommonStringSequence proto = new @Prototype CommonStringSequence();

  /** Returns the prototype invariant for CommonStringSequence. */
  public static @Prototype CommonStringSequence get_proto() {
    return proto;
  }

  @Override
  public boolean enabled() {
    return dkconfig_enabled;
  }

  @Override
  protected CommonStringSequence instantiate_dyn(
      @Prototype CommonStringSequence this, PptSlice slice) {
    return new CommonStringSequence(slice);
  }

  // Don't define clone (use the Object version instead), because this.intersect is read-only.
  // protected Object clone();

  @Override
  public String repr(@GuardSatisfied CommonStringSequence this) {
    return "CommonStringSequence " + varNames() + ": count=\"" + count;
  }

  private String printIntersect(@GuardSatisfied CommonStringSequence this) {
    if (intersect == null) {
      return "{}";
    }

    StringJoiner result = new StringJoiner(", ", "{", "}");
    for (int i = 0; i < intersect.length; i++) {
      result.add(intersect[i]);
    }
    return result.toString();
  }

  @SideEffectFree
  @Override
  public String format_using(@GuardSatisfied CommonStringSequence this, OutputFormat format) {
    if (format == OutputFormat.DAIKON) {
      return format_daikon();
    }
    if (format == OutputFormat.CSHARPCONTRACT) {
      return format_csharp_contract();
    }

    return format_unimplemented(format);
  }

  public String format_daikon(@GuardSatisfied CommonStringSequence this) {
    return printIntersect() + " subset of " + var().name();
  }

  public String format_csharp_contract(@GuardSatisfied CommonStringSequence this) {
    if (intersect == null) {
      return "()";
    }

    if (intersect.length == 1) {
      return var().csharp_name() + ".Contains(" + intersect[0] + ")";
    }

    StringJoiner exp = new StringJoiner(", ", "{", "}");
    for (String i : intersect) {
      exp.add(i);
    }

    return "Contract.ForAll(new[] " + exp + " , x => " + var().csharp_name() + ".Contains(x))";
  }

  @Override
  public InvariantStatus check_modified(@Interned String @Interned [] a, int count) {
    if (a == null) {
      return InvariantStatus.FALSIFIED;
    } else if (intersect == null) {
      return InvariantStatus.NO_CHANGE;
    } else {
      String[] tmp = new String[intersect.length];
      int size = 0;
      for (int i = 1; i < a.length; i++) {
        if ((ArraysPlume.indexOf(intersect, a[i]) != -1)
            && ((size == 0)
                || (ArraysPlume.indexOf(ArraysPlume.subarray(tmp, 0, size), a[i]) == -1)))
          tmp[size++] = a[i];
      }

      if (size == 0) {
        return InvariantStatus.FALSIFIED;
      }
    }
    return InvariantStatus.NO_CHANGE;
  }

  @Override
  public InvariantStatus add_modified(@Interned String @Interned [] a, int count) {
    if (a == null) {
      return InvariantStatus.FALSIFIED;
    } else if (intersect == null) {
      intersect = Intern.intern(a);
      return InvariantStatus.NO_CHANGE;
    } else {
      @Interned String[] tmp = new @Interned String[intersect.length];
      int size = 0;
      for (int i = 1; i < a.length; i++) {
        if ((ArraysPlume.indexOf(intersect, a[i]) != -1)
            && ((size == 0)
                || (ArraysPlume.indexOf(ArraysPlume.subarray(tmp, 0, size), a[i]) == -1)))
          tmp[size++] = a[i];
      }

      if (size == 0) {
        return InvariantStatus.FALSIFIED;
      }
      intersect = Intern.intern(ArraysPlume.subarray(tmp, 0, size));
    }

    // Use a lesser count, because this invariant is frequently a false positive.
    // this.count += count;
    this.count++;
    return InvariantStatus.NO_CHANGE;
  }

  @Override
  protected double computeConfidence() {
    throw new Error("Not yet implemented");
  }

  @Pure
  public @Nullable DiscardInfo isObviousImplied() {
    return null;
  }

  @Pure
  @Override
  public boolean isSameFormula(Invariant other) {
    assert other instanceof CommonStringSequence;
    return true;
  }

  @Override
  public @Nullable @NonPrototype CommonStringSequence merge(
      @Prototype CommonStringSequence this,
      List<@NonPrototype Invariant> invs,
      PptSlice parent_ppt) {
    @SuppressWarnings("nullness") // super.merge does not return null
    @NonNull CommonStringSequence result = (CommonStringSequence) super.merge(invs, parent_ppt);
    for (int i = 1; i < invs.size(); i++) {
      CommonStringSequence inv = (CommonStringSequence) invs.get(i);
      @Interned String @MonotonicNonNull @Interned [] invIntersect = inv.intersect;
      if (invIntersect != null) {
        InvariantStatus status = result.add_modified(invIntersect, inv.count);
        if (status == InvariantStatus.FALSIFIED) {
          return null;
        }
      }
    }
    return result;
  }
}
