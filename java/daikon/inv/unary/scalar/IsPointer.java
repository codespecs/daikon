package daikon.inv.unary.scalar;

import daikon.PptSlice;
import daikon.ProglangType;
import daikon.VarInfo;
import daikon.inv.Invariant;
import daikon.inv.InvariantStatus;
import daikon.inv.OutputFormat;
import daikon.inv.ValueSet;
import org.checkerframework.checker.lock.qual.GuardSatisfied;
import org.checkerframework.dataflow.qual.Pure;
import org.checkerframework.dataflow.qual.SideEffectFree;
import typequals.prototype.qual.Prototype;

/**
 * IsPointer is an invariant that heuristically determines whether an integer represents a pointer
 * (a 32-bit memory address). Since both a 32-bit integer and an address have the same
 * representation, sometimes a a pointer can be mistaken for an integer. When this happens, several
 * scalar invariants are computed for integer variables. Most of them would not make any sense for
 * pointers. Determining whether a 32-bit variable is a pointer can thus spare the computation of
 * many irrelevant invariants.
 *
 * <p>The basic approach is to discard the invariant if any values that are not valid pointers are
 * encountered. By default values between -100,000 and 100,000 (except 0) are considered to be
 * invalid pointers. This approach has been experimentally confirmed on Windows x86 executables.
 */
public class IsPointer extends SingleScalar {

  private static final long serialVersionUID = 20080221L;

  /** Boolean. True iff IsPointer invariants should be considered. */
  public static boolean dkconfig_enabled = false;

  // pointers values in ff_prepare.dtrace were starting from 65536,
  // in other dtrace files, the pointer values were even larger
  private static long largestNonPointerValue = 100000;

  private static long smallestNonPointerValue = -100000;

  protected IsPointer(PptSlice ppt) {
    super(ppt);
  }

  protected @Prototype IsPointer() {
    super();
  }

  private static @Prototype IsPointer proto = new @Prototype IsPointer();

  /** Returns the prototype invariant for IsPointer. */
  public static @Prototype IsPointer get_proto() {
    return proto;
  }

  @Override
  protected IsPointer instantiate_dyn(PptSlice slice) {
    return new IsPointer(slice);
  }

  @Override
  public boolean enabled() {
    return dkconfig_enabled;
  }

  @Override
  public boolean instantiate_ok(VarInfo[] vis) {
    if (!super.valid_types(vis)) {
      return false;
    }

    ProglangType file_rep_type = vis[0].file_rep_type;

    return (file_rep_type == ProglangType.INT);
  }

  @Override
  public InvariantStatus add_modified(long value, int count) {
    return check_modified(value, count);
  }

  @Override
  public InvariantStatus check_modified(long v, int count) {
    if (!isWithinPointerRange(v)) {
      return InvariantStatus.FALSIFIED;
    }
    return InvariantStatus.NO_CHANGE;
  }

  @Pure
  private boolean isWithinPointerRange(long value) {
    if (value == 0) {
      return true;
    }
    return (value >= largestNonPointerValue) || (value <= smallestNonPointerValue);
  }

  @Override
  @SideEffectFree
  public String format_using(@GuardSatisfied IsPointer this, OutputFormat format) {
    String varname = var().name_using(format);
    if (format == OutputFormat.SIMPLIFY) {
      // trivially true
      return "(AND)";
    } else if (format == OutputFormat.JAVA) {
      return "daikon.tools.runtimechecker.Runtime.isWithinPointerRange(" + varname + ")";
    } else {
      return varname + " is a pointer";
    }
  }

  @Override
  protected double computeConfidence() {
    return 1 - computeProbability();
  }

  // computes the probability that this is the result
  // of chance
  protected double computeProbability() {
    assert !falsified;

    ValueSet.ValueSetScalar vs = (ValueSet.ValueSetScalar) ppt.var_infos[0].get_value_set();

    if (!isWithinPointerRange(vs.max()) || !isWithinPointerRange(vs.min())) {
      return Invariant.PROBABILITY_UNJUSTIFIED;
    }

    return Invariant.PROBABILITY_JUSTIFIED;
  }

  @Pure
  @Override
  public boolean isSameFormula(Invariant other) {
    assert other instanceof IsPointer;
    return true;
  }
}
