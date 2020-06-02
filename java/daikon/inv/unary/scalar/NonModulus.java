package daikon.inv.unary.scalar;

import daikon.PptSlice;
import daikon.VarInfo;
import daikon.inv.Invariant;
import daikon.inv.InvariantStatus;
import daikon.inv.OutputFormat;
import java.util.NavigableSet;
import java.util.TreeSet;
import org.checkerframework.checker.lock.qual.GuardSatisfied;
import org.checkerframework.dataflow.qual.Pure;
import org.checkerframework.dataflow.qual.SideEffectFree;
import org.plumelib.util.Intern;
import org.plumelib.util.MathPlume;
import typequals.prototype.qual.Prototype;

/**
 * Represents long scalars that are never equal to {@code r (mod m)} where all other numbers in the
 * same range (i.e., all the values that {@code x} doesn't take from {@code min(x)} to {@code
 * max(x)}) are equal to {@code r (mod m)}. Prints as {@code x != r (mod m)}, where {@code r} is the
 * remainder and {@code m} is the modulus.
 */
public class NonModulus extends SingleScalar {
  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20020122L;

  // Variables starting with dkconfig_ should only be set via the
  // daikon.config.Configuration interface.
  /** Boolean. True iff NonModulus invariants should be considered. */
  public static boolean dkconfig_enabled = false;

  // Set elements = new HashSet();
  NavigableSet<Long> elements = new TreeSet<>();

  private long modulus = 0;
  private long remainder = 0;
  // The next two variables indicate whether the "modulus" and "result"
  // fields are up to date.
  // Indicates that no nonmodulus has been found; maybe with more
  // samples, one will appear.
  private boolean no_result_yet = false;
  // We don't continuously keep the modulus and remainder field up to date.
  // This indicates whether it is.
  private boolean results_accurate = false;

  private NonModulus(PptSlice ppt) {
    super(ppt);
  }

  private @Prototype NonModulus() {
    super();
  }

  private static @Prototype NonModulus proto = new @Prototype NonModulus();

  /** Returns the prototype invariant for NonModulus. */
  public static @Prototype NonModulus get_proto() {
    return proto;
  }

  /** NonModulus is only valid on integral types. */
  @Override
  public boolean instantiate_ok(VarInfo[] vis) {

    if (!valid_types(vis)) {
      return false;
    }

    return (vis[0].file_rep_type.baseIsIntegral());
  }

  /** Returns whether or not this invariant is enabled. */
  @Override
  public boolean enabled() {
    return dkconfig_enabled;
  }

  /** instantiate an invariant on the specified slice */
  @Override
  protected NonModulus instantiate_dyn(@Prototype NonModulus this, PptSlice slice) {
    return new NonModulus(slice);
  }

  @SideEffectFree
  @Override
  public NonModulus clone(@GuardSatisfied NonModulus this) {
    NonModulus result = (NonModulus) super.clone();
    result.elements = new TreeSet<Long>(this.elements);
    return result;
  }

  @Override
  public String repr(@GuardSatisfied NonModulus this) {
    return "NonModulus" + varNames() + ": m=" + modulus + ",r=" + remainder;
  }

  @SideEffectFree
  @Override
  public String format_using(@GuardSatisfied NonModulus this, OutputFormat format) {
    updateResults();
    String name = var().name_using(format);

    if (format == OutputFormat.DAIKON) {
      if (no_result_yet) {
        return name + " != ? (mod ?) ***";
      }
      return name + " != " + remainder + "  (mod " + modulus + ")";
    }

    if (no_result_yet) {
      return format_too_few_samples(format, null);
    }

    if (format.isJavaFamily()) {

      if (var().type.isFloat()) {
        return "daikon.Quant.fuzzy.ne(" + name + " % " + modulus + ", " + remainder + ")";
      } else {
        return name + " % " + modulus + " != " + remainder;
      }
    }

    if (format == OutputFormat.CSHARPCONTRACT) {
      return name + " % " + modulus + " != " + remainder;
    }

    if (format == OutputFormat.SIMPLIFY) {
      return "(NEQ (MOD "
          + var().simplify_name()
          + " "
          + simplify_format_long(modulus)
          + ") "
          + simplify_format_long(remainder)
          + ")";
    }

    return format_unimplemented(format);
  }

  // Set either modulus and remainder, or no_result_yet.
  void updateResults(@GuardSatisfied NonModulus this) {
    if (results_accurate) {
      return;
    }
    if (elements.size() == 0) {
      no_result_yet = true;
    } else {
      // Do I want to communicate back some information about the smallest
      // possible modulus?
      long[] result = MathPlume.nonmodulusStrictLong(elements.iterator());
      if (result == null) {
        no_result_yet = true;
      } else {
        remainder = result[0];
        modulus = result[1];
        no_result_yet = false;
      }
    }
    results_accurate = true;
  }

  @Override
  public InvariantStatus check_modified(long value, int count) {
    return InvariantStatus.NO_CHANGE;
  }

  // XXX have to deal with flowing this; maybe it should live at all ppts?
  @Override
  public InvariantStatus add_modified(long value, int count) {
    if (elements.add(Intern.internedLong(value))
        && results_accurate
        && !no_result_yet
        && (MathPlume.modNonnegative(value, modulus) == remainder)) results_accurate = false;
    return InvariantStatus.NO_CHANGE;
  }

  @Override
  protected double computeConfidence() {
    updateResults();
    if (no_result_yet) {
      return Invariant.CONFIDENCE_UNJUSTIFIED;
    }
    double probability_one_elt_nonmodulus = 1 - 1.0 / modulus;
    // return 1 - Math.pow(probability_one_elt_nonmodulus, ppt.num_mod_samples());
    return 1 - Math.pow(probability_one_elt_nonmodulus, ppt.num_samples());
  }

  @Pure
  @Override
  public boolean isSameFormula(Invariant o) {
    NonModulus other = (NonModulus) o;

    updateResults();
    other.updateResults();

    if (no_result_yet && other.no_result_yet) {
      return true;
    } else if (no_result_yet || other.no_result_yet) {
      return false;
    } else {
      return (modulus == other.modulus) && (remainder == other.remainder);
    }
  }

  /** Returns true if this has the given modulus and remainder. */
  public boolean hasModulusRemainder(long modulus, long remainder) {
    updateResults();
    if (no_result_yet) {
      return false;
    }

    return ((modulus == this.modulus) && (remainder == this.remainder));
  }

  @Pure
  @Override
  public boolean isExclusiveFormula(Invariant o) {
    updateResults();
    if (no_result_yet) {
      return false;
    }
    if (o instanceof NonModulus) {
      NonModulus other = (NonModulus) o;
      other.updateResults();
      if (other.no_result_yet) {
        return false;
      }
      return ((modulus == other.modulus) && (remainder != other.remainder));
    } else if (o instanceof Modulus) {
      Modulus other = (Modulus) o;
      return ((modulus == other.modulus) && (remainder == other.remainder));
    }

    return false;
  }
}
