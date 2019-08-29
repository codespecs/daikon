package daikon.inv.unary.scalar;

import daikon.PptSlice;
import daikon.VarInfo;
import daikon.derive.unary.SequenceLength;
import daikon.inv.DiscardCode;
import daikon.inv.DiscardInfo;
import daikon.inv.Invariant;
import daikon.inv.InvariantStatus;
import daikon.inv.OutputFormat;
import org.checkerframework.checker.lock.qual.GuardSatisfied;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.checkerframework.dataflow.qual.Pure;
import org.checkerframework.dataflow.qual.SideEffectFree;
import org.plumelib.util.MathPlume;
import typequals.prototype.qual.Prototype;

/**
 * Represents the invariant {@code x == r (mod m)} where {@code x} is a long scalar variable, {@code
 * r} is the (constant) remainder, and {@code m} is the (constant) modulus.
 */
public class Modulus extends SingleScalar {
  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20030822L;

  // Variables starting with dkconfig_ should only be set via the
  // daikon.config.Configuration interface.
  /** Boolean. True iff Modulus invariants should be considered. */
  public static boolean dkconfig_enabled = false;

  long modulus = 0;
  long remainder = 0;

  // An arbitrarily-chosen value used for computing the differences among
  // all the values.  Arbitrary initial value 2222 will be replaced by the
  // first actual value seen.
  long value1 = 2222;
  // used for initializing value1
  boolean no_samples_seen = true;

  private Modulus(PptSlice ppt) {
    super(ppt);
  }

  private @Prototype Modulus() {
    super();
  }

  private static @Prototype Modulus proto = new @Prototype Modulus();

  /** Returns the prototype invariant for Modulus. */
  public static @Prototype Modulus get_proto() {
    return proto;
  }

  /** Returns whether or not this invariant is enabled. */
  @Override
  public boolean enabled() {
    return dkconfig_enabled;
  }

  /** Modulus is only valid on integral types. */
  @Override
  public boolean instantiate_ok(VarInfo[] vis) {

    if (!valid_types(vis)) {
      return false;
    }

    return (vis[0].file_rep_type.baseIsIntegral());
  }

  /** Instantiate an invariant on the specified slice. */
  @Override
  protected Modulus instantiate_dyn(@Prototype Modulus this, PptSlice slice) {
    return new Modulus(slice);
  }

  @Override
  public String repr(@GuardSatisfied Modulus this) {
    return "Modulus" + varNames() + ": modulus=" + modulus + ",remainder=" + remainder;
  }

  @SideEffectFree
  @Override
  public String format_using(@GuardSatisfied Modulus this, OutputFormat format) {
    String name = var().name_using(format);

    if (format == OutputFormat.DAIKON) {
      return var().name() + " == " + remainder + "  (mod " + modulus + ")";
    }

    if (format.isJavaFamily()) {
      name = var().name_using(format);
      if (var().type.isFloat()) {
        return "daikon.Quant.fuzzy.eq(" + name + " % " + modulus + ", " + remainder + ")";
      } else {
        return name + " % " + modulus + " == " + remainder;
      }
    }

    if (format == OutputFormat.CSHARPCONTRACT) {
      return var().csharp_name() + " % " + modulus + " == " + remainder;
    }

    //   if (format == OutputFormat.JAVA
    //     || format == OutputFormat.JML) {
    //   return var().name.name() + " % " + modulus + " == " + remainder;
    //  }

    if (format == OutputFormat.SIMPLIFY) {
      if (modulus > 0) {
        return "(EQ (MOD "
            + var().simplify_name()
            + " "
            + simplify_format_long(modulus)
            + ") "
            + simplify_format_long(remainder)
            + ")";
      } else {
        return format_too_few_samples(format, null);
      }
    }

    return format_unimplemented(format);
  }

  @Override
  public InvariantStatus check_modified(long value, int count) {
    if (modulus == 1) {
      // We shouldn't ever get to this case; the invariant should have been
      // destroyed instead.
      throw new Error("Modulus = 1");
    } else if (no_samples_seen) {
      return InvariantStatus.NO_CHANGE;
    } else if (value == value1) {
      // no new information, so nothing to do
      return InvariantStatus.NO_CHANGE;
    } else if (modulus == 0) {
      // only one value seen so far
      // REACHABLE?
      if (modulus == 1) {
        return InvariantStatus.FALSIFIED;
      }
    } else {
      long new_modulus_long = Math.abs(MathPlume.gcd(modulus, value1 - value));
      int new_modulus;
      if (new_modulus_long > Integer.MAX_VALUE || (new_modulus_long < Integer.MIN_VALUE)) {
        new_modulus = 1;
      } else {
        new_modulus = (int) new_modulus_long;
        assert new_modulus > 0;
      }
      if (new_modulus != modulus) {
        if (new_modulus == 1) {
          return InvariantStatus.FALSIFIED;
        }
      }
    }
    assert modulus != 1;
    return InvariantStatus.NO_CHANGE;
  }

  @Override
  public InvariantStatus add_modified(long value, int count) {
    if (modulus == 1) {
      // We shouldn't ever get to this case; the invariant should have been
      // destroyed instead.
      throw new Error("Modulus = 1");
      // assert falsified;
      // // We already know this confidence fails
      // return;
    } else if (no_samples_seen) {
      value1 = value;
      no_samples_seen = false;
      return InvariantStatus.NO_CHANGE;
    } else if (value == value1) {
      // no new information, so nothing to do
      return InvariantStatus.NO_CHANGE;
    } else if (modulus == 0) {
      // only one value seen so far
      long new_modulus = Math.abs(value1 - value);

      if (new_modulus == 1) {
        return InvariantStatus.FALSIFIED;
      }
      modulus = new_modulus;
      remainder = MathPlume.modNonnegative(value, modulus);
    } else {
      long new_modulus_long = Math.abs(MathPlume.gcd(modulus, value1 - value));
      int new_modulus;
      if (new_modulus_long > Integer.MAX_VALUE || (new_modulus_long < Integer.MIN_VALUE)) {
        new_modulus = 1;
      } else {
        new_modulus = (int) new_modulus_long;
        assert new_modulus > 0;
      }
      if (new_modulus != modulus) {
        if (new_modulus == 1) {
          return InvariantStatus.FALSIFIED;
        } else {
          remainder = remainder % new_modulus;
          modulus = new_modulus;
        }
      }
    }
    assert modulus != 1;
    return InvariantStatus.NO_CHANGE;
  }

  @Override
  protected double computeConfidence() {
    if (modulus == 1) {
      return Invariant.CONFIDENCE_NEVER;
    }
    if (modulus == 0) {
      return Invariant.CONFIDENCE_UNJUSTIFIED;
    }
    double probability_one_elt_modulus = 1 - 1.0 / modulus;
    // return 1 - Math.pow(probability_one_elt_modulus, ppt.num_mod_samples());
    return 1 - Math.pow(probability_one_elt_modulus, ppt.num_samples());
  }

  @Pure
  @Override
  public boolean isSameFormula(Invariant other) {
    Modulus otherModulus = (Modulus) other;

    boolean thisMeaningless = (modulus == 0 || modulus == 1);
    boolean otherMeaningless = (otherModulus.modulus == 0 || otherModulus.modulus == 1);

    if (thisMeaningless && otherMeaningless) {
      return true;
    } else {
      return (modulus != 1)
          && (modulus != 0)
          && (modulus == otherModulus.modulus)
          && (remainder == otherModulus.remainder);
    }
  }

  @Pure
  @Override
  public boolean isExclusiveFormula(Invariant other) {
    if ((modulus == 0) || (modulus == 1)) {
      return false;
    }

    // Weak test, can be strengthened.
    //  * x = 1 mod 4  is exclusive with  x = 6 mod 8
    //  * x = 1 mod 4  is exclusive with  x = 0 mod 2
    //  * x = 0 mod 4  is exclusive with  1 <= x <= 3
    if (other instanceof Modulus) {
      return ((modulus == ((Modulus) other).modulus) && (remainder != ((Modulus) other).remainder));
    } else if (other instanceof NonModulus) {
      return ((NonModulus) other).hasModulusRemainder(modulus, remainder);
    }

    return false;
  }

  // Look up a previously instantiated invariant.
  public static @Nullable Modulus find(PptSlice ppt) {
    assert ppt.arity() == 1;
    for (Invariant inv : ppt.invs) {
      if (inv instanceof Modulus) {
        return (Modulus) inv;
      }
    }
    return null;
  }

  /**
   * Checks to see if this is obvious over the specified variables. Implements the following checks:
   *
   * <pre>
   *    size(x[]) = r (mod m) &rArr; size(x[])-1 = (r-1) (mod m)
   * </pre>
   */
  @Pure
  @Override
  public @Nullable DiscardInfo isObviousDynamically(VarInfo[] vis) {

    // Do not show x-1 = a (mod b).  There must be a different mod
    // invariant over x.  JHP: This should really find the invariant rather
    // than presuming it is true.
    VarInfo x = vis[0];
    if ((x.derived instanceof SequenceLength) && (((SequenceLength) x.derived).shift != 0)) {
      return (new DiscardInfo(
          this,
          DiscardCode.obvious,
          "The invariant "
              + format()
              + " is implied by a mod invariant "
              + "over "
              + x.name()
              + " without the offset"));
    }
    return null;
  }
}
