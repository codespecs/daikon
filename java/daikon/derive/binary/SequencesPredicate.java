package daikon.derive.binary;

import daikon.*;
import daikon.derive.*;

import utilMDE.*;

import java.util.ArrayList;
import java.util.List;
import java.util.Arrays;
import java.util.Random;

import org.apache.log4j.Category;

/**
 * Derived variable representing the selecting of elements of one
 * sequence based on the values of another sequence.  We only
 * predicate if we know that both sequences came from the same
 * original data structure.  Derived type is the same as that of
 * the first sequence.
 *
 **/

public final class SequencesPredicate
  extends BinaryDerivation
{
  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20020122L;

  /**
   * Debug tracer
   *
   **/
  public static final Category debug = Category.getInstance("daikon.derive.binary.SequencesPredicate");

  // Variables starting with dkconfig_ should only be set via the
  // daikon.config.Configuration interface.
  /**
   * Boolean.  True iff SequencesPredicate derived variables should be generated.
   **/
  public static boolean dkconfig_enabled = true;

  public VarInfo var1() { return base1; }
  public VarInfo var2() { return base2; }

  /**
   * What value to predicate on.
   *
   **/
  private long choose;

  /**
   * What this predication is called (e.g. for choose == 0 and 1, use "false"
   * and "true").
   *
   **/
  private String name;

  /**
   * Create a new SequencesJoin derivation.
   * @param vi1 
   * @param vi2 The two variables this is based on
   **/
  public SequencesPredicate (VarInfo vi1, VarInfo vi2, long argChoose, String argName) {
    super(vi1, vi2);
    choose = argChoose;
    name = argName;
  }

  /**
   * Returns a subset of val1 such that the corresponding element in
   * var2 equals this.choose.  It is assumed that val1 and val2
   * originated from the same, larger data structure.
   * @param full_vt the value tuple of a program point to compute the
   * derived value from.
   **/
  public ValueAndModified computeValueAndModified(ValueTuple full_vt) {
    Object val1 = var1().getValue(full_vt);
    Object val2 = var2().getValue(full_vt);

    int length1 = -1;
    int length2 = -2; // They must equal in the end

    if (val1 == null) {
      length1 = 0;
    }

    if (val2 == null) {
      length2 = 0;
    }

    if (val1 instanceof long[]) {
      length1 = ((long[]) val1).length;
    }

    if (val2 instanceof long[]) {
      length2 = ((long[]) val2).length;
    }

    if (val1 instanceof Object[]) {
      length1 = ((long[]) val1).length;
    }

    Assert.assert(val2 == null || val2 instanceof long[]);

    Assert.assert(length1 == length2);

    int mod = ValueTuple.UNMODIFIED;
    if (var1().getModified(full_vt) == ValueTuple.MODIFIED) mod = ValueTuple.MODIFIED;
    if (var2().getModified(full_vt) == ValueTuple.MODIFIED) mod = ValueTuple.MODIFIED;
    if (var1().getModified(full_vt) == ValueTuple.MISSING) mod = ValueTuple.MISSING;
    if (var2().getModified(full_vt) == ValueTuple.MISSING) mod = ValueTuple.MISSING;
    /**
     * v1\v2  Unm  Mod  Mis
     *
     * Unm    Unm  Mod  Mis
     * Mod    Mod  Mod  Mis
     * Mis    Mis  Mis  Mis
     *
     **/

    long[] predicate = (long[]) val2;
    int count = 0;
    for (int i = 0; i < predicate.length; i++) {
      if (predicate[i] == choose) count += 1;
    }

    if (val1 instanceof long[]) {
      long[] result = new long[count];
      long[] values = (long[]) val1;
      int j = 0;
      for (int i = 0; i < length1; i++) {
	if (predicate[i] == choose) {
	  result[j] = (values[i]);
	  j++;
	}
      }
      return new ValueAndModified (Intern.intern(result), mod);
    } else if (val1 instanceof Object[]) {
      Object[] result = new Object[count];
      Object[] values = (Object[]) val1;
      int j = 0;
      for (int i = 0; i < length1; i++) {
	if (predicate[i] == choose) {
	  result[j] = (values[i]);
	  j++;
	}
      }
      return new ValueAndModified (Intern.intern(result), mod);
    } else if (val1 == null) {
      return new ValueAndModified (null, mod);
    } else {
      throw new RuntimeException("Invalid input arrays");
    }


  }
  


  protected VarInfo makeVarInfo() {
    VarInfo var1 = var1();
    VarInfo var2 = var2();
    return new VarInfo(VarInfoName.applyFunctionOfN("predicateSlice",
						    new VarInfoName[] {var1.name,
								       var2.name,
								       new VarInfoName.Simple(name)
						    }),
		       var1.type,
		       var1.file_rep_type,
		       var1.comparability
		       );
  }

  public String toString() {
    return "[SequencesPredicate of " + var1().toString() + " " +
      var2().toString() + " for " + name + "]";

  }

  public boolean isSameFormula(Derivation other) {
    // For Toh (tohn) to do.
    throw new UnsupportedOperationException("Not implemented");
  }

}
