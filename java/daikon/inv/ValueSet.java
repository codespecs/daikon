package daikon.inv;

import daikon.*;

import utilMDE.*;

import java.io.Serializable;
import java.util.*;


// This is the successor to ValueTracker1.
// It is a thin wrapper around LimitedSizeIntSet.
// (Actually, maybe it will just subclass that.)


/**
 * ValueSet stores up to some maximum number of unique non-zero integer
 * values, at which point its rep is nulled.  This is used for efficient
 * justification tests.
 **/
public abstract class ValueSet extends LimitedSizeIntSet
  implements Serializable, Cloneable
{
  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20020122L;

  public ValueSet(int max_values) {
    super(max_values);
  }

  public static ValueSet factory(VarInfo var_info) {
    ProglangType rep_type = var_info.rep_type;
    boolean is_scalar = rep_type.isScalar();
    if (is_scalar) {
      return new ValueSet.ValueSetScalar(44);
    } else if (rep_type == ProglangType.INT_ARRAY) {
      return new ValueSet.ValueSetScalarArray(44);
    } else if (Daikon.dkconfig_enable_floats
               && rep_type == ProglangType.DOUBLE) {
      return new ValueSet.ValueSetFloat(44);
    } else if (Daikon.dkconfig_enable_floats
               && rep_type == ProglangType.DOUBLE_ARRAY) {
      return new ValueSet.ValueSetFloatArray(44);
    } else if (rep_type == ProglangType.STRING) {
      return new ValueSet.ValueSetString(44);
    } else if (rep_type == ProglangType.STRING_ARRAY) {
      return new ValueSet.ValueSetStringArray(44);
    } else {
      throw new Error("Can't create ValueSet for " + var_info.name.name());
    }
  }

  /** track the specified object **/
  public abstract void add(Object v1);

  public void add(ValueSet other) {
    if (this.getClass() != other.getClass()) {
      throw new Error("ValueSet type mismatch: " + this.getClass() + " " + other.getClass());
    }
    addAll(other);
  }

  public static class ValueSetScalar extends ValueSet {
    // We are Serializable, so we specify a version to allow changes to
    // method signatures without breaking serialization.  If you add or
    // remove fields, you should change this number to the current date.
    static final long serialVersionUID = 20031017L;

    public ValueSetScalar(int max_values) {
      super(max_values);
    }
    public void add(Object v1) {
      Assert.assertTrue(v1 != null);
      add(UtilMDE.hash(((Long) v1).longValue()));
    }
  }

  public static class ValueSetFloat extends ValueSet {
    // We are Serializable, so we specify a version to allow changes to
    // method signatures without breaking serialization.  If you add or
    // remove fields, you should change this number to the current date.
    static final long serialVersionUID = 20031017L;

    public ValueSetFloat(int max_values) {
      super(max_values);
    }
    public void add(Object v1) {
      add(UtilMDE.hash(((Double) v1).doubleValue()));
    }
  }

  public static class ValueSetScalarArray extends ValueSet {
    // We are Serializable, so we specify a version to allow changes to
    // method signatures without breaking serialization.  If you add or
    // remove fields, you should change this number to the current date.
    static final long serialVersionUID = 20031017L;

    public ValueSetScalarArray(int max_values) {
      super(max_values);
    }
    public void add(Object v1) {
    	add(UtilMDE.hash((long[]) v1));
    }
  }

  public static class ValueSetFloatArray extends ValueSet {
    // We are Serializable, so we specify a version to allow changes to
    // method signatures without breaking serialization.  If you add or
    // remove fields, you should change this number to the current date.
    static final long serialVersionUID = 20031017L;

    public ValueSetFloatArray(int max_values) {
      super(max_values);
    }
    public void add(Object v1) {
      add(UtilMDE.hash((double[]) v1));
    }
  }

  public static class ValueSetString extends ValueSet {
    // We are Serializable, so we specify a version to allow changes to
    // method signatures without breaking serialization.  If you add or
    // remove fields, you should change this number to the current date.
    static final long serialVersionUID = 20031017L;

    public ValueSetString(int max_values) {
      super(max_values);
    }
    public void add(Object v1) {
      add(UtilMDE.hash((String) v1));
    }
  }

  public static class ValueSetStringArray extends ValueSet {
    // We are Serializable, so we specify a version to allow changes to
    // method signatures without breaking serialization.  If you add or
    // remove fields, you should change this number to the current date.
    static final long serialVersionUID = 20031017L;

    public ValueSetStringArray(int max_values) {
      super(max_values);
    }
    public void add(Object v1) {
      add(UtilMDE.hash((String[]) v1));
    }
  }



}
