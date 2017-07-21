package daikon.inv;

import daikon.*;
import java.io.Serializable;
import plume.*;

// This is the successor to ValueTracker1.
// It is a thin wrapper around LimitedSizeIntSet.
// (Actually, maybe it will just subclass that.)

/**
 * ValueSet stores a set of unique integers. When adding a value, for efficiency its hash code is
 * added rather than the value itself. If the set size exceeds a specified limit, then its rep is
 * nulled.
 *
 * <p>This class is used for efficient justification tests.
 *
 * <p>Relevant subclasses are:
 *
 * <ul>
 *   <li>ValueSetScalar
 *   <li>ValueSetFloat
 *   <li>ValueSetScalarArray
 *   <li>ValueSetFloatArray
 *   <li>ValueSetString
 *   <li>ValueSetStringArray
 * </ul>
 *
 * These subclasses store a hashcode.
 */
public abstract class ValueSet extends LimitedSizeIntSet implements Serializable, Cloneable {
  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20020122L;

  public ValueSet(int max_values) {
    super(max_values);
  }

  // There is one ValueSet per variable (not one per slice or invariant),
  // so pre-allocating an array with 44 slots should not be a problem.  If
  // it is, then change LimitedSizeIntSet to optionally not pre-allocate
  // the entire array.
  /**
   * The number 44 comes from the fact that .9^44 &lt; .01. So, if the confidence limit is .01 and
   * the probability of a given event is set at .1, then 44 values is enough to demonstrate that
   * never seeing the event is statistically justified (not a coincidence).
   */
  static final int DEFAULT_MAX_VALUES = 44;

  public static ValueSet factory(VarInfo var_info) {
    ProglangType rep_type = var_info.rep_type;
    boolean is_scalar = rep_type.isScalar();
    if (is_scalar) {
      return new ValueSet.ValueSetScalar(DEFAULT_MAX_VALUES);
    } else if (rep_type == ProglangType.INT_ARRAY) {
      return new ValueSet.ValueSetScalarArray(DEFAULT_MAX_VALUES);
    } else if (Daikon.dkconfig_enable_floats && rep_type == ProglangType.DOUBLE) {
      return new ValueSet.ValueSetFloat(DEFAULT_MAX_VALUES);
    } else if (Daikon.dkconfig_enable_floats && rep_type == ProglangType.DOUBLE_ARRAY) {
      return new ValueSet.ValueSetFloatArray(DEFAULT_MAX_VALUES);
    } else if (rep_type == ProglangType.STRING) {
      return new ValueSet.ValueSetString(DEFAULT_MAX_VALUES);
    } else if (rep_type == ProglangType.STRING_ARRAY) {
      return new ValueSet.ValueSetStringArray(DEFAULT_MAX_VALUES);
    } else {
      throw new Error(
          "Can't create ValueSet for " + var_info.name() + " with rep type " + rep_type);
    }
  }

  /** Add the specified object (really, its hashcode) to the set. */
  public abstract void add(Object v1);

  /** Add stats from the specified value set. */
  protected abstract void add_stats(ValueSet other);

  /** Returns a short description of the values seen. */
  public abstract String repr_short();

  public void add(ValueSet other) {
    if (this.getClass() != other.getClass()) {
      throw new Error("ValueSet type mismatch: " + this.getClass() + " " + other.getClass());
    }
    addAll(other);
    add_stats(other);
  }

  public static class ValueSetScalar extends ValueSet {
    // We are Serializable, so we specify a version to allow changes to
    // method signatures without breaking serialization.  If you add or
    // remove fields, you should change this number to the current date.
    static final long serialVersionUID = 20031017L;

    long min_val = Long.MAX_VALUE;
    long max_val = Long.MIN_VALUE;

    public ValueSetScalar(int max_values) {
      super(max_values);
    }

    @Override
    public void add(Object v1) {
      assert v1 != null;
      long val = ((Long) v1).longValue();
      if (val < min_val) {
        min_val = val;
      }
      if (val > max_val) {
        max_val = val;
      }
      add(UtilMDE.hash(val));
    }

    @Override
    protected void add_stats(ValueSet other) {
      ValueSetScalar vs = (ValueSetScalar) other;
      min_val = Math.min(min_val, vs.min_val);
      max_val = Math.max(max_val, vs.max_val);
    }

    public long min() {
      return min_val;
    }

    public long max() {
      return max_val;
    }

    @Override
    public String repr_short() {
      if (size() > 0) {
        return (size() + " values " + min_val + ".." + max_val);
      } else {
        return "0 values";
      }
    }
  }

  public static class ValueSetFloat extends ValueSet {
    // We are Serializable, so we specify a version to allow changes to
    // method signatures without breaking serialization.  If you add or
    // remove fields, you should change this number to the current date.
    static final long serialVersionUID = 20031017L;

    double min_val = Double.MAX_VALUE;
    double max_val = -Double.MAX_VALUE;
    boolean can_be_NaN = false;

    public ValueSetFloat(int max_values) {
      super(max_values);
    }

    @Override
    public void add(Object v1) {
      assert v1 != null;
      double val = ((Double) v1).doubleValue();
      if (val < min_val) {
        min_val = val;
      }
      if (val > max_val) {
        max_val = val;
      }
      if (Double.isNaN(val)) {
        can_be_NaN = true;
      }
      add(UtilMDE.hash(val));
    }

    @Override
    protected void add_stats(ValueSet other) {
      ValueSetFloat vs = (ValueSetFloat) other;
      min_val = Math.min(min_val, vs.min_val);
      max_val = Math.max(max_val, vs.max_val);
      can_be_NaN = can_be_NaN || vs.can_be_NaN;
    }

    public double min() {
      return min_val;
    }

    public double max() {
      return max_val;
    }

    public boolean canBeNaN() {
      return can_be_NaN;
    }

    @Override
    public String repr_short() {
      if (size() > 0) {
        return (size()
            + " values "
            + min_val
            + ".."
            + max_val
            + "; "
            + (can_be_NaN ? "can be " : "never ")
            + "NaN");
      } else {
        return "0 values";
      }
    }
  }

  public static class ValueSetScalarArray extends ValueSet {
    // We are Serializable, so we specify a version to allow changes to
    // method signatures without breaking serialization.  If you add or
    // remove fields, you should change this number to the current date.
    static final long serialVersionUID = 20031017L;

    long min_val = Long.MAX_VALUE;
    long max_val = Long.MIN_VALUE;
    int max_length = 0;
    int elem_cnt = 0;
    int multi_arr_cnt = 0; // number of arrays with 2 or more elements

    public ValueSetScalarArray(int max_values) {
      super(max_values);
    }

    @Override
    public void add(Object v1) {
      assert v1 != null;
      long[] val = (long[]) v1;
      for (int i = 0; i < val.length; i++) {
        if (val[i] < min_val) {
          min_val = val[i];
        }
        if (val[i] > max_val) {
          max_val = val[i];
        }
      }
      elem_cnt += val.length;
      if (val.length > 1) multi_arr_cnt++;
      if (val.length > max_length) max_length = val.length;
      add(UtilMDE.hash((long[]) v1));
    }

    @Override
    protected void add_stats(ValueSet other) {
      ValueSetScalarArray vs = (ValueSetScalarArray) other;
      min_val = Math.min(min_val, vs.min_val);
      max_val = Math.max(max_val, vs.max_val);
      elem_cnt += vs.elem_cnt;
      multi_arr_cnt += vs.multi_arr_cnt;
      max_length = Math.max(max_length, vs.max_length);
    }

    public long min() {
      return min_val;
    }

    public long max() {
      return max_val;
    }

    public int elem_cnt() {
      return elem_cnt;
    }

    public int multi_arr_cnt() {
      return multi_arr_cnt;
    }

    public int max_length() {
      return max_length;
    }

    @Override
    public String repr_short() {
      if (size() > 0) {
        return (size() + " values " + min_val + ".." + max_val);
      } else {
        return "0 values";
      }
    }
  }

  public static class ValueSetFloatArray extends ValueSet {
    // We are Serializable, so we specify a version to allow changes to
    // method signatures without breaking serialization.  If you add or
    // remove fields, you should change this number to the current date.
    static final long serialVersionUID = 20031017L;

    double min_val = Long.MAX_VALUE;
    double max_val = Long.MIN_VALUE;
    boolean can_be_NaN = false;
    int max_length = 0;
    int elem_cnt = 0;
    int multi_arr_cnt = 0; // number of arrays with 2 or more elements

    public ValueSetFloatArray(int max_values) {
      super(max_values);
    }

    @Override
    public void add(Object v1) {
      assert v1 != null;
      double[] val = (double[]) v1;
      for (int i = 0; i < val.length; i++) {
        if (val[i] < min_val) {
          min_val = val[i];
        }
        if (val[i] > max_val) {
          max_val = val[i];
        }
        if (Double.isNaN(val[i])) {
          can_be_NaN = true;
        }
      }
      elem_cnt += val.length;
      if (val.length > 1) multi_arr_cnt++;
      if (val.length > max_length) max_length = val.length;
      add(UtilMDE.hash(val));
    }

    @Override
    protected void add_stats(ValueSet other) {
      ValueSetFloatArray vs = (ValueSetFloatArray) other;
      min_val = Math.min(min_val, vs.min_val);
      max_val = Math.max(max_val, vs.max_val);
      can_be_NaN = can_be_NaN || vs.can_be_NaN;
      elem_cnt += vs.elem_cnt;
      multi_arr_cnt += vs.multi_arr_cnt;
      max_length = Math.max(max_length, vs.max_length);
    }

    public double min() {
      return min_val;
    }

    public double max() {
      return max_val;
    }

    public boolean canBeNaN() {
      return can_be_NaN;
    }

    public int elem_cnt() {
      return elem_cnt;
    }

    public int multi_arr_cnt() {
      return multi_arr_cnt;
    }

    public int max_length() {
      return max_length;
    }

    @Override
    public String repr_short() {
      if (size() > 0) {
        return (size()
            + " values "
            + min_val
            + ".."
            + max_val
            + "; "
            + (can_be_NaN ? "can be " : "never ")
            + "NaN");
      } else {
        return "0 values";
      }
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

    @Override
    public void add(Object v1) {
      assert v1 != null;
      add(UtilMDE.hash((String) v1));
    }

    @Override
    protected void add_stats(ValueSet other) {}

    @Override
    public String repr_short() {
      return (size() + " values ");
    }
  }

  public static class ValueSetStringArray extends ValueSet {
    // We are Serializable, so we specify a version to allow changes to
    // method signatures without breaking serialization.  If you add or
    // remove fields, you should change this number to the current date.
    static final long serialVersionUID = 20031017L;

    int elem_cnt = 0;
    int multi_arr_cnt = 0; // number of arrays with 2 or more elements

    public ValueSetStringArray(int max_values) {
      super(max_values);
    }

    @Override
    public void add(Object v1) {
      assert v1 != null;
      String[] val = (String[]) v1;
      elem_cnt += val.length;
      if (val.length > 1) multi_arr_cnt++;
      add(UtilMDE.hash(val));
    }

    @Override
    protected void add_stats(ValueSet other) {
      ValueSetStringArray vs = (ValueSetStringArray) other;
      elem_cnt += vs.elem_cnt;
      multi_arr_cnt += vs.multi_arr_cnt;
    }

    public int elem_cnt() {
      return elem_cnt;
    }

    public int multi_arr_cnt() {
      return multi_arr_cnt;
    }

    @Override
    public String repr_short() {
      return (size() + " values ");
    }
  }
}
