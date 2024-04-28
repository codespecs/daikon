package daikon;

import java.util.ArrayList;
import java.util.EnumSet;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.StringJoiner;
import org.checkerframework.checker.lock.qual.GuardSatisfied;
import org.checkerframework.checker.nullness.qual.NonNull;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.checkerframework.dataflow.qual.SideEffectFree;

/** Helper classes for quantification for various output formats. */
@SuppressWarnings("UnusedVariable") // messy code, need to investigate later
public class Quantify {

  /** Flags describing how quantifications are to be built. */
  public enum QuantFlags {
    /** two indices where they refer to corresponding positions. */
    ELEMENT_WISE,
    /** two indices where the second is one more than the first. */
    ADJACENT,
    /** two indices are different. */
    DISTINCT,
    /** Return the names of the index variables. */
    INCLUDE_INDEX;

    /** set with just ELEMENT_WISE turned on. */
    public static EnumSet<QuantFlags> element_wise() {
      return EnumSet.of(QuantFlags.ELEMENT_WISE);
    }

    /** set with just ADJACENT turned on. */
    public static EnumSet<QuantFlags> adjacent() {
      return EnumSet.of(QuantFlags.ADJACENT);
    }

    /** set with just DISTINCT turned on. */
    public static EnumSet<QuantFlags> distinct() {
      return EnumSet.of(QuantFlags.DISTINCT);
    }

    /** set with just INCLUDE_INDEX turned on. */
    public static EnumSet<QuantFlags> include_index() {
      return EnumSet.of(QuantFlags.INCLUDE_INDEX);
    }
  }

  /** Returns a set with ELEMENT_WISE turned on if specified. */
  public static EnumSet<QuantFlags> get_flags(boolean elementwise) {
    if (elementwise) {
      return EnumSet.of(QuantFlags.ELEMENT_WISE);
    } else {
      return EnumSet.noneOf(QuantFlags.class);
    }
  }

  /**
   * Class the represents terms that can be used in variable expressions. These include constants
   * (such as 0 and 1), free variables used for quantification (i, j, etc), and normal Daikon
   * variables.
   */
  public abstract static class Term {
    @SideEffectFree
    public abstract String name(@GuardSatisfied Term this);

    @SideEffectFree
    public String esc_name() {
      return name();
    }

    @SideEffectFree
    public String jml_name() {
      return esc_name();
    }

    @SideEffectFree
    public String jml_name(boolean in_prestate) {
      return jml_name();
    }

    @SideEffectFree
    public String simplify_name() {
      return name();
    }

    @SideEffectFree
    public String csharp_name() {
      return name();
    }

    @SideEffectFree
    protected static String name_with_offset(String name, int offset) {
      if (offset == 0) {
        return name;
      } else {
        return String.format("%s%+d", name, offset);
      }
    }
  }

  /** Free variable normally used for quantification. */
  public static class FreeVar extends Term {
    String name;

    public FreeVar(String name) {
      this.name = name;
    }

    @SideEffectFree
    @Override
    public String name(@GuardSatisfied FreeVar this) {
      return name;
    }

    @SideEffectFree
    @Override
    public String simplify_name() {
      return "|" + name + "|";
    }
  }

  /** Represents a constant integer. */
  public static class Constant extends Term {
    int val;

    public Constant(int val) {
      this.val = val;
    }

    @SideEffectFree
    @Override
    public String name(@GuardSatisfied Constant this) {
      return "" + val;
    }

    public int get_value() {
      return val;
    }
  }

  /** Represents the length of a sequence and an optional offset. */
  public static class Length extends Term {
    VarInfo sequence;
    int offset;

    public Length(VarInfo sequence, int offset) {
      this.sequence = sequence;
      this.offset = offset;
    }

    @SideEffectFree
    @Override
    public String toString(@GuardSatisfied Length this) {
      return name();
    }

    @SideEffectFree
    @Override
    public String name(@GuardSatisfied Length this) {
      return name_with_offset("size(" + sequence.name() + ")", offset);
    }

    @SideEffectFree
    @Override
    public String esc_name() {
      VarInfo arr_var = get_check_array_var("ESC");
      if (arr_var.isPrestate()) {
        assert arr_var.postState != null; // because isPrestate() = true
        return String.format(
            "\\old(%s)", name_with_offset(arr_var.postState.esc_name() + ".length", offset));
      } else { // array is not orig
        return name_with_offset(arr_var.esc_name() + ".length", offset);
      }
    }

    @SideEffectFree
    @Override
    public String jml_name() {
      VarInfo arr_var = get_check_array_var("JML");
      if (arr_var.isPrestate()) {
        assert arr_var.postState != null; // because isPrestate() = true
        String name = String.format("daikon.Quant.size(%s)", arr_var.postState.jml_name());
        return name_with_offset(String.format("\\old(%s)", name), offset);
        // return String.format ("\\old(%s)", name_with_offset (name, offset));
      } else {
        String name = String.format("daikon.Quant.size(%s)", arr_var.jml_name());
        return name_with_offset(name, offset);
      }
    }

    @SideEffectFree
    @Override
    public String jml_name(boolean in_prestate) {
      if (!in_prestate) {
        return jml_name();
      }

      VarInfo arr_var = get_check_array_var("JML");
      if (arr_var.isPrestate()) {
        assert arr_var.postState != null; // because isPrestate() = true
        String name = String.format("daikon.Quant.size(%s)", arr_var.postState.jml_name());
        return name_with_offset(name, offset);
      } else {
        String name = String.format("daikon.Quant.size(\\new(%s))", arr_var.jml_name());
        return name_with_offset(name, offset);
      }
    }

    @SideEffectFree
    @Override
    public String simplify_name() {
      VarInfo arr_var = get_check_array_var("Simplify");
      String length = String.format("(arrayLength %s)", arr_var.simplify_name());
      if (offset < 0) {
        return String.format("(- %s %d)", length, -offset);
      } else if (offset > 0) {
        return String.format("(+ %s %d)", length, offset);
      } else {
        return length;
      }
    }

    @SideEffectFree
    @Override
    public String csharp_name() {
      VarInfo arr_var = get_check_array_var("CHARPCONTRACT");
      return name_with_offset(arr_var.csharp_name() + ".Count()", offset);
    }

    public void set_offset(int offset) {
      this.offset = offset;
    }

    /**
     * Looks up the array variable which is the base of this array. Throws an exception if one does
     * not exist.
     */
    @SuppressWarnings("all:sideeffectfree") // throws exception in case of error
    @SideEffectFree
    private VarInfo get_check_array_var(String output_format) {
      VarInfo arr_var = sequence.get_base_array_hashcode();
      if (arr_var != null) {
        return arr_var;
      }

      throw new Daikon.UserError(
          String.format(
              "Error: Can't create %s expression for the size of an array: "
                  + "No base array (hashcode) variable declared for array '%s'"
                  + " in program point %s",
              output_format, sequence.name(), sequence.ppt.name()));
    }
  }

  /**
   * Represents a Daikon variable with an optional integer offset. Usually used for the bounds of a
   * slice.
   */
  public static class VarPlusOffset extends Term {
    VarInfo var;
    int offset;

    public VarPlusOffset(VarInfo var) {
      this(var, 0);
    }

    public VarPlusOffset(VarInfo var, int offset) {
      this.var = var;
      this.offset = offset;
    }

    @SideEffectFree
    @Override
    public String name(@GuardSatisfied VarPlusOffset this) {
      return name_with_offset(var.name(), offset);
    }

    @SideEffectFree
    @Override
    public String esc_name() {
      return name_with_offset(var.esc_name(), offset);
    }

    @SideEffectFree
    @Override
    public String jml_name() {
      return name_with_offset(var.jml_name(), offset);
    }

    @SideEffectFree
    @Override
    public String jml_name(boolean in_prestate) {
      if (!in_prestate) {
        return jml_name();
      }

      if (var.isPrestate()) {
        assert var.postState != null; // because isPrestate() = true
        return name_with_offset(var.postState.jml_name(), offset);
      } else {
        return name_with_offset(String.format("\\new(%s)", var.jml_name()), offset);
      }
    }

    @SideEffectFree
    @Override
    public String simplify_name() {
      if (offset < 0) {
        return String.format("(- %s %d)", var.simplify_name(), -offset);
      } else if (offset > 0) {
        return String.format("(+ %s %d)", var.simplify_name(), offset);
      } else {
        return var.simplify_name();
      }
    }
  }

  public static class QuantifyReturn {
    /** Variable being quantified. */
    public VarInfo var;

    /** Index into the variable. If null, variable is not a sequence. */
    public @Nullable Term index;

    public QuantifyReturn(VarInfo var) {
      this.var = var;
    }
  }

  /**
   * Given a list of sequences, determines a free variable that can be used as a subscript for each
   * sequence. If any of the vars are not sequences, no index is calculated for them.
   */
  public static QuantifyReturn[] quantify(VarInfo[] vars) {
    assert vars != null;

    // create empty result
    QuantifyReturn[] result = new QuantifyReturn[vars.length];
    for (int ii = 0; ii < vars.length; ii++) {
      result[ii] = new QuantifyReturn(vars[ii]);
    }

    // Determine all of the simple identifiers used by the given variables (vars)
    Set<String> simples = new HashSet<>();
    for (VarInfo vi : vars) {
      for (String name : vi.get_all_simple_names()) {
        simples.add(name);
      }
    }
    // System.out.printf("simple names = %s%n", simples);

    // Loop through each of the variables, choosing an index for each
    char tmp = 'i';
    for (int ii = 0; ii < vars.length; ii++) {
      VarInfo vi = vars[ii];

      // If this variable is not an array, there is not much to do
      if (!vi.file_rep_type.isArray()) {
        continue;
      }

      // Get a unique free variable name
      String idx_name;
      do {
        idx_name = String.valueOf(tmp++);
      } while (simples.contains(idx_name));
      assert tmp <= 'z' : "Ran out of letters in quantification";
      result[ii].index = new FreeVar(idx_name);
    }
    return result;
  }

  /** Class that represents an ESC quantification over one or two variables. */
  public static class ESCQuantification {

    private EnumSet<QuantFlags> flags;
    // private VarInfo[] vars;
    private VarInfo[] arr_vars;
    private String[] arr_vars_indexed;
    private String[] quants;
    private String quant;
    private Term[] indices;

    public ESCQuantification(EnumSet<QuantFlags> flags, VarInfo... vars) {
      this.flags = flags.clone();

      assert vars != null;
      assert (vars.length == 1) || (vars.length == 2) : vars.length;
      assert vars[0].file_rep_type.isArray();

      // quantification for first var
      Term index1 = new FreeVar("i");
      String quant1 = bld_quant(vars[0], index1);
      VarInfo arr_var1 = vars[0].get_array_var();
      String arr_var1_index = arr_var1.esc_name(index1.esc_name());

      // If there is a second array variable, get quant for it
      if ((vars.length > 1) && vars[1].file_rep_type.isArray()) {
        Term index2 = new FreeVar("j");
        String quant2 = bld_quant(vars[1], index2);
        indices = new Term[] {index1, index2};
        quants = new String[] {quant1, quant2};
        if (flags.contains(QuantFlags.ELEMENT_WISE)) {
          quant =
              String.format(
                  "(\\forall int %s, %s; (%s && %s && %s == %s)",
                  index1.esc_name(),
                  index2.esc_name(),
                  quant1,
                  quant2,
                  index1.esc_name(),
                  index2.esc_name());
        } else {
          quant =
              String.format(
                  "(\\forall int %s, %s; (%s && %s)",
                  index1.esc_name(), index2.esc_name(), quant1, quant2);
        }

        VarInfo arr_var2 = vars[1].get_array_var();
        arr_vars = new VarInfo[] {arr_var1, arr_var2};
        String arr_var2_index = arr_var2.esc_name(index2.esc_name());
        arr_vars_indexed = new String[] {arr_var1_index, arr_var2_index};
      } else { // only one array variable
        indices = new Term[] {index1};
        quants = new String[] {quant1};
        quant = String.format("(\\forall int %s; (%s)", index1.esc_name(), quant1);
        arr_vars = new VarInfo[] {arr_var1};
        arr_vars_indexed = new String[] {arr_var1_index};
      }
    }

    /**
     * Returns a string quantification expression for the array variable var using index. The
     * expression is of the form
     *
     * <pre>{@code lower_bound <= index && index <= upper_bound}</pre>
     */
    private static String bld_quant(VarInfo var, Term index) {
      return String.format(
          "%s <= %s && %s <= %s",
          var.get_lower_bound().esc_name(),
          index.esc_name(),
          index.esc_name(),
          var.get_upper_bound().esc_name());
    }

    /**
     * Returns the quantification string. For example, if there is one array variable (a[]) that is
     * not a slice, it will return
     *
     * <pre>{@code
     * '(\forall int i; (0 <= i <= size(a[]) ==> '
     * }</pre>
     */
    public String get_quantification() {
      return quant + " ==> ";
    }

    /**
     * Returns the specified array variable indexed by its index. For example, if the array variable
     * is 'a.b[]' and the index is 'i', returns a.b[i].
     */
    public String get_arr_vars_indexed(int num) {
      return arr_vars_indexed[num];
    }
  }

  /** Class that represents an Simplify quantification over one or two variables. */
  public static class SimplifyQuantification {

    EnumSet<QuantFlags> flags;
    String quantification;
    String[] arr_vars_indexed;
    @Nullable String[] indices;

    public SimplifyQuantification(EnumSet<QuantFlags> flags, VarInfo... vars) {
      this.flags = flags.clone();

      assert vars != null;
      assert (vars.length == 1) || (vars.length == 2) : vars.length;
      assert vars[0].file_rep_type.isArray();

      if (flags.contains(QuantFlags.ADJACENT) || flags.contains(QuantFlags.DISTINCT)) {
        assert vars.length == 2;
      }

      QuantifyReturn[] qrets = quantify(vars);

      // build the forall predicate
      StringJoiner int_list, conditions;
      {
        // "i j ..."
        int_list = new StringJoiner(" ");
        // "(AND (<= ai i) (<= i bi) (<= aj j) (<= j bj) ...)"
        // if elementwise, also insert "(EQ (- i ai) (- j aj)) ..."
        conditions = new StringJoiner(" ");
        for (int i = 0; i < qrets.length; i++) {
          Term idx = qrets[i].index;
          if (idx == null) {
            continue;
          }
          VarInfo vi = qrets[i].var;
          Term low = vi.get_lower_bound();
          Term high = vi.get_upper_bound();
          int_list.add(idx.simplify_name());
          conditions.add("(<= " + low.simplify_name() + " " + idx.simplify_name() + ")");
          conditions.add("(<= " + idx.simplify_name() + " " + high.simplify_name() + ")");
          if (flags.contains(QuantFlags.ELEMENT_WISE) && (i >= 1)) {
            // Term[] _boundv = qret.bound_vars.get(i-1);
            // Term _idx = _boundv[0], _low = _boundv[1];
            @SuppressWarnings(
                "nullness") // if variable is a sequence (is it?), then index is non-null
            @NonNull Term _idx = qrets[i - 1].index;
            Term _low = qrets[i - 1].var.get_lower_bound();
            if (_low.simplify_name().equals(low.simplify_name())) {
              conditions.add("(EQ " + _idx.simplify_name() + " " + idx.simplify_name() + ")");
            } else {
              conditions.add("(EQ (- " + _idx.simplify_name() + " " + _low.simplify_name() + ")");
              conditions.add("(- " + idx.simplify_name() + " " + low.simplify_name() + "))");
            }
          }
          if (i == 1
              && (flags.contains(QuantFlags.ADJACENT) || flags.contains(QuantFlags.DISTINCT))) {
            // Term[] _boundv = qret.bound_vars.get(i-1);
            // Term prev_idx = _boundv[0];
            @SuppressWarnings(
                "nullness") // if variable is a sequence (is it?), then index is non-null
            @NonNull Term prev_idx = qrets[i - 1].index;
            if (flags.contains(QuantFlags.ADJACENT)) {
              conditions.add(
                  "(EQ (+ " + prev_idx.simplify_name() + " 1) " + idx.simplify_name() + ")");
            }
            if (flags.contains(QuantFlags.DISTINCT)) {
              conditions.add("(NEQ " + prev_idx.simplify_name() + " " + idx.simplify_name() + ")");
            }
          }
        }
      }
      quantification = "(FORALL (" + int_list + ") (IMPLIES (AND " + conditions + ") ";

      // stringify the terms
      List<String> avi_list = new ArrayList<>(vars.length);
      for (QuantifyReturn qret : qrets) {
        String arr_var_indexed;
        if (qret.index != null) {
          Term index = qret.index;
          VarInfo arr_var = qret.var.get_array_var();
          arr_var_indexed = arr_var.simplify_name(index.simplify_name());
          // System.out.printf("vi = %s, arr_var = %s%n", vi, arr_var);
        } else {
          arr_var_indexed = qret.var.simplify_name();
        }
        avi_list.add(arr_var_indexed);
        // result[i + 1] = qret.root_primes[i].simplify_name();
      }
      arr_vars_indexed = avi_list.toArray(new String[0]);

      // stringify the indices,
      // note that the index should be relative to the slice, not relative
      // to the original array (we used to get this wrong)
      indices = new @Nullable String[vars.length];
      for (int i = 0; i < qrets.length; i++) {
        // Term[] boundv = qret.bound_vars.get(i);
        // Term idx_var = boundv[0];
        QuantifyReturn qret = qrets[i];
        if (qret.index == null) {
          continue;
        }
        String idx_var_name = qret.index.simplify_name();
        String lower_bound = qret.var.get_lower_bound().simplify_name();
        String idx_expr = "(- " + idx_var_name + " " + lower_bound + ")";
        indices[i] = idx_expr;
      }
    }

    /** Returns the quantification string that quantifies over each of the free variables. */
    public String get_quantification() {
      return quantification;
    }

    /**
     * Returns the specified array variable indexed by its index. For example, if the array variable
     * is 'a[]' and the index is 'i', returns 'select i a'.
     */
    public String get_arr_vars_indexed(int num) {
      return arr_vars_indexed[num];
    }

    /**
     * Returns the term at the specified index.
     *
     * @param num the term's index
     * @return the term at the specified index
     */
    @SuppressWarnings("nullness:return") // possible application invariant?
    public String get_index(int num) {
      assert indices[num] != null; // will this assertion fail?
      return indices[num];
    }

    /** Returns the string to be appended to the end of the quantification. */
    public String get_closer() {
      return "))"; // close IMPLIES, FORALL
    }
  }
}
