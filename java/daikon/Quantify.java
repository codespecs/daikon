package daikon;

import daikon.VarInfoName.QuantHelper;
import daikon.VarInfoName.QuantHelper.QuantifyReturn;

import java.util.*;

/**
 * Helper classes for quantification for various output formats
 */
public class Quantify {

  public enum QuantFlags {ELEMENT_WISE};

  /** Returns a set with ELEMENT_WISE turned on if specified **/
  public static EnumSet<QuantFlags> get_flags (boolean elementwise) {
    if (elementwise)
      return EnumSet.of (QuantFlags.ELEMENT_WISE);
    else
      return EnumSet.noneOf (QuantFlags.class);
  }

  /**
   * Class the represents terms that can be used in variable expressions.
   * These include constants (such as 0 and 1), free variables used
   * for quantification (i, j, etc), and normal daikon variables
   */
  public static abstract class Term {
    public abstract String name();
    public String ioa_name() { return name(); }
    public String esc_name() { return name(); }
    public String jml_name() { return esc_name(); }
    public String jml_name(boolean in_prestate) { return jml_name(); }
    public String simplify_name() { return name(); }
    protected static String name_with_offset (String name, int offset) {
      if (offset == 0)
        return name;
      else
        return String.format ("%s%+d", name, offset);
    }
  }

  /**
   * Free variable normally used for quantification
   */
  public static class FreeVar extends Term {
    String name;
    public FreeVar (String name) {
      this.name = name;
    }
    public String name() {
      return name;
    }
  }

  /** Represents a constant integer **/
  public static class Constant extends Term {
    int val;
    public Constant (int val) { this.val = val; }
    public String name() { return "" + val; }
  }

  /** Represents the length of a sequence  and an optional offset **/
  public static class Length extends Term {
    VarInfo sequence;
    int offset;
    public Length (VarInfo sequence, int offset) {
      this.sequence = sequence;
      this.offset = offset;
    }
    public String name() {
      return name_with_offset ("size(" + sequence.name() + ")", offset);
    }
    public String esc_name() {
      VarInfo arr_var = sequence.get_base_array_hashcode();
      if (arr_var.isPrestate()) {
        return String.format ("\\old(%s)",
         name_with_offset (arr_var.postState.esc_name() + ".length", offset));
      } else { // array is not orig
        return name_with_offset (arr_var.esc_name() + ".length", offset);
      }
    }
    public String jml_name() {
      VarInfo arr_var = sequence.get_base_array_hashcode();
      if (arr_var.isPrestate()) {
        String name = String.format ("daikon.Quant.size(%s)",
                                     arr_var.postState.jml_name());
        return name_with_offset (String.format ("\\old(%s)", name), offset);
        // return String.format ("\\old(%s)", name_with_offset (name, offset));
      } else {
        String name = String.format ("daikon.Quant.size(%s)",
                                     arr_var.jml_name());
        return name_with_offset (name, offset);
      }
    }
    public String jml_name (boolean in_prestate) {
      if (!in_prestate)
        return jml_name();

      VarInfo arr_var = sequence.get_base_array_hashcode();
      if (arr_var.isPrestate()) {
        String name = String.format ("daikon.Quant.size(%s)",
                                     arr_var.postState.jml_name());
        return name_with_offset (name, offset);
      } else {
        String name = String.format ("daikon.Quant.size(\\new(%s))",
                                     arr_var.jml_name());
        return name_with_offset (name, offset);
      }
    }
    public String simplify_name() {
      String length = String.format ("(arrayLength %s)",
                   sequence.get_base_array_hashcode().simplify_name());
      if (offset < 0)
        return String.format ("(- %s %d)", length, -offset);
      else if (offset > 0)
        return String.format ("(+ %s %d)", length, offset);
      else
        return length;
    }

    public void set_offset (int offset) {
      this.offset = offset;
    }
  }

  /**
   * Represents a daikon variable with an optional integer offset.
   * usually used for the bounds of a slice.
   */
  public static class VarPlusOffset extends Term {
    VarInfo var;
    int offset;

    public VarPlusOffset (VarInfo var) {
      this (var, 0);
    }

    public VarPlusOffset (VarInfo var, int offset) {
      this.var = var;
      this.offset = offset;
    }

    public String name() {
      return name_with_offset (var.name(), offset);
    }

    public String esc_name() {
      return name_with_offset (var.esc_name(), offset);
    }

    public String jml_name() {
      return name_with_offset (var.jml_name(), offset);
    }

    public String jml_name (boolean in_prestate) {
      if (!in_prestate)
        return jml_name();

      if (var.isPrestate())
        return name_with_offset (var.postState.jml_name(), offset);
      else
        return name_with_offset (String.format ("\\new(%s)", var.jml_name()),
                                 offset);
    }
  }

  public static class QuantifyReturn {
    public VarInfo[] vars;
    // each element is Term[3] = <variable, lower, upper>
    public List<Term[]> bound_vars;
  }

  /**
   * Given a list of variables, changes all arrays and slices to
   * subscripts by inserting a new free variable; also return bounds
   * for the new variables.
   * <root*> -> <root'*, <index, lower, upper>*>
   * (The lengths of root* and root'* are the same; not sure about <i,l,u>*.)
   **/
  public static QuantifyReturn quantify( VarInfo[] vars) {
    assert vars != null;

    // create empty result
    QuantifyReturn result = new QuantifyReturn();
    result.vars = vars.clone();
    result.bound_vars = new ArrayList<Term[]>();

    // all of the simple identifiers used by these variables
    Set<String> simples = new HashSet<String>();
    for (VarInfo vi : vars) {
      for (VarInfo cvar : vi.get_all_constituent_vars())
        simples.add (vi.name());
    }

    // Loop through each of the variables
    char tmp = 'i';
    for (int ii = 0; ii < vars.length; ii++) {
      VarInfo vi = vars[ii];

      // Get a unique free variable name
      String idx_name;
      do {
        idx_name = String.valueOf(tmp++);
      } while (simples.contains(idx_name));
      assert tmp <= 'z' : "Ran out of letters in quantification";
      Term idx = new FreeVar(idx_name);

      Term lower = vi.get_lower_bound();
      Term upper = vi.get_upper_bound();

      result.vars[ii] = vi;
      result.bound_vars.add (new Term[] {idx, lower, upper});
    }
    return (result);
  }

  /**
   * It's too complex (and error prone) to hold quantification
   * results for IOA in a string array; so we create a helper object
   * that has accessors.  Otherwise this works just like a
   * format_ioa method here would work.
   **/
  public static class IOAQuantification {
    private static final String quantifierExistential = "\\E ";
    private static final String quantifierUniversal = "\\A ";

    // private VarInfo[] sets;
    private VarInfo[] vars;
    private String quantifierExp;
    private QuantifyReturn qret;
    private int numVars;

    public IOAQuantification (VarInfo v1) {
      this (new VarInfo[] { v1 });
    }

    public IOAQuantification (VarInfo v1, VarInfo v2) {
      this (new VarInfo[] { v1, v2 });
    }

    public IOAQuantification (VarInfo[] sets) {
      assert sets != null;

      // this.sets = sets;
      numVars = sets.length;

      vars = sets.clone();
      qret = quantify (vars);


      // Build the quantifier
      StringBuffer quantifier = new StringBuffer();
      for (int i=0; i < qret.bound_vars.size(); i++) {
        Term idx = qret.bound_vars.get(i)[0];
        quantifier.append (quantifierUniversal);
        quantifier.append (idx.ioa_name());
        quantifier.append (" : ");
        quantifier.append (sets[i].domainTypeIOA());
        quantifier.append (" ");

      }
      quantifierExp = quantifier.toString() + "(";
    }

    public String getQuantifierExp() {
      // \A i : DomainType
      return quantifierExp;
    }

    public String getMembershipRestrictions() {
      StringBuffer sb = new StringBuffer();
      for (int i = 0; i < numVars; i++) {
        if (i != 0) sb.append(" /\\ ");
        sb.append (getMembershipRestriction(i));
      }
      return sb.toString();
    }

    public String getMembershipRestriction(int num) {
      return getFreeVar(num).ioa_name() + " \\in " + vars[num].ioa_name();
    }

    public String getClosingExp() {
      // This isn't very smart right now, but maybe later we can
      // pretty print based on whether we need parens or not
      return ")";
    }

    public Term getFreeVar (int num) {
      return qret.bound_vars.get(num) [0];
    }

    public String getFreeVarName (int num) {
      return qret.bound_vars.get(num)[0].ioa_name();
    }

    //public VarInfo getVarIndexed (int num) {
    // }

    public String getVarIndexedString (int num) {
      return qret.vars[num].apply_subscript (getFreeVarName (num));
      // return getVarIndexed(num).ioa_name();
    }
  }

  /**
   * Class that represents an ESC quantification over one or two variables
   */
  public static class ESCQuantification {

    private EnumSet<QuantFlags> flags;
    private VarInfo[] vars;
    private VarInfo[] arr_vars;
    private String[] arr_vars_indexed;
    private String[] quants;
    private String quant;
    private Term[] indices;

    public ESCQuantification (EnumSet<QuantFlags> flags, VarInfo... vars) {
      this.flags = flags.clone();

      assert vars != null;
      assert (vars.length == 1) || (vars.length == 2) : vars.length;
      assert vars[0].file_rep_type.isArray();

      // quantification for first var
      Term index1 = new FreeVar ("i");
      String quant1 = bld_quant (vars[0], index1);
      VarInfo arr_var1 = vars[0].get_array_var();
      String arr_var1_index = arr_var1.esc_name (index1.esc_name());

      // If there is a second array variable, get quant for it
      if ((vars.length > 1) && (vars[1].file_rep_type.isArray())) {
        Term index2 = new FreeVar ("j");
        String quant2 = bld_quant (vars[1], index2);
        indices = new Term[] {index1, index2};
        quants  = new String[] {quant1, quant2};
        if (flags.contains (QuantFlags.ELEMENT_WISE))
          quant = String.format("(\\forall int %s, %s; (%s && %s && %s == %s)",
                                 index1.esc_name(), index2.esc_name(),
                                 quant1, quant2,
                                 index1.esc_name(), index2.esc_name());
        else
          quant = String.format ("(\\forall int %s, %s; (%s && %s)",
                       index1.esc_name(), index2.esc_name(), quant1, quant2);

        VarInfo arr_var2 = vars[1].get_array_var();
        arr_vars = new VarInfo[] {arr_var1, arr_var2};
        String arr_var2_index = arr_var2.esc_name (index2.esc_name());
        arr_vars_indexed = new String[] {arr_var1_index, arr_var2_index};
      } else { // only one array variable
        indices = new Term[] {index1};
        quants = new String[] {quant1};
        quant = String.format ("(\\forall int %s; (%s)", index1.esc_name(),
                               quant1);
        arr_vars = new VarInfo[] {arr_var1};
        arr_vars_indexed = new String[] {arr_var1_index};
      }
    }

    /**
     * Returns a string quantification expression for the array variable
     * var using index.  The expression is of the form
     * lower_bound <= index && index <= upper_bound
     */
    private String bld_quant (VarInfo var, Term index) {
      return String.format ("%s <= %s && %s <= %s",
                            var.get_lower_bound().esc_name(),
                            index.esc_name(), index.esc_name(),
                            var.get_upper_bound().esc_name());
    }

    /**
     * Returns the quantification string.  For example, if there is one
     * array variable (a[]) that is not a slice, it will return
     * '(\forall int i; (0 <= i <= size(a[]) ==> '
     **/
    public String get_quantification() {
      return quant + " ==> ";
    }

    /**
     * Returns the specified array variable indexed by its index.
     * For example, if the array variable is 'a.b[]' and the index is 'i',
     * returns a.b[i]
      **/
    public String get_arr_vars_indexed (int num) {
      return arr_vars_indexed [num];
    }
  }
}
