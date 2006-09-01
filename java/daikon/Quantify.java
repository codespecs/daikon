package daikon;

import daikon.VarInfoName.QuantHelper;
import daikon.VarInfoName.QuantHelper.QuantifyReturn;

import java.util.*;

/**
 * Helper classes for quantification for various output formats
 */
public class Quantify {

  /**
   * Interface implemented by derived variables that form a slice
   * (eg, arr[i..j])
   */
  public interface Slice {
    public Term get_lower_bound();
    public Term get_upper_bound();
  }

  /**
   * Class the represents terms that can be used in variable expressions.
   * These include constants (such as 0 and 1), free variables used
   * for quantification (i, j, etc), and normal daikon variables
   */
  public static abstract class Term {
    public abstract String name();
  }

  public static class FreeVar extends Term {
    String name;

    public FreeVar (String name) {
      this.name = name;
    }

    public String name() {
      return name;
    }
  }

  public static class NewQuantifyReturn {
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
    public static NewQuantifyReturn quantify( VarInfo[] vars) {
      assert vars != null;

      // create empty result
      NewQuantifyReturn result = new NewQuantifyReturn();
      result.vars = new VarInfo[vars.length];
      for (int ii = 0; ii < vars.length; ii++)
        result.vars[ii] = vars[ii];
      result.bound_vars = new ArrayList<Term[]>();

      // all of the simple identifiers used by these variables
      Set<String> simples = new HashSet<String>();
      for (VarInfo vi : vars) {
        for (VarInfo cvar : vi.get_all_constituent_vars())
          simples.add (vi.name());
      }

      // Loop through each of the variables
      char tmp = 'i';
      for (VarInfo vi : vars) {

        // Get a unique free variable name
        String idx_name;
        do {
          idx_name = String.valueOf(tmp++);
        } while (simples.contains(idx_name));
        assert tmp <= 'z' : "Ran out of letters in quantification";
        Term idx = new FreeVar(idx_name);

        // call replace and unpack results
        //  VarInfoName[] replace_result = replace(roots[i], uq_elt, idx);
        //  VarInfoName root_prime = replace_result[0];
        //  VarInfoName lower = replace_result[1];
        //  VarInfoName upper = replace_result[2];

        //  result.root_primes[i] = root_prime;
        //  result.bound_vars.add(new VarInfoName[] { idx, lower, upper });
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
    private VarInfoName[] setNames;
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

      setNames = new VarInfoName[sets.length];
      for (int i=0; i<sets.length; i++)
        setNames[i] = sets[i].get_VarInfoName();

      qret = QuantHelper.quantify(setNames);


      // Build the quantifier
      StringBuffer quantifier = new StringBuffer();
      for (int i=0; i < qret.bound_vars.size(); i++) {
        VarInfoName var = qret.bound_vars.get(i)[0];
        quantifier.append (quantifierUniversal);
        quantifier.append (var.ioa_name());
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
      return getVarName(num).ioa_name() + " \\in " + setNames[num].ioa_name();
    }

    public String getClosingExp() {
      // This isn't very smart right now, but maybe later we can
      // pretty print based on whether we need parens or not
      return ")";
    }

    public VarInfoName getVarName (int num) {
      return qret.bound_vars.get(num) [0];
    }

    public String getVarString (int num) {
      return qret.bound_vars.get(num)[0].ioa_name();
    }

    public VarInfoName getVarIndexed (int num) {
      return qret.root_primes[num];
    }

    public String getVarIndexedString (int num) {
      return getVarIndexed(num).ioa_name();
    }

  }
}
