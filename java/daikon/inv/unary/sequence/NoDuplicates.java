package daikon.inv.unary.sequence;

import daikon.*;
import daikon.inv.*;
import daikon.inv.binary.twoSequence.*;

import utilMDE.*;

import java.util.*;


public class NoDuplicates
  extends SingleSequence
{
  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20020122L;

  // Variables starting with dkconfig_ should only be set via the
  // daikon.config.Configuration interface.
  public static boolean dkconfig_enabled = true;

  final static boolean debugNoDuplicates = false;

  int elts = 0;

  protected NoDuplicates(PptSlice ppt) {
    super(ppt);
  }

  public static NoDuplicates instantiate(PptSlice ppt) {
    if (!dkconfig_enabled) return null;
    return new NoDuplicates(ppt);
  }

  public String repr() {
    return "NoDuplicates" + varNames() + ": "
      + "elts=\"" + elts;
  }

  public String format() {
    if (debugNoDuplicates) {
      System.out.println(repr());
    }
    return (var().name + " contains no duplicates");
  }

  /* IOA */
  public String format_ioa() {
    if (debugPrint.isDebugEnabled()) {
      debugPrint.debug ("Format_ioa: " + this.toString());
    }
    if (var().isIOASet())
      return "IOA Set " + var().name.ioa_name() + " contains no duplicates by definition";
    
    String[] form =
      VarInfoName.QuantHelper.format_ioa(new VarInfo[] {var(),var()});

    //     \A i, j(                 i \in X /\ j \ in X
    return form[0] + form[1] + " \\in " + var().name.ioa_name() + " /\\ " + form[3] + " \\in " + var().name.ioa_name() +
    //
      " /\\ " + form[2] + " = " + form[4] +
    //            i           =       j           )
      " => " + form[1] + " = " + form [3] + form[5];




    /***
    String s = "";
    for (int i = 0; i < form.length; i++) {
      s = s + form[i] + " | ";
    }
    return s;
    //    return form[0]+"("+form[1]+"="+form[2]+") => ("+form[4]+"="+form[5]+")"+form[3];
    **/
  }

  public String format_esc() {
    String classname = this.getClass().toString().substring(6); // remove leading "class"
    return "warning: method " + classname + ".format_esc() needs to be implemented: " + format();
  }

  public String format_simplify() {
    String classname = this.getClass().toString().substring(6); // remove leading "class"
    return "warning: method " + classname + ".format_simplify() needs to be implemented: " + format();
  }

  public void add_modified(long[] a, int count) {
    for (int i=1; i<a.length; i++) {
      if (ArraysMDE.indexOf(a, a[i]) < i) {
	flowThis();
        destroy();
        return;
      }
    }
    if (a.length > 1)
      elts += 1;
  }

  protected double computeProbability() {
    if (no_invariant) {
      return Invariant.PROBABILITY_NEVER;
    } else {
      return Math.pow(.9, elts);
    }
  }

  // Lifted from EltNonZero; should abstract some of this out.
  public boolean isObviousImplied() {
    // For every other NoDuplicates at this program point, see if there is a
    // subsequence relationship between that array and this one.

    PptTopLevel parent = (PptTopLevel)ppt.parent;
    for (Iterator itor = parent.invariants_iterator(); itor.hasNext(); ) {
      Invariant inv = (Invariant) itor.next();
      if ((inv instanceof NoDuplicates) && (inv != this) && inv.enoughSamples()) {
        VarInfo v1 = var();
        VarInfo v2 = inv.ppt.var_infos[0];
        if (SubSequence.isObviousDerived(v1, v2)) {
          // System.out.println("obvious: " + format() + "   because of " + inv.format());
          return true;
        }

        boolean this_var_first = (v1.varinfo_index < v2.varinfo_index);
        if (! this_var_first) { VarInfo temp = v1; v1 = v2; v2 = temp; }
        Assert.assert(v1.varinfo_index < v2.varinfo_index);
        PptSlice2 slice_2seq = parent.findSlice(v1, v2);
        if (slice_2seq == null) {
          // System.out.println("NoDuplicates.isObviousImplied: no slice for " + v1.name + ", " + v2.name);
        } else  {
          // slice_2seq != null
          SubSequence ss = SubSequence.find(slice_2seq);
          if (ss == null) {
            // System.out.println("NoDuplicates.isObviousImplied: no SubSequence for " + v1.name + ", " + v2.name);
          } else {
            // System.out.println("NoDuplicates.isObviousImplied: found SubSequence: " + ss.repr());
            if (this_var_first
                ? ss.var1_in_var2
                : ss.var2_in_var1) {
              return true;
            }
          }
        }
      }
    }

    return false;
  }

  public boolean isSameFormula(Invariant other)
  {
    Assert.assert(other instanceof NoDuplicates);
    return true;
  }
}
