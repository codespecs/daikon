package daikon.inv.unary.sequence;

import daikon.*;
import daikon.inv.*;
import daikon.inv.binary.twoSequence.*;

import daikon.derive.binary.SequencesPredicate;
import daikon.derive.binary.SequencesConcat;
import daikon.derive.binary.SequencesJoin;

import utilMDE.*;

import org.apache.log4j.Category;

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
  /**
   * Boolean.  True iff NoDuplicates invariants should be considered.
   **/
  public static boolean dkconfig_enabled = true;

  /**
   * Debug tracer
   **/

  public static final Category debug = Category.getInstance("daikon.inv.unary.sequence.NoDuplicates");
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

  public String format_using(OutputFormat format) {
    if (debug.isDebugEnabled()) {
      debug.debug(repr());
    }

    if (format == OutputFormat.DAIKON) {
      return var().name + " contains no duplicates";
    }

    if (format == OutputFormat.IOA) {
      return format_ioa();
    }

    return format_unimplemented(format);
  }

  /* IOA */
  public String format_ioa() {
    if (debugPrint.isDebugEnabled()) {
      debugPrint.debug ("Format_ioa: " + this.toString());
    }
    if (var().isIOASet())
      return "IOA Set " + var().name.ioa_name() + " contains no duplicates by definition";

    // We first see if we can special case for certain types of variables
    if (var().isDerived() && var().derived instanceof SequencesPredicate) {
      VarInfoName.FunctionOfN myName = (VarInfoName.FunctionOfN) var().name;
      String predicateValue = myName.getArg(2).ioa_name();

      SequencesPredicate derivation = (SequencesPredicate) var().derived;
      VarInfo varField = derivation.var1();
      VarInfoName.Field varFieldName = (VarInfoName.Field) varField.name;
      String fieldName = varFieldName.field;

      VarInfo varPredicateField = derivation.var2();
      VarInfoName.Field varPredicateFieldName = (VarInfoName.Field) varPredicateField.name;
      String predicateName = varPredicateFieldName.field;

      VarInfoName varOrigName = varFieldName.term;
      VarInfo fakeVarOrig = new VarInfo (varOrigName, varField.type, varField.file_rep_type, varField.comparability);

      VarInfoName.QuantHelper.IOAQuantification quant = new VarInfoName.QuantHelper.IOAQuantification (fakeVarOrig, fakeVarOrig);

      //     \A i : type, j : type(   i \in X
      return quant.getQuantifierExp() + "(" + quant.getMembershipRestriction(0) +
	//         /\ j \ in X
	" /\\ " + quant.getMembershipRestriction(1) +
	//           i.field = j.field
	" /\\ " + quant.getVarName(0) + "." + fieldName + " = " + quant.getVarName(1) + "." + fieldName +
	//           i.pred = value
	" /\\ " + quant.getVarName(0) + "." + predicateName + " = " + predicateValue +
	//           j.pred = value
	" /\\ " + quant.getVarName(1) + "." + predicateName + " = " + predicateValue +
	//  =>      i           =       j           )
	") => " + quant.getVarName(0) + " = " + quant.getVarName(1) + quant.getClosingExp();

    } else if (var().isDerived() && var().derived instanceof SequencesJoin) {
      SequencesJoin derivation = (SequencesJoin) var().derived;
      VarInfo varField1 = derivation.var1();
      VarInfoName.Field varFieldName1 = (VarInfoName.Field) varField1.name;
      String fieldName1 = varFieldName1.field;
      VarInfo varField2 = derivation.var2();
      VarInfoName.Field varFieldName2 = (VarInfoName.Field) varField2.name;
      String fieldName2 = varFieldName2.field;

      VarInfoName varOrigName = varFieldName1.term;
      VarInfo fakeVarOrig = new VarInfo (varOrigName, varField1.type, varField1.file_rep_type, varField1.comparability);

      VarInfoName.QuantHelper.IOAQuantification quant = new VarInfoName.QuantHelper.IOAQuantification (fakeVarOrig, fakeVarOrig);

      //     \A i : type, j : type(   i \in X
      return quant.getQuantifierExp() + "(" + quant.getMembershipRestriction(0) +
	//         /\ j \ in X
	" /\\ " + quant.getMembershipRestriction(1) +
	//           i.field = j.field
	" /\\ " + quant.getVarName(0) + "." + fieldName1 + " = " + quant.getVarName(1) + "." + fieldName1 +
	//           i.field = j.field
	" /\\ " + quant.getVarName(0) + "." + fieldName2 + " = " + quant.getVarName(1) + "." + fieldName2 +
	//  =>      i           =       j           )
	") => " + quant.getVarName(0) + " = " + quant.getVarName(1) + quant.getClosingExp();

    } else {
      VarInfoName.QuantHelper.IOAQuantification quant = new VarInfoName.QuantHelper.IOAQuantification (var(), var());

      //     \A i, j(                 i \in X /\ j \ in X
      return quant.getQuantifierExp() + "(" + quant.getMembershipRestriction(0) +
	" /\\ " + quant.getMembershipRestriction(1) +
	//           X[i] = X[j]
	" /\\ " + quant.getVarIndexed(0) + " = " + quant.getVarIndexed(1) +
	//  =>      i           =       j           )
	") => " + quant.getVarName(0) + " = " + quant.getVarName(1) + quant.getClosingExp();

    }

  }

  public void add_modified(long[] a, int count) {
    for (int i=1; i<a.length; i++) {
      if (ArraysMDE.indexOf(a, a[i]) < i) {
	flowThis();
	if (debug.isDebugEnabled()) {
	  debug.debug ("Flowing myself with: " + var().name.repr());
	  debug.debug (ArraysMDE.toString(a));
	}
        destroy();
        return;
      }
    }
    if (a.length > 1)
      elts += 1;
  }

  protected double computeProbability() {
    if (falsified) {
      return Invariant.PROBABILITY_NEVER;
    } else {
      return Math.pow(.9, elts);
    }
  }

  // Lifted from EltNonZero; should abstract some of this out.
  public boolean isObviousImplied() {
    // For every other NoDuplicates at this program point, see if there is a
    // subsequence relationship between that array and this one.

    PptTopLevel parent = ppt.parent;
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
