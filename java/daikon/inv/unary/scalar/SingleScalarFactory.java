package daikon.inv.unary.scalar;

import daikon.*;

import utilMDE.*;

import java.util.*;

public final class SingleScalarFactory {

  // Adds the appropriate new Invariant objects to the specified Invariants
  // collection.
  public static Vector instantiate(PptSlice ppt) {
    // System.out.println("Ppt arity " + ppt.arity() + " " + ppt.name() + " " + ppt);
    Assert.assertTrue(ppt.arity() == 1);
    VarInfo var = ppt.var_infos[0];
    // Assert.assertTrue(! var.rep_type.isArray());
    Assert.assertTrue(var.rep_type == ProglangType.INT);

    Vector result = new Vector();
    result.add(OneOfScalar.instantiate(ppt));
    { // previously only if (pass == 2)
      { // previously only if !dynamicConstant

        // WEIRD: Adding this if statement (which I believe is a proper
        // change, only eliminating boolean NonZero results) causes output
        // diffs:  new conditional program points show up (and others
        // disappear).
        // if (var.file_rep_type.isIntegral()
        //  || var.file_rep_type == ProglangType.HASHCODE)
        {
          result.add(NonZero.instantiate(ppt));
          // if (Debug.logOn() && Debug.ppt_match (ppt))
          //   System.out.println("Instantiated NonZero (var.file_rep_type=" + var.file_rep_type + "): " + ppt.name());
        }
        if (var.file_rep_type.isIntegral()) {
          result.add(LowerBound.instantiate(ppt));
          result.add(Modulus.instantiate(ppt));
          result.add(NonModulus.instantiate(ppt));
          result.add(UpperBound.instantiate(ppt));
          result.addAll (RangeInt.instantiate_all (ppt));
          // Add a line like this for each invariant you choose to add.
          // "Positive" is a pedagogical example only and should not be
          // used in normal use.
          // result.add(Positive.instantiate(ppt));

          // if (Debug.logOn() && Debug.ppt_match (ppt))
          //   System.out.println("Instantiated LowerBound: " + ppt.name());
        } else {
          // This is suppressed because of types; not sure what global
          // variable to increment for statistics output.

          // if (Debug.logOn() && Debug.ppt_match (ppt))
          //   System.out.println("Suppressed LowerBound: " + ppt.name());
        }
      }
    }
    return result;
  }

  private SingleScalarFactory() {
  }

}
