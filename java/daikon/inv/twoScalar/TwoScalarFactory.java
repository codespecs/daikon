package daikon.inv.twoScalar;

import daikon.*;

import java.lang.reflect.*;

// I think this is likely to disappear, except possibly as a place to keep
// common data like minimum and maximum.

public class TwoScalarFactory {

  private static Method Math_abs;
  private static Method MathMDE_negate;
  private static Method MathMDE_bitwiseComplement;

  static {
    try {
      Class java_lang_Math = Class.forName("java.lang.Math");
      Class utilMDE_MathMDE = Class.forName("utilMDE.MathMDE");
      Class int_class = Integer.TYPE;
      Class[] int_class_args = new Class[] { int_class };

      Math_abs = java_lang_Math.getDeclaredMethod("abs", int_class_args);
      MathMDE_negate
	= utilMDE_MathMDE.getDeclaredMethod("negate", int_class_args);
      MathMDE_bitwiseComplement
	= utilMDE_MathMDE.getDeclaredMethod("bitwiseComplement", int_class_args);
    } catch (Exception e) {
      e.printStackTrace();
      throw new Error(e.toString());
    }
  }

  // Adds the appropriate new Invariant objects to the specified Invariants
  // collection.
  public static void instantiate(PptSlice ppt) {
    // Not really the right place for this test
    if (!((ppt.var_infos[0].type.dimensions() == 0)
	  && (ppt.var_infos[1].type.dimensions() == 0)))
      return;

    new Comparison(ppt);
    for (int i=0; i<2; i++) {
      boolean b = (i==1);
      new Function(ppt, Math_abs, b);
      new Function(ppt, MathMDE_negate, b);
      new Function(ppt, MathMDE_bitwiseComplement, b);
    }
    new Linear(ppt);
    // new NonAliased(ppt);
    new NonEqual(ppt);
  }

  private TwoScalarFactory() {
  }

}
