package daikon.inv;

import utilMDE.*;

import java.lang.reflect.*;

public class Functions {

  public final static Method Math_abs;
  public final static Method MathMDE_negate;
  public final static Method MathMDE_bitwiseComplement;

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

  // don't permit instantiation
  private Functions() { }

}
