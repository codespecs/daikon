package daikon.inv;

import utilMDE.*;

import java.lang.reflect.*;

public class Functions {

  public final static Method[] unaryFunctions;
  /// Java language operators (in precedence order)
  // Unnecessary:  increment, decrement, and logicalComplement are subsumed
  // by LinearBinary.
  // public final static Method MathMDE_increment;
  // public final static Method MathMDE_decrement;
  public final static Method MathMDE_bitwiseComplement;
  // public final static Method MathMDE_logicalComplement;
  public final static Method MathMDE_negate;
  /// Non-operators.
  public final static Method Math_abs;


  public final static Method[] binarySymmetricFunctions;
  /// Java language operators (in precedence order, omitting boolean operators)
  // Unnecessary: plus is subsumed by LinearTernary.
  // Maybe instead of mul I should have a specific invariant that also
  // looks for a leading constant.
  public final static Method MathMDE_mul;
  // public final static Method MathMDE_plus;
  public final static Method MathMDE_bitwiseAnd;
  public final static Method MathMDE_logicalAnd;
  public final static Method MathMDE_bitwiseXor;
  public final static Method MathMDE_logicalXor;
  public final static Method MathMDE_bitwiseOr;
  public final static Method MathMDE_logicalOr;
  /// Non-operators.
  public final static Method Math_min;
  public final static Method Math_max;
  public final static Method MathMDE_gcd;


  public final static Method[] binaryNonSymmetricFunctions;
  /// Java language operators (in precedence order, omitting boolean operators)
  // Unnecessary: minus is subsumed by LinearTernary.
  // (Are the shifts, also?)
  public final static Method MathMDE_div;
  public final static Method MathMDE_mod;
  // public final static Method MathMDE_minus;
  public final static Method MathMDE_lshift;
  public final static Method MathMDE_rshiftSigned;
  public final static Method MathMDE_rshiftUnsigned;
  /// Non-operators.
  public final static Method MathMDE_pow;
  // public final static Method MathMDE_cmp;
  // public final static Method MathMDE_round;


  static Class java_lang_Math;
  static Class utilMDE_MathMDE;

  static {
    try {
      java_lang_Math = Class.forName("java.lang.Math");
      utilMDE_MathMDE = Class.forName("utilMDE.MathMDE");
    } catch (Exception e) {
      e.printStackTrace();
      throw new Error(e.toString());
    }
  }

  private static Method getMath(String name, Class[] parameterTypes) {
    try {
      return java_lang_Math.getDeclaredMethod(name, parameterTypes);
    } catch (Exception e) {
      e.printStackTrace();
      throw new Error(e.toString());
    }
  }

  private static Method getMathMDE(String name, Class[] parameterTypes) {
    try {
      return utilMDE_MathMDE.getDeclaredMethod(name, parameterTypes);
    } catch (Exception e) {
      e.printStackTrace();
      throw new Error(e.toString());
    }
  }

  static {
    Class int_class = Integer.TYPE;
    Class[] int_class_args = new Class[] { int_class };
    Class[] int_int_class_args = new Class[] { int_class, int_class };

    MathMDE_bitwiseComplement = getMathMDE("bitwiseComplement", int_class_args);
    MathMDE_negate = getMathMDE("negate", int_class_args);
    Math_abs = getMath("abs", int_class_args);
    unaryFunctions = new Method[] {
      MathMDE_bitwiseComplement,
      MathMDE_negate,
      Math_abs
    };

    MathMDE_mul = getMathMDE("mul", int_int_class_args);
    MathMDE_bitwiseAnd = getMathMDE("bitwiseAnd", int_int_class_args);
    MathMDE_logicalAnd = getMathMDE("logicalAnd", int_int_class_args);
    MathMDE_bitwiseXor = getMathMDE("bitwiseXor", int_int_class_args);
    MathMDE_logicalXor = getMathMDE("logicalXor", int_int_class_args);
    MathMDE_bitwiseOr = getMathMDE("bitwiseOr", int_int_class_args);
    MathMDE_logicalOr = getMathMDE("logicalOr", int_int_class_args);
    Math_min = getMath("min", int_int_class_args);
    Math_max = getMath("max", int_int_class_args);
    MathMDE_gcd = getMathMDE("gcd", int_int_class_args);
    binarySymmetricFunctions = new Method[] {
      MathMDE_mul,
      MathMDE_bitwiseAnd,
      MathMDE_logicalAnd,
      MathMDE_bitwiseXor,
      MathMDE_logicalXor,
      MathMDE_bitwiseOr,
      MathMDE_logicalOr,
      Math_min,
      Math_max,
      MathMDE_gcd
    };

    MathMDE_div = getMathMDE("div", int_int_class_args);
    MathMDE_mod = getMathMDE("mod", int_int_class_args);
    MathMDE_lshift = getMathMDE("lshift", int_int_class_args);
    MathMDE_rshiftSigned = getMathMDE("rshiftSigned", int_int_class_args);
    MathMDE_rshiftUnsigned = getMathMDE("rshiftUnsigned", int_int_class_args);
    MathMDE_pow = getMathMDE("pow", int_int_class_args);
    // MathMDE_cmp = getMathMDE("cmp", int_int_class_args);
    binaryNonSymmetricFunctions = new Method[] {
      MathMDE_div,
      MathMDE_mod,
      MathMDE_lshift,
      MathMDE_rshiftSigned,
      MathMDE_rshiftUnsigned,
      MathMDE_pow
    };

  }



  // don't permit instantiation
  private Functions() { }

}
