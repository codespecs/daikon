package daikon.inv;

import java.lang.reflect.*;

import utilMDE.*;

public final class Functions {

  public final static String[] unaryFunctionNames;
  public final static Method[] unaryFunctions;

  public final static String[] binarySymmetricFunctionNames;
  public final static Method[] binarySymmetricFunctions;

  public final static String[] binaryNonSymmetricFunctionNames;
  public final static Method[] binaryNonSymmetricFunctions;

  private static Method[] methodNamesToMethods(String[] names) {
    try {
      Method[] result = new Method[names.length];
      for (int i=0; i<unaryFunctionNames.length; i++)
        result[i] = UtilMDE.methodForName(names[i]);
      return result;
    } catch (Exception e) {
      e.printStackTrace();
      throw new Error(e.toString());
    }
  }

  static {

    // I need to have the names available so the methods can be serialized.

    unaryFunctionNames = new String[] {
      /// Java language operators (in precedence order)
      // increment: subsumed by LinearBinary
      // decrement: subsumed by LinearBinary
      "utilMDE.MathMDE.bitwiseComplement(int)",
      // logicalComplement: subsumed by LinearBinary
      "utilMDE.MathMDE.negate(int)",
      /// Non-operators
      "java.lang.Math.abs(int)"
    };
    unaryFunctions = methodNamesToMethods(unaryFunctionNames);

    binarySymmetricFunctionNames = new String[] {
      /// Java language operators (in precedence order, omitting boolean operators)
      // Maybe instead of mul I should have a specific invariant that also
      // looks for a leading constant.
      "utilMDE.MathMDE.mul(int,int)",
      // plus: subsumed by LinearTernary.
      "utilMDE.MathMDE.bitwiseAnd(int,int)",
      "utilMDE.MathMDE.logicalAnd(int,int)",
      "utilMDE.MathMDE.bitwiseXor(int,int)",
      "utilMDE.MathMDE.logicalXor(int,int)",
      "utilMDE.MathMDE.bitwiseOr(int,int)",
      "utilMDE.MathMDE.logicalOr(int,int)",
      /// Non-operators.
      "java.lang.Math.min(int,int)",
      "java.lang.Math.max(int,int)",
      "utilMDE.MathMDE.gcd(int,int)"
    };
    binarySymmetricFunctions = methodNamesToMethods(binarySymmetricFunctionNames);

    binaryNonSymmetricFunctionNames = new String[] {
      /// Java language operators (in precedence order, omitting boolean operators)
      "utilMDE.MathMDE.div(int,int)",
      "utilMDE.MathMDE.mod(int,int)",
      // minus: subsumed by LinearTernary
      // (Are the shifts also subsumed by LinearTernary?)
      "utilMDE.MathMDE.lshift(int,int)",
      "utilMDE.MathMDE.rshiftSigned(int,int)",
      "utilMDE.MathMDE.rshiftUnsigned(int,int)",
      /// Non-operators.
      "utilMDE.MathMDE.pow(int,int)"
      // MathMDE_cmp = "utilMDE.MathMDE.cmp(int,int)"
      // MathMDE_cmp = "utilMDE.MathMDE.round(int,int)"
    };
    binaryNonSymmetricFunctions = methodNamesToMethods(binaryNonSymmetricFunctionNames);

  }

  // don't permit instantiation
  private Functions() { }

}
