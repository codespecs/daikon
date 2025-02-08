package daikon.chicory;

import static java.lang.constant.ConstantDescs.*;

import java.lang.classfile.*;
import java.lang.classfile.Attributes;
import java.lang.classfile.attribute.*;
import java.lang.classfile.instruction.*;
import java.lang.constant.*;
import java.lang.reflect.AccessFlag;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.LinkedList;
import java.util.List;
import java.util.Optional;
import org.checkerframework.checker.lock.qual.GuardSatisfied;
import org.checkerframework.checker.nullness.qual.Nullable;

/**
 * The MethodGen24 class is a simplfied replacement for the BCEL MethodGen class. It collects and
 * stores all the relevant information about a method.
 */
public class MethodGen24 {
  /*
   * Corresponds to the fields found in a MethodModel object.
   */
  // Optional<CodeModel> code() Returns the body of this method, if there is one.
  // AccessFlags flags() Returns the access flags.
  // Utf8Entry methodName() Returns the name of this method.
  // Utf8Entry methodType() Returns the method descriptor of this method.
  // default MethodTypeDesc methodTypeSymbol() Returns the method descriptor of this method, as a
  // symbolic descriptor.
  // Optional<ClassModel> parent() Returns the class model this method is a member of, if known.

  /** The method's code model or null if no code. */
  private @Nullable CodeModel code;

  /** The method's access flags. */
  protected AccessFlags accessFlags;

  /** The method's name. */
  private String methodName;

  /** The method's type descriptor. */
  private MethodTypeDesc mtd;

  /** True if the method is static. */
  private boolean isStatic;

  /** The method's CodeAttribute. */
  private @Nullable CodeAttribute codeAttribute;

  // fields of the code attribute
  /** The method's maximum number of locals. */
  private int maxLocals;

  /** The method's maximum stack size. */
  private int maxStack;

  /** The name of the method's enclosing class. */
  private String className;

  /** The method's instruction list. */
  private List<CodeElement> codeList;

  /** The method's descriptor. */
  private String descriptor;

  /** The method's signature. */
  private String signature;

  // fields of the MethodTypeDescriptor
  /** The method's parameter types. */
  private ClassDesc[] paramTypes;

  /** The method's return type. */
  private ClassDesc returnType;

  /** The method's parameter names. */
  private String[] paramNames;

  /** The method's local variable table. */
  private LocalVariable[] localVariables;

  /**
   * Creates a MethodGen24 object.
   *
   * @param methodModel the method
   * @param className the containing class
   * @param inst_obj the daikon.chicory.Instrument instance
   */
  public MethodGen24(
      final MethodModel methodModel, final String className, daikon.chicory.Instrument24 inst_obj) {

    accessFlags = methodModel.flags();
    methodName = methodModel.methodName().stringValue();
    descriptor = methodModel.methodType().stringValue();
    this.className = className;
    isStatic = accessFlags.has(AccessFlag.STATIC);

    Optional<CodeModel> code = methodModel.code();
    if (code.isPresent()) {
      this.code = code.get();
      // the original elementList is immutable, so we need to make a copy
      this.codeList = new LinkedList<CodeElement>(this.code.elementList());
    } else {
      this.code = null;
      this.codeList = new ArrayList<>();
    }

    codeAttribute = methodModel.findAttribute(Attributes.code()).orElse(null);
    if (codeAttribute != null) {
      maxLocals = codeAttribute.maxLocals();
      // It seems like a checker bug that this assert is needed.
      assert codeAttribute != null : "@AssumeAssertion(nullness): just tested";
      maxStack = codeAttribute.maxStack();
    } else {
      maxLocals = 0;
      maxStack = 0;
    }

    SignatureAttribute sa = methodModel.findAttribute(Attributes.signature()).orElse(null);
    if (sa != null) {
      signature = sa.signature().stringValue();
    } else {
      // if no signature then probably no type arguments so descriptor will do
      signature = descriptor;
    }

    mtd = methodModel.methodTypeSymbol();
    paramTypes = mtd.parameterArray();
    returnType = mtd.returnType();

    // set up the localsTable
    inst_obj.localsTable = new ArrayList<>();

    for (CodeElement ce : codeList) {
      if (ce instanceof LocalVariable lv) {
        inst_obj.localsTable.add(lv);
      } else {
        // IS THIS WRONG?
        // we assume all LocalVariable elements come first
        if (ce instanceof Instruction) {
          break;
        }
      }
    }

    // Not necessarily sorted, so sort to make searching/insertion easier.
    inst_obj.localsTable.sort(Comparator.comparing(LocalVariable::slot));
    localVariables = inst_obj.localsTable.toArray(new LocalVariable[inst_obj.localsTable.length]);

    // System.out.println("locals:" + Arrays.toString(localVariables));
    // System.out.println("types:" + Arrays.toString(paramTypes));
    // System.out.println("length: " + paramTypes.length + ", offset: " + offset);

    paramNames = new String[paramTypes.length];
    int offset = isStatic ? 0 : 1;
    for (int i = 0; i < paramTypes.length; i++) {
      if ((offset + i) < localVariables.length) {
        paramNames[i] = localVariables[offset + i].name().stringValue();
      }
    }
  }

  /**
   * Return the method's access flags.
   *
   * @return the access flags
   */
  public AccessFlags getAccessFlags() {
    return accessFlags;
  }

  /**
   * Return true if the method is static.
   *
   * @return static flag
   */
  public final boolean isStatic() {
    return isStatic;
  }

  /**
   * Return the method's name.
   *
   * @return the method's name
   */
  public String getName() {
    return methodName;
  }

  /**
   * Return the method's return type.
   *
   * @return the method's return type
   */
  public ClassDesc getReturnType() {
    return returnType;
  }

  /**
   * Return the name of the ith parameter.
   *
   * @param i which parameter's name is requested
   * @return the parameter name
   */
  public String getParameterName(final int i) {
    return paramNames[i];
  }

  /**
   * Return the parameter names.
   *
   * @return the parameter names
   */
  public String[] getParameterNames() {
    return paramNames.clone();
  }

  /**
   * Return the type of the ith parameter.
   *
   * @param i which parameter's type is requested
   * @return the indicated parameter type
   */
  public ClassDesc getParameterType(final int i) {
    return paramTypes[i];
  }

  /**
   * Return the parameter types for the method.
   *
   * @return the parameter types for the method
   */
  public ClassDesc[] getParameterTypes() {
    return paramTypes.clone();
  }

  /**
   * Return the local variable table.
   *
   * @return the local variable table
   */
  public LocalVariable[] getLocalVariables() {
    return localVariables.clone();
  }

  /**
   * Return the name of the containing class.
   *
   * @return the class that contains this method
   */
  public String getClassName() {
    return className;
  }

  /**
   * Return the CodeAttribute for the method.
   *
   * @return the CodeAttribute for the method
   */
  public @Nullable CodeAttribute getCodeAttribute() {
    return codeAttribute;
  }

  /**
   * Return the maximum number of locals.
   *
   * @return the maximum number of locals
   */
  public int getMaxLocals() {
    return maxLocals;
  }

  /**
   * Return the maximum stack size.
   *
   * @return the maximum stack size
   */
  public int getMaxStack() {
    return maxStack;
  }

  /**
   * Return the descriptor for the current method.
   *
   * @return descriptor for the current method
   */
  public String getDescriptor() {
    return descriptor;
  }

  /**
   * Return the signature for the current method.
   *
   * @return signature for the current method
   */
  public String getSignature() {
    return signature;
  }

  /**
   * Return the name of the containing class.
   *
   * @return class that contains this method
   */
  public List<CodeElement> getInstructionList() {
    return codeList;
  }

  // Not sure we need this
  //  public void setInstructionList(List<CodeElement> il) {
  //     codeList = il;
  //  }

  // need to fancy up!
  @Override
  public final String toString(@GuardSatisfied MethodGen24 this) {
    return methodName;
  }

  /*
     @Override
     public final String toString() {
         final String access = Utility.accessToString(super.getAccessFlags());
         String signature = Type.getMethodSignature(super.getType(), paramTypes);
         signature = Utility.methodSignatureToString(signature, super.getName(), access, true, getLocalVariableTable(super.getConstantPool()));
         final StringBuilder buf = new StringBuilder(signature);
         for (final Attribute a : getAttributes()) {
             if (!(a instanceof Code || a instanceof ExceptionTable)) {
                 buf.append(" [").append(a).append("]");
             }
         }

         if (!throwsList.isEmpty()) {
             for (final String throwsDescriptor : throwsList) {
                 buf.append("\n\t\tthrows ").append(throwsDescriptor);
             }
         }
         return buf.toString();
     }
  */

}
