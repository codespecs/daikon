package daikon.chicory;

import java.lang.classfile.AccessFlags;
import java.lang.classfile.Attributes;
import java.lang.classfile.ClassBuilder;
import java.lang.classfile.CodeElement;
import java.lang.classfile.CodeModel;
import java.lang.classfile.Instruction;
import java.lang.classfile.MethodModel;
import java.lang.classfile.attribute.CodeAttribute;
import java.lang.classfile.attribute.SignatureAttribute;
import java.lang.classfile.constantpool.ConstantPoolBuilder;
import java.lang.classfile.instruction.LocalVariable;
import java.lang.constant.ClassDesc;
import java.lang.constant.MethodTypeDesc;
import java.lang.reflect.AccessFlag;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.LinkedList;
import java.util.List;
import java.util.Optional;
import org.checkerframework.checker.lock.qual.GuardSatisfied;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.checkerframework.checker.signature.qual.BinaryName;
import org.checkerframework.checker.signature.qual.Identifier;
import org.checkerframework.checker.signature.qual.MethodDescriptor;

/**
 * MethodGen24 collects and stores all the relevant information about a method that Instrument24
 * might need. MethodGen24 is analogous to the BCEL MethodGen class. The similarity makes it easier
 * to keep Instrument.java and Instrument24.java in sync.
 *
 * <p>MethodGen24 uses Java's ({@code java.lang.classfile}) APIs for reading and modifying .class
 * files. Those APIs were added in JDK 24. Compared to BCEL, these APIs are more complete and robust
 * (no more fiddling with StackMaps) and are always up to date with any .class file changes (since
 * they are part of the JDK). (We will need to continue to support Instrument.java using BCEL, as we
 * anticipate our clients using JDK 21 or less for quite some time.)
 */
public class MethodGen24 {

  /**
   * Models the body of the method (the Code attribute). A Code attribute is viewed as a sequence of
   * CodeElements, which is the only way to access Instructions; the order of elements of a code
   * model is significant. May be null if the method has no code.
   *
   * <p>Several fields of CodeModel are declared as fields of MethodGen24 to better correspond to
   * BCEL's version of MethodGen and to reduce re-computation. Currently we set these fields in the
   * constructor, but they could be calculated lazily on first reference.
   */
  private @Nullable CodeModel code;

  /** The method's access flags. */
  private AccessFlags accessFlags;

  /** The method's name. */
  private String methodName;

  /**
   * The method's type descriptor. This contains information about the parameters and return type of
   * the method.
   */
  private MethodTypeDesc mtd;

  /** True if the method is static. */
  private boolean isStatic;

  /**
   * The method's CodeAttribute. This contains information about the bytecodes (instructions) of
   * this method. May be null if the method has no code.
   *
   * <p>Several fields of CodeAttribute are declared as fields of MedthodGen24 to better model
   * BCEL's version of MethodGen and to reduce re-computation. Note that we set these fields in the
   * constructor, but they could be calculated lazily on first reference.
   */
  private @Nullable CodeAttribute codeAttribute;

  /** The method's maximum number of locals. */
  private int maxLocals;

  /** The method's maximum stack size. */
  private int maxStack;

  /** The name of the method's enclosing class, in binary name format. */
  private @BinaryName String className;

  /**
   * The method's instruction list. Code elements can be categorized into Instructions (models the
   * bytecodes) and PseudoInstructions (information about local variables, line numbers, and
   * labels).
   */
  private List<CodeElement> codeList;

  /**
   * The method's descriptor. This is a String that encodes type information about the parameters
   * that the method takes (if any) and the method's return type (if any). It does not contain the
   * method name or any parameter names.
   */
  private @MethodDescriptor String descriptor;

  /**
   * The method's signature. This is a String that encodes type information about a (possibly
   * generic) method declaration. It describes any type parameters of the method; the (possibly
   * parameterized) types of any formal parameters; the (possibly parameterized) return type, if
   * any. It is not a true method signature as documented in the Java Vitural Machine Specification
   * as it does not include the types of any exceptions declared in the method's throws clause.
   */
  private @MethodDescriptor String signature;

  // Informatation extracted from {@code mtd}, the MethodTypeDescriptor.
  /** The method's parameter types. */
  private ClassDesc[] paramTypes;

  /** The method's return type. */
  private ClassDesc returnType;

  /** The method's parameter names, not including the receiver. */
  private @Identifier String[] paramNames;

  /** The method's original local variables. After initialization, this array is never modified. */
  private LocalVariable[] origLocalVariables;

  /**
   * The method's local variable table. Often modified by clients, normally to add additional local
   * variables needed for instrumentation.
   */
  protected List<LocalVariable> localsTable;

  /** ConstantPool builder for entire class. */
  // TODO: Should uses of this be synchronized?
  private ConstantPoolBuilder poolBuilder;

  /**
   * Creates a MethodGen24 object.
   *
   * @param methodModel the method
   * @param className the containing class, in binary name format
   * @param classBuilder for the class
   */
  public MethodGen24(
      final MethodModel methodModel,
      final @BinaryName String className,
      ClassBuilder classBuilder) {

    accessFlags = methodModel.flags();
    methodName = methodModel.methodName().stringValue();
    @SuppressWarnings("signature") // JDK 24 is not annotated as yet
    @MethodDescriptor String descriptor1 = methodModel.methodType().stringValue();
    descriptor = descriptor1;
    this.className = className;
    isStatic = accessFlags.has(AccessFlag.STATIC);

    Optional<CodeModel> code = methodModel.code();
    if (code.isPresent()) {
      this.code = code.get();
      // The original elementList is immutable, so we need to make a copy.
      // As the list can be quite long and we do not need random access (we make one linear pass
      // over the list) and almost all the changes we make are insertions, this is one of the few
      // cases where a LinkedList outperforms an ArrayList.
      @SuppressWarnings("JdkObsolete")
      List<CodeElement> cl = new LinkedList<CodeElement>(this.code.elementList());
      this.codeList = cl;
    } else {
      this.code = null;
      this.codeList = new ArrayList<>();
    }

    Optional<CodeAttribute> ca = methodModel.findAttribute(Attributes.code());
    if (ca.isPresent()) {
      codeAttribute = ca.get();
      maxLocals = codeAttribute.maxLocals();
      // We need an annotated version of JDK 24 to prevent the need for this suppression.
      assert codeAttribute != null : "@AssumeAssertion(nullness): maxLocals() is @SideEffectFree";
      maxStack = codeAttribute.maxStack();
    } else {
      codeAttribute = null;
      maxLocals = 0;
      maxStack = 0;
    }

    Optional<SignatureAttribute> sa = methodModel.findAttribute(Attributes.signature());
    if (sa.isPresent()) {
      @SuppressWarnings("signature") // JDK 24 is not annotated as yet
      @MethodDescriptor String signature1 = sa.get().signature().stringValue();
      signature = signature1;
    } else {
      // If no signature then probably no type arguments, so descriptor will do.
      signature = descriptor;
    }

    mtd = methodModel.methodTypeSymbol();
    paramTypes = mtd.parameterArray();
    returnType = mtd.returnType();

    // Set up the localsTable.
    localsTable = new ArrayList<>();

    for (CodeElement ce : codeList) {
      if (ce instanceof LocalVariable lv) {
        localsTable.add(lv);
      } else {
        // We assume all LocalVariable elements come before any instructions.
        if (ce instanceof Instruction) {
          break;
        }
      }
    }

    // Not necessarily sorted, so sort to make searching/insertion easier.
    localsTable.sort(Comparator.comparing(LocalVariable::slot));
    origLocalVariables = localsTable.toArray(new LocalVariable[localsTable.size()]);

    // System.out.println("locals:" + Arrays.toString(origLocalVariables));
    // System.out.println("types:" + Arrays.toString(paramTypes));
    // System.out.println("length: " + paramTypes.length + ", offset: " + offset);

    paramNames = new String[paramTypes.length];
    int offset = isStatic ? 0 : 1;
    for (int i = 0; i < paramTypes.length; i++) {
      if ((offset + i) < origLocalVariables.length) {
        @SuppressWarnings("signature:assignment") // need JDK annotations
        @Identifier String paramName = origLocalVariables[offset + i].name().stringValue();
        paramNames[i] = paramName;
      }
    }

    poolBuilder = classBuilder.constantPool();
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
    return origLocalVariables.clone();
  }

  /**
   * Return the name of the containing class.
   *
   * @return the binary name of the class that contains this method
   */
  public @BinaryName String getClassName() {
    return className;
  }

  /**
   * Return the CodeAttribute for the method. This contains information about the bytecodes
   * (instructions) of the method. May be null if the method has no code.
   *
   * @return the CodeAttribute for the method, or null
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
   * Return the descriptor for the current method. This is a String that encodes type information
   * about the parameters that the method takes (if any) and the method's return type (if any).
   *
   * @return descriptor for the current method
   */
  public @MethodDescriptor String getDescriptor() {
    return descriptor;
  }

  /**
   * Return the signature for the current method. This is a String that encodes type information
   * about a (possibly generic) method declaration. It describes any type parameters of the method;
   * the (possibly parameterized) types of any formal parameters; the (possibly parameterized)
   * return type, if any.
   *
   * @return signature for the current method
   */
  public @MethodDescriptor String getSignature() {
    return signature;
  }

  /**
   * Return the instruction list for the current method.
   *
   * @return instruction list as CodeElements
   */
  public List<CodeElement> getInstructionList() {
    return codeList;
  }

  /**
   * Return the constant pool builder.
   *
   * @return the constant pool builder
   */
  public ConstantPoolBuilder getPoolBuilder() {
    return poolBuilder;
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
