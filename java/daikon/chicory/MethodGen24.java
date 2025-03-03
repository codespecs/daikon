package daikon.chicory;

import java.lang.classfile.AccessFlags;
import java.lang.classfile.Attributes;
import java.lang.classfile.CodeElement;
import java.lang.classfile.CodeModel;
import java.lang.classfile.Instruction;
import java.lang.classfile.MethodModel;
import java.lang.classfile.attribute.CodeAttribute;
import java.lang.classfile.attribute.SignatureAttribute;
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

/**
 * MethodGen24 collects and stores all the relevant information about a method that Instrument24
 * might need. MethodGen24 is analogous to the BCEL MethodGen class. The similarity makes it easier
 * to create Instrument24.java from Instrument.java.
 *
 * <p>MethodGen24 uses Java's ({@code java.lang.classfile}) APIs for reading and modifying .class
 * files. Those APIs were added in JDK 24.
 *
 * <p>We are migrating from BCEL to this new set of APIs for two main reasons:
 *
 * <ol>
 *   <li>The new APIs are more complete and robust - no more fiddling with StackMaps.
 *   <li>Since the new APIs are part of the official JDK release, they will always be up to date
 *       with any .class file changes.
 * </ol>
 *
 * <p>The files Instrument24.java and MethodGen24.java were added to Chicory to use this new set of
 * APIs instead of BCEL. (We will need to continue to support Instrument.java using BCEL, as we
 * anticipate our clients using JDK 17 or less for quite some time.)
 */
public class MethodGen24 {

  /**
   * Models the body of the method (the Code attribute). A Code attribute is viewed as a composition
   * of CodeElements, which is the only way to access Instructions; the order of elements of a code
   * model is significant. May be null if the method has no code.
   *
   * <p>Several fields of CodeModel are declared as fields of MethodGen24 to better correspond to
   * BCEL's version of MethodGen and to reduce re-computation. Note that we set these fields in the
   * constructor, but they could be calculated lazily on first reference.
   */
  private @Nullable CodeModel code;

  /** The method's access flags. */
  protected AccessFlags accessFlags;

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
  private String descriptor;

  /**
   * The method's signature. This is a String that encodes type information about a (possibly
   * generic) method declaration. It describes any type parameters of the method; the (possibly
   * parameterized) types of any formal parameters; the (possibly parameterized) return type, if
   * any; and the types of any exceptions declared in the method's throws clause. It does not
   * contain the method name or any parameter names.
   */
  private String signature;

  // Informatation extracted from {@code mtd}, the MethodTypeDescriptor.
  /** The method's parameter types. */
  private ClassDesc[] paramTypes;

  /** The method's return type. */
  private ClassDesc returnType;

  /** The method's parameter names. */
  private @Identifier String[] paramNames;

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
      final MethodModel methodModel,
      final @BinaryName String className,
      daikon.chicory.Instrument24 inst_obj) {

    accessFlags = methodModel.flags();
    methodName = methodModel.methodName().stringValue();
    descriptor = methodModel.methodType().stringValue();
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
      // We need an annotated version of JDK 24 to avoid this. (maxLocals() is @SideEffectFree)
      if (codeAttribute != null) {
        maxStack = codeAttribute.maxStack();
      }
    } else {
      codeAttribute = null;
      maxLocals = 0;
      maxStack = 0;
    }

    Optional<SignatureAttribute> sa = methodModel.findAttribute(Attributes.signature());
    if (sa.isPresent()) {
      signature = sa.get().signature().stringValue();
    } else {
      // If no signature then probably no type arguments, so descriptor will do.
      signature = descriptor;
    }

    mtd = methodModel.methodTypeSymbol();
    paramTypes = mtd.parameterArray();
    returnType = mtd.returnType();

    // Set up the localsTable in the instrumentation object.
    inst_obj.localsTable = new ArrayList<>();

    for (CodeElement ce : codeList) {
      if (ce instanceof LocalVariable lv) {
        inst_obj.localsTable.add(lv);
      } else {
        // We assume all LocalVariable elements come before any instructions.
        if (ce instanceof Instruction) {
          break;
        }
      }
    }

    // Not necessarily sorted, so sort to make searching/insertion easier.
    inst_obj.localsTable.sort(Comparator.comparing(LocalVariable::slot));
    localVariables = inst_obj.localsTable.toArray(new LocalVariable[inst_obj.localsTable.size()]);

    // System.out.println("locals:" + Arrays.toString(localVariables));
    // System.out.println("types:" + Arrays.toString(paramTypes));
    // System.out.println("length: " + paramTypes.length + ", offset: " + offset);

    paramNames = new String[paramTypes.length];
    int offset = isStatic ? 0 : 1;
    for (int i = 0; i < paramTypes.length; i++) {
      if ((offset + i) < localVariables.length) {
        @SuppressWarnings("signature:assignment") // need JDK annotations
        @Identifier String paramName = localVariables[offset + i].name().stringValue();
        paramNames[i] = paramName;
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
   * @return the binary name if the class that contains this method
   */
  public String getClassName() {
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
  public String getDescriptor() {
    return descriptor;
  }

  /**
   * Return the signature for the current method. This is a String that encodes type information
   * about a (possibly generic) method declaration. It describes any type parameters of the method;
   * the (possibly parameterized) types of any formal parameters; the (possibly parameterized)
   * return type, if any; and the types of any exceptions declared in the method's throws clause.
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
