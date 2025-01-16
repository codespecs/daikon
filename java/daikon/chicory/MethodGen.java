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

public class MethodGen {
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
  private CodeModel code;
  protected AccessFlags accessFlags;
  private String methodName;
  private MethodTypeDesc mtd;

  private boolean isStatic;

  // fields of the code attribute
  private int maxLocals;
  private int maxStack;

  private String className;
  private List<CodeElement> codeList;
  private String descriptor;
  private String signature;

  // fields of the MethodTypeDescriptor
  private ClassDesc[] argTypes;
  private ClassDesc returnType;

  private String[] argNames;
  private LocalVariable[] localVariables;

  private daikon.chicory.Instrument inst_obj;

  public MethodGen(
      final MethodModel methodModel, final String className, daikon.chicory.Instrument inst_obj) {
    this.inst_obj = inst_obj;

    accessFlags = methodModel.flags();
    methodName = methodModel.methodName().stringValue();
    // System.out.println("methodName: " + methodName);
    descriptor = methodModel.methodType().stringValue();
    this.className = className;
    isStatic = accessFlags.has(AccessFlag.STATIC);

    Optional<CodeModel> code = methodModel.code();
    if (code.isPresent()) {
      this.code = code.get();
      // the original elementList is immutable, so we need to make a copy
      this.codeList = new LinkedList(this.code.elementList());
    } else {
      this.code = null;
      this.codeList = new ArrayList<>();
    }

    CodeAttribute ca = methodModel.findAttribute(Attributes.code()).orElse(null);
    if (ca != null) {
      maxLocals = ca.maxLocals();
      maxStack = ca.maxStack();
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
    argTypes = mtd.parameterArray();
    returnType = mtd.returnType();

    // set up the localsTable
    inst_obj.localsTable = new ArrayList<>();
    argNames = new String[argTypes.length];

    for (CodeElement ce : codeList) {
      if (ce instanceof LocalVariable lv) {
        inst_obj.localsTable.add(lv);
      } else {
        // WRONG // we assume all LocalVariable elements come first
        if (ce instanceof Instruction) {
          break;
        }
      }
    }
    // Not presented in sorted order so sort to make searching/insertion easier.
    inst_obj.localsTable.sort(Comparator.comparing(LocalVariable::slot));
    localVariables = inst_obj.localsTable.toArray(new LocalVariable[0]);
    int offset = isStatic ? 0 : 1;

    // System.out.println("locals:" + Arrays.toString(localVariables));
    // System.out.println("types:" + Arrays.toString(argTypes));
    // System.out.println("length: " + argTypes.length + ", offset: " + offset);

    for (int i = 0; i < argTypes.length; i++) {
      if ((offset + i) < localVariables.length) {
        argNames[i] = localVariables[offset + i].name().stringValue();
      }
    }
  }

  public AccessFlags getAccessFlags() {
    return accessFlags;
  }

  public final boolean isStatic() {
    return isStatic;
  }

  public String getName() {
    return methodName;
  }

  public ClassDesc getReturnType() {
    return returnType;
  }

  public String getArgumentName(final int i) {
    return argNames[i];
  }

  public String[] getArgumentNames() {
    return argNames.clone();
  }

  public ClassDesc getArgumentType(final int i) {
    return argTypes[i];
  }

  public ClassDesc[] getArgumentTypes() {
    return argTypes.clone();
  }

  public LocalVariable[] getLocalVariables() {
    return localVariables.clone();
  }

  /**
   * @return class that contains this method
   */
  public String getClassName() {
    return className;
  }

  public int getMaxLocals() {
    return maxLocals;
  }

  public int getMaxStack() {
    return maxStack;
  }

  public String getDescriptor() {
    return descriptor;
  }

  public String getSignature() {
    return signature;
  }

  public List<CodeElement> getInstructionList() {
    return codeList;
  }

  // Not sure we need this
  //  public void setInstructionList(List<CodeElement> il) {
  //     codeList = il;;
  //  }

  // need to fancy up!
  @Override
  public final String toString() {
    return methodName;
  }

  /*
     @Override
     public final String toString() {
         final String access = Utility.accessToString(super.getAccessFlags());
         String signature = Type.getMethodSignature(super.getType(), argTypes);
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
