package daikon.dcomp;

import static java.lang.constant.ConstantDescs.CD_Class;
import static java.lang.constant.ConstantDescs.CD_String;
import static java.lang.constant.ConstantDescs.CD_boolean;
import static java.lang.constant.ConstantDescs.CD_byte;
import static java.lang.constant.ConstantDescs.CD_char;
import static java.lang.constant.ConstantDescs.CD_double;
import static java.lang.constant.ConstantDescs.CD_float;
import static java.lang.constant.ConstantDescs.CD_int;
import static java.lang.constant.ConstantDescs.CD_long;
import static java.lang.constant.ConstantDescs.CD_short;
import static java.lang.constant.ConstantDescs.CD_void;

import daikon.chicory.MethodGen24;
import java.lang.classfile.CodeElement;
import java.lang.classfile.Instruction;
import java.lang.classfile.Label;
import java.lang.classfile.Opcode;
import java.lang.classfile.constantpool.ClassEntry;
import java.lang.classfile.constantpool.ConstantDynamicEntry;
import java.lang.classfile.constantpool.DoubleEntry;
import java.lang.classfile.constantpool.FloatEntry;
import java.lang.classfile.constantpool.IntegerEntry;
import java.lang.classfile.constantpool.LoadableConstantEntry;
import java.lang.classfile.constantpool.LongEntry;
import java.lang.classfile.constantpool.MethodHandleEntry;
import java.lang.classfile.constantpool.MethodTypeEntry;
import java.lang.classfile.constantpool.StringEntry;
import java.lang.classfile.instruction.BranchInstruction;
import java.lang.classfile.instruction.ConstantInstruction;
import java.lang.classfile.instruction.ExceptionCatch;
import java.lang.classfile.instruction.FieldInstruction;
import java.lang.classfile.instruction.InvokeDynamicInstruction;
import java.lang.classfile.instruction.InvokeInstruction;
import java.lang.classfile.instruction.LineNumber;
import java.lang.classfile.instruction.LoadInstruction;
import java.lang.classfile.instruction.LookupSwitchInstruction;
import java.lang.classfile.instruction.NewMultiArrayInstruction;
import java.lang.classfile.instruction.NewObjectInstruction;
import java.lang.classfile.instruction.NewPrimitiveArrayInstruction;
import java.lang.classfile.instruction.NewReferenceArrayInstruction;
import java.lang.classfile.instruction.StoreInstruction;
import java.lang.classfile.instruction.SwitchCase;
import java.lang.classfile.instruction.TableSwitchInstruction;
import java.lang.classfile.instruction.TypeCheckInstruction;
import java.lang.constant.ClassDesc;
import java.lang.constant.MethodTypeDesc;
import java.util.List;
import java.util.Set;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.checkerframework.checker.signature.qual.FieldDescriptor;

/**
 * This class calculates the state of the operand stack via simulation.
 *
 * <p>During this process, it may modify the state of DCInstrument24.locals and
 * DCInstrument24.stacks.
 */
public final class CalcStack24 {

  /** Do not instantiate. */
  private CalcStack24() {
    throw new Error("Do not instantiate");
  }

  /** Sentinel ClassDesc representing {@code null} on the operand stack (not a real class). */
  static final ClassDesc NULL_CD = ClassDesc.of("daikon.dcomp.CalcStack24$NullSentinel");

  /** Set of ClassDesc items that map to CD_int. */
  static final Set<ClassDesc> INTEGRAL = Set.of(CD_boolean, CD_byte, CD_char, CD_int, CD_short);

  /**
   * Calculates changes in the operand stack based on the symbolic execution of a CodeElement. Note
   * that we assume the class file is valid and make no attempt to verify the code's correctness.
   *
   * @param mgen method containing the instruction (currently unused)
   * @param minfo for the given method's code (currently unused)
   * @param ce CodeElement to be interpreted
   * @param instIndex index of {@code ce} in code element list
   * @param stack current state of operand stack; is side-effected
   * @return true if control falls through to the next instruction, false otherwise (when {@code ce}
   *     is an instruction like jump or return)
   * @throws DynCompError if we don't recognize {@code ce}
   */
  static boolean simulateCodeElement(
      MethodGen24 mgen,
      MethodGen24.MInfo24 minfo,
      CodeElement ce,
      int instIndex,
      OperandStack24 stack) {

    if (DCInstrument24.debugOperandStack) {
      System.out.println("code element: " + ce);
    }
    switch (ce) {
      case Instruction inst -> {
        return simulateInstruction(inst, instIndex, stack);
      }

      // We ignore most PseudoInstructions.

      case ExceptionCatch ec -> {
        // Nothing needs to be done.
        return true;
      }

      // Technically, a Classfile element, not a PseudoInstruction.
      case Label l -> {
        if (DCInstrument24.stacks[instIndex] != null) {
          // We've seen this label before.
          DCInstrument24.verifyOperandStackMatches(l, DCInstrument24.stacks[instIndex], stack);
          // Stacks match; we're done with this worklist.
          return false;
        } else {
          // We have not seen this label before; remember the operand stack.
          DCInstrument24.stacks[instIndex] = stack.getClone();
          if (DCInstrument24.debugOperandStack) {
            System.out.println("save stack state at: " + l);
            System.out.println("  " + instIndex + ", " + DCInstrument24.stacks[instIndex]);
          }
          return true;
        }
      }

      case LineNumber ln -> {
        // Nothing needs to be done.
        return true;
      }

      default -> {
        throw new DynCompError("Unexpected CodeElement: " + ce);
      }
    }
  }

  /**
   * Calculates changes in the operand stack based on the symbolic execution of a Java bytecode
   * instruction. Note that we assume the class file is valid and make no attempt to verify the
   * code's correctness.
   *
   * @param inst instruction to be interpreted
   * @param instIndex index of inst in code element list
   * @param stack current state of operand stack; is side-effected
   * @return true if control falls through to the next instruction, false otherwise (when {@code
   *     inst} is an instruction like jump or return)
   * @throws DynCompError if there is an error during the instruction simulation
   */
  @SuppressWarnings("fallthrough")
  static boolean simulateInstruction(Instruction inst, int instIndex, OperandStack24 stack) {
    if (DCInstrument24.stacks[instIndex] != null) {
      throw new DynCompError("instruction revisited at index " + instIndex + ": " + inst);
    } else {
      DCInstrument24.stacks[instIndex] = stack.getClone();
    }
    if (DCInstrument24.debugOperandStack) {
      System.out.println(
          "save stack state at: " + instIndex + ", " + DCInstrument24.stacks[instIndex]);
      System.out.println("opcode: " + inst.opcode());
    }
    // calculate stack changes
    switch (inst.opcode()) {

      // operand stack before: ..., arrayref, index
      // operand stack after:  ..., value
      case Opcode.AALOAD:
        {
          stack.pop(); // discard the index
          final ClassDesc t = stack.pop(); // pop the arrayref
          if (t == null || NULL_CD.equals(t)) {
            // The arrayref is null. A NullPointerException will be thrown at run time if this
            // AALOAD is executed.
            stack.push(NULL_CD);
          } else {
            final ClassDesc ct = t.componentType();
            if (ct == null) {
              throw new DynCompError("stack item not an arrayref: " + inst);
            }
            stack.push(ct);
          }
          return true;
        }

      // operand stack before: ..., arrayref, index, value
      // operand stack after:  ...
      case Opcode.AASTORE:
      case Opcode.BASTORE:
      case Opcode.CASTORE:
      case Opcode.DASTORE:
      case Opcode.FASTORE:
      case Opcode.IASTORE:
      case Opcode.LASTORE:
      case Opcode.SASTORE:
        stack.pop(3);
        return true;

      // operand stack before: ...
      // operand stack after:  ..., null
      case Opcode.ACONST_NULL:
        stack.push(NULL_CD);
        return true;

      // operand stack before: ...
      // operand stack after:  ..., objectref
      case Opcode.ALOAD:
      case Opcode.ALOAD_0:
      case Opcode.ALOAD_1:
      case Opcode.ALOAD_2:
      case Opcode.ALOAD_3:
      case Opcode.ALOAD_W:
        LoadInstruction li = (LoadInstruction) inst;
        stack.push(DCInstrument24.locals[li.slot()]);
        return true;

      // operand stack before: ..., count
      // operand stack after:  ..., arrayref
      case Opcode.ANEWARRAY:
        stack.pop(); // discard the count
        final NewReferenceArrayInstruction nrai = (NewReferenceArrayInstruction) inst;
        // make an array type from the component type
        stack.push(nrai.componentType().asSymbol().arrayType(1));
        return true;

      // operand stack before: ...
      // operand stack after:  [empty]
      case Opcode.ARETURN:
      case Opcode.DRETURN:
      case Opcode.FRETURN:
      case Opcode.IRETURN:
      case Opcode.LRETURN:
      case Opcode.RETURN:
        // execution pump will reset stack
        return false;

      // operand stack before: ..., arrayref
      // operand stack after:  ..., length
      case Opcode.ARRAYLENGTH:
        stack.pop(); // discard the arrayref
        stack.push(CD_int);
        return true;

      // operand stack before: ..., value
      // operand stack after:  ...
      case Opcode.ASTORE:
      case Opcode.ASTORE_0:
      case Opcode.ASTORE_1:
      case Opcode.ASTORE_2:
      case Opcode.ASTORE_3:
      case Opcode.ASTORE_W:
      case Opcode.DSTORE:
      case Opcode.DSTORE_0:
      case Opcode.DSTORE_1:
      case Opcode.DSTORE_2:
      case Opcode.DSTORE_3:
      case Opcode.DSTORE_W:
      case Opcode.FSTORE:
      case Opcode.FSTORE_0:
      case Opcode.FSTORE_1:
      case Opcode.FSTORE_2:
      case Opcode.FSTORE_3:
      case Opcode.FSTORE_W:
      case Opcode.ISTORE:
      case Opcode.ISTORE_0:
      case Opcode.ISTORE_1:
      case Opcode.ISTORE_2:
      case Opcode.ISTORE_3:
      case Opcode.ISTORE_W:
      case Opcode.LSTORE:
      case Opcode.LSTORE_0:
      case Opcode.LSTORE_1:
      case Opcode.LSTORE_2:
      case Opcode.LSTORE_3:
      case Opcode.LSTORE_W:
        StoreInstruction si = (StoreInstruction) inst;
        // We assume code is correct and do not verify that si.typeKind() matches stack.pop().
        DCInstrument24.locals[si.slot()] = stack.pop();
        return true;

      // operand stack before: ..., objectref
      // operand stack after:  objectref
      case Opcode.ATHROW:
        // execution pump will reset stack
        return false;

      // operand stack before: ..., arrayref, index
      // operand stack after:  ..., value
      case Opcode.BALOAD:
      case Opcode.CALOAD:
      case Opcode.IALOAD:
      case Opcode.SALOAD:
        stack.pop(2);
        stack.push(CD_int);
        return true;

      // operand stack before: ...
      // operand stack after:  ..., value
      case Opcode.BIPUSH:
      case Opcode.ICONST_0:
      case Opcode.ICONST_1:
      case Opcode.ICONST_2:
      case Opcode.ICONST_3:
      case Opcode.ICONST_4:
      case Opcode.ICONST_5:
      case Opcode.ICONST_M1:
      case Opcode.SIPUSH:
        stack.push(CD_int);
        return true;

      // operand stack before: ..., objectref
      // operand stack after:  ..., objectref
      case Opcode.CHECKCAST:
        {
          final ClassDesc t = stack.pop(); // pop the objectref
          if (t == null || NULL_CD.equals(t)) {
            stack.push(NULL_CD);
          } else {
            TypeCheckInstruction tci = (TypeCheckInstruction) inst;
            final ClassDesc ct = tci.type().asSymbol();
            // We assume the type check will succeed
            stack.push(ct);
          }
          // just assume it will be ok, otherwise it will throw when executed
          return true;
        }

      // operand stack before: ..., value
      // operand stack after:  ..., result
      case Opcode.D2F: // double to float
      case Opcode.I2F: // integer to float
      case Opcode.L2F: // long to float
        stack.pop(); // discard the value
        stack.push(CD_float);
        return true;

      // operand stack before: ..., value
      // operand stack after:  ..., result
      case Opcode.D2L: // double to long
      case Opcode.F2L: // float to long
      case Opcode.I2L: // integer to long
        stack.pop(); // discard the value
        stack.push(CD_long);
        return true;

      // operand stack before: ..., value1, value2
      // operand stack after:  ..., result
      case Opcode.DADD:
      case Opcode.DDIV:
      case Opcode.DMUL:
      case Opcode.DREM:
      case Opcode.DSUB:
        stack.pop(2); // discard the values
        stack.push(CD_double);
        return true;

      // operand stack before: ..., arrayref, index
      // operand stack after:  ..., value
      case Opcode.DALOAD:
        stack.pop(2); // discard the arrayref and index
        stack.push(CD_double);
        return true;

      // operand stack before: ..., value1, value2
      // operand stack after:  ..., result
      case Opcode.DCMPG:
      case Opcode.DCMPL:
      case Opcode.FCMPG:
      case Opcode.FCMPL:
        stack.pop(2); // discard the values
        stack.push(CD_int);
        return true;

      // operand stack before: ...
      // operand stack after:  ..., value
      case Opcode.DCONST_0:
      case Opcode.DCONST_1:
        stack.push(CD_double);
        return true;

      // operand stack before: ...
      // operand stack after:  ..., value
      case Opcode.DLOAD:
      case Opcode.DLOAD_0:
      case Opcode.DLOAD_1:
      case Opcode.DLOAD_2:
      case Opcode.DLOAD_3:
      case Opcode.DLOAD_W:
        stack.push(CD_double);
        return true;

      // operand stack before: ..., value
      // operand stack after:  ..., result
      case Opcode.DNEG:
        stack.pop(); // discard the value
        stack.push(CD_double);
        return true;

      // operand stack before: ..., value
      // operand stack after:  ..., value, value
      case Opcode.DUP:
        {
          final ClassDesc t = stack.pop();
          stack.push(t);
          stack.push(t);
          return true;
        }

      // operand stack before: ..., value2, value1
      // operand stack after:  ..., value1, value2, value1
      case Opcode.DUP_X1:
        {
          final ClassDesc v1 = stack.pop();
          final ClassDesc v2 = stack.pop();
          stack.push(v1);
          stack.push(v2);
          stack.push(v1);
          return true;
        }

      // operand stack before: ..., value3, value2, value1
      // operand stack after:  ..., value1, value3, value2, value1
      // where value1, value2, and value3 are all a category 1 computational type
      // OR
      // ..., value2, value1
      // ..., value1, value2, value1
      // where value1 is a category 1 computational type and value2 is a category 2
      // computational type
      case Opcode.DUP_X2:
        {
          final ClassDesc v1 = stack.pop();
          final ClassDesc v2 = stack.pop();
          if (stack.slotSize(v2) == 2) {
            stack.push(v1);
          } else {
            final ClassDesc v3 = stack.pop();
            stack.push(v1);
            stack.push(v3);
          }
          stack.push(v2);
          stack.push(v1);
          return true;
        }

      // operand stack before: ..., value2, value1
      // operand stack after:  ..., value2, value1, value2, value1
      // where value1, and value2 are all a category 1 computational type
      // OR
      // ..., value
      // ..., value, value
      // where value is a category 2 computational type
      case Opcode.DUP2:
        {
          final ClassDesc v1 = stack.pop();
          if (stack.slotSize(v1) == 2) {
            stack.push(v1);
          } else { // slotSize(v1) == 1
            final ClassDesc v2 = stack.pop();
            stack.push(v2);
            stack.push(v1);
            stack.push(v2);
          }
          stack.push(v1);
          return true;
        }

      // operand stack before: ..., value3, value2, value1
      // operand stack after:  ..., value2, value1, value3, value2, value1
      // where value1, value2, and value3 are all a category 1 computational type
      // OR
      // ..., value2, value1
      // ..., value1, value2, value1
      // where value1 is a category 2 computational type and value2 a category 1 computational
      // type
      case Opcode.DUP2_X1:
        {
          final ClassDesc v1 = stack.pop();
          if (stack.slotSize(v1) == 2) {
            final ClassDesc v2 = stack.pop();
            stack.push(v1);
            stack.push(v2);
          } else { // slotSize(v1) == 1
            final ClassDesc v2 = stack.pop();
            final ClassDesc v3 = stack.pop();
            stack.push(v2);
            stack.push(v1);
            stack.push(v3);
            stack.push(v2);
          }
          stack.push(v1);
          return true;
        }

      // ..., value4, value3, value2, value1
      // ..., value2, value1, value4, value3, value2, value1
      // where value1, value2, value3, and value4 are all a category 1 computational type
      // OR
      // ..., value3, value2, value1
      // ..., value1, value3, value2, value1
      // where value1 is a category 2 computational type and value2 and value3 are both a
      // category 1 computational type
      // OR
      // ..., value3, value2, value1
      // ..., value2, value1, value3, value2, value1
      // where value1 and value2 are both a category 1 computational type and value3 is a
      // category 2 computational type
      // OR
      // ..., value2, value1
      // ..., value1, value2, value1
      // where value1 and value2 are both a category 2 computational type
      case Opcode.DUP2_X2:
        {
          final ClassDesc v1 = stack.pop();
          if (stack.slotSize(v1) == 2) {
            final ClassDesc v2 = stack.pop();
            if (stack.slotSize(v2) == 2) {
              stack.push(v1);
            } else {
              final ClassDesc v3 = stack.pop();
              stack.push(v1);
              stack.push(v3);
            }
            stack.push(v2);
            stack.push(v1);
          } else { // slotSize(v1) is 1
            final ClassDesc v2 = stack.pop();
            final ClassDesc v3 = stack.pop();
            if (stack.slotSize(v3) == 2) {
              stack.push(v2);
              stack.push(v1);
            } else {
              final ClassDesc v4 = stack.pop();
              stack.push(v2);
              stack.push(v1);
              stack.push(v4);
            }
            stack.push(v3);
            stack.push(v2);
            stack.push(v1);
          }
          return true;
        }

      // operand stack before: ..., value
      // operand stack after:  ..., result
      case Opcode.F2D: // float to double
      case Opcode.I2D: // integer to double
      case Opcode.L2D: // long to double
        stack.pop(); // discard the value
        stack.push(CD_double);
        return true;

      // operand stack before: ..., value1, value2
      // operand stack after:  ..., result
      case Opcode.FADD:
      case Opcode.FDIV:
      case Opcode.FMUL:
      case Opcode.FREM:
      case Opcode.FSUB:
        stack.pop(2); // discard the values
        stack.push(CD_float);
        return true;

      // operand stack before: ..., arrayref, index
      // operand stack after:  ..., value
      case Opcode.FALOAD:
        stack.pop(2); // discard the arrayref and index
        stack.push(CD_float);
        return true;

      // operand stack before: ...
      // operand stack after:  ..., value
      case Opcode.FCONST_0:
      case Opcode.FCONST_1:
      case Opcode.FCONST_2:
        stack.push(CD_float);
        return true;

      // operand stack before: ...
      // operand stack after:  ..., value
      case Opcode.FLOAD:
      case Opcode.FLOAD_0:
      case Opcode.FLOAD_1:
      case Opcode.FLOAD_2:
      case Opcode.FLOAD_3:
      case Opcode.FLOAD_W:
        stack.push(CD_float);
        return true;

      // operand stack before: ..., value
      // operand stack after:  ..., result
      case Opcode.FNEG:
        stack.pop(); // discard the value
        stack.push(CD_float);
        return true;

      // operand stack before: ..., objectref
      // operand stack after:  ..., value
      case Opcode.GETFIELD:
        stack.pop(); // discard the objectref

      // fall through is intentional:

      // operand stack before: ...
      // operand stack after:  ..., value
      case Opcode.GETSTATIC:
        {
          FieldInstruction fi = (FieldInstruction) inst;
          pushResultClassDesc(fi.typeSymbol(), stack);
          return true;
        }

      // operand stack: no change
      case Opcode.GOTO:
      case Opcode.GOTO_W:
        {
          BranchInstruction bi = (BranchInstruction) inst;
          addLabelsToWorklist(bi.target(), null, stack);
          return false;
        }

      // operand stack before: ..., value
      // operand stack after:  ..., result
      case Opcode.I2B: // integer to byte
      case Opcode.I2C: // integer to char
      case Opcode.D2I: // double to integer
      case Opcode.F2I: // float to integer
      case Opcode.L2I: // long to int
      case Opcode.I2S: // integer to short
        stack.pop(); // discard the value
        stack.push(CD_int);
        return true;

      // operand stack before: ..., value1, value2
      // operand stack after:  ..., result
      case Opcode.IADD:
      case Opcode.IAND:
      case Opcode.IDIV:
      case Opcode.IMUL:
      case Opcode.IOR:
      case Opcode.IREM:
      case Opcode.ISHL:
      case Opcode.ISHR:
      case Opcode.ISUB:
      case Opcode.IUSHR:
      case Opcode.IXOR:
      case Opcode.LCMP:
        stack.pop(2); // discard the values
        stack.push(CD_int);
        return true;

      // operand stack before: ..., value1, value2
      // operand stack after:  ...
      case Opcode.IF_ACMPEQ:
      case Opcode.IF_ACMPNE:
      case Opcode.IF_ICMPEQ:
      case Opcode.IF_ICMPGE:
      case Opcode.IF_ICMPGT:
      case Opcode.IF_ICMPLE:
      case Opcode.IF_ICMPLT:
      case Opcode.IF_ICMPNE:
        {
          stack.pop(2); // discard the values
          BranchInstruction bi = (BranchInstruction) inst;
          addLabelsToWorklist(bi.target(), null, stack);
          return true;
        }

      // operand stack before: ..., value
      // operand stack after:  ...
      case Opcode.IFEQ:
      case Opcode.IFNE:
      case Opcode.IFLT:
      case Opcode.IFGE:
      case Opcode.IFGT:
      case Opcode.IFLE:
      case Opcode.IFNONNULL:
      case Opcode.IFNULL:
        stack.pop(); // discard the value
        BranchInstruction bi = (BranchInstruction) inst;
        addLabelsToWorklist(bi.target(), null, stack);
        return true;

      // operand stack: no change
      case Opcode.IINC:
      case Opcode.IINC_W:
      case Opcode.NOP:
        return true;

      // operand stack before: ...
      // operand stack after:  ..., value
      case Opcode.ILOAD:
      case Opcode.ILOAD_0:
      case Opcode.ILOAD_1:
      case Opcode.ILOAD_2:
      case Opcode.ILOAD_3:
      case Opcode.ILOAD_W:
        stack.push(CD_int);
        return true;

      // operand stack before: ..., value
      // operand stack after:  ..., result
      case Opcode.INEG:
        // The top of the stack should be a CD_int, so we could just return true. However, we
        // include the pop and push so the sequence is comparable to that of other operators.
        stack.pop(); // discard the value
        stack.push(CD_int);
        return true;

      // operand stack before: ..., objectref
      // operand stack after:  ..., result
      case Opcode.INSTANCEOF:
        stack.pop(); // discard the value
        stack.push(CD_int);
        return true;

      // JSR and RET have been illegal since JDK 7 (class file version 51.0).
      // We don't have a case label for JSR, JSR_W, RET or RET_W so that they fall
      // into the default case and report an "Unexpected instruction opcode" error.

      // operand stack before: ..., value1, value2
      // operand stack after:  ..., result
      case Opcode.LADD:
      case Opcode.LAND:
      case Opcode.LDIV:
      case Opcode.LMUL:
      case Opcode.LOR:
      case Opcode.LREM:
      case Opcode.LSHL:
      case Opcode.LSHR:
      case Opcode.LSUB:
      case Opcode.LUSHR:
      case Opcode.LXOR:
        stack.pop(2); // discard the values
        stack.push(CD_long);
        return true;

      // operand stack before: ..., arrayref, index
      // operand stack after:  ..., value
      case Opcode.LALOAD:
        stack.pop(2); // discard the arrayref and index
        stack.push(CD_long);
        return true;

      // operand stack before: ...
      // operand stack after:  ..., value
      case Opcode.LCONST_0:
      case Opcode.LCONST_1:
        stack.push(CD_long);
        return true;

      // operand stack before: ...
      // operand stack after:  ..., value
      case Opcode.LDC:
      case Opcode.LDC_W:
      case Opcode.LDC2_W:
        ConstantInstruction.LoadConstantInstruction ldc =
            (ConstantInstruction.LoadConstantInstruction) inst;
        LoadableConstantEntry lce = ldc.constantEntry();
        stack.push(lceToCD(lce));
        return true;

      // operand stack before: ...
      // operand stack after:  ..., value
      case Opcode.LLOAD:
      case Opcode.LLOAD_0:
      case Opcode.LLOAD_1:
      case Opcode.LLOAD_2:
      case Opcode.LLOAD_3:
      case Opcode.LLOAD_W:
        stack.push(CD_long);
        return true;

      // operand stack before: ..., value
      // operand stack after:  ..., result
      case Opcode.LNEG:
        stack.pop(); // discard the value
        stack.push(CD_long);
        return true;

      // operand stack before: ..., key
      // operand stack after:  ...
      case Opcode.LOOKUPSWITCH:
        stack.pop(); // discard the value
        LookupSwitchInstruction lsi = (LookupSwitchInstruction) inst;
        addLabelsToWorklist(lsi.defaultTarget(), lsi.cases(), stack);
        return false;

      // operand stack before: ..., objectref
      // operand stack after:  ...
      case Opcode.MONITORENTER:
      case Opcode.MONITOREXIT:
        stack.pop(); // discard the value
        return true;

      // operand stack before: ..., count1, [count2, ...]
      // operand stack after:  ..., arrayref
      case Opcode.MULTIANEWARRAY:
        final NewMultiArrayInstruction nmai = (NewMultiArrayInstruction) inst;
        stack.pop(nmai.dimensions()); // discard all the counts
        stack.push(nmai.arrayType().asSymbol());
        return true;

      // operand stack before: ...
      // operand stack after:  ..., objectref
      case Opcode.NEW:
        final NewObjectInstruction noi = (NewObjectInstruction) inst;
        stack.push(noi.className().asSymbol());
        return true;

      // operand stack before: ..., count
      // operand stack after:  ..., arrayref
      case Opcode.NEWARRAY:
        stack.pop(); // discard the count
        final NewPrimitiveArrayInstruction npai = (NewPrimitiveArrayInstruction) inst;
        stack.push(ClassDesc.ofDescriptor("[" + npaiToElementTypeDescriptor(npai)));
        return true;

      // operand stack before: ..., value
      // operand stack after:  ...
      case Opcode.POP:
        stack.pop(); // discard the value
        return true;

      // operand stack before: ..., value2, value1
      // operand stack after:  ...
      // where each of value1 and value2 is a category 1 computational type
      // OR
      // ..., value
      // ...
      // where value is of a category 2 computational type
      case Opcode.POP2:
        {
          final ClassDesc v1 = stack.pop();
          if (stack.slotSize(v1) == 1) {
            stack.pop();
          }
          return true;
        }

      // operand stack before: ..., objectref, value
      // operand stack after:  ...
      case Opcode.PUTFIELD:
        stack.pop(2);
        return true;

      // operand stack before: ..., value
      // operand stack after:  ...
      case Opcode.PUTSTATIC:
        stack.pop(); // discard the value
        return true;

      // operand stack before: ..., value1, value2
      // operand stack after:  ..., value2, value1
      case Opcode.SWAP:
        {
          final ClassDesc v2 = stack.pop();
          final ClassDesc v1 = stack.pop();
          stack.push(v2);
          stack.push(v1);
          return true;
        }

      // operand stack before: ..., index
      // operand stack after:  ...
      case Opcode.TABLESWITCH:
        stack.pop(); // discard the index
        TableSwitchInstruction tsi = (TableSwitchInstruction) inst;
        addLabelsToWorklist(tsi.defaultTarget(), tsi.cases(), stack);
        return false;

      // operand stack before: ..., objectref, [arg1, [arg2 ...]]
      // operand stack after:  ...
      case Opcode.INVOKEINTERFACE:
      case Opcode.INVOKESPECIAL:
      case Opcode.INVOKEVIRTUAL:
        stack.pop(); // Discard the last argument (which is at the top of the stack),
      // or the objectref if there are no arguments.

      // may actually be removing an arg, but we'll
      // account for that when we remove args below

      // fall through is intentional:

      // operand stack before: ..., [arg1, [arg2 ...]]
      // operand stack after:  ...
      case Opcode.INVOKESTATIC:
        {
          final InvokeInstruction ii = (InvokeInstruction) inst;
          final MethodTypeDesc mtd = ii.typeSymbol();
          stack.pop(mtd.parameterCount()); // discard the arguments
          // We are sure the invoked method will xRETURN eventually.
          // We simulate xRETURN's functionality here because we don't
          // really "jump into" and simulate the invoked method.
          pushResultClassDesc(mtd.returnType(), stack);
          return true;
        }

      // operand stack before: ..., [arg1, [arg2 ...]]
      // operand stack after:  ...
      case Opcode.INVOKEDYNAMIC:
        {
          final InvokeDynamicInstruction idi = (InvokeDynamicInstruction) inst;
          final MethodTypeDesc mtd = idi.typeSymbol();
          stack.pop(mtd.parameterCount()); // discard the arguments
          // We are sure the invoked method will xRETURN eventually.
          // We simulate xRETURNs functionality here because we don't
          // really "jump into" and simulate the invoked method.
          pushResultClassDesc(mtd.returnType(), stack);
          return true;
        }

      default:
        throw new DynCompError("Unexpected instruction opcode: " + inst);
    }
  }

  /**
   * Calculate a ClassDesc from a LoadableConstantEntry.
   *
   * @param lce a LoadableConstantEntry
   * @return the ClassDesc of {@code lce}
   * @throws DynCompError if we don't recognize {@code lce}
   */
  static ClassDesc lceToCD(LoadableConstantEntry lce) {
    switch (lce) {
      case ClassEntry cle -> {
        return CD_Class;
      }
      case ConstantDynamicEntry cde -> {
        return cde.typeSymbol();
      }
      case DoubleEntry de -> {
        return CD_double; // LDC2_W only, but we assume correct code
      }
      case FloatEntry fe -> {
        return CD_float;
      }
      case IntegerEntry ie -> {
        return CD_int;
      }
      case LongEntry le -> {
        return CD_long; // LDC2_W only, but we assume correct code
      }
      case MethodHandleEntry mhe -> {
        return ClassDesc.of("java.lang.invoke.MethodHandle");
      }
      case MethodTypeEntry mte -> {
        return ClassDesc.of("java.lang.invoke.MethodType");
      }
      case StringEntry se -> {
        return CD_String;
      }
      default -> {
        throw new DynCompError("Illegal LoadableConstantEntry: " + lce);
      }
    }
  }

  /**
   * Calculate an element type descriptor from a NewPrimitiveArrayInstruction.
   *
   * @param npai a NewPrimitiveArrayInstruction
   * @return a String containing the element type descriptor
   * @throws DynCompError if we don't recognize the type of {@code npai}
   */
  static @FieldDescriptor String npaiToElementTypeDescriptor(NewPrimitiveArrayInstruction npai) {
    return switch (npai.typeKind()) {
      case BOOLEAN -> "Z";
      case BYTE -> "B";
      case CHAR -> "C";
      case DOUBLE -> "D";
      case FLOAT -> "F";
      case INT -> "I";
      case LONG -> "J";
      case SHORT -> "S";
      default ->
          throw new DynCompError("unknown primitive type " + npai.typeKind() + " in: " + npai);
    };
  }

  /**
   * Calculate a Java bytecode's result type and push it on the operand stack. If the type is void,
   * nothing is pushed. If the type is boolean, byte, char, or short, then an int is pushed.
   * Otherwise, the result itself is pushed.
   *
   * @param result the result type descriptor to push (may be void or a primitive/reference type)
   * @param stack state of operand stack
   */
  static void pushResultClassDesc(ClassDesc result, OperandStack24 stack) {
    switch (result) {
      case ClassDesc c when c.equals(CD_void) -> {}
      case ClassDesc c when INTEGRAL.contains(c) -> {
        stack.push(CD_int);
      }
      default -> {
        stack.push(result);
      }
    }
  }

  /**
   * Calls DCInstrument24.addLabelToWorklist to create a set of worklist items.
   *
   * @param target label where to start operand stack simulation
   * @param cases a set of case statement labels; additional start points for simulation
   * @param stack state of operand stack at target and case labels
   */
  static void addLabelsToWorklist(
      Label target, @Nullable List<SwitchCase> cases, OperandStack24 stack) {
    DCInstrument24.addLabelToWorklist(target, stack);
    if (cases == null) {
      return;
    }
    for (SwitchCase item : cases) {
      DCInstrument24.addLabelToWorklist(item.target(), stack);
    }
  }
}
