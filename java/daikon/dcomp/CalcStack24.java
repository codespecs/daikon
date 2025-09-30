package daikon.dcomp;

import static java.lang.constant.ConstantDescs.CD_Object;
import static java.lang.constant.ConstantDescs.CD_String;
import static java.lang.constant.ConstantDescs.CD_double;
import static java.lang.constant.ConstantDescs.CD_float;
import static java.lang.constant.ConstantDescs.CD_int;
import static java.lang.constant.ConstantDescs.CD_long;
import static java.lang.constant.ConstantDescs.CD_void;

import daikon.chicory.MethodGen24;
import java.lang.classfile.CodeElement;
import java.lang.classfile.Instruction;
import java.lang.classfile.Label;
import java.lang.classfile.Opcode;
import java.lang.classfile.constantpool.ClassEntry;
import java.lang.classfile.constantpool.DoubleEntry;
import java.lang.classfile.constantpool.FloatEntry;
import java.lang.classfile.constantpool.IntegerEntry;
import java.lang.classfile.constantpool.LoadableConstantEntry;
import java.lang.classfile.constantpool.LongEntry;
import java.lang.classfile.constantpool.StringEntry;
import java.lang.classfile.instruction.BranchInstruction;
import java.lang.classfile.instruction.ConstantInstruction;
import java.lang.classfile.instruction.DiscontinuedInstruction;
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
import org.checkerframework.checker.nullness.qual.Nullable;

/** This class calculates the state of the operand stack via simulation. */
public final class CalcStack24 {

  /** Don't allow others to create a new CalcStack24 object. */
  private CalcStack24() {}

  /** ClassDesc for 'null'. */
  static final ClassDesc nullCD = ClassDesc.of("fake.ClassDecs.for.null");

  /**
   * Calculates changes in contents of operand stack based on the symbolic execution of a Java
   * bytecode instruction. Note that we assume the class file is valid and make no attempt to verify
   * the code's correctness.
   *
   * @param mgen method containing the instruction (currently unused)
   * @param minfo for the given method's code (currently unused)
   * @param ce instruction to be interpreted
   * @param inst_index index of ce in code element list
   * @param stack current state of operand stack
   * @return true if should continue with next instruction, false otherwise
   */
  @SuppressWarnings("fallthrough")
  static boolean calcOperandStack(
      MethodGen24 mgen,
      MethodGen24.MInfo24 minfo,
      CodeElement ce,
      int inst_index,
      OperandStack24 stack) {

    if (DCInstrument24.debugOperandStack) {
      System.out.println("code element: " + ce);
    }
    switch (ce) {
      case Instruction inst -> {
        if (DCInstrument24.stacks[inst_index] != null) {
          throw new DynCompError("instruction revisited: " + inst);
        } else {
          DCInstrument24.stacks[inst_index] = stack.getClone();
        }
        if (DCInstrument24.debugOperandStack) {
          System.out.println(
              "save stack state at: " + inst_index + ", " + DCInstrument24.stacks[inst_index]);
          System.out.println("opcode: " + inst.opcode());
        }
        // caculate stack changes
        switch (inst.opcode()) {

          // operand stack:
          // ..., arrayref, index
          // ..., value
          case Opcode.AALOAD:
            {
              stack.pop(); // discard the index
              final ClassDesc t = stack.pop(); // pop the arrayref
              if (t == null || nullCD.equals(t)) {
                // Push nullCD to maintain stack consistency. The actual NullPointerException
                // will be thrown at runtime if this code path is executed, not during
                // instrumentation.
                stack.push(nullCD);
              } else {
                final ClassDesc ct = t.componentType();
                if (ct == null) {
                  throw new DynCompError("stack item not an arrayref: " + inst);
                }
                stack.push(ct);
              }
              return true;
            }

          // operand stack:
          // ..., arrayref, index, value
          // ...
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

          // operand stack:
          // ...
          // ..., null
          case Opcode.ACONST_NULL:
            stack.push(nullCD);
            return true;

          // operand stack:
          // ...
          // ..., objectref
          case Opcode.ALOAD:
          case Opcode.ALOAD_0:
          case Opcode.ALOAD_1:
          case Opcode.ALOAD_2:
          case Opcode.ALOAD_3:
          case Opcode.ALOAD_W:
            LoadInstruction li = (LoadInstruction) inst;
            stack.push(DCInstrument24.locals[li.slot()]);
            return true;

          // operand stack:
          // ..., count
          // ..., arrayref
          case Opcode.ANEWARRAY:
            stack.pop(); // discard the count
            final NewReferenceArrayInstruction nrai = (NewReferenceArrayInstruction) inst;
            // make an array type from the component type
            stack.push(nrai.componentType().asSymbol().arrayType(1));
            return true;

          // operand stack:
          // ...
          // [empty]
          case Opcode.ARETURN:
          case Opcode.DRETURN:
          case Opcode.FRETURN:
          case Opcode.IRETURN:
          case Opcode.LRETURN:
          case Opcode.RETURN:
            // execution pump will reset stack
            return false;

          // operand stack:
          // ..., arrayref
          // ..., length
          case Opcode.ARRAYLENGTH:
            stack.pop(); // discard the arrayref
            stack.push(CD_int);
            return true;

          // operand stack:
          // ..., value
          // ...
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

          // operand stack:
          // ..., objectref
          // objectref
          case Opcode.ATHROW:
            // execution pump will reset stack
            return false;

          // UNDONE: the JVM says result is int, but should we track 'true' type?
          // operand stack:
          // ..., arrayref, index
          // ..., value
          case Opcode.BALOAD:
          case Opcode.CALOAD:
          case Opcode.IALOAD:
          case Opcode.SALOAD:
            stack.pop(2);
            stack.push(CD_int);
            return true;

          // operand stack:
          // ...
          // ..., value
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

          // operand stack:
          // ..., objectref
          // ..., objectref
          case Opcode.CHECKCAST:
            {
              final ClassDesc t = stack.pop(); // pop the objectref
              if (t == null || nullCD.equals(t)) {
                stack.push(nullCD);
              } else {
                TypeCheckInstruction tci = (TypeCheckInstruction) inst;
                final ClassDesc ct = tci.type().asSymbol();
                // We assume the type check will succeed
                stack.push(ct);
              }
              // just assume it will be ok, otherwise it will throw when executed
              return true;
            }

          // operand stack:
          // ..., value
          // ..., result
          case Opcode.D2F: // double to float
          case Opcode.I2F: // integer to float
          case Opcode.L2F: // long to float
            stack.pop(); // discard the value
            stack.push(CD_float);
            return true;

          // operand stack:
          // ..., value
          // ..., result
          case Opcode.D2L: // double to long
          case Opcode.F2L: // float to long
          case Opcode.I2L: // integer to long
            stack.pop(); // discard the value
            stack.push(CD_long);
            return true;

          // operand stack:
          // ..., value1, value2
          // ..., result
          case Opcode.DADD:
          case Opcode.DDIV:
          case Opcode.DMUL:
          case Opcode.DREM:
          case Opcode.DSUB:
            stack.pop(2); // discard the values
            stack.push(CD_double);
            return true;

          // operand stack:
          // ..., arrayref, index
          // ..., value
          case Opcode.DALOAD:
            stack.pop(2); // discard the arrayref and index
            stack.push(CD_double);
            return true;

          // operand stack:
          // ..., value1, value2
          // ..., result
          case Opcode.DCMPG:
          case Opcode.DCMPL:
          case Opcode.FCMPG:
          case Opcode.FCMPL:
            stack.pop(2); // discard the values
            stack.push(CD_int);
            return true;

          // operand stack:
          // ...
          // ..., value
          case Opcode.DCONST_0:
          case Opcode.DCONST_1:
            stack.push(CD_double);
            return true;

          // operand stack:
          // ...
          // ..., value
          case Opcode.DLOAD:
          case Opcode.DLOAD_0:
          case Opcode.DLOAD_1:
          case Opcode.DLOAD_2:
          case Opcode.DLOAD_3:
          case Opcode.DLOAD_W:
            stack.push(CD_double);
            return true;

          // operand stack:
          // ..., value
          // ..., result
          case Opcode.DNEG:
            stack.pop(); // discard the value
            stack.push(CD_double);
            return true;

          // operand stack:
          // ..., value
          // ..., value, value
          case Opcode.DUP:
            {
              final ClassDesc t = stack.pop();
              stack.push(t);
              stack.push(t);
              return true;
            }

          // operand stack:
          // ..., value2, value1
          // ..., value1, value2, value1
          case Opcode.DUP_X1:
            {
              final ClassDesc v1 = stack.pop();
              final ClassDesc v2 = stack.pop();
              stack.push(v1);
              stack.push(v2);
              stack.push(v1);
              return true;
            }

          // operand stack:
          // ..., value3, value2, value1
          // ..., value1, value3, value2, value1
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

          // operand stack:
          // ..., value2, value1
          // ..., value2, value1, value2, value1
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

          // operand stack:
          // ..., value3, value2, value1
          // ..., value2, value1, value3, value2, value1
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

          // operand stack:
          // ..., value
          // ..., result
          case Opcode.F2D: // float to double
          case Opcode.I2D: // integer to double
          case Opcode.L2D: // long to double
            stack.pop(); // discard the value
            stack.push(CD_double);
            return true;

          // operand stack:
          // ..., value1, value2
          // ..., result
          case Opcode.FADD:
          case Opcode.FDIV:
          case Opcode.FMUL:
          case Opcode.FREM:
          case Opcode.FSUB:
            stack.pop(2); // discard the values
            stack.push(CD_float);
            return true;

          // operand stack:
          // ..., arrayref, index
          // ..., value
          case Opcode.FALOAD:
            stack.pop(2); // discard the arrayref and index
            stack.push(CD_float);
            return true;

          // operand stack:
          // ...
          // ..., value
          case Opcode.FCONST_0:
          case Opcode.FCONST_1:
          case Opcode.FCONST_2:
            stack.push(CD_float);
            return true;

          // operand stack:
          // ...
          // ..., value
          case Opcode.FLOAD:
          case Opcode.FLOAD_0:
          case Opcode.FLOAD_1:
          case Opcode.FLOAD_2:
          case Opcode.FLOAD_3:
          case Opcode.FLOAD_W:
            stack.push(CD_float);
            return true;

          // operand stack:
          // ..., value
          // ..., result
          case Opcode.FNEG:
            stack.pop(); // discard the value
            stack.push(CD_float);
            return true;

          // operand stack:
          // ..., objectref
          // ..., value
          case Opcode.GETFIELD:
            {
              stack.pop(); // discard the value
              FieldInstruction fi = (FieldInstruction) inst;
              stack.push(fi.typeSymbol());
              return true;
            }

          // operand stack:
          // ...
          // ..., value
          case Opcode.GETSTATIC:
            {
              FieldInstruction fi = (FieldInstruction) inst;
              stack.push(fi.typeSymbol());
              return true;
            }

          // operand stack:
          // [no change]
          case Opcode.GOTO:
          case Opcode.GOTO_W:
            {
              BranchInstruction bi = (BranchInstruction) inst;
              addLabelsToWorklist(bi.target(), null, stack);
              return false;
            }

          // UNDONE: the JVM says result is int, but should we track 'true' type?
          // operand stack:
          // ..., value
          // ..., result
          case Opcode.I2B: // integer to byte
          case Opcode.I2C: // integer to char
          case Opcode.D2I: // double to integer
          case Opcode.F2I: // float to integer
          case Opcode.L2I: // long to int
          case Opcode.I2S: // integer to short
            stack.pop(); // discard the value
            stack.push(CD_int);
            return true;

          // operand stack:
          // ..., value1, value2
          // ..., result
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

          // operand stack:
          // ..., value1, value2
          // ...
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

          // operand stack:
          // ..., value
          // ...
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

          // operand stack:
          // [no change]
          case Opcode.IINC:
          case Opcode.IINC_W:
          case Opcode.NOP:
            return true;

          // operand stack:
          // ...
          // ..., value
          case Opcode.ILOAD:
          case Opcode.ILOAD_0:
          case Opcode.ILOAD_1:
          case Opcode.ILOAD_2:
          case Opcode.ILOAD_3:
          case Opcode.ILOAD_W:
            stack.push(CD_int);
            return true;

          // operand stack:
          // ..., value
          // ..., result
          case Opcode.INEG:
            stack.pop(); // discard the value
            stack.push(CD_int);
            return true;

          // UNDONE: the JVM says result is int, but should we track 'true' type?
          // operand stack:
          // ..., objectref
          // ..., result
          case Opcode.INSTANCEOF:
            stack.pop(); // discard the value
            stack.push(CD_int);
            return true;

          // TODO: JSR and RET have been illegal since JDK 7 (class file version 51.0).
          // Consider throwing an UnsupportedOperationException for these opcodes
          // if we're only supporting modern class files (version 51.0+).
          // Currently maintaining support for completeness.

          // operand stack:
          // ...
          // ..., [return address]
          case Opcode.JSR:
          case Opcode.JSR_W:
            // Perhaps we should add label of next instruction to work list but we have no idea
            // what stack will be after return from the JSR.
            stack.push(CD_Object); // there is no way to represent a return address, fake it
            DiscontinuedInstruction.JsrInstruction ji =
                (DiscontinuedInstruction.JsrInstruction) inst;
            addLabelsToWorklist(ji.target(), null, stack);
            return false;

          // operand stack:
          // ..., value1, value2
          // ..., result
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

          // operand stack:
          // ..., arrayref, index
          // ..., value
          case Opcode.LALOAD:
            stack.pop(2); // discard the arrayref and index
            stack.push(CD_long);
            return true;

          // operand stack:
          // ...
          // ..., value
          case Opcode.LCONST_0:
          case Opcode.LCONST_1:
            stack.push(CD_long);
            return true;

          // operand stack:
          // ...
          // ..., value
          case Opcode.LDC:
          case Opcode.LDC_W:
          case Opcode.LDC2_W:
            ConstantInstruction.LoadConstantInstruction ldc =
                (ConstantInstruction.LoadConstantInstruction) inst;
            LoadableConstantEntry lce = ldc.constantEntry();
            if (lce instanceof IntegerEntry) {
              stack.push(CD_int);
            } else if (lce instanceof LongEntry) {
              stack.push(CD_long);
            } else if (lce instanceof FloatEntry) {
              stack.push(CD_float);
            } else if (lce instanceof DoubleEntry) {
              stack.push(CD_double);
            } else if (lce instanceof StringEntry) {
              stack.push(CD_String);
            } else if (lce instanceof ClassEntry cent) {
              stack.push(cent.asSymbol());
            }
            return true;

          // operand stack:
          // ...
          // ..., value
          case Opcode.LLOAD:
          case Opcode.LLOAD_0:
          case Opcode.LLOAD_1:
          case Opcode.LLOAD_2:
          case Opcode.LLOAD_3:
          case Opcode.LLOAD_W:
            stack.push(CD_long);
            return true;

          // operand stack:
          // ..., value
          // ..., result
          case Opcode.LNEG:
            stack.pop(); // discard the value
            stack.push(CD_long);
            return true;

          // operand stack:
          // ..., key
          // ...
          case Opcode.LOOKUPSWITCH:
            stack.pop(); // discard the value
            LookupSwitchInstruction lsi = (LookupSwitchInstruction) inst;
            addLabelsToWorklist(lsi.defaultTarget(), lsi.cases(), stack);
            return false;

          // operand stack:
          // ..., objectref
          // ...
          case Opcode.MONITORENTER:
          case Opcode.MONITOREXIT:
            stack.pop(); // discard the value
            return true;

          // operand stack:
          // ..., count1, [count2, ...]
          // ..., arrayref
          case Opcode.MULTIANEWARRAY:
            final NewMultiArrayInstruction nmai = (NewMultiArrayInstruction) inst;
            stack.pop(nmai.dimensions()); // discard all the counts
            stack.push(nmai.arrayType().asSymbol());
            return true;

          // operand stack:
          // ...
          // ..., objectref
          case Opcode.NEW:
            final NewObjectInstruction noi = (NewObjectInstruction) inst;
            stack.push(noi.className().asSymbol());
            return true;

          // operand stack:
          // ..., count
          // ..., arrayref
          case Opcode.NEWARRAY:
            stack.pop(); // discard the count
            final NewPrimitiveArrayInstruction npai = (NewPrimitiveArrayInstruction) inst;
            String descriptor;
            switch (npai.typeKind()) {
              case BOOLEAN -> {
                descriptor = "[Z";
              }
              case BYTE -> {
                descriptor = "[B";
              }
              case CHAR -> {
                descriptor = "[C";
              }
              case DOUBLE -> {
                descriptor = "[D";
              }
              case FLOAT -> {
                descriptor = "[F";
              }
              case INT -> {
                descriptor = "[I";
              }
              case LONG -> {
                descriptor = "[J";
              }
              case SHORT -> {
                descriptor = "[S";
              }
              default -> {
                throw new DynCompError("unknown primitive array type: " + inst);
              }
            }
            stack.push(ClassDesc.ofDescriptor(descriptor));
            return true;

          // operand stack:
          // ..., value
          // ...
          case Opcode.POP:
            stack.pop(); // discard the value
            return true;

          // operand stack:
          // ..., value2, value1
          // ...
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

          // operand stack:
          // ..., objectref, value
          // ...
          case Opcode.PUTFIELD:
            stack.pop(2);
            return true;

          // operand stack:
          // ..., value
          // ...
          case Opcode.PUTSTATIC:
            stack.pop(); // discard the value
            return true;

          // TODO: JSR and RET have been illegal since JDK 7 (class file version 51.0).
          // Consider throwing an UnsupportedOperationException for these opcodes
          // if we're only supporting modern class files (version 51.0+).
          // Currently maintaining support for completeness.

          // operand stack:
          // [no change]
          case Opcode.RET:
          case Opcode.RET_W:
            // the variable referenced must contain a return address
            // that we would treat as target of jump; no way to do that
            return false;

          // operand stack:
          // ..., value1, value2
          // ..., value2, value1
          case Opcode.SWAP:
            {
              final ClassDesc v2 = stack.pop();
              final ClassDesc v1 = stack.pop();
              stack.push(v2);
              stack.push(v1);
              return true;
            }

          // operand stack:
          // ..., index
          // ...
          case Opcode.TABLESWITCH:
            stack.pop(); // discard the index
            TableSwitchInstruction tsi = (TableSwitchInstruction) inst;
            addLabelsToWorklist(tsi.defaultTarget(), tsi.cases(), stack);
            return false;

          // operand stack:
          // ..., objectref, [arg1, [arg2 ...]]
          // ...
          case Opcode.INVOKEINTERFACE:
          case Opcode.INVOKESPECIAL:
          case Opcode.INVOKEVIRTUAL:
            stack.pop(); // discard the objectref
          // may actually be removing an arg, but we'll
          // account for that when we remove args below

          // fall through is intentional:

          // operand stack:
          // ..., [arg1, [arg2 ...]]
          // ...
          case Opcode.INVOKESTATIC:
            {
              final InvokeInstruction ii = (InvokeInstruction) inst;
              final MethodTypeDesc mtd = ii.typeSymbol();
              stack.pop(mtd.parameterCount()); // discard the arguments
              // We are sure the invoked method will xRETURN eventually.
              // We simulate xRETURNs functionality here because we don't
              // really "jump into" and simulate the invoked method.
              final ClassDesc rt = mtd.returnType();
              if (!rt.equals(CD_void)) {
                stack.push(rt);
              }
              return true;
            }

          // operand stack:
          // ..., [arg1, [arg2 ...]]
          // ...
          case Opcode.INVOKEDYNAMIC:
            {
              final InvokeDynamicInstruction idi = (InvokeDynamicInstruction) inst;
              final MethodTypeDesc mtd = idi.typeSymbol();
              stack.pop(mtd.parameterCount()); // discard the arguments
              // We are sure the invoked method will xRETURN eventually.
              // We simulate xRETURNs functionality here because we don't
              // really "jump into" and simulate the invoked method.
              final ClassDesc rt = mtd.returnType();
              if (!rt.equals(CD_void)) {
                stack.push(rt);
              }
              return true;
            }

          default:
            throw new DynCompError("Unexpected instruction opcode: " + inst);
        }
      }

      // We ignore most PseudoInstructions.

      case ExceptionCatch ec -> {
        // Nothing needs to be done.
        return true;
      }

      // Technically, a Classfile element, not a PseudoInstruction.
      case Label l -> {
        if (DCInstrument24.stacks[inst_index] != null) {
          // we've seen this label before
          // will throw if stacks don't match
          DCInstrument24.verifyOperandStackMatches(l, DCInstrument24.stacks[inst_index], stack);
          // stacks match; we're done with this worklist
          return false;
        } else {
          // have not seen this label before; remember state of operand stack
          DCInstrument24.stacks[inst_index] = stack.getClone();
          if (DCInstrument24.debugOperandStack) {
            System.out.println("save stack state at: " + l);
            System.out.println("  " + inst_index + ", " + DCInstrument24.stacks[inst_index]);
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
   * Create a set of worklist items.
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
