package daikon.dcomp;

import java.util.*;
import java.util.regex.*;
import java.lang.reflect.Constructor;
import java.lang.reflect.Modifier;
import java.io.*;

import org.apache.bcel.*;
import org.apache.bcel.classfile.*;
import org.apache.bcel.generic.*;
import org.apache.bcel.verifier.*;
import org.apache.bcel.verifier.structurals.*;
import daikon.util.BCELUtil;

import daikon.util.*;
import org.apache.commons.io.*;

import daikon.chicory.MethodInfo;
import daikon.chicory.ClassInfo;
import daikon.chicory.DaikonWriter;

import daikon.DynComp;

/**
 * Instruments a class file to perform Data Flow.
 */
class DFInstrument extends DCInstrument {

  public static SimpleLog debug = new SimpleLog (false);

  /** True if the current method is the test sequence **/
  private static boolean test_sequence = false;

  /** Array from local variable index to local variable name in the test seq **/
  public static String[] test_seq_locals = null;

  /**
   * Initialize with the original class and whether or not the class
   * is part of the JDK
   */
  public DFInstrument (JavaClass orig_class, boolean in_jdk,
                       ClassLoader loader) {
    super (orig_class, in_jdk, loader);
    ignore_toString = false;
    debug.log ("dataflow instrumentation of %s", gen.getClassName());
  }


  /**
   * Instrument the specified method for DataFlow
   */
  public void instrument_method (Method m, MethodGen mg) {

    // See if this method has branch information
    CodeRange branch_cr = find_branch (DynComp.branch, gen.getClassName(), m);
    if (branch_cr != null)
      debug.log ("branch_cr for method %s.%s = %s", mg.getClassName(),
                 mg.getName(), branch_cr);

    // See if this method is the test sequence
    test_sequence = false;
    if (has_specified_method (DynComp.input_method, mg.getClassName(), m)) {
      test_sequence = true;
      debug.log ("test sequence method: %s.%s", mg.getClassName(),
                 mg.getName());
    }

    // Get Stack information
    StackTypes stack_types = null;
    TypeStack type_stack = null;
    if (use_StackVer) {
      stack_types = bcel_calc_stack_types (mg);
      if (stack_types == null) {
        skip_method (mg);
        return;
      }
    } else { // Use Eric's version
      type_stack = new TypeStack (mg);
    }

    // Loop through each instruction, making substitutions
    InstructionList il = mg.getInstructionList();
    OperandStack stack = null;
    for (InstructionHandle ih = il.getStart(); ih != null; ) {
      if (debug_instrument_inst.enabled()) {
        debug_instrument_inst.log ("instrumenting instruction %s%n", ih);
                     // ih.getInstruction().toString(pool.getConstantPool()));
      }
      InstructionList new_il = null;

      // Remember the next instruction to process
      InstructionHandle next_ih = ih.getNext();

      // Get the stack information
      if (use_StackVer)
        stack = stack_types.get (ih.getPosition());

      // Get the translation for this instruction (if any)
      new_il = xform_inst (mg, ih, stack, branch_cr);
      if (debug_instrument_inst.enabled())
        debug_instrument_inst.log ("  new inst: %s%n", new_il);

      if (!use_StackVer)
        stack = type_stack.getAfterInst (ih);

      // If this instruction was modified, replace it with the new
      // instruction list. If this instruction was the target of any
      // jumps or line numbers , replace them with the first
      // instruction in the new list
      replace_instructions (il, ih, new_il);

      ih = next_ih;
    }
  }

  /**
   * Post processes instrumented methods.  If the method is the test
   * sequence, the local variable table is saved so that we can
   * translate from indices back to variables
   */
  public void post_process (Method m, MethodGen mg) {

    if (!test_sequence)
      return;

    LocalVariable[] lvt
      = mg.getMethod().getLocalVariableTable().getLocalVariableTable();
      // = mg.getLocalVariableTable(pool).getLocalVariableTable();

    debug.log ("Local variable table for test sequence %s%n",
               mg.getName());
    int max_index = 0;
    for (LocalVariable lv : lvt) {
      debug.log  ("local variable %s index %d%n", lv.getName(),
                  lv.getIndex());
      if (lv.getIndex() > max_index)
        max_index = lv.getIndex();
    }
    test_seq_locals = new String[max_index+1];
    for (LocalVariable lv : lvt) {
      test_seq_locals[lv.getIndex()] = lv.getName();
    }
}

  /**
   * Transforms instructions to track dataflow.  Returns a list
   * of instructions that replaces the specified instruction.  Returns
   * null if the instruction should not be replaced.
   *
   *    @param mg Method being instrumented
   *    @param ih Handle of Instruction to translate
   *    @param stack Current contents of the stack.
   *    @param branch_cr Code range of branch whose dataflow is desired.
   *    null indicates there are no branches of interest in this method.
   */
  /*@Nullable*/ InstructionList xform_inst (MethodGen mg, InstructionHandle ih,
                              OperandStack stack, CodeRange branch_cr) {

    Instruction inst = ih.getInstruction();

    switch (inst.getOpcode()) {

    // if this is a frontier branch, remember the DF of the objects being
    // compared, otherwise do nothing
    case Constants.IF_ACMPEQ:
    case Constants.IF_ACMPNE: {
      if ((branch_cr != null) && branch_cr.contains (ih.getPosition())) {
        return build_il (new DUP2(),
                         dcr_call ("ref2_branch_df", Type.VOID, two_objects),
                         inst);
      } else { // not a branch of interest
        return null;
      }
    }

    // These instructions compare the integer on the top of the stack
    // to zero.  There is no dataflow here, so we need only
    // discard the tag on the top of the stack.
    case Constants.IFEQ:
    case Constants.IFNE:
    case Constants.IFLT:
    case Constants.IFGE:
    case Constants.IFGT:
    case Constants.IFLE: {
      if ((branch_cr != null) && branch_cr.contains (ih.getPosition())) {
        debug.log ("generating code for branch at %s:%s", mg.getName(), inst);
        return build_il (ifact.createConstant (0),
                         dcr_call ("prim_branch_df", Type.VOID, integer_arg),
                         inst);
      } else { // not a branch of interest
        return discard_tag_code (inst, 1);
      }
    }

    // These instructions compare the top of stack to Null.  There is
    // no work to do unless this is branch of interest, in which
    // case we want to note the dataflow at the branch.
    case Constants.IFNONNULL:
    case Constants.IFNULL: {
      if ((branch_cr != null) && branch_cr.contains (ih.getPosition())) {
        debug.log ("generating code for branch at %s:%s", mg.getName(), inst);
        return build_il (dcr_call ("ref_cmp_null_df", Type.OBJECT, object_arg),
                         inst);
      }
      return (null);
    }


    // Instanceof pushes either 0 or 1 on the stack depending on whether
    // the object on top of stack is of the specified type.  The resulting
    // boolean's tag should take on the dataflow value of the object being
    // checked.  We duplicate the object on the top of the stack and call
    // dup_obj_tag_val to push a new tag that points to the same values
    case Constants.INSTANCEOF: {
      return build_il (ifact.createDup (1),
                       dcr_call ("dup_obj_tag_val", Type.VOID, object_arg),
                       inst);
    }

    // Duplicates the item on the top of stack.  If the value on the
    // top of the stack is a primitive, we need to do the same on the
    // tag stack.  Otherwise, we need do nothing.
    case Constants.DUP: {
      return dup_tag (inst, stack);
    }

    // Duplicates the item on the top of the stack and inserts it 2
    // values down in the stack.  If the value at the top of the stack
    // is not a primitive, there is nothing to do here.  If the second
    // value is not a primitive, then we need only to insert the duped
    // value down 1 on the tag stack (which contains only primitives)
    case Constants.DUP_X1: {
      return dup_x1_tag (inst, stack);
    }

    // Duplicates either the top 2 category 1 values or a single
    // category 2 value and inserts it 2 or 3 values down on the
    // stack.
    case Constants.DUP2_X1: {
      return dup2_x1_tag (inst, stack);
    }

    // Duplicate either one category 2 value or two category 1 values.
    case Constants.DUP2: {
      return dup2_tag (inst, stack);
    }

    // Dup the category 1 value on the top of the stack and insert it either
    // two or three values down on the stack.
    case Constants.DUP_X2: {
      return dup_x2 (inst, stack);
    }

    case Constants.DUP2_X2: {
      return dup2_x2 (inst, stack);
    }

    // Pop instructions discard the top of the stack.  We want to discard
    // the top of the tag stack iff the item on the top of the stack is a
    // primitive.
    case Constants.POP: {
      return pop_tag (inst, stack);
    }

    // Pops either the top 2 category 1 values or a single category 2 value
    // from the top of the stack.  We must do the same to the tag stack
    // if the values are primitives.
    case Constants.POP2: {
      return pop2_tag (inst, stack);
    }

    // Swaps the two category 1 types on the top of the stack.  We need
    // to swap the top of the tag stack if the two top elements on the
    // real stack are primitives.
    case Constants.SWAP: {
      return swap_tag (inst, stack);
    }

    // Compare 2 integers on the top of the stack and jumps accordingly.
    // This doesn't create any direct dataflow so we need only pop the
    // two tags on the tag stack unless this is the branch of interest
    // If it is the branch of interest, we want to dump the DF for each
    // primitive in the comparison
    case Constants.IF_ICMPEQ:
    case Constants.IF_ICMPGE:
    case Constants.IF_ICMPGT:
    case Constants.IF_ICMPLE:
    case Constants.IF_ICMPLT:
    case Constants.IF_ICMPNE: {
      if ((branch_cr != null) && branch_cr.contains (ih.getPosition())) {
        debug.log ("generating code for branch at %s:%s", mg.getName(), inst);
        return build_il (new DUP2(),
                         dcr_call ("int2_branch_df", Type.VOID, two_ints),
                         inst);
      } else { // not a branch of interest
        return discard_tag_code (inst, 2);
      }
    }

    case Constants.GETFIELD: {
      return load_store_field (mg, (GETFIELD) inst);
    }

    case Constants.PUTFIELD: {
      return load_store_field (mg, (PUTFIELD) inst);
    }

    case Constants.GETSTATIC: {
      return load_store_field (mg, ((GETSTATIC) inst));
    }

    case Constants.PUTSTATIC: {
      return load_store_field (mg, ((PUTSTATIC) inst));
    }

    case Constants.DLOAD:
    case Constants.DLOAD_0:
    case Constants.DLOAD_1:
    case Constants.DLOAD_2:
    case Constants.DLOAD_3:
    case Constants.FLOAD:
    case Constants.FLOAD_0:
    case Constants.FLOAD_1:
    case Constants.FLOAD_2:
    case Constants.FLOAD_3:
    case Constants.ILOAD:
    case Constants.ILOAD_0:
    case Constants.ILOAD_1:
    case Constants.ILOAD_2:
    case Constants.ILOAD_3:
    case Constants.LLOAD:
    case Constants.LLOAD_0:
    case Constants.LLOAD_1:
    case Constants.LLOAD_2:
    case Constants.LLOAD_3: {
      return load_store_local ((LoadInstruction)inst, tag_frame_local,
                               "push_local_tag");
    }

    case Constants.DSTORE:
    case Constants.DSTORE_0:
    case Constants.DSTORE_1:
    case Constants.DSTORE_2:
    case Constants.DSTORE_3:
    case Constants.FSTORE:
    case Constants.FSTORE_0:
    case Constants.FSTORE_1:
    case Constants.FSTORE_2:
    case Constants.FSTORE_3:
    case Constants.ISTORE:
    case Constants.ISTORE_0:
    case Constants.ISTORE_1:
    case Constants.ISTORE_2:
    case Constants.ISTORE_3:
    case Constants.LSTORE:
    case Constants.LSTORE_0:
    case Constants.LSTORE_1:
    case Constants.LSTORE_2:
    case Constants.LSTORE_3: {
      if (test_sequence)
        return load_store_local ((StoreInstruction) inst, tag_frame_local,
                                 "pop_local_tag_df");
      else
        return load_store_local ((StoreInstruction) inst, tag_frame_local,
                                 "pop_local_tag");
    }

    case Constants.LDC:
    case Constants.LDC_W:
    case Constants.LDC2_W: {
      return ldc_tag_df (mg, inst, stack);
    }

    // Push the tag for the array onto the tag stack.  This causes
    // anything comparable to the length to be comparable to the array
    // as an index.
    case Constants.ARRAYLENGTH: {
      return array_length_df (inst);
    }

    // These instructions push a constant on the stack.  We push a
    // tag on the tag stack the refers to the constant location
    case Constants.BIPUSH:
    case Constants.SIPUSH:
    case Constants.DCONST_0:
    case Constants.DCONST_1:
    case Constants.FCONST_0:
    case Constants.FCONST_1:
    case Constants.FCONST_2:
    case Constants.ICONST_0:
    case Constants.ICONST_1:
    case Constants.ICONST_2:
    case Constants.ICONST_3:
    case Constants.ICONST_4:
    case Constants.ICONST_5:
    case Constants.ICONST_M1:
    case Constants.LCONST_0:
    case Constants.LCONST_1: {
      ConstantPushInstruction cpi = (ConstantPushInstruction) inst;
      String descr = String.format ("%s.%s:constant@%d:%s", mg.getClassName(),
                               mg.getName(), ih.getPosition(), cpi.getValue());
      return build_il (ifact.createConstant (descr),
                       dcr_call ("push_const_src", Type.VOID, string_arg),
                       inst);
    }

    // Primitive Binary operators.  Each is augmented with a call to
    // DCRuntime.binary_tag_df that pops the operand tags from the tag
    // stack and creates and pushes a new tag that refers to the union
    // of the operand tags
    case Constants.DADD:
    case Constants.DCMPG:
    case Constants.DCMPL:
    case Constants.DDIV:
    case Constants.DMUL:
    case Constants.DREM:
    case Constants.DSUB:
    case Constants.FADD:
    case Constants.FCMPG:
    case Constants.FCMPL:
    case Constants.FDIV:
    case Constants.FMUL:
    case Constants.FREM:
    case Constants.FSUB:
    case Constants.IADD:
    case Constants.IAND:
    case Constants.IDIV:
    case Constants.IMUL:
    case Constants.IOR:
    case Constants.IREM:
    case Constants.ISHL:
    case Constants.ISHR:
    case Constants.ISUB:
    case Constants.IUSHR:
    case Constants.IXOR:
    case Constants.LADD:
    case Constants.LAND:
    case Constants.LCMP:
    case Constants.LDIV:
    case Constants.LMUL:
    case Constants.LOR:
    case Constants.LREM:
    case Constants.LSHL:
    case Constants.LSHR:
    case Constants.LSUB:
    case Constants.LUSHR:
    case Constants.LXOR:
      return build_il (dcr_call ("binary_tag_df", Type.VOID, Type.NO_ARGS),
                       inst);

    // Computed jump based on the int on the top of stack.  Since that int
    // doesn't contribute directly to dataflow its tag is just discarded.
    case Constants.LOOKUPSWITCH:
    case Constants.TABLESWITCH:
      return discard_tag_code (inst, 1);

    // Allocates arrays of a single dimension.  The tag of the array size
    // is on the tag stack.  The array size does not contribute to the
    // returned array, but it does contribute to any calls to arraylength.
    // We thus remember that as a special field of the array.
    case Constants.ANEWARRAY:
    case Constants.NEWARRAY: {
      return new_array_df (mg, inst);
    }

    // For each allocated array, associate the DF of its size with its
    // arraylength and the array itself with its allocation point
    case Constants.MULTIANEWARRAY: {
      return multiarray_df(mg, inst);
    }

    // Do nothing for new, the entry for the allocated object is created
    // with the call to the constructor
    case Constants.NEW: {
      return null;
    }

    // Mark the array and its index as comparable.  Also for primitives,
    // push the tag of the array element on the tag stack
    case Constants.AALOAD:
    case Constants.BALOAD:
    case Constants.CALOAD:
    case Constants.DALOAD:
    case Constants.FALOAD:
    case Constants.IALOAD:
    case Constants.LALOAD:
    case Constants.SALOAD: {
      return array_load_df (inst);
    }

    // Handle array store instruction.  The DF of the array element is
    // set to the union of the value being stored and the index
    case Constants.AASTORE:
      return array_store (inst, "aastore_df", Type.OBJECT);
    case Constants.BASTORE:
      return array_store (inst, "bastore_df", Type.BYTE);
    case Constants.CASTORE:
      return array_store (inst, "castore_df", Type.CHAR);
    case Constants.DASTORE:
      return array_store (inst, "dastore_df", Type.DOUBLE);
    case Constants.FASTORE:
      return array_store (inst, "fastore_df", Type.FLOAT);
    case Constants.IASTORE:
      return array_store (inst, "iastore_df", Type.INT);
    case Constants.LASTORE:
      return array_store (inst, "lastore_df", Type.LONG);
    case Constants.SASTORE:
      return array_store (inst, "sastore_df", Type.SHORT);

    // Prefix the return with a call to the correct normal_exit method
    // to handle the tag stack
    case Constants.ARETURN:
    case Constants.DRETURN:
    case Constants.FRETURN:
    case Constants.IRETURN:
    case Constants.LRETURN:
    case Constants.RETURN: {
      return return_tag (mg, inst);
    }

    // Handle subroutine calls.  Calls to instrumented code are modified
    // to call the instrumented version (with the DCompMarker argument).
    // Calls to uninstrumented code (rare) discard primitive arguments
    // from the tag stack and produce an arbitrary return tag.
    case Constants.INVOKESTATIC:
    case Constants.INVOKEVIRTUAL:
    case Constants.INVOKESPECIAL:
    case Constants.INVOKEINTERFACE: {
      InstructionHandle prev = ih.getPrev();
      boolean prev_new = false;
      if (prev != null) {
        // System.out.printf ("previous instruction = %s\n", prev);
        InstructionHandle prev_prev = prev.getPrev();
        if (prev_prev != null) {
          prev_new = (prev.getInstruction().getOpcode() == Constants.DUP)
            && (prev_prev.getInstruction().getOpcode() == Constants.NEW);
        }
      }
      return handle_invoke_df (mg, (InvokeInstruction) inst, stack,
                               ih.getPosition());
    }

    // Throws an exception.  This clears the operand stack of the current
    // frame.  We need to clear the tag stack as well.
    case Constants.ATHROW:
      return build_il (dcr_call ("throw_op", Type.VOID, Type.NO_ARGS), inst);

    // Stores of references into a local.  If this is the test sequence we
    // want to include a reference to this local in the dataflow for the
    // object
    case Constants.ASTORE:
    case Constants.ASTORE_0:
    case Constants.ASTORE_1:
    case Constants.ASTORE_2:
    case Constants.ASTORE_3: {
      if (test_sequence) {
        LocalVariableInstruction lvi = (LocalVariableInstruction)inst;
        return build_il (new DUP(),
                         ifact.createConstant (lvi.getIndex()),
                         dcr_call ("pop_local_obj_df", Type.VOID, object_int),
                         inst);
      } else {
        return null;
      }
    }

    // Opcodes that don't need any modifications.  Here for reference
    case Constants.ACONST_NULL:
    case Constants.ALOAD:
    case Constants.ALOAD_0:
    case Constants.ALOAD_1:
    case Constants.ALOAD_2:
    case Constants.ALOAD_3:
    case Constants.CHECKCAST:
    case Constants.D2F:     // double to float
    case Constants.D2I:     // double to integer
    case Constants.D2L:     // double to long
    case Constants.DNEG:    // Negate double on top of stack
    case Constants.F2D:     // float to double
    case Constants.F2I:     // float to integer
    case Constants.F2L:     // float to long
    case Constants.FNEG:    // Negate float on top of stack
    case Constants.GOTO:
    case Constants.GOTO_W:
    case Constants.I2B:     // integer to byte
    case Constants.I2C:     // integer to char
    case Constants.I2D:     // integer to double
    case Constants.I2F:     // integer to float
    case Constants.I2L:     // integer to long
    case Constants.I2S:     // integer to short
    case Constants.IINC:    // increment local variable by a constant
    case Constants.INEG:    // negate integer on top of stack
    case Constants.JSR:     // pushes return address on the stack, but that
                            // is thought of as an object, so we don't need
                            // a tag for it.
    case Constants.JSR_W:
    case Constants.L2D:     // long to double
    case Constants.L2F:     // long to float
    case Constants.L2I:     // long to int
    case Constants.LNEG:    // negate long on top of stack
    case Constants.MONITORENTER:
    case Constants.MONITOREXIT:
    case Constants.NOP:
    case Constants.RET:     // this is the internal JSR return
      return (null);

    // Make sure we didn't miss anything
    default:
      throw new Error("instruction " + inst + " unsupported");
    }

  }

  /**
   * Adjusts the tag stack for load constant opcodes.  If the constant is
   * a primitive, creates a new tag and pushes it on the tag stack.
   * A string description of the constant is included.
   * If the constant is a reference (string, class), does nothing
   */
  InstructionList ldc_tag_df (MethodGen mg, Instruction inst,
                              OperandStack stack) {
    Type type;
    Object value;
    if (inst instanceof LDC) { // LDC_W extends LDC
      type = ((LDC)inst).getType (pool);
      value = ((LDC)inst).getValue (pool);
    } else {
      type = ((LDC2_W)inst).getType (pool);
      value = ((LDC2_W)inst).getValue (pool);
    }
    if (type instanceof BasicType) {
      String descr = mg.getName() + ": constant " + value;
      return build_il (ifact.createConstant (descr),
                       dcr_call ("push_const_src", Type.VOID, string_arg),
                       inst);
    } else { // Must be a string or class
      String descr = mg.getName() + ": consant " + value;
      return build_il (inst,
                       new DUP(),
                       ifact.createConstant (descr),
                       dcr_call ("push_const_obj_src", Type.VOID,object_string)
                       );
    }
  }

  /**
   * Creates code that creates a new tag that references the same value
   * set as does the arrays tag.  First, the arrayref is duplicated on
   * the stack.  Then a method is called to create a new tag with the
   * same values as that of the array.  Finally the original arraylength
   * instruction is performed.
   */
  public InstructionList array_length_df (Instruction inst) {

    InstructionList il = new InstructionList();

    // Duplicate the array ref and pass it to DCRuntime which will push
    // it onto the tag stack.
    il.append (new DUP());
    il.append (dcr_call ("arraylen_df", Type.VOID,
                         new Type[] {Type.OBJECT}));

    // Perform the original instruction
    il.append (inst);

    return (il);
  }

  /**
   * The new-array opcodes pop a size from the stack and push an array
   * reference of the specified size.  We want to make the dataflow of any
   * subsequent array-length instructions to be related to the size.
   * We also need to initialize the array reference itself with its
   * location.
   */
  public InstructionList new_array_df (MethodGen mg, Instruction inst) {
    InstructionList il = new InstructionList();

    // Perform the original instruction
    il.append (inst);

    // Duplicate the array ref from the top of the stack and pass it
    // to DCRuntime which will create dataflow information that includes
    // its size and associate the array object with it.  Also pass a
    // description of this location so we know the source of the array
    il.append (new DUP());
    String descr = mg.getName() + ": new-array ";
    il.append (ifact.createConstant (descr));
    il.append (dcr_call ("setup_array_df", Type.VOID,
                         new Type[] {Type.OBJECT, Type.STRING}));


    return (il);
  }

  /**
   * The multi new-array opcodes pop dim sizes from the stack and push an array
   * reference of the specified dimenensions and size.  We want to make the
   * dataflow of any subsequent array-length instructions to be related to
   * the size of that array.  We also need to initialize the array references
   * themselves with this location.
   */
  public InstructionList multiarray_df (MethodGen mg, Instruction inst) {
    InstructionList il = new InstructionList();

    // Perform the original instruction
    il.append (inst);

    // Duplicate the new arrayref
    il.append (new DUP());

    // Push the number of dimensions
    il.append (ifact.createConstant (((MULTIANEWARRAY)inst).getDimensions()));

    // Push the description of the allocation
    String descr = mg.getName() + ": new-array ";
    il.append (ifact.createConstant (descr));

    // Call setup_multiarray_df to properly initialize the DF references
    // for the arrays and their lengths.
    il.append (dcr_call ("setup_multiarray_df", Type.VOID,
                         new Type[] {Type.OBJECT, Type.INT, Type.STRING}));

    return (il);
  }

  /**
   * Creates code that makes the DF of an array load equal to the
   * union of the index DF and the array element DF First the arrayref
   * and its index are duplicated on the stack.  Then the appropriate
   * array load method is called to calculate the DF and update the
   * tag stack.  Finally the original load instruction is performed.
   */
  public InstructionList array_load_df (Instruction inst) {

    InstructionList il = new InstructionList();

    // Duplicate the array ref and index and pass them to DCRuntime
    il.append (new DUP2());
    String method = "primitive_array_load_df";
    if (inst instanceof AALOAD)
      method = "ref_array_load_df";

    il.append (dcr_call (method, Type.VOID,
                         new Type[] {Type.OBJECT, Type.INT}));

    // Perform the original instruction
    il.append (inst);

    return (il);
  }

  /**
   * Returns the code range of the branch specified in branch_id.
   * The branch is identified as class:method;line#.  The
   * class is the fully qualified class name.  The method is just the
   * method name (not the full signature), and the line # is the line
   * number in the java source of the conditional of interest.
   * Returns null if the specified branch is not in this method.
   */
  public /*@Nullable*/ CodeRange find_branch (String branch_id, String classname, Method m) {

    // Get the classname, method name, and linenumber of the branch
    String[] sa = branch_id.split(":");
    String bclassname = sa[0];
    String bmethod = sa[1];
    int line = Integer.decode(sa[2]);

    // System.out.printf ("Comparing %s:%s:%d to %s %s\n", bclassname, bmethod,
    //                   line, classname, m.getName());

    if (!bclassname.equals (classname)) {
      // System.out.printf ("classname '%s' does not match '%s'\n", bclassname,
      //                   classname);
      return null;
    }

    if (!bmethod.equals (m.getName())) {
      // System.out.printf ("method '%s' does not match '%s'\n", bmethod,
      //                   m.getName());
      return null;
    }

    // Get a line number table and complain if it doesn't exist
    LineNumberTable lnt = m.getLineNumberTable();
    if (lnt == null)
      throw new RuntimeException ("No line number table for " + classname +
                                  "." + m);
    LineNumber[] lna = lnt.getLineNumberTable();
    if ((lna == null) || (lna.length == 0))
      throw new RuntimeException ("Empty line number table for " + classname +
                                  "." + m);

    // Look for the specified line in this method, keep track of min/max
    // line in the method so we can tell if the line falls within the method
    // but has no code associated with it.
    int min_line = 1000000;
    int max_line = -1;
    for (int i = 0; i < lna.length; i++) {
      LineNumber ln = lna[i];
      int lnum = ln.getLineNumber();
      if (lnum == line) {
        int last_pc;
        int next_line = i+1;
        if (next_line < lna.length)
          last_pc = lna[next_line].getStartPC()-1;
        else
          last_pc = m.getCode().getCode().length;
        return new CodeRange (ln.getStartPC(), (last_pc - ln.getStartPC()) + 1);
      }
      if (lnum < min_line)
        min_line = lnum;
      if (lnum > max_line)
        max_line = lnum;
    }

    // If line is in the method, then there is no code for this line
    if ((line >= min_line) && (line <= max_line))
      throw new RuntimeException
        (String.format ("line %d in %s.%s[%d..%d] has no code", line,
                        classname, m, min_line, max_line));

    System.out.printf ("line %d is not in method %s.%s [%d..%d]\n", line,
                       bclassname, bmethod, min_line, max_line);
    return (null);
  }

  /**
   * Discards primitive tags for each primitive argument to a non-instrumented
   * method and adds a tag for a primitive return value.  Insures that the
   * tag stack is correct for non-instrumented methods
   */
  InstructionList handle_invoke_df (MethodGen mg, InvokeInstruction invoke,
                                    OperandStack stack, int position) {

    // Get information about the call
    String classname = invoke.getClassName (pool);
    String method_name = invoke.getMethodName(pool);
    Type ret_type = invoke.getReturnType(pool);
    Type[] arg_types = invoke.getArgumentTypes(pool);

    InstructionList il = new InstructionList();

    // Determine if the callee is instrumented.
    boolean callee_instrumented = callee_instrumented (classname);
    if (is_object_method (method_name, invoke.getArgumentTypes(pool)))
      callee_instrumented = false;

    if (callee_instrumented) {

      // Add the DCompMarker argument so that the instrumented version
      // will be used
      il.append (new ACONST_NULL());
      Type[] new_arg_types = BCELUtil.add_type (arg_types, dcomp_marker);
      il.append (ifact.createInvoke (classname, method_name, ret_type,
                                     new_arg_types, invoke.getOpcode()));

    } else { // not instrumented, discard the tags before making the call

      // Determine if there is a replacement method that will calculate DF
      String replacement_method = null;
      Map<MethodDef,String> class_method_map
        = SummaryInfo.jdk_method_map.get (classname);
      if (class_method_map != null) {
        MethodDef md = new MethodDef (method_name, arg_types);
        replacement_method = class_method_map.get (md);
      }

      // If there is a replacement method, call that method instead and return
      if (replacement_method != null) {
        if (invoke.getOpcode() == Constants.INVOKEVIRTUAL) {
          Type invoke_class = invoke.getReferenceType(pool);
          arg_types = BCELUtil.insert_type (invoke_class, arg_types);
          il.append (dcr_call (replacement_method, ret_type, arg_types));
        } else if (invoke.getOpcode() == Constants.INVOKESPECIAL) {
          constructor_summary (il, invoke, replacement_method);
        } else { // static call
          il.append (dcr_call (replacement_method, ret_type, arg_types));
        }
        debug.log ("insert call to replacement method %s(%s)%n",
                   replacement_method, Arrays.toString(arg_types));
        return (il);
      }

      // Handle equals by calling a static method that calculates DF
      // and delegates appropriately
      if (is_object_equals (method_name, ret_type, arg_types)) {

        if (invoke.getOpcode() == Constants.INVOKEVIRTUAL) {
          il.append (dcr_call ("equals_df", ret_type, two_objects));
          return (il);
        } else { // super call
          il.append (new DUP2());
          il.append (dcr_call ("super_equals_df", Type.VOID, two_objects));
          il.append (invoke);
          return (il);
        }

      } else if (is_object_clone(method_name, ret_type, arg_types) ||
               (is_object_toString(method_name, ret_type, arg_types)
                && !ignore_toString)) {

        return  instrument_object_call (invoke, "_df");
      }

      // Discard the tags for any primitive arguments passed to system
      // methods
      il.append (discard_primitive_tags (arg_types));

      // Add a tag for the return type if it is primitive
      if ((ret_type instanceof BasicType) && (ret_type != Type.VOID)) {
        // System.out.printf ("push tag for return  type of %s%n",
        //                   invoke.getReturnType(pool));
        String descr = String.format ("%s.%s:%s-ret-val@%d", mg.getClassName(),
                                      mg.getName(), method_name, position);
        il.append (ifact.createConstant (descr));
        il.append (dcr_call ("push_const_src", Type.VOID, string_arg));
      }
      il.append (invoke);
    }

    // Augment constructors with code to associate the object-id with
    // its allocation point.  This is done to the constructor and not to
    // the NEW opcode, because the reference returned by NEW can only be
    // passed to a constructor.  Unfortunately, there are some constructor
    // calls that are not associated with a new and thus shouldn't have this
    // code added.  For example, calls to super class constructor and calls
    // from one constructor to another.  We attempt to find these cases
    // by looking at the stack immediately after the constructor is finished.
    // If the constructor is the one called immediately after a 'new', there
    // should be an unintialized object on the stack (of the correct type)
    // Note that you can't just look for an immediately preceeding 'new' and
    // 'dup' because any arguments to the constructor (including other
    // allocations) will occur after the 'new/dup' combination.
    if ((invoke.getOpcode() == Constants.INVOKESPECIAL)
        && method_name.equals ("<init>")) {
      // System.out.printf ("is (non-super) constructor\n");
      int call_size = arg_types.length + 1;
      if (call_size >= stack.size()) {
        // System.out.printf ("nothing on stack for %s.%s %s init call @%d%n",
        //                   mg.getClassName(),mg.getName(),
        //                   invoke.getClassName(pool), position);
      } else {
        Type top_after_invoke = stack.peek (call_size);
        // System.out.printf ("top of stack for %s.%s %s init call @%d = %s\n",
        //                   mg.getClassName(),mg.getName(),
        //              invoke.getClassName(pool), position, top_after_invoke);
        if (top_after_invoke instanceof UninitializedObjectType) {
          UninitializedObjectType uot = (UninitializedObjectType)top_after_invoke;
          assert uot.getInitialized().equals (invoke.getReferenceType(pool))
            : uot + " != " + invoke.getReferenceType(pool);
          il.append (new DUP());
          String descr = String.format ("%s.%s:new-%s@%d", mg.getClassName(),
                                        mg.getName(), invoke.getClassName(pool),
                                        position);
          il.append (ifact.createConstant (descr));
          il.append (dcr_call ("setup_obj_df", Type.VOID,
                               new Type[] {Type.OBJECT, Type.STRING}));
        }
      }
    }

    return (il);
  }

  /**
   * Make a summary call for a constructor.  This is different from
   * other summary calls because the summary call can't duplicate the
   * actions of the original call.  The newly initialized object can
   * only be passed to a constructor (until the constructor has returned).
   * We thus duplicate the parameters to the constructor and then make
   * the summary call.
   *
   * The current implementation only handles constructors with 8 bytes
   * of arguments.  More arguments can be supported in the future by
   * copying the parameters to a local.  This version uses dup which
   * is limited to 4 or 8 bytes.
   */

  public void constructor_summary (InstructionList il, InvokeInstruction invoke,
                                   String replacement_method) {
    throw new Error();
  }

}
