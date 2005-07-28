package daikon.dcomp;

import java.util.*;

import org.apache.bcel.*;
import org.apache.bcel.classfile.*;
import org.apache.bcel.generic.*;

import edu.mit.csail.pag.testfactoring.instrument.BCELUtil;

import daikon.chicory.MethodInfo;
import daikon.chicory.ClassInfo;

/**
 * Instruments a class file to perform Dynamic Comparability.
 */
class DCInstrument {

  private JavaClass orig_class;
  private ClassGen gen;
  private ConstantPoolGen pool;
  private boolean in_jdk;
  private InstructionFactory ifact;
  private ClassLoader loader;

  private Type[] two_objects = new Type[] {Type.OBJECT, Type.OBJECT};

  /**
   * Initialize with the original class and whether or not the class
   * is part of the JDK
   */
  public DCInstrument (JavaClass orig_class, boolean in_jdk,
                       ClassLoader loader) {
    this.orig_class = orig_class;
    this.in_jdk = in_jdk;
    this.loader = loader;
    gen = new ClassGen (orig_class);
    pool = gen.getConstantPool();
    ifact = new InstructionFactory (gen);
    System.out.printf ("DCInstrument %s%n", orig_class.getClassName());
  }

  /**
   * Instruments the original class to perform dynamic comparabilty and
   * returns the new class definition
   */
  public JavaClass instrument() {

    // Create the ClassInfo for this class and its list of methods
    ClassInfo class_info = new ClassInfo (gen.getClassName(), loader);

    // Process each method
    for (Method m : gen.getMethods()) {
      MethodGen mg = new MethodGen (m, gen.getClassName(), pool);
      System.out.printf ("  Processing method %s\n", m);

      // Create a MethodInfo that describes this methods arguments
      // and exit line numbers (information not available via reflection)
      // and add it to the list for this class.
      MethodInfo mi = create_method_info (class_info, mg);
      class_info.method_infos.add (mi);
      DCRuntime.methods.add (mi);

      instrument_method (mg, mi, DCRuntime.methods.size()-1);
      add_enter (mg, mi, DCRuntime.methods.size()-1);
      mg.setMaxLocals();
      mg.setMaxStack();
      gen.replaceMethod (m, mg.getMethod());
    }

    return (gen.getJavaClass().copy());
  }

  /**
   * Instrument the specified method for dynamic comparability
   */
  public void instrument_method (MethodGen mg, MethodInfo mi,
                                 int method_info_index) {

    // Ignore methods with no instructions
    InstructionList il = mg.getInstructionList();
    if (il == null)
      return;

    // Iterator over all of the exit line numbers for this method
    Iterator <Integer> exit_iter = mi.exit_locations.iterator();

    // Loop through each instruction, making substitutions
    for (InstructionHandle ih = il.getStart(); ih != null; ) {
      System.out.printf ("instrumenting instruction %s%n", ih);
      InstructionList new_il = null;
      Instruction inst = ih.getInstruction();

      // Get the translation for this instruction (if any)
      new_il = xform_inst (mg, method_info_index, inst, exit_iter);

      // Remember the next instruction to process
      InstructionHandle next_ih = ih.getNext();

      // If this instruction was modified, replace it with the new
      // instruction list. If this instruction was the target of any
      // jumps or line numbers , replace them with the first
      // instruction in the new list
      if (new_il != null) {
        if (new_il.getLength() == 1)
          ih.setInstruction (new_il.getEnd().getInstruction());
        else { // more than one instruction to replace
          InstructionHandle new_end = new_il.getEnd();
          InstructionHandle new_start = il.insert (ih, new_il);
          il.redirectBranches (ih, new_start);
          if (ih.hasTargeters()) {
            for (InstructionTargeter it : ih.getTargeters()) {
              if (it instanceof LineNumberGen) {
                it.updateTarget (ih, new_start);
              } else if (it instanceof LocalVariableGen) {
                it.updateTarget (ih, new_end);
              } else {
                System.out.printf ("unexpected target %s%n", it);
              }
            }
          }
          try {
            il.delete (ih);
          } catch (Exception e) {
            throw new Error ("Can't delete instruction", e);
          }
        }
      }
      ih = next_ih;
    }
  }

  /**
   * Adds the calls to DCRuntime.enter and DCRuntime.exit to the beginning
   * and end of the method.
   */
  public void add_enter (MethodGen mg, MethodInfo mi, int method_info_index) {

    // Ignore methods with no instructions
    InstructionList il = mg.getInstructionList();
    if (il == null)
      return;

    // Add the call to enter to the beginning of the method.  Move any
    // line number or local variable targeters to point to the new
    // instructions.  Other targeters (branches, exceptions) are left
    // unchanged.
    InstructionList enter_il = call_enter_exit (mg, method_info_index,
                                                "enter", -1);
    InstructionHandle old_start = il.getStart();
    InstructionHandle new_start = il.insert (enter_il);
    for (InstructionTargeter it : old_start.getTargeters()) {
      if ((it instanceof LineNumberGen) || (it instanceof LocalVariableGen))
        it.updateTarget (old_start, new_start);
    }

  }

  /**
   * Pushes the object, method info index,  parameters, and return value
   * on the stack and calls the specified Method (normally
   * enter or exit) in DCRuntime.  The parameters are passed
   * as an array of objects.
   */
   InstructionList call_enter_exit (MethodGen mgen, int method_info_index,
                                    String method_name, int line) {

     InstructionList il = new InstructionList();
     Type[] arg_types = mgen.getArgumentTypes();

     // Push the object.  Null if this is a static method or a constructor
     if (mgen.isStatic() ||
         (method_name.equals ("enter") && BCELUtil.is_constructor (mgen))) {
       il.append (new ACONST_NULL());
     } else { // must be an instance method
       il.append (ifact.createLoad (Type.OBJECT, 0));
     }

     // Determine the offset of the first parameter
     int param_offset = 1;
     if (mgen.isStatic())
       param_offset = 0;

     // Push the MethodInfo index
     il.append (ifact.createConstant (method_info_index));

     // Create an array of objects with elements for each parameter
     il.append (ifact.createConstant (arg_types.length));
     Type object_arr_typ = new ArrayType ("java.lang.Object", 1);
     il.append (ifact.createNewArray (Type.OBJECT, (short) 1));

     // Put each argument into the array
     int param_index = param_offset;
     for (int ii = 0; ii < arg_types.length; ii++) {
       il.append (ifact.createDup (object_arr_typ.getSize()));
       il.append (ifact.createConstant (ii));
       Type at = arg_types[ii];
       if (at instanceof BasicType) {
         il.append (new ACONST_NULL());
         // il.append (create_wrapper (c, at, param_index));
       } else { // must be reference of some sort
         il.append (ifact.createLoad (Type.OBJECT, param_index));
       }
       il.append (ifact.createArrayStore (Type.OBJECT));
       param_index += at.getSize();
     }

     // If this is an exit, push the return value and line number.
     // The return value
     // is stored in the local "return__$trace2_val"  If the return
     // value is a primitive, wrap it in the appropriate runtime wrapper
     if (method_name.equals ("exit")) {
       Type ret_type = mgen.getReturnType();
       if (ret_type == Type.VOID) {
         il.append (new ACONST_NULL());
       } else {
         LocalVariableGen return_local = get_return_local (mgen, ret_type);
         if (ret_type instanceof BasicType) {
           il.append (new ACONST_NULL());
           //il.append (create_wrapper (c, ret_type, return_local.getIndex()));
         } else {
           il.append (ifact.createLoad (Type.OBJECT, return_local.getIndex()));
         }
       }

       //push line number
       il.append (ifact.createConstant (line));
     }

     // Call the specified method
     Type[] method_args = null;
     if (method_name.equals ("exit"))
       method_args = new Type[] {Type.OBJECT, Type.INT, object_arr_typ,
                                 Type.OBJECT, Type.INT};
     else
       method_args = new Type[] {Type.OBJECT, Type.INT, object_arr_typ};
     il.append (ifact.createInvoke (DCRuntime.class.getName(), method_name,
                             Type.VOID, method_args, Constants.INVOKESTATIC));


     return (il);
   }


  /**
   * Returns a list of instructions that replaces the specified instruction.
   * Returns null if the instruction should not be replaced.
   *
   *    @param mg Method being instrumented
   *    @param method_info_index Index into the list of MethodInfos of this
   *           method
   *    @param inst Instruction to translate
   *    @param exit_iter Iterator over the exit locations of this method.
   *           It should point to the next exit location
   */
  InstructionList xform_inst (MethodGen mg, int method_info_index,
                              Instruction inst, Iterator<Integer> exit_iter) {

    switch (inst.getOpcode()) {

    // Replace the object comparison instructions with a call to
    // DCRuntime.object_eq or DCRuntime.object_ne.  Those methods
    // return a boolean which is used in a ifeq/ifne instruction
    case Constants.IF_ACMPEQ:
      return (object_comparison ((BranchInstruction) inst, "object_eq",
                                 Constants.IFNE));
    case Constants.IF_ACMPNE:
      return (object_comparison ((BranchInstruction) inst, "object_ne",
                                 Constants.IFNE));

    // Prefix the return with a call to exit that passes the object,
    // method info index, return value, and parameters.
    case Constants.ARETURN:
    case Constants.DRETURN:
    case Constants.FRETURN:
    case Constants.IRETURN:
    case Constants.LRETURN:
    case Constants.RETURN: {
      Type type = mg.getReturnType();
      InstructionList il = new InstructionList();
      if (type != Type.VOID) {
        LocalVariableGen return_loc = get_return_local (mg, type);
        il.append (ifact.createDup (type.getSize()));
        il.append (ifact.createStore (type, return_loc.getIndex()));
      }
      il.append (call_enter_exit (mg, method_info_index, "exit",
                                  exit_iter.next()));
      il.append (inst);
      return (il);
    }

    default:
      return (null);
    }

  }

  /**
   * Create the instructions that replace the object eq or ne branch
   * instruction.  They are replaced by a call to the specified
   * compare_method (which returns a boolean) followed by the specified
   * boolean ifeq or ifne instruction
   */
  InstructionList object_comparison (BranchInstruction branch,
                                     String compare_method, short boolean_if) {

    InstructionList il = new InstructionList();
    il.append (ifact.createInvoke (DCRuntime.class.getName(),
                                   compare_method, Type.BOOLEAN,
                                   two_objects, Constants.INVOKESTATIC));
    il.append (ifact.createBranchInstruction (boolean_if,
                                              branch.getTarget()));
    return (il);
  }

  /**
   * Returns the local variable used to store the return result.  If it
   * is not present, creates it with the specified type.  If the variable
   * is known to already exist, the type can be null
   */
  LocalVariableGen get_return_local (MethodGen mgen, Type return_type) {

    // Find the local used for the return value
    LocalVariableGen return_local = null;
    for (LocalVariableGen lv : mgen.getLocalVariables()) {
      if (lv.getName().equals ("return__$trace2_val")) {
        return_local = lv;
        break;
      }
    }

    // If a type was specified and the variable was found, they must match
    if (return_local == null)
      assert (return_type != null) : " return__$trace2_val doesn't exist";
    else
      assert (return_type.equals (return_local.getType())) :
        " return_type = " + return_type + "current type = "
        + return_local.getType();

    if (return_local == null) {
      // log ("Adding return local of type %s%n", return_type);
      return_local = mgen.addLocalVariable ("return__$trace2_val", return_type,
                                            null, null);
    }

    return (return_local);
  }

  /**
   * Creates a MethodInfo corresponding to the specified method.  The
   * exit locations are filled in, but the reflection information is
   * not generated
   */
  private MethodInfo create_method_info (ClassInfo class_info, MethodGen mgen){

    // Get the argument names for this method
    String[] arg_names = mgen.getArgumentNames();
    LocalVariableGen[] lvs = mgen.getLocalVariables();
    int param_offset = 1;
    if (mgen.isStatic())
      param_offset = 0;
    if (lvs != null) {
      for (int ii = 0; ii < arg_names.length; ii++) {
        if ((ii + param_offset) < lvs.length)
          arg_names[ii] = lvs[ii + param_offset].getName();
      }
    }

    boolean shouldInclude = false;

    shouldInclude = true;

    // Get the argument types for this method
    Type[] arg_types = mgen.getArgumentTypes();
    String[] arg_type_strings = new String[arg_types.length];
    for (int ii = 0; ii < arg_types.length; ii++) {
      Type t = arg_types[ii];
      if (t instanceof ObjectType)
        arg_type_strings[ii] = ((ObjectType) t).getClassName();
      else
        arg_type_strings[ii] = t.getSignature().replace('/', '.');
    }

    // Loop through each instruction and find the line number for each
    // return opcode
    List<Integer> exit_locs = new ArrayList<Integer>();

    // Tells whether each exit loc in the method is included or not
    // (based on filters)
    List<Boolean> isIncluded = new ArrayList<Boolean>();

    // log ("Looking for exit points in %s%n", mgen.getName());
    InstructionList il = mgen.getInstructionList();
    int line_number = 0;
    int last_line_number = 0;
    boolean foundLine;

    for (InstructionHandle ih = il.getStart(); ih != null; ih = ih.getNext()) {
      foundLine = false;

      if (ih.hasTargeters()) {
        for (InstructionTargeter it : ih.getTargeters()) {
          if (it instanceof LineNumberGen) {
            LineNumberGen lng = (LineNumberGen) it;
            // log ("  line number at %s: %d%n", ih, lng.getSourceLine());
            // System.out.printf("  line number at %s: %d%n", ih,
            // lng.getSourceLine());
            line_number = lng.getSourceLine();
            foundLine = true;
          }
        }
      }

      switch (ih.getInstruction().getOpcode()) {

      case Constants.ARETURN :
      case Constants.DRETURN :
      case Constants.FRETURN :
      case Constants.IRETURN :
      case Constants.LRETURN :
      case Constants.RETURN :
        // log ("Exit at line %d%n", line_number);
        //only do incremental lines if we don't have the line generator
        if (line_number == last_line_number && foundLine == false) {
          line_number++;
        }
        last_line_number = line_number;

        shouldInclude = true;
        exit_locs.add(new Integer(line_number));
        isIncluded.add(true);
        break;

      default :
        break;
      }
    }

    if (shouldInclude)
      return new MethodInfo(class_info, mgen.getName(), arg_names,
                            arg_type_strings, exit_locs, isIncluded);
    else
      return null;
  }
}
