package daikon.dcomp;

import java.util.*;
import java.util.regex.*;

import org.apache.bcel.*;
import org.apache.bcel.classfile.*;
import org.apache.bcel.generic.*;

import utilMDE.BCELUtil;

import daikon.chicory.MethodInfo;
import daikon.chicory.ClassInfo;
import daikon.chicory.DaikonWriter;

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

  /** Local that stores the tag frame for the current method **/
  private LocalVariableGen tag_frame_local;

  // Argument descriptors
  private Type[] two_objects = new Type[] {Type.OBJECT, Type.OBJECT};
  private Type[] two_ints = new Type[] {Type.INT, Type.INT};
  private Type[] object_int = new Type[] {Type.OBJECT, Type.INT};
  private Type[] string_arg = new Type[] {Type.STRING};
  private Type[] integer_arg = new Type[] {Type.INT};

  // Type descriptors
  private Type object_arr = new ArrayType (Type.OBJECT, 1);
  private Type int_arr = new ArrayType (Type.INT, 1);
  private ObjectType throwable = new ObjectType ("java.lang.Throwable");

  /**
   * Don't instrument toString functions.  Useful in debugging since
   * we call toString on objects from our code (which then triggers
   * (recursive) instrumentation)
   */
  private static boolean ignore_toString = true;

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

      // Skip methods that should not be instrumented
      if (!should_instrument (methodEntryName (gen.getClassName(), m))) {
        System.out.printf ("Skipping method %s%n", m);
        continue;
      }

      MethodGen mg = new MethodGen (m, gen.getClassName(), pool);
      System.out.printf ("  Processing method %s\n", m);

      // Create a MethodInfo that describes this methods arguments
      // and exit line numbers (information not available via reflection)
      // and add it to the list for this class.
      MethodInfo mi = create_method_info (class_info, mg);
      class_info.method_infos.add (mi);
      DCRuntime.methods.add (mi);

      // Create the local to store the tag frame for this method
      tag_frame_local = create_tag_frame_local (mg);

      instrument_method (mg, mi, DCRuntime.methods.size()-1);
      add_enter (mg, mi, DCRuntime.methods.size()-1);
      handle_exceptions (mg);
      mg.setMaxLocals();
      mg.setMaxStack();
      gen.replaceMethod (m, mg.getMethod());
    }

    // If one or more methods were instrumented, return the new class
    // Otherwise, return null since nothing has changed.
    if (class_info.method_infos.size() > 0) {
      track_class_init();
      return (gen.getJavaClass().copy());
    }
    else
      return (null);
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
   * Adds a try/catch block around the entire method.  If an exception
   * occurs, the tag stack is cleaned up and the exception is rethrown.
   */
  public void handle_exceptions (MethodGen mgen) {

    InstructionList il = new InstructionList();
    il.append (ifact.createInvoke (DCRuntime.class.getName(),
                                       "exception_exit", Type.VOID,
                                       Type.NO_ARGS, Constants.INVOKESTATIC));
    il.append (new ATHROW());

    InstructionList cur_il = mgen.getInstructionList();
    InstructionHandle start = cur_il.getStart();
    InstructionHandle end = cur_il.getEnd();
    InstructionHandle exc = cur_il.append (il);

    mgen.addExceptionHandler (start, end, exc, throwable);
  }

  /**
   * Adds the calls to DCRuntime.create_tag_frame and DCRuntime.enter to the
   * beginning of the method.
   */
  public void add_enter (MethodGen mg, MethodInfo mi, int method_info_index) {

    // Ignore methods with no instructions
    InstructionList il = mg.getInstructionList();
    if (il == null)
      return;

    // Create the tag frame for this method
    InstructionList tf_il = create_tag_frame (mg, tag_frame_local);

    // Create the call that processes daikon varaibles upon enter
    InstructionList enter_il = call_enter_exit (mg, method_info_index,
                                                "enter", -1);

    // Add the new code to the beginning of the method.  Move any
    // line number or local variable targeters to point to the new
    // instructions.  Other targeters (branches, exceptions) are left
    // unchanged.
    enter_il.insert (tf_il);
    InstructionHandle old_start = il.getStart();
    InstructionHandle new_start = il.insert (enter_il);
    for (InstructionTargeter it : old_start.getTargeters()) {
      if ((it instanceof LineNumberGen) || (it instanceof LocalVariableGen))
        it.updateTarget (old_start, new_start);
    }

  }

  /**
   * Creates the local used to store the tag frame and returns it
   */
  LocalVariableGen create_tag_frame_local (MethodGen mgen) {

    return mgen.addLocalVariable ("dcomp_tag_frame$5a", object_arr, null,
                                  null);
  }

  /**
   * Creates code to create the tag frame for this method and store it
   * in tag_frame_local
   */
  InstructionList create_tag_frame (MethodGen mgen,
                                    LocalVariableGen tag_frame_local) {

    Type arg_types[] = mgen.getArgumentTypes();
    LocalVariableGen[] locals = mgen.getLocalVariables();

    // Determine the offset of the first argument in the frame
    int offset = 1;
    if (mgen.isStatic())
      offset = 0;

    // Encode the primitive parameter information in a string
    int frame_size = arg_types.length + locals.length;
    String params = "" + Character.forDigit (frame_size, Character.MAX_RADIX);
    for (int ii = arg_types.length-1; ii >= 0; ii--) {
      if (arg_types[ii] instanceof BasicType) {
        params += Character.forDigit (offset + ii, Character.MAX_RADIX);
      }
    }

    // Create code to create/init the tag frame and store in tag_frame_local
    InstructionList il = new InstructionList();
    il.append (ifact.createConstant (params));
    il.append (ifact.createInvoke (DCRuntime.class.getName(),
                                   "create_tag_frame", object_arr, string_arg,
                                   Constants.INVOKESTATIC));
    il.append (ifact.createStore (object_arr, tag_frame_local.getIndex()));
    System.out.printf ("Store Tag frame local at index %d%n",
                       tag_frame_local.getIndex());

    return (il);
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

     // Push the tag frame
    il.append (ifact.createLoad (tag_frame_local.getType(),
                                 tag_frame_local.getIndex()));

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
     il.append (ifact.createNewArray (Type.OBJECT, (short) 1));

     // Put each argument into the array
     int param_index = param_offset;
     for (int ii = 0; ii < arg_types.length; ii++) {
       il.append (ifact.createDup (object_arr.getSize()));
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
       method_args = new Type[] {object_arr, Type.OBJECT, Type.INT,
                                 object_arr, Type.OBJECT, Type.INT};
     else
       method_args = new Type[] {object_arr, Type.OBJECT, Type.INT,
                                 object_arr};
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

    case Constants.GETFIELD: {
      return load_store_field ((GETFIELD) inst, "push_field_tag");
    }

    case Constants.PUTFIELD: {
      return load_store_field ((PUTFIELD) inst, "pop_field_tag");
    }

    case Constants.GETSTATIC: {
      return load_store_static ((GETSTATIC) inst, "push_static_tag");
    }

    case Constants.PUTSTATIC: {
      return load_store_static ((PUTSTATIC) inst, "pop_static_tag");
    }

    case Constants.ILOAD:
    case Constants.ILOAD_0:
    case Constants.ILOAD_1:
    case Constants.ILOAD_2:
    case Constants.ILOAD_3: {
      return load_store_local ((ILOAD)inst, tag_frame_local, "push_local_tag");
    }

    case Constants.ISTORE:
    case Constants.ISTORE_0:
    case Constants.ISTORE_1:
    case Constants.ISTORE_2:
    case Constants.ISTORE_3: {
      return load_store_local ((ISTORE)inst, tag_frame_local, "pop_local_tag");
    }

    case Constants.BIPUSH:
    case Constants.ICONST_0:
    case Constants.ICONST_1:
    case Constants.ICONST_2:
    case Constants.ICONST_3:
    case Constants.ICONST_4:
    case Constants.ICONST_5: {
      InstructionList il = new InstructionList();
      il.append(ifact.createInvoke (DCRuntime.class.getName(), "push_const",
                            Type.VOID, Type.NO_ARGS, Constants.INVOKESTATIC));
      il.append (inst);
      return (il);
    }

    // Primitive Binary operators.  Each is augmented with a call to
    // DCRuntime.binary_tag_op that merges the tags and updates the tag
    // Stack.
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
    case Constants.IXOR: {
      InstructionList il = new InstructionList();
      il.append(ifact.createInvoke (DCRuntime.class.getName(), "binary_tag_op",
                            Type.VOID, Type.NO_ARGS, Constants.INVOKESTATIC));
      il.append (inst);
      return (il);
    }

    // Discard the tag for the integer argument ANEWARRAY
    case Constants.ANEWARRAY:
    case Constants.NEWARRAY: {
      return discard_tag_code (inst, 1);
    }

    // Discard the tags for each dimension to MULTIANEWARRAY
    case Constants.MULTIANEWARRAY: {
      return discard_tag_code (inst, ((MULTIANEWARRAY)inst).getDimensions());
    }

    // Discard the tag for the index argument to aastore and aaload
    case Constants.AASTORE:
    case Constants.AALOAD: {
      return discard_tag_code (inst, 1);
    }

    // Replace iastore opcode with a calld to DCRuntime.iastore.  The iastore
    // method stores the tag at the top of stack into the tag storage for
    // the index into the array.  It also discards the tag for the index from
    // the tag stack and performs the iastore instruction
    case Constants.IASTORE:
      return new InstructionList (dcr_call ("iastore", Type.VOID,
                               new Type[] {int_arr, Type.INT, Type.INT}));


    // Replace iasload opcode with a calld to DCRuntime.iaload.  The iaload
    // method pushes the tag for the specified index in the array onto the
    // tag stackthe tag at the top of stack into the tag storage for
    // the index into the array.  It also discards the tag for the index from
    // the tag stack and performs the iaload instruction.
    case Constants.IALOAD:
      return new InstructionList (dcr_call ("iaload", Type.VOID,
                               new Type[] {int_arr, Type.INT}));


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
      il.append (ifact.createInvoke (DCRuntime.class.getName(), "normal_exit",
                            Type.VOID, Type.NO_ARGS, Constants.INVOKESTATIC));
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
   * Handles load and store field instructions.  The instructions must
   * be augmented to either push (load) or pop (store) the tag on the
   * tag stack.  This is accomplished by calling the specified method
   * in DCRuntime and passing that method the object containing the
   * the field and the offset of that field within the object
   */
  InstructionList load_store_field (FieldInstruction f, String method) {

    Type field_type = f.getFieldType (pool);
    if (field_type instanceof ReferenceType)
      return (null);
    ObjectType obj_type = (ObjectType) f.getReferenceType (pool);
    InstructionList il = new InstructionList();

    if (f instanceof GETFIELD) {
      il.append (ifact.createDup (obj_type.getSize()));
    } else {
      il.append (new SWAP());
      il.append (ifact.createDup (obj_type.getSize()));
    }

    int field_num = get_field_num (f.getFieldName(pool), obj_type);
    il.append (ifact.createConstant (field_num));
    il.append (ifact.createInvoke (DCRuntime.class.getName(), method,
                                  Type.VOID, object_int,
                                  Constants.INVOKESTATIC));
    if (f instanceof PUTFIELD)
      il.append (new SWAP());
    il.append (f);
    return (il);
  }

  /**
   * Handles load and store static instructions.  The instructions must
   * be augmented to either push (load) or pop (store) the tag on the
   * tag stack.  This is accomplished by calling the specified method
   * in DCRuntime and passing that method the object containing the
   * the field and the offset of that field within the object
   */
  InstructionList load_store_static (FieldInstruction f, String method) {

    Type field_type = f.getFieldType (pool);
    if (field_type instanceof ReferenceType)
      return (null);
    String name = f.getClassName(pool) + "." + f.getFieldName(pool);
    System.out.printf ("static field name for %s = %s%n", f, name);

    // Get the index of this static in the list of all statics and allocate
    // a tag for it.
    Integer index = DCRuntime.static_map.get (name);
    if (index == null) {
      index = DCRuntime.static_map.size();
      DCRuntime.static_map.put (name, index);
      DCRuntime.static_tags.add (new Object());
    }

    // Create code to call the method passing it the static's index
    InstructionList il = new InstructionList();
    il.append (ifact.createConstant (index));
    il.append (ifact.createInvoke (DCRuntime.class.getName(), method,
                                  Type.VOID, new Type[] {Type.INT},
                                  Constants.INVOKESTATIC));
    il.append (f);
    return (il);
  }

  /**
   * Handles load and store local instructions.  The instructions must
   * be augmented to either push (load) or pop (store) the tag on the
   * tag stack.  This is accomplished by calling the specified method
   * in DCRuntime and passing that method the tag frame and the offset
   * of local/parameter
   */
  InstructionList load_store_local  (LocalVariableInstruction lvi,
                                    LocalVariableGen tag_frame_local,
                                    String method) {

    // Don't need tags for objects
    if (lvi instanceof ALOAD)
      return (null);

    InstructionList il = new InstructionList();

    // Push the tag frame and the index of this local
    il.append (ifact.createLoad (tag_frame_local.getType(),
                                 tag_frame_local.getIndex()));
    System.out.printf ("CreateLoad %s %d%n", tag_frame_local.getType(),
                       tag_frame_local.getIndex());
    il.append (ifact.createConstant (lvi.getIndex()));

    // Call the runtime method to handle loading/storing the local/parameter
    il.append (ifact.createInvoke (DCRuntime.class.getName(), method,
                                  Type.VOID, new Type[] {object_arr, Type.INT},
                                  Constants.INVOKESTATIC));
    il.append (lvi);
    return (il);
  }

  /**
   * Returns the number of the specified field in the primitive fields
   * of obj_type
   */
  int get_field_num (String name, ObjectType obj_type) {

    // If this is the current class, get the information directly
    if (obj_type.getClassName().equals (orig_class.getClassName())) {
      int fcnt = 0;
      for (Field f : orig_class.getFields()) {
        if (f.getName().equals (name))
          return (fcnt);
        if (f.getType() instanceof BasicType)
          fcnt++;
      }
      assert false : "Can't find " + name + " in " + obj_type;
      return (-1);
    }

    // Look up the class using this classes class loader.  This may
    // not be the best way to accomplish this.
    Class obj_class = null;
    try {
      obj_class = Class.forName (obj_type.getClassName(), false, loader);
    } catch (Exception e) {
      throw new Error ("can't find class " + obj_type.getClassName(), e);
    }

    // Loop through all of the fields, counting the number of primitive fields
    int fcnt = 0;
    for (java.lang.reflect.Field f : obj_class.getDeclaredFields()) {
      if (f.getName().equals (name))
        return (fcnt);
      if (f.getType().isPrimitive())
        fcnt++;
    }
    assert false : "Can't find " + name + " in " + obj_class;
    return (-1);
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

  /**
   * Adds a call to DCRuntime.class_init (String classname) to the
   * class initializer for this class.  Creates a class initializer if
   * one is not currently present
   */
  public void track_class_init () {

    // Look for the class init method.  If not found, create an empty one.
    Method cinit = null;
    for (Method m : gen.getMethods()) {
      if (m.getName().equals ("<clinit>")) {
        cinit = m;
        break;
      }
    }
    if (cinit == null) {
      InstructionList il = new InstructionList();
      il.append (ifact.createReturn (Type.VOID));
      MethodGen cinit_gen = new MethodGen (Constants.ACC_STATIC, Type.VOID,
        Type.NO_ARGS, new String[0], "<clinit>", gen.getClassName(), il, pool);
      cinit_gen.setMaxLocals();
      cinit_gen.setMaxStack();
      cinit_gen.update();
      cinit = cinit_gen.getMethod();
      gen.addMethod (cinit);

    }

    // Add a call to DCRuntime.class_init to the beginning of the method
    InstructionList il = new InstructionList();
    il.append (ifact.createConstant (gen.getClassName()));
    il.append (ifact.createInvoke (DCRuntime.class.getName(), "class_init",
                              Type.VOID, string_arg, Constants.INVOKESTATIC));

    MethodGen cinit_gen = new MethodGen (cinit, gen.getClassName(), pool);
    InstructionList cur = cinit_gen.getInstructionList();
    InstructionHandle old_start = cur.getStart();
    InstructionHandle new_start = cur.insert (il);
    if (old_start.hasTargeters()) {
      for (InstructionTargeter it : old_start.getTargeters()) {
        if ((it instanceof LineNumberGen) || (it instanceof LocalVariableGen))
          it.updateTarget (old_start, new_start);
      }
    }
    cinit_gen.setMaxLocals();
    cinit_gen.setMaxStack();
    gen.replaceMethod (cinit, cinit_gen.getMethod());
  }

  /**
   * Returns whether or not this ppt should be included.  A ppt is included
   * if it matches ones of the select patterns and doesn't match any of the
   * omit patterns.
   */
  public boolean should_instrument (String pptname) {

    // System.out.printf ("Considering ppt %s%n", pptname);

    // Don't instrument toString methods because we call them in
    // our debug statements.
    if (ignore_toString && pptname.contains ("toString"))
      return (false);

    // If any of the omit patterns match, exclude the ppt
    for (Pattern p : Premain.ppt_omit_pattern) {
      if (p.matcher (pptname).find())
        return (false);
    }

    // If there are no select patterns, everything matches
    if (Premain.ppt_select_pattern.size() == 0)
      return (true);

    // One of the select patterns must match to include
    for (Pattern p : Premain.ppt_select_pattern) {
      if (p.matcher (pptname).find())
        return (true);
    }
    return (false);
  }

  /**
   * Constructs a ppt entry name from a Method
   */
  public static String methodEntryName (String fullClassName, Method m) {

    // System.out.printf ("classname = %s, method = %s, short_name = %s%n",
    //                   fullClassName, m, m.getName());

    // Get an array of the type names
    Type[] arg_types = m.getArgumentTypes();
    String[] type_names = new String[arg_types.length];
    for (int ii = 0; ii < arg_types.length; ii++)
        type_names[ii] = arg_types[ii].toString();

    return fullClassName + "." +
      DaikonWriter.methodEntryName (fullClassName, type_names,
                                    m.toString(), m.getName());
  }

  /** Convenience function to call a static method in DCRuntime **/
  private InvokeInstruction dcr_call (String method_name, Type ret_type,
                                             Type[] arg_types) {

    return ifact.createInvoke (DCRuntime.class.getName(), method_name,
                               ret_type, arg_types, Constants.INVOKESTATIC);
  }

  /**
   * Create the code to call discard_tag(tag_count) and append inst to the
   * end of that code
   */
  private InstructionList discard_tag_code (Instruction inst, int tag_count) {
    InstructionList il = new InstructionList();
    il.append (ifact.createConstant (tag_count));
    il.append (dcr_call ("discard_tag", Type.VOID, integer_arg));
    il.append (inst);
    return (il);
  }
}
