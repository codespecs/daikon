package daikon.chicory;

import java.lang.instrument.*;
import java.lang.reflect.Modifier;
import java.security.*;
import java.io.*;
import java.util.*;
import java.util.regex.*;

import static java.lang.System.out;

// Sun included version of BCEL
// import com.sun.org.apache.bcel.internal.*;
// import com.sun.org.apache.bcel.internal.classfile.*;
// import com.sun.org.apache.bcel.internal.generic.InstructionFactory;
// import com.sun.org.apache.bcel.internal.generic.*;

import org.apache.bcel.*;
import org.apache.bcel.classfile.*;
import org.apache.bcel.generic.InstructionFactory;
import org.apache.bcel.generic.*;
import org.apache.bcel.verifier.VerificationResult;

import sun.nio.cs.ext.ISCII91;
// uncomment this and uses of it below, to get bcel verify info
// import edu.mit.csail.pag.testfactoring.verify.StackVer;

import daikon.util.SimpleLog;

import daikon.Chicory;

/**
 * The Instrument class is responsible for modifying another class'
 * bytecode.  Specifically, its main task is to add "hooks" into the
 * other class at method entries and exits for instrumentation
 * purposes.
 */
@SuppressWarnings("nullness")
public class Instrument implements ClassFileTransformer {

  boolean debug = false;
  boolean log_on = false;

  /** current Constant Pool * */
  static ConstantPoolGen pgen = null;

  /** the index of this method into Runtime.methods **/
  int cur_method_info_index = 0;

  /** the location of the runtime support class **/
  private static final String runtime_classname = "daikon.chicory.Runtime";

  /** Debug information about which classes are transformed and why **/
  public static SimpleLog debug_transform
    = new SimpleLog (Chicory.debug_transform);

  public
  Instrument () {
      debug = Chicory.debug;
      log_on = Chicory.debug_transform;
  }


  private void
  log (String format, /*@Nullable*/ Object... args) {
    if (!log_on)
      return;
    System.out.printf (format, args);
  }


  // uses Runtime.ppt_omit_pattern and Runtime.ppt_select_pattern
  // to see if the given ppt should be "filtered out"
  private boolean
  shouldFilter (String className, String methodName, String pptName) {

    // if (debug)
    //    out.format ("shouldFilter: %s, %s, %s%n", className, methodName, pptName);

    // Don't instrument class if it matches an excluded regular expression
    for (Pattern pattern : Runtime.ppt_omit_pattern)
      {

        Matcher mPpt = pattern.matcher(pptName);
        Matcher mClass = pattern.matcher(className);
        Matcher mMethod = pattern.matcher(methodName);

        if (mPpt.find() || mClass.find() || mMethod.find())
          {
            log("not instrumenting %s, it matches regex %s%n", pptName, pattern);

            // System.out.println("filtering 1 true on --- " + pptName);

            // omit takes priority over include
            return true;
          }
      }

    // If any include regular expressions are specified, only instrument
    // classes that match them
    if (Runtime.ppt_select_pattern.size() > 0)
      {
        for (Pattern pattern: Runtime.ppt_select_pattern)
          {

            Matcher mPpt = pattern.matcher(pptName);
            Matcher mClass = pattern.matcher(className);
            Matcher mMethod = pattern.matcher(methodName);

            // System.out.println("--->" + regex);

            if (mPpt.find() || mClass.find() || mMethod.find())
              {
                log("instrumenting %s, it matches regex %s%n", pptName, pattern);

                // System.out.println("filtering 2 false on --- " + pptName);
                return false; // don't filter out
              }
          }
      }

    // if we're here, this ppt not explicitly included or excluded
    // so keep unless there were items in the "include only" list
    boolean ret = (Runtime.ppt_select_pattern.size() > 0);

    // System.out.println("filtering 3: " + ret + " on --- " + pptName);
    return ret;
  }


  /**
   * Given another class, return a transformed version of the class which
   * contains "hooks" at method entries and exits.
   * Because Chicory is invoked as a javaagent, the transform method is
   * called by the Java runtime each time a new class is loaded.
   */
  public byte /*@Nullable*/ []
  transform (ClassLoader loader, String className,
             Class<?> classBeingRedefined, ProtectionDomain protectionDomain,
             byte[] classfileBuffer) throws IllegalClassFormatException {

    // debug = className.equals ("DataStructures/StackAr");
    // debug = className.equals ("chicory/Test");
    // debug = className.equals ("DataStructures/BinarySearchTree");

    String fullClassName = className.replace("/", ".");
    // String fullClassName = className;

    debug_transform.log ("In chicory.Instrument.transform(): class = %s%n",
                         className);

    // Don't instrument boot classes.  They are uninteresting and will
    // not be able to access daikon.chicory.Runtime (because it is not
    // on the boot classpath).  Previously this code skipped classes
    // that started with java, com, javax, or sun, but this is not
    // correct in many cases.  Most boot classes have the null loader,
    // but some generated classes (such as those in sun.reflect) will
    // have a non-null loader.  Some of these have a null parent loader,
    // but some do not.  The check for the sun.reflect package is a hack
    // to catch all of these.  A more consistent mechanism to determine
    // boot classes would be preferrable.
    if (Chicory.boot_classes != null) {
      Matcher matcher = Chicory.boot_classes.matcher (fullClassName);
      if (matcher.find()) {
        debug_transform.log ("ignoring sys class %s, "
                             + "matches boot_classes regex", fullClassName);
        return (null);
      }
    } else if (loader == null) {
      debug_transform.log ("ignoring system class %s, class loader == null",
                           fullClassName);
      return (null);
    } else if (loader.getParent() == null) {
      debug_transform.log ("ignoring system class %s, parent loader == null\n",
                           fullClassName);
      return (null);
    } else if (fullClassName.startsWith ("sun.reflect")) {
      debug_transform.log ("ignoring system class %s, in sun.reflect package",
                           fullClassName);
      return (null);
    } else if (fullClassName.startsWith ("com.sun")) {
      System.out.printf ("Class from com.sun package %s with nonnull loaders\n",
                         fullClassName);
    }

    // Don't intrument our code
    if (is_chicory (className)) {
      debug_transform.log ("Not considering chicory class %s%n",fullClassName);
      return (null);
    }

      debug_transform.log ("transforming class %s, loader %s - %s%n", className,
                           loader, loader.getParent());

    // Parse the bytes of the classfile, die on any errors
    JavaClass c = null;
    ClassParser parser = new ClassParser(new ByteArrayInputStream(classfileBuffer),
                                         className);
    try
      {
        c = parser.parse();
      }
    catch (Exception e)
      {
        throw new RuntimeException("Unexpected error", e);
      }

    try {
      // Get the class information
      ClassGen cg = new ClassGen (c);

      // Convert reach non-void method to save its result in a local
      // before returning
      ClassInfo c_info = instrument_all_methods (cg, fullClassName, loader);

      // get constant static fields!
      Field[] fields = cg.getFields();
      for (Field field: fields) {
          if (field.isFinal() && field.isStatic() &&
             (field.getType() instanceof BasicType)) {
              ConstantValue value = field.getConstantValue();
              String valString;

              if (value == null) {
                  // System.out.println("WARNING FROM " + field.getName());
                  // valString = "WARNING!!!";
                  valString = null;
              } else {
                  valString = value.toString();
                  // System.out.println("GOOD FROM " + field.getName() +
                  //                    " --- " + valString);
              }

              if (valString != null)
                c_info.staticMap.put(field.getName(), valString);
          }
      }

      if (Chicory.checkStaticInit)
        {
          // check for static initializer
          boolean hasInit = false;
          for (Method meth : cg.getMethods())
            {
              if (meth.getName().equals("<clinit>"))
                hasInit = true;
            }

          // if not found, add our own!
          if (!hasInit)
            cg.addMethod(createClinit(cg, fullClassName));
        }


      JavaClass njc = cg.getJavaClass();
      if (debug)
        njc.dump ("/tmp/ret/" + njc.getClassName() + ".class");

      if (c_info.shouldInclude) {
        // System.out.println ("Instrumented class " + className);
        // String filename = "/homes/gws/mernst/tmp/" + className +
        //                   "Transformed.class";
        // System.out.println ("About to dump class " + className +
        //                     " to " + filename);
        // cg.getJavaClass().dump(filename);
        return (cg.getJavaClass().getBytes());
      } else {
        debug_transform.log ("not including class %s (filtered out)",
                             className);
        // No changes to the bytecodes
        return null;
      }

    } catch (Throwable e) {
      out.format ("Unexpected error %s in transform", e);
      e.printStackTrace();
      // No changes to the bytecodes
      return (null);
    }
  }


  // used to add a "hook" into the <clinit> static initializer
  private Method
  addInvokeToClinit (ClassGen cg, MethodGen mg, String fullClassName) {

    call_initNotify(cg, cg.getConstantPool(), fullClassName,
                    new MethodContext(cg, mg).ifact);

    InstructionList newList = mg.getInstructionList();
    mg.update();

    InstructionList il = mg.getInstructionList();
    MethodContext context = new MethodContext(cg,mg);

    for (InstructionHandle ih = il.getStart(); ih != null; ) {
      InstructionList new_il = null;
      Instruction inst = ih.getInstruction();

      // Get the translation for this instruction (if any)
      new_il = xform_clinit (cg, cg.getConstantPool(), fullClassName,
                             inst, context);

      // Remember the next instruction to process
      InstructionHandle next_ih = ih.getNext();

      // If this instruction was modified, replace it with the new
      // instruction list. If this instruction was the target of any
      // jumps, replace it with the first instruction in the new list
      if (new_il != null) {
        if (true) {
          try
            {
              new_il.delete (new_il.getEnd());
            }
          catch (TargetLostException e)
            {
              throw new Error ("unexpected lost target exception", e);
            }
          InstructionHandle new_start = il.insert (ih, new_il);
          // out.format ("old start = %s, new_start = %s%n", ih, new_start);
          il.redirectBranches (ih, new_start);

          // Fix up line numbers to point at the new code
          if (ih.hasTargeters()) {
            for (InstructionTargeter it : ih.getTargeters()) {
              if (it instanceof LineNumberGen) {
                it.updateTarget (ih, new_start);
              }
            }
          }

          ih = next_ih;
          continue;
        }

        if (debug)
          out.format ("Replacing %s by %s%n", ih, new_il);

        il.append (ih, new_il);
        InstructionTargeter[] targeters = ih.getTargeters();
        if (targeters != null) {
          // out.format ("targeters length = %d%n", targeters.length);
          for (int j = 0; j < targeters.length; j++)
            targeters[j].updateTarget (ih, ih.getNext());
        }
        try {
          il.delete (ih);
        } catch (TargetLostException e) {
          throw new Error ("unexpected lost target exception", e);
        }
      }
      // Go on to the next instruction in the list
      ih = next_ih;
    }

    mg.setInstructionList (newList);

    Attribute a = get_local_variable_type_table_attribute(mg);
    if (a != null)
        mg.removeCodeAttribute (a);

    // Update the max stack and Max Locals
    mg.setMaxLocals();
    mg.setMaxStack();
    mg.update();

    return mg.getMethod();
  }


  // called by addInvokeToClinit to add in a hook at return opcodes
  private /*@Nullable*/ InstructionList
  xform_clinit (ClassGen cg, ConstantPoolGen cp, String fullClassName,
                Instruction inst, MethodContext context) {

    switch (inst.getOpcode()) {

    case Constants.ARETURN:
    case Constants.DRETURN:
    case Constants.FRETURN:
    case Constants.IRETURN:
    case Constants.LRETURN:
    case Constants.RETURN:
      break;

    default:
      return (null);
    }

    InstructionList il = new InstructionList();
    il.append(call_initNotify(cg, cp, fullClassName, context.ifact));
    il.append(inst);
    return (il);
  }


  // create a <clinit> method, if none exists; guarantees we have this hook
  private Method
  createClinit (ClassGen cg, String fullClassName) {
    /*
     * System.out.println(mg.getAccessFlags());
     * System.out.println(mg.getReturnType());
     * System.out.println(mg.getArgumentTypes());
     * System.out.println(mg.getName());
     * System.out.println(mg.getClassName());
     */

    InstructionFactory factory = new InstructionFactory(cg);

    InstructionList il = new InstructionList();
    il.append(call_initNotify(cg, cg.getConstantPool(), fullClassName,
                              factory));
    il.append(InstructionFactory.createReturn(Type.VOID)); // need to
    // return!

    MethodGen newMethGen = new MethodGen(8, Type.VOID, new Type[0],
                                new String[0], "<clinit>", fullClassName,
                                il, cg.getConstantPool());
    newMethGen.update();

    Attribute a = get_local_variable_type_table_attribute(newMethGen);
    if (a != null)
        newMethGen.removeCodeAttribute (a);

    // MethodContext context = new MethodContext (cg, newMethGen);
    // InstructionFactory ifact = context.ifact;

    // il.append (ifact.createConstant (0));
    // newMethGen.setInstructionList(il);
    // newMethGen.update();

    // Update the max stack and Max Locals
    newMethGen.setMaxLocals();
    newMethGen.setMaxStack();
    newMethGen.update();

    return newMethGen.getMethod();
  }


  // created the InstructionList to insert for adding the <clinit> hookd
  private InstructionList
  call_initNotify(ClassGen cg, ConstantPoolGen cp, String fullClassName,
                  InstructionFactory factory) {

    InstructionList invokeList = new InstructionList();

    invokeList.append(new PUSH(cp, fullClassName));
    invokeList.append(factory.createInvoke(runtime_classname, "initNotify",
                               Type.VOID, new Type[] {Type.STRING}, Constants.INVOKESTATIC));

    // System.out.println(fullClassName + " --- " + invokeList.size());
    return invokeList;
  }


  // This really should be part of the abstraction provided by BCEL,
  // similar to LineNumberTable and LocalVariableTable.  However, for 
  // now we'll do it all within Instrument.java.

  // Set by instrument_all_methods
  // Used by instrument_all_methods, find_stack_map, update_stack_map_offset,
  // add_entry_instrumentation
  StackMapTableEntry[] stack_map_table;
  StackMapTableEntry[] empty_stack_map_table = {};
  
 
 /**
   * Instrument all the methods in a class.  For each method, add
   * instrumentation code at the entry and at each return from the method.
   * In additon, changes each return statement to first place the value
   * being returned into a local and then return. This allows us to work
   * around the JDI deficiency of not being able to query return values.
   * @param fullClassName must be packageName.className
   */
  private ClassInfo
  instrument_all_methods (ClassGen cg, String fullClassName, ClassLoader loader) {

    ClassInfo class_info = new ClassInfo (cg.getClassName(), loader);
    List<MethodInfo> method_infos = new ArrayList<MethodInfo>();

    boolean shouldInclude = false;

    try {
      pgen = cg.getConstantPool();

      // Loop through each method in the class
      Method[] methods = cg.getMethods();
      for (int i = 0; i < methods.length; i++) {
        MethodGen mg = new MethodGen (methods[i], cg.getClassName(), pgen);
        MethodContext context = new MethodContext (cg, mg);

        // check for the class init method
        if (mg.getName().equals("<clinit>"))
          {
            if (Chicory.checkStaticInit)
              {
                cg.replaceMethod(methods[i],
                                 addInvokeToClinit(cg, mg, fullClassName));
                cg.update();
              }
            continue;
          }

        // If method is synthetic...
        if ((Constants.ACC_SYNTHETIC & mg.getAccessFlags()) > 0)
          {
            continue;
          }

        // Get the instruction list and skip methods with no instructions
        InstructionList il = mg.getInstructionList();
        if (il == null)
          continue;

        if (debug) {
          out.format ("%nMethod = %s%n", mg);
          out.format ("Original code: %s%n", mg.getMethod().getCode());
          out.format ("ClassInfo: %s%n", class_info);
          out.format ("MethodGen: %s%n", mg);
          dump_code_attributes (mg);
        }

        // Get existing StackMapTable (if present)
        Attribute smta = get_stack_map_table_attribute(mg);
        if (smta != null) {
            stack_map_table = ((StackMapTable)smta).getStackMapTable();
            if (debug) {
                out.format ("Original StackMap: " + smta + "%n");
                out.format ("Attribute tag: " + smta.getTag() + " length: "
                        + smta.getLength() + " nameIndex: " + smta.getNameIndex() + "%n");
            }
            mg.removeCodeAttribute(smta);
        } else {
            stack_map_table = empty_stack_map_table;
        }    

        // Create a MethodInfo that describes this methods arguments
        // and exit line numbers (information not available via reflection)
        // and add it to the list for this class.
        MethodInfo mi = (create_method_info(class_info, mg));

        if (mi == null)  // method filtered out!
          continue;

        if (!shouldInclude && ChicoryPremain.debug) {
          out.format ("Class %s included [%s]%n", cg.getClassName(), mi);
        }    
        shouldInclude = true; // at least one method not filtered out

        method_infos.add(mi);

        cur_method_info_index = Runtime.methods.size();
        Runtime.methods.add(mi);

        // Add nonce local to matchup enter/exits
        String entry_ppt_name = DaikonWriter.methodEntryName(fullClassName,
                                    getArgTypes(mg), mg.toString(), mg.getName());
        add_entry_instrumentation(il, context, !shouldFilter(fullClassName,
                                      mg.getName(), entry_ppt_name));

        Iterator<Boolean> shouldIncIter = mi.is_included.iterator();
        Iterator<Integer> exitIter = mi.exit_locations.iterator();

        // Loop through each instruction looking for the return
        for (InstructionHandle ih = il.getStart(); ih != null; ) {
          InstructionList new_il = null;
          Instruction inst = ih.getInstruction();

          // If this is a return instruction, insert method exit instrumentation
          new_il = add_return_instrumentation(fullClassName, inst, context,
                                               shouldIncIter, exitIter);

          // Remember the next instruction to process
          InstructionHandle next_ih = ih.getNext();

          // If this instruction was modified, replace it with the new
          // instruction list. If this instruction was the target of any
          // jumps, replace it with the first instruction in the new list
          if (new_il != null) {
              if (next_ih != null) {
                  // This return is not at the end of the method. That
                  // means the instruction after the RETURN is a branch
                  // target and that means it has a StackMap entry. (Java7)
                  // We need to adjust its offset for our inserted code.

                  if (stack_map_table.length > 0) {
                      int len = (new_il.getByteCode()).length;
                      il.setPositions();
                      int current_offset = next_ih.getPosition();
    
                      if (debug) {
                          out.format ("Current offset: %d Inserted length: %d%n",
                                      current_offset, len);
                      }    

                      // find stack map for current location
                      StackMapTableEntry stack_map = find_stack_map(current_offset);
                      modify_stack_map_offset(stack_map, len);
                  }    
              }    

              InstructionHandle new_start = il.insert(ih, new_il);
              // out.format ("old start = %s, new_start = %s%n", ih, new_start);
              il.redirectBranches(ih, new_start);

              // Fix up line numbers to point at the new code
              if (ih.hasTargeters()) {
                for (InstructionTargeter it : ih.getTargeters()) {
                  if (it instanceof LineNumberGen) {
                    it.updateTarget(ih, new_start);
                  }
                }
              }
          }
          // Go on to the next instruction in the list
          ih = next_ih;
        }


        // Build new StackMapTable attribute
        int map_table_size = 2;  // space for the number_of_entries
        for (int j = 0; j < stack_map_table.length; j++) {
            map_table_size += stack_map_table[j].getEntryByteSize();
        }
        StackMapTable map_table = new StackMapTable(pgen.addUtf8("StackMapTable"),
                                  map_table_size, stack_map_table, pgen.getConstantPool());
        mg.addCodeAttribute(map_table);

        // UNDONE: not sure this is necessary anymore?
        // Remove the Local variable type table attribute (if any).
        // Evidently, some changes we make require this to be updated, but
        // without BCEL support, that would be hard to do.  Just delete it
        // for now (since it is optional, and we are unlikely to be used by
        // a debugger)
        Attribute a = get_local_variable_type_table_attribute(mg);
        if (a != null)
            mg.removeCodeAttribute(a);

        // Update the instruction list
        mg.setInstructionList(il);
        mg.update();

        // Update the max stack and Max Locals
        // mg.setMaxLocals();
        mg.setMaxStack();
        mg.update();

        // Update the method in the class
        cg.replaceMethod(methods[i], mg.getMethod());
        if (debug) {
          out.format ("Modified code: %s%n", mg.getMethod().getCode());
          dump_code_attributes(mg);
        }  

        // verify the new method
        // StackVer stackver = new StackVer();
        // VerificationResult vr = stackver.do_stack_ver(mg);
        // log ("vr for method %s = %s%n", mg.getName(), vr);
        // if (vr.getStatus() != VerificationResult.VERIFIED_OK) {
        //  System.out.printf ("Warning BCEL Verify failed for method %s: %s",
        //                     mg.getName(), vr);
        //  System.out.printf ("Code: %n%s%n", mg.getMethod().getCode());
        // System.exit(1);
        // }
      }

      cg.update();
    } catch (Exception e) {
      out.format ("Unexpected exception encountered: " + e);
      e.printStackTrace();
    }

    // Add the class and method information to runtime so it is available
    // as enter/exit ppts are processed.
    class_info.set_method_infos(method_infos);

    if (shouldInclude) {
      debug_transform.log ("Added trace info to class %s%n", class_info);
      synchronized(Runtime.new_classes) {
        Runtime.new_classes.add(class_info);
      }
      synchronized(Runtime.all_classes) {
        Runtime.all_classes.add(class_info);
      }
    } else { // not included
      debug_transform.log ("Trace info not added to class %s%n", class_info);
    }

    class_info.shouldInclude = shouldInclude;
    return class_info;
  }


  // This method exists only to suppress interning warnings
  @SuppressWarnings("interning") // special, unique value
  /*@Pure*/ private static boolean isVoid(Type t) {
    return t == Type.VOID;
  }


  /**
   * Transforms return instructions to first assign the result to a local
   * variable (return__$trace2_val) and then do the return.  Also, calls
   * Runtime.exit() immediately before the return.
   */
  private /*@Nullable*/ InstructionList
  add_return_instrumentation (String fullClassName, Instruction inst,
      MethodContext c, Iterator<Boolean> shouldIncIter, Iterator<Integer> exitIter) {

    switch (inst.getOpcode()) {

    case Constants.ARETURN:
    case Constants.DRETURN:
    case Constants.FRETURN:
    case Constants.IRETURN:
    case Constants.LRETURN:
    case Constants.RETURN:
      break;

    default:
      return (null);
    }

    if (!shouldIncIter.hasNext())
      throw new RuntimeException("Not enough entries in shouldIncIter");

    boolean shouldInclude = shouldIncIter.next();

    if (!shouldInclude)
      return null;

    Type type = c.mgen.getReturnType();
    InstructionList il = new InstructionList();
    if (! isVoid(type)) {
      LocalVariableGen return_loc = get_return_local(c.mgen, type);
      il.append(InstructionFactory.createDup(type.getSize()));
      il.append(InstructionFactory.createStore(type, return_loc.getIndex()));
    }

    if (!exitIter.hasNext())
      throw new RuntimeException("Not enough exit locations in the exitIter");

    il.append(call_enter_exit(c, "exit", exitIter.next()));
    return(il);
  }


  /**
   * Returns the local variable used to store the return result.  If it
   * is not present, creates it with the specified type.  If the variable
   * is known to already exist, the type can be null.
   */
  private LocalVariableGen
  get_return_local (MethodGen mgen, /*@Nullable*/ Type return_type) {

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
   * Finds the nonce local variable.  Returns null if not present.
   */
  private /*@Nullable*/ LocalVariableGen
  get_nonce_local(MethodGen mgen) {

    // Find the local used for the nonce value
    for (LocalVariableGen lv : mgen.getLocalVariables()) {
      if (lv.getName().equals ("this_invocation_nonce")) {
        return (lv);
      }
    }

    return (null);
  }


  /**
   * We have inserted an additional single byte into the instruction list;
   * update the StackMaps, if required.
   */
  private void
  update_stack_map_offset (int location, MethodContext c) {

    int running_offset = -1; // no +1 on first entry
    for (int i = 0; i < stack_map_table.length; i++) {
        running_offset = stack_map_table[i].getByteCodeOffsetDelta()
                                            + running_offset + 1;
        
        if (running_offset > location) {
            modify_stack_map_offset(stack_map_table[i], 1);
            // Only update the first StackMap that occurs after the current
            // location as map offsets are relative to previous map.
            return;
        }
    }    
  }    


  /**
   * Find the StackMap entry who's offset matches the input argument
   */
  private StackMapTableEntry
  find_stack_map (int offset) {

    int running_offset = -1; // no +1 on first entry
    for (int i = 0; i < stack_map_table.length; i++) {
      running_offset = stack_map_table[i].getByteCodeOffsetDelta()
                                          + running_offset + 1;

      if (running_offset > offset) {
          throw new RuntimeException("Invalid StackMap offset");
      }

      if (running_offset == offset) {
          return stack_map_table[i];
      }
      // try next map entry
    }    

    // no offset matched
    throw new RuntimeException("Invalid StackMap offset");
  }    


  private void
  modify_stack_map_offset (StackMapTableEntry stack_map, int delta) {

      int frame_type = stack_map.getFrameType();
      int new_delta = stack_map.getByteCodeOffsetDelta() + delta;

      if (new_delta < 0 || new_delta > 32767) {
          throw new RuntimeException("Invalid StackMap offset_delta");
      }

      if (frame_type >= Constants.SAME_FRAME &&
          frame_type <= Constants.SAME_FRAME_MAX) {
          if (new_delta > Constants.SAME_FRAME_MAX) {
              stack_map.setFrameType(Constants.SAME_FRAME_EXTENDED);
          } else {    
              stack_map.setFrameType(new_delta);
          }    
      } else if (frame_type >= Constants.SAME_LOCALS_1_STACK_ITEM_FRAME &&
                 frame_type <= Constants.SAME_LOCALS_1_STACK_ITEM_FRAME_MAX) {
          if (new_delta > Constants.SAME_FRAME_MAX) {
              stack_map.setFrameType(Constants.SAME_LOCALS_1_STACK_ITEM_FRAME_EXTENDED);
          } else {    
              stack_map.setFrameType(Constants.SAME_LOCALS_1_STACK_ITEM_FRAME + new_delta);
          }    
      } else if (frame_type == Constants.SAME_LOCALS_1_STACK_ITEM_FRAME_EXTENDED) {
      } else if (frame_type >= Constants.CHOP_FRAME && 
                 frame_type <= Constants.CHOP_FRAME_MAX) {
      } else if (frame_type == Constants.SAME_FRAME_EXTENDED) {
      } else if (frame_type >= Constants.APPEND_FRAME &&
                 frame_type <= Constants.APPEND_FRAME_MAX) {
      } else if (frame_type == Constants.FULL_FRAME) {        
      } else {
          throw new RuntimeException("Invalid StackMap frame_type");
      }

      stack_map.setByteCodeOffsetDelta(new_delta);
  }


  // Set by create_local_nonce
  // Used by create_local_nonce, xform_local_ref, add_method_startup
  private int nonce_index;


  /**
   * Transforms instructions that reference locals that are 'higher'
   * in the local map that the nonce local.  Need to add one to their
   * operand offset.  This may require changing the instruction as well.
   */
  private void
  xform_local_ref (InstructionHandle ih, InstructionList il, MethodContext c) {

      Instruction inst = ih.getInstruction();

      int operand;
      short opcode = inst.getOpcode();

      switch (opcode) {

      case Constants.RET:
          operand = ((IndexedInstruction)inst).getIndex();
          if (operand >= nonce_index) {
              ((IndexedInstruction)inst).setIndex(operand + 1);
          }    
          break;

      case Constants.IINC:
          operand = ((LocalVariableInstruction)inst).getIndex();
          if (operand >= nonce_index) {
              ((LocalVariableInstruction)inst).setIndex(operand + 1);
          }    
          break;

      case Constants.ILOAD:
      case Constants.ILOAD_0:
      case Constants.ILOAD_1:
      case Constants.ILOAD_2:
      case Constants.ILOAD_3:
      case Constants.LLOAD:
      case Constants.LLOAD_0:
      case Constants.LLOAD_1:
      case Constants.LLOAD_2:
      case Constants.LLOAD_3:
      case Constants.FLOAD:
      case Constants.FLOAD_0:
      case Constants.FLOAD_1:
      case Constants.FLOAD_2:
      case Constants.FLOAD_3:
      case Constants.DLOAD:
      case Constants.DLOAD_0:
      case Constants.DLOAD_1:
      case Constants.DLOAD_2:
      case Constants.DLOAD_3:
      case Constants.ALOAD:
      case Constants.ALOAD_0:
      case Constants.ALOAD_1:
      case Constants.ALOAD_2:
      case Constants.ALOAD_3:
      case Constants.ISTORE:
      case Constants.ISTORE_0:
      case Constants.ISTORE_1:
      case Constants.ISTORE_2:
      case Constants.ISTORE_3:
      case Constants.LSTORE:
      case Constants.LSTORE_0:
      case Constants.LSTORE_1:
      case Constants.LSTORE_2:
      case Constants.LSTORE_3:
      case Constants.FSTORE:
      case Constants.FSTORE_0:
      case Constants.FSTORE_1:
      case Constants.FSTORE_2:
      case Constants.FSTORE_3:
      case Constants.DSTORE:
      case Constants.DSTORE_0:
      case Constants.DSTORE_1:
      case Constants.DSTORE_2:
      case Constants.DSTORE_3:
      case Constants.ASTORE:
      case Constants.ASTORE_0:
      case Constants.ASTORE_1:
      case Constants.ASTORE_2:
      case Constants.ASTORE_3:
          // BCEL handles all the details of which opcode and if index
          // is implicit or explicit; also, and if needs to be WIDE.
          operand = ((LocalVariableInstruction)inst).getIndex();
          if (operand >= nonce_index) {
              ((LocalVariableInstruction)inst).setIndex(operand + 1);
          }    
          // Unfortunately, it doesn't take care of incrementing the
          // offset within StackMapEntrys.
          if (operand == 3) {
              // If operand was 3 (implicit), it will now be 4 (explicit)
              // which makes the instruction one byte longer.  We need to
              // update the instruction list to account for this.
              il.setPositions();
              // Which means we might need to update a StackMap offset.
              update_stack_map_offset(ih.getPosition(), c);
          }    
          break;

      default:
    }
  }


  /**
   * Create the nonce local variable.  This may have the side effect of
   * causing us to rewrite the method byte codes to adjust the offsets
   * of existing local variables - see below for details.
   */
  private LocalVariableGen
  create_local_nonce(InstructionList il, MethodContext c) {

    // BCEL sorts local vars and presents in index order.  Search locals for
    // first var with start != 0. If none, just add nonce at end of table and
    // exit.  Otherwise, insert nonce prior to local we found.  Now we need
    // to make a pass over the byte codes to update the local index values of
    // all the locals we just shifted up one slot.  This may have a 'knock on'
    // effect if we are forced to change an instruction that references
    // implict local #3 to an instruction with an explict reference to local #4
    // as this would require the insertion of an offset into the byte codes. 
    // This means we would need to make an additional pass to update branch
    // targets (no - BCEL does this for us) and the StackMapTable (yes).

    LocalVariableGen lv_nonce;

    int max_index = -1;
    nonce_index = -1;
    for (LocalVariableGen lv : c.mgen.getLocalVariables()) {
        if (lv.getStart().getPosition() != 0) {
            if (nonce_index == -1) {
                nonce_index = lv.getIndex();
            }
            lv.setIndex(lv.getIndex() + 1);
        }
        // need to add 1 if type is double or long
        max_index = lv.getIndex() + lv.getType().getSize() - 1;
    }

    // Special case: sometimes the java compiler allocates an unnamed
    // local temp for saving the exception in a finally clause.
    if (nonce_index == -1) {
        if (c.mgen.getMaxLocals() > max_index + 1) {
            nonce_index = max_index + 1;
        }
    }

    if (nonce_index != -1) {
        // insert the local variable into existing table at slot 'nonce_index'
        lv_nonce = c.mgen.addLocalVariable("this_invocation_nonce", Type.INT,
                                           nonce_index, null, null);
        c.mgen.setMaxLocals(c.mgen.getMaxLocals() + 1);

        // Loop through each instruction looking for local variable references
        for (InstructionHandle ih = il.getStart(); ih != null; ) {

          xform_local_ref(ih, il, c);

          // Go on to the next instruction in the list
          ih = ih.getNext();
        }

    } else {
        // create the local variable at end of locals
        // will automatically update max_locals
        lv_nonce = c.mgen.addLocalVariable("this_invocation_nonce",
                                           Type.INT, null, null);
        nonce_index = lv_nonce.getIndex();
    }    

    if (debug) {
        out.format (c.mgen.getLocalVariableTable(pgen) + "%n");
    }
    return lv_nonce;
  }


  /**
   * Inserts instrumentation code at the start of the method.  This includes
   * adding a local variable (this_invocation_nonce) that is initialized
   * to Runtime.nonce++.  This provides a unique id on each method entry/exit
   * that allows them to be matched up from the dtrace file.  Inserts code
   * to call Runtime.enter().
   */
  private void
  add_entry_instrumentation (InstructionList il, MethodContext c,
                             boolean shouldCallEnter) throws IOException {

    InstructionList nl = new InstructionList();

    // create the local variable
    LocalVariableGen nonce_lv = create_local_nonce(il, c);

    // The following implements:
    //     this_invocation_nonce = Runtime.nonce++;

    // getstatic Runtime.nonce (push its current value on stack)
    nl.append(c.ifact.createGetStatic(runtime_classname, "nonce",
                                        Type.INT));

    // dup (make a second copy of runtime.nonce on the stack)
    nl.append(InstructionFactory.createDup(Type.INT.getSize()));

    // iconst_1 (push 1 on the stack)
    nl.append(c.ifact.createConstant(1));

    // iadd (add the top two items on the stack together)
    nl.append(InstructionFactory.createBinaryOperation("+", Type.INT));

    // putstatic Runtime.nonce (pop result of add to Runtime.nonce)
    nl.append(c.ifact.createPutStatic(runtime_classname, "nonce",
                                        Type.INT));

    // istore <lv> (pop original value of nonce into this_invocation_nonce)
    nl.append(InstructionFactory.createStore(Type.INT, nonce_lv.getIndex()));

    // Seems like there should be a easier way to do this.
    // Just need the size of nl in bytes, not instructions.
    byte[] instrumentation = nl.getByteCode();
    int len_part1 = instrumentation.length;

    if (shouldCallEnter) {
        // call Runtime.enter()
        nl.append(call_enter_exit(c, "enter", -1));
    }

    instrumentation = nl.getByteCode();
    int len_part2 = instrumentation.length - len_part1;

    // Add the new instructions at the start and move any LineNumbers
    // and Local variables to point to them.  Other targeters
    // (branches, exceptions) should still point to the old start
    InstructionHandle old_start = il.getStart();
    InstructionHandle new_start = il.insert(nl);
    for (InstructionTargeter it : old_start.getTargeters()) {
      if ((it instanceof LineNumberGen) || (it instanceof LocalVariableGen))
        it.updateTarget(old_start, new_start);
    }

    // For Java 7 and beyond the StackMapTable is part of the
    // verification process.  We need to create and or update it to 
    // account for instrumentation code we have inserted as well as
    // adjustments for the new 'nonce' local.

    // Get existing StackMapTable (if present)
    if (stack_map_table.length > 0) {
        // We need to adjust the offset_delta of the first old 
        // StackMapEntry due to the fact that it will no longer be
        // the first entry.  We must subtract 1.
        modify_stack_map_offset(stack_map_table[0], -1);
    }

    StackMapTableEntry[] new_map = new StackMapTableEntry[stack_map_table.length
                                                          + ((len_part2 > 0) ? 2 : 1)];
    StackMapType nonce_type = new StackMapType(Constants.ITEM_Integer, -1,
                                               pgen.getConstantPool());
    StackMapType[] old_nonce_type = {nonce_type};
    new_map[0] = new StackMapTableEntry(Constants.APPEND_FRAME, len_part1, 1,
                                        old_nonce_type, 0, null, pgen.getConstantPool());

    int new_index = 1;
    if (len_part2 > 0) {
        new_map[1] = new StackMapTableEntry(Constants.SAME_FRAME+len_part2-1,
                             len_part2-1, 0, null, 0, null, pgen.getConstantPool());
        new_index++;
    }

// We cannot just copy over the existing stack map entires.  If any of them
// are FULL_FAME we need to add our 'nonce' variable to the local table.

    for (int i = 0; i < stack_map_table.length; i++) {
        if (stack_map_table[i].getFrameType() == Constants.FULL_FRAME) {

            // need to add our 'nonce' variable into the list of locals
            // We must account for the args and this pointer which
            // means insert type of 'nonce' at 'nonce_index'

            int num_locals = stack_map_table[i].getNumberOfLocals();
            // if num_locals < insert index nothing to do
            // (can this happen?)
            if (num_locals >= nonce_index) {
                StackMapType[] old_local_types = stack_map_table[i].getTypesOfLocals();
                StackMapType[] new_local_types = new StackMapType[num_locals+1];

                for (int j = num_locals; j > nonce_index; j--) {
                    new_local_types[j] = old_local_types[j-1];
                }
                new_local_types[nonce_index] = nonce_type;
                for (int j = nonce_index; j != 0; j--) {
                    new_local_types[j-1] = old_local_types[j-1];
                }

                stack_map_table[i].setNumberOfLocals(num_locals+1);
                stack_map_table[i].setTypesOfLocals(new_local_types);
            }    
        }    
        new_map[new_index++] = stack_map_table[i];
    }    
    if (debug) {
        out.format ("new_map: " + Arrays.toString(new_map) + "%n");
    }
    stack_map_table = new_map;
  }


  /**
   * Pushes the object, nonce, parameters, and return value
   * on the stack and calls the specified Method (normally
   * enter or exit) in Runtime.  The parameters are passed
   * as an array of objects.  Any primitive values are wrapped
   * in the appropriate Runtime wrapper (IntWrap, FloatWrap, etc)
   */
  private InstructionList
  call_enter_exit (MethodContext c, String method_name, int line) {

    InstructionList il = new InstructionList();
    InstructionFactory ifact = c.ifact;
    MethodGen mgen = c.mgen;
    Type[] arg_types = mgen.getArgumentTypes();

    // aload
    // Push the object.  Null if this is a static method or a constructor
    if (mgen.isStatic() ||
        (method_name.equals ("enter") && is_constructor(mgen))) {
      il.append(new ACONST_NULL());
    } else { // must be an instance method
      il.append(InstructionFactory.createLoad(Type.OBJECT, 0));
    }

    // Determine the offset of the first parameter
    int param_offset = 1;
    if (c.mgen.isStatic())
      param_offset = 0;

    // iload
    // Push the nonce
    LocalVariableGen nonce_lv = get_nonce_local(mgen);
    il.append(InstructionFactory.createLoad(Type.INT, nonce_lv.getIndex()));

    // iconst
    // Push the MethodInfo index
    il.append(ifact.createConstant(cur_method_info_index));

    // iconst
    // anewarray
    // Create an array of objects with elements for each parameter
    il.append(ifact.createConstant(arg_types.length));
    Type object_arr_typ = new ArrayType ("java.lang.Object", 1);
    il.append(ifact.createNewArray(Type.OBJECT, (short) 1));

    // Put each argument into the array
    int param_index = param_offset;
    for (int ii = 0; ii < arg_types.length; ii++) {
      il.append(InstructionFactory.createDup(object_arr_typ.getSize()));
      il.append(ifact.createConstant(ii));
      Type at = arg_types[ii];
      if (at instanceof BasicType) {
        il.append(create_wrapper(c, at, param_index));
      } else { // must be reference of some sort
        il.append(InstructionFactory.createLoad(Type.OBJECT, param_index));
      }
      il.append(InstructionFactory.createArrayStore(Type.OBJECT));
      param_index += at.getSize();
    }

    // If this is an exit, push the return value and line number.
    // The return value
    // is stored in the local "return__$trace2_val"  If the return
    // value is a primitive, wrap it in the appropriate runtime wrapper
    if (method_name.equals ("exit")) {
      Type ret_type = mgen.getReturnType();
      if (isVoid(ret_type)) {
        il.append(new ACONST_NULL());
      } else {
        LocalVariableGen return_local = get_return_local(mgen, ret_type);
        if (ret_type instanceof BasicType) {
          il.append(create_wrapper(c, ret_type, return_local.getIndex()));
        } else {
          il.append(InstructionFactory.createLoad(Type.OBJECT, return_local.getIndex()));
        }
      }


      // push line number
      // System.out.println(c.mgen.getName() + " --> " + line);
      il.append(ifact.createConstant(line));
    }

    // Call the specified method
    Type[] method_args = null;
    if (method_name.equals ("exit"))
      method_args = new Type[] {Type.OBJECT, Type.INT, Type.INT,
                                object_arr_typ, Type.OBJECT, Type.INT};
    else
      method_args = new Type[] {Type.OBJECT, Type.INT, Type.INT,
                                object_arr_typ};
    il.append(c.ifact.createInvoke(runtime_classname, method_name,
                                     Type.VOID, method_args, Constants.INVOKESTATIC));

    return (il);
  }


  /**
   * Creates code to put the local var/param at the specified var_index
   * into a wrapper appropriate for prim_type.  prim_type should be one
   * of the basic types (eg, Type.INT, Type.FLOAT, etc).  The wrappers
   * are those defined in Runtime.
   *
   * The stack is left with a pointer to the newly created wrapper at the
   * top.
   */
  private InstructionList
  create_wrapper (MethodContext c, Type prim_type, int var_index) {

    String wrapper = null;
    switch (prim_type.getType()) {
    case Constants.T_BOOLEAN: wrapper = "BooleanWrap"; break;
    case Constants.T_BYTE:    wrapper = "ByteWrap"; break;
    case Constants.T_CHAR:    wrapper = "CharWrap"; break;
    case Constants.T_DOUBLE:  wrapper = "DoubleWrap"; break;
    case Constants.T_FLOAT:   wrapper = "FloatWrap"; break;
    case Constants.T_INT:     wrapper = "IntWrap"; break;
    case Constants.T_LONG:    wrapper = "LongWrap"; break;
    case Constants.T_SHORT:   wrapper = "ShortWrap"; break;
    default: throw new Error("unexpected type " + prim_type);
    }

    InstructionList il = new InstructionList();
    String classname = runtime_classname + "$" + wrapper;
    il.append(c.ifact.createNew(classname));
    il.append(InstructionFactory.createDup(Type.OBJECT.getSize()));
    il.append(InstructionFactory.createLoad(prim_type, var_index));
    il.append(c.ifact.createInvoke(classname, "<init>", Type.VOID,
                                     new Type[] {prim_type}, Constants.INVOKESPECIAL));

    return (il);
  }


  /**
   * Returns true iff mgen is a constructor
   * @return true iff mgen is a constructor
   */
  /*@Pure*/
  private boolean
  is_constructor (MethodGen mgen) {

    if (mgen.getName().equals ("<init>") || mgen.getName().equals ("")) {
      // log ("method '%s' is a constructor%n", mgen.getName());
      return (true);
    } else
      return (false);
  }


  /**
   * Return an array of strings, each corresponding to mgen's argument types
   * @return an array of strings, each corresponding to mgen's argument types
   */
  /*@BinaryName*/
  private String[]
  getArgTypes (MethodGen mgen) {

    Type[] arg_types = mgen.getArgumentTypes();
    /*@BinaryName*/ String[]
    arg_type_strings = new /*@BinaryName*/ String[arg_types.length];

    for (int ii = 0; ii < arg_types.length; ii++)
      {
        Type t = arg_types[ii];
        /*if (t instanceof ObjectType)
          arg_type_strings[ii] = ((ObjectType) t).getClassName();
          else
          arg_type_strings[ii] = t.getSignature().replace('/', '.');
        */
        arg_type_strings[ii] = t.toString();
      }

    return arg_type_strings;
  }


  @SuppressWarnings("signature") // conversion routine
  private static /*@ClassGetName*/ String
  typeToClassGetName (Type t) {

    if (t instanceof ObjectType) {
      return ((ObjectType) t).getClassName();
    } else {
      // Array type: just convert '/' to '.'
      return t.getSignature().replace('/', '.');
    }
  }


  // creates a MethodInfo struct corresponding to mgen
  @SuppressWarnings("unchecked")
  private /*@Nullable*/ MethodInfo
  create_method_info (ClassInfo class_info, MethodGen mgen) {

    // Get the argument names for this method
    String[] arg_names = mgen.getArgumentNames();
    LocalVariableGen[] lvs = mgen.getLocalVariables();
    int param_offset = 1;
    if (mgen.isStatic())
      param_offset = 0;
    if (debug) {
      out.format ("create_method_info1 %s%n", arg_names.length);
      for (int ii = 0; ii < arg_names.length; ii++) {
        out.format ("arg: %s%n", arg_names[ii]);
      }
    }

    int lv_start = 0;
    // If this is an inner class constructor, then its first parameter is
    // the outer class constructor.  I need to detect this and adjust the
    // parameter names appropriately.  This check is ugly.
    if (mgen.getName().equals("<init>") && mgen.getArgumentTypes().length > 0) {
      int dollarPos = mgen.getClassName().lastIndexOf("$");
      if (dollarPos >= 0
          &&
          // type of first parameter is classname up to the "$"
          mgen.getClassName().substring(0, dollarPos).equals(mgen.getArgumentType(0).toString())) {
        // As a further check, for javac-generated classfiles, the
        // constant pool index #1 is "this$0", and the first 5 bytes of
        // the bytecode are:
        //   0: aload_0       
        //   1: aload_1       
        //   2: putfield      #1

        lv_start++;
        param_offset--;
        
        arg_names[0] = mgen.getArgumentType(0).toString() + ".this";
      }
    }

    if (lvs != null)
      {
        for (int ii = lv_start; ii < arg_names.length; ii++)
          {
            if ((ii + param_offset) < lvs.length)
              arg_names[ii] = lvs[ii + param_offset].getName();
          }
      }

    if (debug) {
      out.format ("create_method_info2 %s%n", arg_names.length);
      for (int ii = 0; ii < arg_names.length; ii++) {
        out.format ("arg: %s%n", arg_names[ii]);
      }
    }

    boolean shouldInclude = false;

    // see if we should filter the entry point
    if (!shouldFilter(class_info.class_name, mgen.getName(),
             DaikonWriter.methodEntryName(class_info.class_name, getArgTypes(mgen),
                                          mgen.toString(), mgen.getName()))) {
        shouldInclude = true;
    }
    // Get the argument types for this method
    Type[] arg_types = mgen.getArgumentTypes();
    /*@ClassGetName*/ String[] arg_type_strings = new /*@ClassGetName*/ String[arg_types.length];
    for (int ii = 0; ii < arg_types.length; ii++)
      {
        arg_type_strings[ii] = typeToClassGetName(arg_types[ii]);
      }

    // Loop through each instruction and find the line number for each
    // return opcode
    List<Integer> exit_locs = new ArrayList<Integer>();

    // tells whether each exit loc in the method is included or not (based on filters)
    List<Boolean> isIncluded = new ArrayList<Boolean>();

    // log ("Looking for exit points in %s%n", mgen.getName());
    InstructionList il = mgen.getInstructionList();
    int line_number = 0;
    int last_line_number = 0;
    boolean foundLine;

    for (Iterator<InstructionHandle> ii = il.iterator(); ii.hasNext();)
      {
        InstructionHandle ih = ii.next();

        foundLine = false;

        if (ih.hasTargeters())
          {
            for (InstructionTargeter it : ih.getTargeters())
              {
                if (it instanceof LineNumberGen)
                  {
                    LineNumberGen lng = (LineNumberGen) it;
                    // log ("  line number at %s: %d%n", ih, lng.getSourceLine());
                    // System.out.printf("  line number at %s: %d%n", ih, lng.getSourceLine());
                    line_number = lng.getSourceLine();
                    foundLine = true;
                  }
              }
          }

        switch (ih.getInstruction().getOpcode())
          {
          case Constants.ARETURN :
          case Constants.DRETURN :
          case Constants.FRETURN :
          case Constants.IRETURN :
          case Constants.LRETURN :
          case Constants.RETURN :
            // log ("Exit at line %d%n", line_number);

            // only do incremental lines if we don't have the line generator
            if (line_number == last_line_number && foundLine == false)
              {
                // System.out.printf("Could not find line... at %d%n", line_number);
                line_number++;
              }

            last_line_number = line_number;

            if (!shouldFilter(class_info.class_name, mgen.getName(),
                     DaikonWriter.methodExitName(class_info.class_name, getArgTypes(mgen),
                         mgen.toString(), mgen.getName(), line_number)))
              {
                shouldInclude = true;
                exit_locs.add(new Integer(line_number));

                isIncluded.add(true);
              }
            else
              isIncluded.add(false);

            break;

          default :
            break;
          }
      }

    if (shouldInclude)
      return new MethodInfo(class_info, mgen.getName(), arg_names, arg_type_strings, exit_locs, isIncluded);
    else
      return null;
  }


  public void
  dump_code_attributes (MethodGen mg) {
      // mg.getMethod().getCode().getAttributes() forces attributes
      // to be instantiated; mg.getCodeAttributes() does not
      for (Attribute a : mg.getMethod().getCode().getAttributes()) {
          int con_index = a.getNameIndex();
          Constant c = pgen.getConstant(con_index);
          String att_name = ((ConstantUtf8) c).getBytes();
          out.format ("Attribute Index: %s Name: %s%n", con_index, att_name);
      }
  }


  public Attribute
  get_stack_map_table_attribute (MethodGen mg) {
      for (Attribute a : mg.getCodeAttributes()) {
          if (is_stack_map_table(a)) {
            return a;
          }
      }
      return null;
  }


  public Attribute
  get_local_variable_type_table_attribute (MethodGen mg) {
      for (Attribute a : mg.getCodeAttributes()) {
          if (is_local_variable_type_table(a)) {
            return a;
          }
      }
      return null;
  }


  /*@Pure*/
  public boolean
  is_local_variable_type_table (Attribute a) {
    return (get_attribute_name(a).equals ("LocalVariableTypeTable"));
  }


  /*@Pure*/
  public boolean
  is_stack_map_table (Attribute a) {
    return (get_attribute_name(a).equals ("StackMapTable"));
  }


  /**
   * Returns the attribute name for the specified attribute
   */
  public String
  get_attribute_name (Attribute a) {

    int con_index = a.getNameIndex();
    Constant c = pgen.getConstant(con_index);
    String att_name = ((ConstantUtf8) c).getBytes();
    return (att_name);
  }


  /**
   * Any information needed by InstTransform routines about the method
   * and class
   */
  private static class
  MethodContext {

    public ClassGen cg;
    public ConstantPoolGen cpg;
    public InstructionFactory ifact;
    public MethodGen mgen;

    public MethodContext (ClassGen cg) {
      this.cg = cg;
      ifact = new InstructionFactory (cg);
      cpg = cg.getConstantPool();
    }

    public MethodContext (ClassGen cg, MethodGen mgen) {
      this.cg = cg;
      ifact = new InstructionFactory (cg);
      cpg = cg.getConstantPool();
      this.mgen = mgen;
    }
  }


  /**
   * Returns whether or not the specified class is part of chicory
   * itself (and thus should not be instrumented).  Some daikon classes
   * that are used by Chicory are included here as well
   */
  /*@Pure*/
  private static boolean
  is_chicory (String classname) {

    if (classname.startsWith ("daikon/chicory")
        && !classname.equals ("daikon/chicory/Test"))
      return true;
    if (classname.equals ("daikon/PptTopLevel$PptType"))
      return true;
    return false;
  }

}
