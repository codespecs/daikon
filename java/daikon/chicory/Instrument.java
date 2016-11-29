package daikon.chicory;

import static java.lang.System.out;

import daikon.Chicory;
import daikon.util.SimpleLog;
import java.io.*;
import java.lang.instrument.*;
import java.security.*;
import java.util.*;
import java.util.regex.*;
import org.apache.bcel.*;
import org.apache.bcel.classfile.*;
import org.apache.bcel.generic.*;
import org.apache.bcel.generic.InstructionFactory;

/*>>>
import org.checkerframework.checker.formatter.qual.*;
import org.checkerframework.checker.nullness.qual.*;
import org.checkerframework.checker.signature.qual.*;
import org.checkerframework.dataflow.qual.*;
*/

/**
 * The Instrument class is responsible for modifying another class' bytecode. Specifically, its main
 * task is to add "hooks" into the other class at method entries and exits for instrumentation
 * purposes.
 */
@SuppressWarnings("nullness")
public class Instrument implements ClassFileTransformer {

  boolean debug = false;
  boolean log_on = false;

  /** current Constant Pool * */
  static ConstantPoolGen pgen = null;

  /** the index of this method into Runtime.methods */
  int cur_method_info_index = 0;

  /** the location of the runtime support class */
  private static final String runtime_classname = "daikon.chicory.Runtime";

  /** Debug information about which classes are transformed and why */
  public static SimpleLog debug_transform = new SimpleLog(Chicory.debug_transform);

  public Instrument() {
    debug = Chicory.debug;
    log_on = Chicory.debug_transform;
  }

  /*@FormatMethod*/
  @SuppressWarnings(
      "formatter") // call to format method is correct because of @FormatMethod annotation
  private void log(String format, /*@Nullable*/ Object... args) {
    if (!log_on) return;
    System.out.printf(format, args);
  }

  // uses Runtime.ppt_omit_pattern and Runtime.ppt_select_pattern
  // to see if the given ppt should be "filtered out"
  private boolean shouldFilter(String className, String methodName, String pptName) {

    // if (debug)
    //    out.format ("shouldFilter: %s, %s, %s%n", className, methodName, pptName);

    // Don't instrument class if it matches an excluded regular expression
    for (Pattern pattern : Runtime.ppt_omit_pattern) {

      Matcher mPpt = pattern.matcher(pptName);
      Matcher mClass = pattern.matcher(className);
      Matcher mMethod = pattern.matcher(methodName);

      if (mPpt.find() || mClass.find() || mMethod.find()) {
        log("not instrumenting %s, it matches ppt_omit regex %s%n", pptName, pattern);

        // System.out.println("filtering 1 true on --- " + pptName);

        // omit takes priority over include
        return true;
      }
    }

    // If any include regular expressions are specified, only instrument
    // classes that match them
    if (Runtime.ppt_select_pattern.size() > 0) {
      for (Pattern pattern : Runtime.ppt_select_pattern) {

        Matcher mPpt = pattern.matcher(pptName);
        Matcher mClass = pattern.matcher(className);
        Matcher mMethod = pattern.matcher(methodName);

        // System.out.println("--->" + regex);

        if (mPpt.find() || mClass.find() || mMethod.find()) {
          log("instrumenting %s, it matches ppt_select regex %s%n", pptName, pattern);

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
   * Given a class, return a transformed version of the class that contains "hooks" at method
   * entries and exits. Because Chicory is invoked as a javaagent, the transform method is called by
   * the Java runtime each time a new class is loaded.
   */
  public byte /*@Nullable*/ [] transform(
      ClassLoader loader,
      /*@InternalForm*/ String className,
      Class<?> classBeingRedefined,
      ProtectionDomain protectionDomain,
      byte[] classfileBuffer)
      throws IllegalClassFormatException {

    // debug = className.equals ("DataStructures/StackAr");
    // debug = className.equals ("chicory/Test");
    // debug = className.equals ("DataStructures/BinarySearchTree");

    String fullClassName = className.replace("/", ".");
    // String fullClassName = className;

    debug_transform.log("In chicory.Instrument.transform(): class = %s%n", className);

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
      Matcher matcher = Chicory.boot_classes.matcher(fullClassName);
      if (matcher.find()) {
        debug_transform.log(
            "ignoring sys class %s, " + "matches boot_classes regex", fullClassName);
        return null;
      }
    } else if (loader == null) {
      debug_transform.log("ignoring system class %s, class loader == null", fullClassName);
      return null;
    } else if (loader.getParent() == null) {
      debug_transform.log("ignoring system class %s, parent loader == null\n", fullClassName);
      return null;
    } else if (fullClassName.startsWith("sun.reflect")) {
      debug_transform.log("ignoring system class %s, in sun.reflect package", fullClassName);
      return null;
    } else if (fullClassName.startsWith("com.sun")) {
      System.out.printf("Class from com.sun package %s with nonnull loaders\n", fullClassName);
    }

    // Don't intrument our code
    if (is_chicory(className)) {
      debug_transform.log("Not considering chicory class %s%n", fullClassName);
      return null;
    }

    debug_transform.log(
        "transforming class %s, loader %s - %s%n", className, loader, loader.getParent());

    // Parse the bytes of the classfile, die on any errors
    JavaClass c = null;
    ClassParser parser = new ClassParser(new ByteArrayInputStream(classfileBuffer), className);
    try {
      c = parser.parse();
    } catch (Exception e) {
      throw new RuntimeException("Unexpected error", e);
    }

    try {
      // Get the class information
      ClassGen cg = new ClassGen(c);

      // Convert reach non-void method to save its result in a local
      // before returning
      ClassInfo c_info = instrument_all_methods(cg, fullClassName, loader);

      // get constant static fields!
      Field[] fields = cg.getFields();
      for (Field field : fields) {
        if (field.isFinal() && field.isStatic() && (field.getType() instanceof BasicType)) {
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

          if (valString != null) c_info.staticMap.put(field.getName(), valString);
        }
      }

      if (Chicory.checkStaticInit) {
        // check for static initializer
        boolean hasInit = false;
        for (Method meth : cg.getMethods()) {
          if (meth.getName().equals("<clinit>")) {
            hasInit = true;
          }
        }

        // if not found, add our own!
        if (!hasInit) {
          cg.addMethod(createClinit(cg, fullClassName));
        }
      }

      JavaClass njc = cg.getJavaClass();
      if (debug) {
        System.out.printf("Dumping %s to %s%n", njc.getClassName(), "/tmp/ret/");
        njc.dump("/tmp/ret/" + njc.getClassName() + ".class");
      }

      if (c_info.shouldInclude) {
        // System.out.println ("Instrumented class " + className);
        // String filename = "/homes/gws/mernst/tmp/" + className +
        //                   "Transformed.class";
        // System.out.println ("About to dump class " + className +
        //                     " to " + filename);
        // cg.getJavaClass().dump(filename);
        return (cg.getJavaClass().getBytes());
      } else {
        debug_transform.log("not including class %s (filtered out)", className);
        // No changes to the bytecodes
        return null;
      }

    } catch (Throwable e) {
      out.format("Unexpected error %s in transform of %s", e, fullClassName);
      e.printStackTrace();
      // No changes to the bytecodes
      return null;
    }
  }

  // used to add a "hook" into the <clinit> static initializer
  private Method addInvokeToClinit(ClassGen cg, MethodGen mg, String fullClassName) {

    InstructionList il = mg.getInstructionList();
    mg.update();
    MethodContext context = new MethodContext(cg, mg);

    for (InstructionHandle ih = il.getStart(); ih != null; ) {
      InstructionList new_il = null;
      Instruction inst = ih.getInstruction();

      // Get the translation for this instruction (if any)
      new_il = xform_clinit(cg, cg.getConstantPool(), fullClassName, inst, context);

      // Remember the next instruction to process
      InstructionHandle next_ih = ih.getNext();

      // If this instruction was modified, replace it with the new
      // instruction list. If this instruction was the target of any
      // jumps, replace it with the first instruction in the new list
      if (new_il != null) {
        if (true) {
          try {
            new_il.delete(new_il.getEnd());
          } catch (TargetLostException e) {
            throw new Error("unexpected lost target exception", e);
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

          ih = next_ih;
          continue;
        }

        if (debug) out.format("Replacing %s by %s%n", ih, new_il);

        il.append(ih, new_il);
        InstructionTargeter[] targeters = ih.getTargeters();
        if (targeters != null) {
          // out.format ("targeters length = %d%n", targeters.length);
          for (int j = 0; j < targeters.length; j++) {
            targeters[j].updateTarget(ih, ih.getNext());
          }
        }
        try {
          il.delete(ih);
        } catch (TargetLostException e) {
          throw new Error("unexpected lost target exception", e);
        }
      }
      // Go on to the next instruction in the list
      ih = next_ih;
    }

    // Current way to remove the LocalVariableTypeTable:
    Attribute a = get_local_variable_type_table_attribute(mg);
    if (a != null) mg.removeCodeAttribute(a);
    // In future version of BCEL the two lines above
    // will probably be replace by:
    // mg.removeLocalVariableTypeTable();

    // Update the max stack and Max Locals
    mg.setMaxLocals();
    mg.setMaxStack();
    mg.update();

    return mg.getMethod();
  }

  // called by addInvokeToClinit to add in a hook at return opcodes
  private /*@Nullable*/ InstructionList xform_clinit(
      ClassGen cg,
      ConstantPoolGen cp,
      String fullClassName,
      Instruction inst,
      MethodContext context) {

    switch (inst.getOpcode()) {
      case Const.ARETURN:
      case Const.DRETURN:
      case Const.FRETURN:
      case Const.IRETURN:
      case Const.LRETURN:
      case Const.RETURN:
        break;

      default:
        return null;
    }

    InstructionList il = new InstructionList();
    il.append(call_initNotify(cg, cp, fullClassName, context.ifact));
    il.append(inst);
    return il;
  }

  // create a <clinit> method, if none exists; guarantees we have this hook
  private Method createClinit(ClassGen cg, String fullClassName) {
    /*
     * System.out.println(mg.getAccessFlags());
     * System.out.println(mg.getReturnType());
     * System.out.println(mg.getArgumentTypes());
     * System.out.println(mg.getName());
     * System.out.println(mg.getClassName());
     */

    InstructionFactory factory = new InstructionFactory(cg);

    InstructionList il = new InstructionList();
    il.append(call_initNotify(cg, cg.getConstantPool(), fullClassName, factory));
    il.append(InstructionFactory.createReturn(Type.VOID)); // need to return!

    MethodGen newMethGen =
        new MethodGen(
            8,
            Type.VOID,
            new Type[0],
            new String[0],
            "<clinit>",
            fullClassName,
            il,
            cg.getConstantPool());
    newMethGen.update();

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

  // created the InstructionList to insert for adding the <clinit> hook
  private InstructionList call_initNotify(
      ClassGen cg, ConstantPoolGen cp, String fullClassName, InstructionFactory factory) {

    InstructionList invokeList = new InstructionList();

    invokeList.append(new PUSH(cp, fullClassName));
    invokeList.append(
        factory.createInvoke(
            runtime_classname,
            "initNotify",
            Type.VOID,
            new Type[] {Type.STRING},
            Const.INVOKESTATIC));

    // System.out.println(fullClassName + " --- " + invokeList.size());
    return invokeList;
  }

  // This really should be part of the abstraction provided by BCEL,
  // similar to LineNumberTable and LocalVariableTable.  However, for
  // now we'll do it all within Instrument.java.
  private StackMapEntry[] stack_map_table;
  private StackMapEntry[] empty_stack_map_table = {};
  // kind of a hack since no pointers in Java and not
  // worth making a container object.
  private int running_offset;
  // original stack map table
  private StackMap smta;
  //Map<Integer, InstructionHandle> offset_map = new HashMap<Integer, InstructionHandle>();
  InstructionHandle[] offset_map;

  /**
   * Instrument all the methods in a class. For each method, add instrumentation code at the entry
   * and at each return from the method. In addition, changes each return statement to first place
   * the value being returned into a local and then return. This allows us to work around the JDI
   * deficiency of not being able to query return values.
   *
   * @param fullClassName must be fully qualified: packageName.className
   */
  private ClassInfo instrument_all_methods(ClassGen cg, String fullClassName, ClassLoader loader) {

    ClassInfo class_info = new ClassInfo(cg.getClassName(), loader);
    List<MethodInfo> method_infos = new ArrayList<MethodInfo>();

    if (cg.getMajor() < Const.MAJOR_1_6) {
      System.out.printf(
          "Chicory warning: ClassFile: %s - classfile version (%d) is out of date and may not be processed correctly.%n",
          cg.getClassName(), cg.getMajor());
    }

    boolean shouldInclude = false;

    try {
      pgen = cg.getConstantPool();

      // Loop through each method in the class
      Method[] methods = cg.getMethods();
      for (int i = 0; i < methods.length; i++) {
        MethodGen mg = new MethodGen(methods[i], cg.getClassName(), pgen);
        MethodContext context = new MethodContext(cg, mg);

        // check for the class static initializer method
        if (mg.getName().equals("<clinit>")) {
          if (Chicory.checkStaticInit) {
            cg.replaceMethod(methods[i], addInvokeToClinit(cg, mg, fullClassName));
            cg.update();
          }
          if (!Chicory.instrument_clinit) {
            continue;
          }
        }

        // If method is synthetic... (default constructors and <clinit> are not synthetic)
        if ((Const.ACC_SYNTHETIC & mg.getAccessFlags()) > 0) {
          continue;
        }

        // Get the instruction list and skip methods with no instructions
        InstructionList il = mg.getInstructionList();
        if (il == null) {
          continue;
        }

        fix_local_variable_table(mg);

        if (debug) {
          Type[] arg_types = mg.getArgumentTypes();
          String[] arg_names = mg.getArgumentNames();
          LocalVariableGen[] local_vars = mg.getLocalVariables();
          String types = "", names = "", locals = "";

          for (int j = 0; j < arg_types.length; j++) {
            types = types + arg_types[j] + " ";
          }
          for (int j = 0; j < arg_names.length; j++) {
            names = names + arg_names[j] + " ";
          }
          for (int j = 0; j < local_vars.length; j++) {
            locals = locals + local_vars[j].getName() + " ";
          }
          out.format("%nMethod = %s%n", mg);
          out.format("arg_types(%d): %s%n", arg_types.length, types);
          out.format("arg_names(%d): %s%n", arg_names.length, names);
          out.format("localvars(%d): %s%n", local_vars.length, locals);
          out.format("Original code: %s%n", mg.getMethod().getCode());
          out.format("ClassInfo: %s%n", class_info);
          out.format("MethodGen: %s%n", mg);
          dump_code_attributes(mg);
        }

        // Get existing StackMapTable (if present)
        smta = (StackMap) get_stack_map_table_attribute(mg);
        if (smta != null) {
          // get a deep copy of the original StackMapTable.
          stack_map_table = ((StackMap) (smta.copy(smta.getConstantPool()))).getStackMap();
          if (debug) {
            out.format(
                "Attribute tag: %s length: %d nameIndex: %d%n",
                smta.getTag(), smta.getLength(), smta.getNameIndex());
          }
          mg.removeCodeAttribute(smta);
        } else {
          stack_map_table = empty_stack_map_table;
        }

        print_stack_map_table("Original");

        // Create a MethodInfo that describes this methods arguments
        // and exit line numbers (information not available via reflection)
        // and add it to the list for this class.
        MethodInfo mi = (create_method_info(class_info, mg));

        print_stack_map_table("After create_method_info");

        if (mi == null) // method filtered out!
        continue;

        // Create a map of Uninitialized_variable_info offsets to
        // InstructionHandles.  We will use this map after we
        // complete instrumentation to update the offsets due
        // to code modification and expansion.
        // The offsets point to 'new' instructions; since we do
        // not modify these, their Instruction Handles will remain
        // unchanged throught the instrumentaion process.
        process_uninitialized_variable_info(il, true);

        if (!shouldInclude && debug) {
          out.format("Class %s included [%s]%n", cg.getClassName(), mi);
        }
        shouldInclude = true; // at least one method not filtered out

        method_infos.add(mi);

        synchronized (Runtime.class) {
          cur_method_info_index = Runtime.methods.size();
          Runtime.methods.add(mi);
        }

        // Add nonce local to matchup enter/exits
        String entry_ppt_name =
            DaikonWriter.methodEntryName(
                fullClassName, getArgTypes(mg), mg.toString(), mg.getName());
        add_entry_instrumentation(
            il, context, !shouldFilter(fullClassName, mg.getName(), entry_ppt_name));

        print_stack_map_table("After add_entry_instrumentation");

        if (debug) {
          out.format("Modified code: %s%n", mg.getMethod().getCode());
        }

        // Need to see if there are any switches after this location.
        // If so, we may need to update the corresponding stackmap if
        // the amount of the switch padding changed.
        modify_stack_maps_for_switches(il.getStart(), il);

        Iterator<Boolean> shouldIncIter = mi.is_included.iterator();
        Iterator<Integer> exitIter = mi.exit_locations.iterator();

        // Loop through each instruction looking for the return(s)
        for (InstructionHandle ih = il.getStart(); ih != null; ) {
          InstructionList new_il = null;
          Instruction inst = ih.getInstruction();

          // If this is a return instruction, insert method exit instrumentation
          new_il =
              add_return_instrumentation(fullClassName, inst, context, shouldIncIter, exitIter);

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

              // If there was no orgiinal StackMapTable (smta == null)
              // then class was compiled for Java 5.
              if (smta != null) {
                int len = (new_il.getByteCode()).length;
                il.setPositions();
                int current_offset = next_ih.getPosition();

                if (debug) {
                  out.format("Current offset: %d Inserted length: %d%n", current_offset, len);
                  //out.format ("Modified code: %s%n", mg.getMethod().getCode());
                  //dump_code_attributes(mg);
                }

                // find stack map for current location
                StackMapEntry stack_map = find_stack_map_equal(current_offset);
                stack_map.updateByteCodeOffset(len);
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

            // Need to see if there are any switches after this location.
            // If so, we may need to update the corresponding stackmap if
            // the amount of the switch padding changed.
            modify_stack_maps_for_switches(next_ih, il);
          }
          // Go on to the next instruction in the list
          ih = next_ih;
        }

        // Update the Uninitialized_variable_info offsets before
        // we write out the new StackMapTable.
        process_uninitialized_variable_info(il, false);
        print_stack_map_table("Final");

        if (cg.getMajor() > Const.MAJOR_1_5) {
          // Build new StackMapTable attribute
          StackMap map_table =
              new StackMap(pgen.addUtf8("StackMapTable"), 0, null, pgen.getConstantPool());
          map_table.setStackMap(stack_map_table);
          mg.addCodeAttribute(map_table);
        }

        // UNDONE: not sure this is necessary anymore?
        // Remove the Local variable type table attribute (if any).
        // Evidently, some changes we make require this to be updated, but
        // without BCEL support, that would be hard to do.  Just delete it
        // for now (since it is optional, and we are unlikely to be used by
        // a debugger)
        // Current way to remove the LocalVariableTypeTable:
        Attribute a = get_local_variable_type_table_attribute(mg);
        if (a != null) mg.removeCodeAttribute(a);
        // In future version of BCEL the two lines above
        // will probably be replace by:
        // mg.removeLocalVariableTypeTable();

        // Update the instruction list
        mg.setInstructionList(il);
        mg.update();

        // Update the max stack
        mg.setMaxStack();
        mg.update();

        // Update the method in the class
        try {
          cg.replaceMethod(methods[i], mg.getMethod());
        } catch (Exception e) {
          if ((e.getMessage()).startsWith("Branch target offset too large")) {
            System.out.printf(
                "Chicory warning: ClassFile: %s - method %s is too large to instrument and is being skipped.%n",
                cg.getClassName(), mg.getName());
            continue;
          } else {
            throw e;
          }
        }

        if (debug) {
          out.format("Modified code: %s%n", mg.getMethod().getCode());
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
      out.format("Unexpected exception encountered: %s", e);
      e.printStackTrace();
    }

    // Add the class and method information to runtime so it is available
    // as enter/exit ppts are processed.
    class_info.set_method_infos(method_infos);

    if (shouldInclude) {
      debug_transform.log("Added trace info to class %s%n", class_info);
      synchronized (Runtime.new_classes) {
        Runtime.new_classes.add(class_info);
      }
      synchronized (Runtime.all_classes) {
        Runtime.all_classes.add(class_info);
      }
    } else { // not included
      debug_transform.log("Trace info not added to class %s%n", class_info);
    }

    class_info.shouldInclude = shouldInclude;
    return class_info;
  }

  // This method exists only to suppress interning warnings
  @SuppressWarnings("interning") // special, unique value
  /*@Pure*/
  private static boolean isVoid(Type t) {
    return t == Type.VOID;
  }

  /**
   * Transforms return instructions to first assign the result to a local variable
   * (return__$trace2_val) and then do the return. Also, calls Runtime.exit() immediately before the
   * return.
   */
  private /*@Nullable*/ InstructionList add_return_instrumentation(
      String fullClassName,
      Instruction inst,
      MethodContext c,
      Iterator<Boolean> shouldIncIter,
      Iterator<Integer> exitIter) {

    switch (inst.getOpcode()) {
      case Const.ARETURN:
      case Const.DRETURN:
      case Const.FRETURN:
      case Const.IRETURN:
      case Const.LRETURN:
      case Const.RETURN:
        break;

      default:
        return null;
    }

    if (!shouldIncIter.hasNext()) throw new RuntimeException("Not enough entries in shouldIncIter");

    boolean shouldInclude = shouldIncIter.next();

    if (!shouldInclude) return null;

    Type type = c.mgen.getReturnType();
    InstructionList il = new InstructionList();
    if (!isVoid(type)) {
      LocalVariableGen return_loc = get_return_local(c.mgen, type);
      il.append(InstructionFactory.createDup(type.getSize()));
      il.append(InstructionFactory.createStore(type, return_loc.getIndex()));
    }

    if (!exitIter.hasNext()) {
      throw new RuntimeException("Not enough exit locations in the exitIter");
    }

    il.append(call_enter_exit(c, "exit", exitIter.next()));
    return il;
  }

  /**
   * Returns the local variable used to store the return result. If it is not present, creates it
   * with the specified type. If the variable is known to already exist, the type can be null.
   */
  private LocalVariableGen get_return_local(MethodGen mgen, /*@Nullable*/ Type return_type) {

    // Find the local used for the return value
    LocalVariableGen return_local = null;
    for (LocalVariableGen lv : mgen.getLocalVariables()) {
      if (lv.getName().equals("return__$trace2_val")) {
        return_local = lv;
        break;
      }
    }

    // If a type was specified and the variable was found, they must match
    if (return_local == null) {
      assert (return_type != null) : " return__$trace2_val doesn't exist";
    } else {
      assert (return_type.equals(return_local.getType()))
          : " return_type = " + return_type + "current type = " + return_local.getType();
    }

    if (return_local == null) {
      // log ("Adding return local of type %s%n", return_type);
      return_local = mgen.addLocalVariable("return__$trace2_val", return_type, null, null);
    }

    return return_local;
  }

  /** Finds the nonce local variable. Returns null if not present. */
  private /*@Nullable*/ LocalVariableGen get_nonce_local(MethodGen mgen) {

    // Find the local used for the nonce value
    for (LocalVariableGen lv : mgen.getLocalVariables()) {
      if (lv.getName().equals("this_invocation_nonce")) {
        return lv;
      }
    }

    return null;
  }

  /**
   * We have inserted an additional single byte into the instruction list; update the StackMaps, if
   * required.
   */
  private void update_stack_map_offset(int offset) {

    running_offset = -1; // no +1 on first entry
    for (int i = 0; i < stack_map_table.length; i++) {
      running_offset = stack_map_table[i].getByteCodeOffset() + running_offset + 1;

      if (running_offset > offset) {
        stack_map_table[i].updateByteCodeOffset(1);
        // Only update the first StackMap that occurs after the given
        // offset as map offsets are relative to previous map entry.
        return;
      }
    }
  }

  /** Find the StackMap entry who's offset matches the input argument. */
  private StackMapEntry find_stack_map_equal(int offset) {

    running_offset = -1; // no +1 on first entry
    for (int i = 0; i < stack_map_table.length; i++) {
      running_offset = stack_map_table[i].getByteCodeOffset() + running_offset + 1;

      if (running_offset > offset) {
        throw new RuntimeException("Invalid StackMap offset");
      }

      if (running_offset == offset) {
        return stack_map_table[i];
      }
      // try next map entry
    }

    // no offset matched
    throw new RuntimeException("Invalid StackMap offset 1");
  }

  /**
   * Find the StackMap entry who's offset is the first one after the input argument. Only called
   * when there must be one.
   */
  private StackMapEntry find_stack_map_after(int offset) {

    running_offset = -1; // no +1 on first entry
    for (int i = 0; i < stack_map_table.length; i++) {
      running_offset = stack_map_table[i].getByteCodeOffset() + running_offset + 1;

      if (running_offset > offset) {
        // also note that running_offset has been set
        return stack_map_table[i];
      }
      // try next map entry
    }

    // no such entry found
    throw new RuntimeException("Invalid StackMap offset 2");
  }

  /**
   * Either save or update the uninitialized_variable_info offsets. If 'save' is true, build a
   * HashMap of offsets to InsturctionHandles. If 'save' is false, update the offsets using the
   * HashMap.
   */
  private void process_uninitialized_variable_info(InstructionList il, boolean save) {
    il.setPositions();
    if (save) {
      // UNDONE: Should this be a sparse array?
      // We allocate one entry for each byte of the instruction list.
      offset_map = new InstructionHandle[il.getEnd().getPosition()];
    }
    for (int i = 0; i < stack_map_table.length; i++) {
      int max_types;
      StackMapType[] types;
      StackMapEntry stack_map = stack_map_table[i];

      max_types = stack_map.getNumberOfLocals();
      if (max_types > 0) {
        types = stack_map.getTypesOfLocals();
        process_uninitialized_variable_items(max_types, types, il, save);
      }

      max_types = stack_map.getNumberOfStackItems();
      if (max_types > 0) {
        types = stack_map.getTypesOfStackItems();
        process_uninitialized_variable_items(max_types, types, il, save);
      }
    }
  }

  private void process_uninitialized_variable_items(
      int max_types, StackMapType[] types, InstructionList il, boolean save) {
    for (int j = 0; j < max_types; j++) {
      if (types[j].getType() == Const.ITEM_NewObject) {
        int offset = types[j].getIndex();
        if (save) {
          // Initial pass over StackMapTable
          // build the offset_map
          InstructionHandle ih = il.findHandle(offset);
          assert (ih != null) : " no InstructionHandle for offset";
          InstructionHandle ih2 = offset_map[offset];
          if (ih2 != null) {
            assert (ih.equals(ih2)) : " InstructionHandles don't match";
          } else {
            offset_map[offset] = ih;
          }
        } else {
          // Final pass over StackMapTable
          // update the offsets
          InstructionHandle ih = offset_map[offset];
          assert (ih != null) : " no InstructionHandle for offset";
          types[j].setIndex(ih.getPosition());
        }
      }
    }
  }

  /**
   * Check to see if there have been any changes in a switch statement's padding bytes. If so, we
   * need to update the corresponding StackMap.
   */
  private void modify_stack_maps_for_switches(InstructionHandle ih, InstructionList il) {
    Instruction inst;
    short opcode;

    // If there was no orginal StackMapTable (smta == null)
    // then there are no switches and/or class was compiled for
    // Java 5.  In either case, no need to process switches.
    if (smta == null) {
      return;
    }

    // Make sure all instruction offsets are uptodate.
    il.setPositions();

    // Loop through each instruction looking for a switch
    while (ih != null) {
      inst = ih.getInstruction();
      opcode = inst.getOpcode();

      if (opcode == Const.TABLESWITCH || opcode == Const.LOOKUPSWITCH) {
        int current_offset = ih.getPosition();
        StackMapEntry stack_map = find_stack_map_after(current_offset);
        int delta = (current_offset + inst.getLength()) - running_offset;
        if (delta != 0) {
          stack_map.updateByteCodeOffset(delta);
        }
        // Since StackMap offsets are relative to the previous one
        // we only have to do the first one after a switch.
      }

      // Go on to the next instruction in the list
      ih = ih.getNext();
    }
  }

  private void print_stack_map_table(String prefix) {

    if (debug) {
      out.format("%nStackMap(%s) %s items:%n", prefix, stack_map_table.length);
      running_offset = -1; // no +1 on first entry
      for (int i = 0; i < stack_map_table.length; i++) {
        running_offset = stack_map_table[i].getByteCodeOffset() + running_offset + 1;
        out.format("@%03d %s %n", running_offset, stack_map_table[i]);
      }
    }
  }

  // Set by create_local_nonce
  // Used by create_local_nonce, xform_local_ref, add_method_startup
  private int nonce_index;
  private int nonce_offset;

  /**
   * Transforms instructions that reference locals that are 'higher' in the local map that the nonce
   * local. Need to add one to their operand offset. This may require changing the instruction as
   * well.
   */
  private void xform_local_ref(InstructionHandle ih, InstructionList il) {

    Instruction inst = ih.getInstruction();

    int operand;
    short opcode = inst.getOpcode();

    switch (opcode) {
      case Const.RET:
        operand = ((IndexedInstruction) inst).getIndex();
        if (operand >= nonce_offset) {
          ((IndexedInstruction) inst).setIndex(operand + 1);
        }
        break;

      case Const.IINC:
        operand = ((LocalVariableInstruction) inst).getIndex();
        if (operand >= nonce_offset) {
          ((LocalVariableInstruction) inst).setIndex(operand + 1);
        }
        break;

      case Const.ILOAD:
      case Const.ILOAD_0:
      case Const.ILOAD_1:
      case Const.ILOAD_2:
      case Const.ILOAD_3:
      case Const.LLOAD:
      case Const.LLOAD_0:
      case Const.LLOAD_1:
      case Const.LLOAD_2:
      case Const.LLOAD_3:
      case Const.FLOAD:
      case Const.FLOAD_0:
      case Const.FLOAD_1:
      case Const.FLOAD_2:
      case Const.FLOAD_3:
      case Const.DLOAD:
      case Const.DLOAD_0:
      case Const.DLOAD_1:
      case Const.DLOAD_2:
      case Const.DLOAD_3:
      case Const.ALOAD:
      case Const.ALOAD_0:
      case Const.ALOAD_1:
      case Const.ALOAD_2:
      case Const.ALOAD_3:
      case Const.ISTORE:
      case Const.ISTORE_0:
      case Const.ISTORE_1:
      case Const.ISTORE_2:
      case Const.ISTORE_3:
      case Const.LSTORE:
      case Const.LSTORE_0:
      case Const.LSTORE_1:
      case Const.LSTORE_2:
      case Const.LSTORE_3:
      case Const.FSTORE:
      case Const.FSTORE_0:
      case Const.FSTORE_1:
      case Const.FSTORE_2:
      case Const.FSTORE_3:
      case Const.DSTORE:
      case Const.DSTORE_0:
      case Const.DSTORE_1:
      case Const.DSTORE_2:
      case Const.DSTORE_3:
      case Const.ASTORE:
      case Const.ASTORE_0:
      case Const.ASTORE_1:
      case Const.ASTORE_2:
      case Const.ASTORE_3:
        // BCEL handles all the details of which opcode and if index
        // is implicit or explicit; also, and if needs to be WIDE.
        operand = ((LocalVariableInstruction) inst).getIndex();
        if (operand >= nonce_offset) {
          ((LocalVariableInstruction) inst).setIndex(operand + 1);
          // Unfortunately, it doesn't take care of incrementing the
          // offset within StackMapEntrys.
          if (operand == 3) {
            // If operand was 3 (implicit), it will now be 4 (explicit)
            // which makes the instruction one byte longer.  We need to
            // update the instruction list to account for this.
            il.setPositions();
            // Which means we might need to update a StackMap offset.
            update_stack_map_offset(ih.getPosition());
            // Also need to see if switch padding has changed
            modify_stack_maps_for_switches(ih.getNext(), il);
          }
        }
        break;

      default:
    }
  }

  /**
   * Create the nonce local variable. This may have the side effect of causing us to rewrite the
   * method byte codes to adjust the offsets of existing local variables - see below for details.
   */
  private LocalVariableGen create_local_nonce(InstructionList il, MethodContext c) {

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
    int var_index = 0;
    nonce_offset = -1;
    for (LocalVariableGen lv : c.mgen.getLocalVariables()) {
      if (lv.getStart().getPosition() != 0) {
        if (nonce_offset == -1) {
          nonce_offset = lv.getIndex();
          nonce_index = var_index;
        }
        lv.setIndex(lv.getIndex() + 1);
        // UNDONE: need to update matching lvtt entry, if there is one
      }
      // need to add 1 if type is double or long
      max_index = lv.getIndex() + lv.getType().getSize() - 1;
      var_index++;
    }

    // Special case: sometimes the java compiler allocates an unnamed
    // local temp for saving the exception in a finally clause.
    if (nonce_offset == -1) {
      if (c.mgen.getMaxLocals() > max_index + 1) {
        nonce_offset = max_index + 1;
        nonce_index = var_index;
      }
    }

    if (nonce_offset != -1) {
      // insert the local variable into existing table at slot 'nonce_offset'
      lv_nonce =
          c.mgen.addLocalVariable("this_invocation_nonce", Type.INT, nonce_offset, null, null);
      c.mgen.setMaxLocals(c.mgen.getMaxLocals() + 1);

      // Loop through each instruction looking for local variable references
      for (InstructionHandle ih = il.getStart(); ih != null; ) {

        xform_local_ref(ih, il);

        // Go on to the next instruction in the list
        ih = ih.getNext();
      }

    } else {
      // create the local variable at end of locals
      // will automatically update max_locals
      lv_nonce = c.mgen.addLocalVariable("this_invocation_nonce", Type.INT, null, null);
      nonce_offset = lv_nonce.getIndex();
      nonce_index = var_index;
    }

    if (debug) {
      out.format("%s%n", c.mgen.getLocalVariableTable(pgen));
    }
    return lv_nonce;
  }

  /**
   * Inserts instrumentation code at the start of the method. This includes adding a local variable
   * (this_invocation_nonce) that is initialized to Runtime.nonce++. This provides a unique id on
   * each method entry/exit that allows them to be matched up from the dtrace file. Inserts code to
   * call Runtime.enter().
   */
  private void add_entry_instrumentation(
      InstructionList il, MethodContext c, boolean shouldCallEnter) throws IOException {

    InstructionList nl = new InstructionList();

    // create the local variable
    LocalVariableGen nonce_lv = create_local_nonce(il, c);

    print_stack_map_table("After cln");

    if (debug) {
      out.format("Modified code: %s%n", c.mgen.getMethod().getCode());
    }

    // The following implements:
    //     this_invocation_nonce = Runtime.nonce++;

    // getstatic Runtime.nonce (push its current value on stack)
    nl.append(c.ifact.createGetStatic(runtime_classname, "nonce", Type.INT));

    // dup (make a second copy of runtime.nonce on the stack)
    nl.append(InstructionFactory.createDup(Type.INT.getSize()));

    // iconst_1 (push 1 on the stack)
    nl.append(c.ifact.createConstant(1));

    // iadd (add the top two items on the stack together)
    nl.append(InstructionFactory.createBinaryOperation("+", Type.INT));

    // putstatic Runtime.nonce (pop result of add to Runtime.nonce)
    nl.append(c.ifact.createPutStatic(runtime_classname, "nonce", Type.INT));

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
      if ((it instanceof LineNumberGen) || (it instanceof LocalVariableGen)) {
        it.updateTarget(old_start, new_start);
      }
    }

    // For Java 7 and beyond the StackMapTable is part of the
    // verification process.  We need to create and or update it to
    // account for instrumentation code we have inserted as well as
    // adjustments for the new 'nonce' local.

    boolean skipFirst = false;

    // Get existing StackMapTable (if present)
    if (stack_map_table.length > 0) {
      // Each stack map frame specifies (explicity or implicitly) an
      // offset_delta that is used to calculate the actual bytecode
      // offset at which the frame applies.  This is caluclated by
      // by adding offset_delta + 1 to the bytecode offset of the
      // previous frame, unless the previous frame is the initial
      // frame of the method, in which case the bytecode offset is
      // offset_delta. (From the Java Virual Machine Specification,
      // Java SE 7 Edition, section 4.7.4)

      // Since we are inserting (1 or 2) new stack map frames at the
      // beginning of the stack map table, we need to adjust the
      // offset_delta of the original first stack map frame due to
      // the fact that it will no longer be the first entry.  We must
      // subtract 1. BUT, if the original first entry has an offset
      // of 0 (because bytecode address 0 is a branch target) then
      // we must delete it as it will be replaced by the new frames
      // we are adding.  (did you get all of that? - markro)

      if (stack_map_table[0].getByteCodeOffset() == 0) {
        skipFirst = true;
      } else {
        stack_map_table[0].updateByteCodeOffset(-1);
      }
    }

    int new_table_length = stack_map_table.length + ((len_part2 > 0) ? 2 : 1) - (skipFirst ? 1 : 0);
    StackMapEntry[] new_map = new StackMapEntry[new_table_length];
    StackMapType nonce_type = new StackMapType(Const.ITEM_Integer, -1, pgen.getConstantPool());
    StackMapType[] old_nonce_type = {nonce_type};
    new_map[0] =
        new StackMapEntry(
            Const.APPEND_FRAME, len_part1, old_nonce_type, null, pgen.getConstantPool());

    int new_index = 1;
    if (len_part2 > 0) {
      // We need to check for len_part2 being too large,
      // Daikon issue #30.
      new_map[1] =
          new StackMapEntry(
              ((len_part2 - 1) > Const.SAME_FRAME_MAX
                  ? Const.SAME_FRAME_EXTENDED
                  : Const.SAME_FRAME + len_part2 - 1),
              len_part2 - 1,
              null,
              null,
              pgen.getConstantPool());
      new_index++;
    }

    // We cannot just copy over the existing stack map entires.  If any of them
    // are FULL_FAME we need to add our 'nonce' variable to the local table.

    for (int i = (skipFirst ? 1 : 0); i < stack_map_table.length; i++) {
      if (stack_map_table[i].getFrameType() == Const.FULL_FRAME) {

        // need to add our 'nonce' variable into the list of locals
        // We must account for the args and this pointer which
        // means insert type of 'nonce' at 'nonce_index'

        int num_locals = stack_map_table[i].getNumberOfLocals();
        // if num_locals < insert index nothing to do
        // (can this happen?)
        if (num_locals >= nonce_index) {
          StackMapType[] old_local_types = stack_map_table[i].getTypesOfLocals();
          StackMapType[] new_local_types = new StackMapType[num_locals + 1];

          for (int j = num_locals; j > nonce_index; j--) {
            new_local_types[j] = old_local_types[j - 1];
          }
          new_local_types[nonce_index] = nonce_type;
          for (int j = nonce_index; j != 0; j--) {
            new_local_types[j - 1] = old_local_types[j - 1];
          }

          //stack_map_table[i].setNumberOfLocals(num_locals+1);
          stack_map_table[i].setTypesOfLocals(new_local_types);
        }
      }
      new_map[new_index++] = stack_map_table[i];
    }
    stack_map_table = new_map;
  }

  /**
   * Pushes the object, nonce, parameters, and return value on the stack and calls the specified
   * Method (normally enter or exit) in Runtime. The parameters are passed as an array of objects.
   * Any primitive values are wrapped in the appropriate Runtime wrapper (IntWrap, FloatWrap, etc).
   */
  private InstructionList call_enter_exit(MethodContext c, String method_name, int line) {

    InstructionList il = new InstructionList();
    InstructionFactory ifact = c.ifact;
    MethodGen mgen = c.mgen;
    Type[] arg_types = mgen.getArgumentTypes();

    // aload
    // Push the object.  Null if this is a static method or a constructor
    if (mgen.isStatic() || (method_name.equals("enter") && is_constructor(mgen))) {
      il.append(new ACONST_NULL());
    } else { // must be an instance method
      il.append(InstructionFactory.createLoad(Type.OBJECT, 0));
    }

    // Determine the offset of the first parameter
    int param_offset = 1;
    if (c.mgen.isStatic()) param_offset = 0;

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
    Type object_arr_typ = new ArrayType("java.lang.Object", 1);
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
    if (method_name.equals("exit")) {
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
    if (method_name.equals("exit")) {
      method_args =
          new Type[] {Type.OBJECT, Type.INT, Type.INT, object_arr_typ, Type.OBJECT, Type.INT};
    } else method_args = new Type[] {Type.OBJECT, Type.INT, Type.INT, object_arr_typ};
    il.append(
        c.ifact.createInvoke(
            runtime_classname, method_name, Type.VOID, method_args, Const.INVOKESTATIC));

    return il;
  }

  /**
   * Creates code to put the local var/param at the specified var_index into a wrapper appropriate
   * for prim_type. prim_type should be one of the basic types (eg, Type.INT, Type.FLOAT, etc). The
   * wrappers are those defined in Runtime.
   *
   * <p>The stack is left with a pointer to the newly created wrapper at the top.
   */
  private InstructionList create_wrapper(MethodContext c, Type prim_type, int var_index) {

    String wrapper = null;
    switch (prim_type.getType()) {
      case Const.T_BOOLEAN:
        wrapper = "BooleanWrap";
        break;
      case Const.T_BYTE:
        wrapper = "ByteWrap";
        break;
      case Const.T_CHAR:
        wrapper = "CharWrap";
        break;
      case Const.T_DOUBLE:
        wrapper = "DoubleWrap";
        break;
      case Const.T_FLOAT:
        wrapper = "FloatWrap";
        break;
      case Const.T_INT:
        wrapper = "IntWrap";
        break;
      case Const.T_LONG:
        wrapper = "LongWrap";
        break;
      case Const.T_SHORT:
        wrapper = "ShortWrap";
        break;
      default:
        throw new Error("unexpected type " + prim_type);
    }

    InstructionList il = new InstructionList();
    String classname = runtime_classname + "$" + wrapper;
    il.append(c.ifact.createNew(classname));
    il.append(InstructionFactory.createDup(Type.OBJECT.getSize()));
    il.append(InstructionFactory.createLoad(prim_type, var_index));
    il.append(
        c.ifact.createInvoke(
            classname, "<init>", Type.VOID, new Type[] {prim_type}, Const.INVOKESPECIAL));

    return il;
  }

  /**
   * Returns true iff mgen is a constructor
   *
   * @return true iff mgen is a constructor
   */
  /*@Pure*/
  private boolean is_constructor(MethodGen mgen) {

    if (mgen.getName().equals("<init>") || mgen.getName().equals("")) {
      // log ("method '%s' is a constructor%n", mgen.getName());
      return true;
    } else {
      return false;
    }
  }

  /**
   * Return an array of strings, each corresponding to mgen's argument types
   *
   * @return an array of strings, each corresponding to mgen's argument types
   */
  private /*@BinaryName*/ String[] getArgTypes(MethodGen mgen) {

    Type[] arg_types = mgen.getArgumentTypes();
    /*@BinaryName*/ String[] arg_type_strings = new /*@BinaryName*/ String[arg_types.length];

    for (int ii = 0; ii < arg_types.length; ii++) {
      Type t = arg_types[ii];
      /*if (t instanceof ObjectType)
        arg_type_strings[ii] = ((ObjectType) t).getClassName();
        else {
        arg_type_strings[ii] = t.getSignature().replace('/', '.');
        }
      */
      arg_type_strings[ii] = t.toString();
    }

    return arg_type_strings;
  }

  @SuppressWarnings("signature") // conversion routine
  private static /*@ClassGetName*/ String typeToClassGetName(Type t) {

    if (t instanceof ObjectType) {
      return ((ObjectType) t).getClassName();
    } else if (t instanceof BasicType) {
      // Use reserved keyword for basic type rather than signature to
      // avoid conflicts with user defined types. Daikon issue #10.
      return t.toString();
    } else {
      // Array type: just convert '/' to '.'
      return t.getSignature().replace('/', '.');
    }
  }

  // creates a MethodInfo struct corresponding to mgen
  @SuppressWarnings("unchecked")
  private /*@Nullable*/ MethodInfo create_method_info(ClassInfo class_info, MethodGen mgen) {

    // Get the argument names for this method
    String[] arg_names = mgen.getArgumentNames();
    LocalVariableGen[] lvs = mgen.getLocalVariables();
    int param_offset = 1;
    if (mgen.isStatic()) param_offset = 0;
    if (debug) {
      out.format("create_method_info1 %s%n", arg_names.length);
      for (int ii = 0; ii < arg_names.length; ii++) {
        out.format("arg: %s%n", arg_names[ii]);
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
        arg_names[0] = mgen.getArgumentType(0).toString() + ".this";
      }
    }

    if (lvs != null) {
      for (int ii = lv_start; ii < arg_names.length; ii++) {
        if ((ii + param_offset) < lvs.length) arg_names[ii] = lvs[ii + param_offset].getName();
      }
    }

    if (debug) {
      out.format("create_method_info2 %s%n", arg_names.length);
      for (int ii = 0; ii < arg_names.length; ii++) {
        out.format("arg: %s%n", arg_names[ii]);
      }
    }

    boolean shouldInclude = false;

    // see if we should filter the entry point
    if (!shouldFilter(
        class_info.class_name,
        mgen.getName(),
        DaikonWriter.methodEntryName(
            class_info.class_name, getArgTypes(mgen), mgen.toString(), mgen.getName()))) {
      shouldInclude = true;
    }
    // Get the argument types for this method
    Type[] arg_types = mgen.getArgumentTypes();
    /*@ClassGetName*/ String[] arg_type_strings = new /*@ClassGetName*/ String[arg_types.length];
    for (int ii = 0; ii < arg_types.length; ii++) {
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

    for (Iterator<InstructionHandle> ii = il.iterator(); ii.hasNext(); ) {
      InstructionHandle ih = ii.next();

      foundLine = false;

      if (ih.hasTargeters()) {
        for (InstructionTargeter it : ih.getTargeters()) {
          if (it instanceof LineNumberGen) {
            LineNumberGen lng = (LineNumberGen) it;
            // log ("  line number at %s: %d%n", ih, lng.getSourceLine());
            // System.out.printf("  line number at %s: %d%n", ih, lng.getSourceLine());
            line_number = lng.getSourceLine();
            foundLine = true;
          }
        }
      }

      switch (ih.getInstruction().getOpcode()) {
        case Const.ARETURN:
        case Const.DRETURN:
        case Const.FRETURN:
        case Const.IRETURN:
        case Const.LRETURN:
        case Const.RETURN:
          // log ("Exit at line %d%n", line_number);

          // only do incremental lines if we don't have the line generator
          if (line_number == last_line_number && foundLine == false) {
            // System.out.printf("Could not find line... at %d%n", line_number);
            line_number++;
          }

          last_line_number = line_number;

          if (!shouldFilter(
              class_info.class_name,
              mgen.getName(),
              DaikonWriter.methodExitName(
                  class_info.class_name,
                  getArgTypes(mgen),
                  mgen.toString(),
                  mgen.getName(),
                  line_number))) {
            shouldInclude = true;
            exit_locs.add(new Integer(line_number));

            isIncluded.add(true);
          } else {
            isIncluded.add(false);
          }

          break;

        default:
          break;
      }
    }

    if (shouldInclude) {
      return new MethodInfo(
          class_info, mgen.getName(), arg_names, arg_type_strings, exit_locs, isIncluded);
    } else {
      return null;
    }
  }

  /**
   * Under some circumstances, there may be problems with the local variable table.
   *
   * <ol>
   *   <li> In some special cases where parameters are added by the Java compiler (eg, constructors
   *       for inner classes), the local variable table is missing the entry for this additional
   *       parameter.
   *   <li> The Java compiler allocates unnamed local temps for:
   *       <ul>
   *         <li>saving the exception in a finally clause
   *         <li>the lock for a synchronized block
   *         <li>(others?)
   *       </ul>
   *       We will create a 'fake' local for these cases.
   * </ol>
   */
  protected void fix_local_variable_table(MethodGen mg) {

    InstructionList il = mg.getInstructionList();

    // Get the current local variables (includes 'this' and parameters)
    LocalVariableGen[] locals = mg.getLocalVariables();
    LocalVariableGen l;
    LocalVariableGen new_lvg;

    // We need a deep copy
    for (int ii = 0; ii < locals.length; ii++) {
      locals[ii] = (LocalVariableGen) (locals[ii].clone());
    }

    // The arg types are correct and include all parameters.
    Type[] arg_types = mg.getArgumentTypes();

    // Initial offset into the stack frame of the first parameter
    int offset = 0;

    // Index into locals of the first parameter
    int loc_index = 0;

    // The first 'true' local index into the local variables.
    int first_local_index = 0;

    // Remove the existing locals
    mg.removeLocalVariables();
    // Reset MaxLocals to 0 and let code below rebuild it.
    mg.setMaxLocals(0);

    if (!mg.isStatic()) {
      // Add the 'this' pointer argument back in.
      l = locals[0];
      new_lvg =
          mg.addLocalVariable(l.getName(), l.getType(), l.getIndex(), l.getStart(), l.getEnd());
      if (debug) {
        out.format(
            "Added <this> %s%n",
            new_lvg.getIndex() + ": " + new_lvg.getName() + ", " + new_lvg.getType());
      }
      loc_index = 1;
      offset = 1;
      first_local_index++;
    }

    // Loop through each argument
    for (int ii = 0; ii < arg_types.length; ii++) {

      // If this parameter doesn't have a matching local
      if ((loc_index >= locals.length) || (offset != locals[loc_index].getIndex())) {

        // Create a local variable to describe the missing argument
        new_lvg = mg.addLocalVariable("$hidden$" + offset, arg_types[ii], offset, null, null);
      } else {
        l = locals[loc_index];
        new_lvg =
            mg.addLocalVariable(l.getName(), l.getType(), l.getIndex(), l.getStart(), l.getEnd());
        loc_index++;
      }
      if (debug) {
        out.format(
            "Added param  %s%n",
            new_lvg.getIndex() + ": " + new_lvg.getName() + ", " + new_lvg.getType());
      }
      offset += arg_types[ii].getSize();
      first_local_index++;
    }

    // Add back the true locals
    //
    // NOTE that the Java compiler uses unnamed local temps for:
    // saving the exception in a finally clause
    // the lock for a synchronized block
    // (others?)
    // We will create a 'fake' local for these cases.

    for (int ii = first_local_index; ii < locals.length; ii++) {
      l = locals[ii];
      if (l.getIndex() > offset) {
        // if offset is 0, probably a lock object
        // there is hidden compiler temp before the next local
        // We set its lifetime to start+1,end to make sure our
        // local_nonce variable is allocated prior to this temp.
        new_lvg =
            mg.addLocalVariable(
                "DaIkOnTeMp" + offset, Type.INT, offset, (il.getStart()).getNext(), il.getEnd());
        ii--; // need to revisit same local
      } else {
        new_lvg =
            mg.addLocalVariable(l.getName(), l.getType(), l.getIndex(), l.getStart(), l.getEnd());
      }
      if (debug) {
        out.format(
            "Added local  %s%n",
            new_lvg.getIndex() + ": " + new_lvg.getName() + ", " + new_lvg.getType());
      }
      offset = offset + (new_lvg.getType()).getSize();
    }

    // Recalculate the highest local used based on looking at code offsets.
    mg.setMaxLocals();
  }

  public void dump_code_attributes(MethodGen mg) {
    // mg.getMethod().getCode().getAttributes() forces attributes
    // to be instantiated; mg.getCodeAttributes() does not
    for (Attribute a : mg.getMethod().getCode().getAttributes()) {
      int con_index = a.getNameIndex();
      Constant c = pgen.getConstant(con_index);
      String att_name = ((ConstantUtf8) c).getBytes();
      out.format("Attribute Index: %s Name: %s%n", con_index, att_name);
    }
  }

  public Attribute get_stack_map_table_attribute(MethodGen mg) {
    for (Attribute a : mg.getCodeAttributes()) {
      if (is_stack_map_table(a)) {
        return a;
      }
    }
    return null;
  }

  public Attribute get_local_variable_type_table_attribute(MethodGen mg) {
    for (Attribute a : mg.getCodeAttributes()) {
      if (is_local_variable_type_table(a)) {
        return a;
      }
    }
    return null;
  }

  /*@Pure*/
  public boolean is_local_variable_type_table(Attribute a) {
    return (get_attribute_name(a).equals("LocalVariableTypeTable"));
  }

  /*@Pure*/
  public boolean is_stack_map_table(Attribute a) {
    return (get_attribute_name(a).equals("StackMapTable"));
  }

  /** Returns the attribute name for the specified attribute. */
  public String get_attribute_name(Attribute a) {

    int con_index = a.getNameIndex();
    Constant c = pgen.getConstant(con_index);
    //     out.format ("get_attribute_name %s %s %s%n", a, con_index, c);
    String att_name = ((ConstantUtf8) c).getBytes();
    return att_name;
  }

  /** Any information needed by InstTransform routines about the method and class. */
  private static class MethodContext {

    public ClassGen cg;
    public ConstantPoolGen cpg;
    public InstructionFactory ifact;
    public MethodGen mgen;

    public MethodContext(ClassGen cg) {
      this.cg = cg;
      ifact = new InstructionFactory(cg);
      cpg = cg.getConstantPool();
    }

    public MethodContext(ClassGen cg, MethodGen mgen) {
      this.cg = cg;
      ifact = new InstructionFactory(cg);
      cpg = cg.getConstantPool();
      this.mgen = mgen;
    }
  }

  /**
   * Returns whether or not the specified class is part of chicory itself (and thus should not be
   * instrumented). Some Daikon classes that are used by Chicory are included here as well.
   */
  /*@Pure*/
  private static boolean is_chicory(String classname) {

    if (classname.startsWith("daikon/chicory") && !classname.equals("daikon/chicory/Test")) {
      return true;
    }
    if (classname.equals("daikon/PptTopLevel$PptType")) return true;
    if (classname.startsWith("daikon/util/UtilMDE")) return true;
    return false;
  }
}
