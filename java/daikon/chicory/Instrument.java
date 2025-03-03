package daikon.chicory;

import daikon.Chicory;
import daikon.plumelib.bcelutil.BcelUtil;
import daikon.plumelib.bcelutil.InstructionListUtils;
import daikon.plumelib.bcelutil.SimpleLog;
import daikon.plumelib.reflection.Signatures;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.lang.instrument.ClassFileTransformer;
import java.lang.instrument.IllegalClassFormatException;
import java.security.ProtectionDomain;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.bcel.Const;
import org.apache.bcel.classfile.Attribute;
import org.apache.bcel.classfile.ClassParser;
import org.apache.bcel.classfile.Constant;
import org.apache.bcel.classfile.ConstantUtf8;
import org.apache.bcel.classfile.ConstantValue;
import org.apache.bcel.classfile.Field;
import org.apache.bcel.classfile.JavaClass;
import org.apache.bcel.classfile.Method;
import org.apache.bcel.classfile.StackMapEntry;
import org.apache.bcel.classfile.StackMapType;
import org.apache.bcel.generic.ACONST_NULL;
import org.apache.bcel.generic.ArrayType;
import org.apache.bcel.generic.BasicType;
import org.apache.bcel.generic.ClassGen;
import org.apache.bcel.generic.ConstantPoolGen;
import org.apache.bcel.generic.Instruction;
import org.apache.bcel.generic.InstructionFactory;
import org.apache.bcel.generic.InstructionHandle;
import org.apache.bcel.generic.InstructionList;
import org.apache.bcel.generic.InstructionTargeter;
import org.apache.bcel.generic.LineNumberGen;
import org.apache.bcel.generic.LocalVariableGen;
import org.apache.bcel.generic.MethodGen;
import org.apache.bcel.generic.ObjectType;
import org.apache.bcel.generic.PUSH;
import org.apache.bcel.generic.Type;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.checkerframework.checker.signature.qual.BinaryName;
import org.checkerframework.checker.signature.qual.ClassGetName;
import org.checkerframework.checker.signature.qual.InternalForm;
import org.checkerframework.dataflow.qual.Pure;

/**
 * The Instrument class is responsible for modifying another class's bytecodes. Specifically, its
 * main task is to add calls into the Chicory Runtime at method entries and exits for
 * instrumentation purposes. These added calls are sometimes referred to as "hooks".
 */
@SuppressWarnings("nullness")
public class Instrument extends InstructionListUtils implements ClassFileTransformer {

  /** Directory for debug output. */
  File debug_dir;

  /** Directory into which to dump debug-instrumented classes. */
  File debug_bin_dir;

  /** Directory into which to dump original classes. */
  File debug_orig_dir;

  /** The index of this method in SharedData.methods. */
  int cur_method_info_index = 0;

  /** The location of the runtime support class. */
  private static final String runtime_classname = "daikon.chicory.Runtime";

  /** Debug information about which classes are transformed and why. */
  public static SimpleLog debug_transform = new SimpleLog(false);

  /** Current class name in binary format. */
  @BinaryName String binaryClassName;

  /** Create a new Instrument. Sets up debug logging. */
  public Instrument() {
    super();
    debug_transform.enabled = Chicory.debug_transform;
    debugInstrument.enabled = Chicory.debug;

    debug_dir = Chicory.debug_dir;
    debug_bin_dir = new File(debug_dir, "bin");
    debug_orig_dir = new File(debug_dir, "orig");

    if (Chicory.dump) {
      debug_bin_dir.mkdirs();
      debug_orig_dir.mkdirs();
    }
  }

  /**
   * Returns true if the given ppt should be ignored. Uses the patterns in {@link
   * daikon.chicory.Runtime#ppt_omit_pattern} and {@link daikon.chicory.Runtime#ppt_select_pattern}.
   * This method is used by both Chicory and Dyncomp.
   *
   * @param className class name to be checked
   * @param methodName method name to be checked
   * @param pptName ppt name to be checked
   * @return true if the item should be filtered out
   */
  public static boolean shouldIgnore(String className, String methodName, String pptName) {

    // Don't instrument the class if it matches an excluded regular expression.
    for (Pattern pattern : Runtime.ppt_omit_pattern) {

      Matcher mPpt = pattern.matcher(pptName);
      Matcher mClass = pattern.matcher(className);
      Matcher mMethod = pattern.matcher(methodName);

      if (mPpt.find() || mClass.find() || mMethod.find()) {
        debug_transform.log("ignoring %s, it matches ppt_omit regex %s%n", pptName, pattern);
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

        if (mPpt.find() || mClass.find() || mMethod.find()) {
          debug_transform.log("including %s, it matches ppt_select regex %s%n", pptName, pattern);
          return false;
        }
      }
    }

    // If we're here, this ppt is not explicitly included or excluded,
    // so keep unless there were items in the "include only" list.
    if (Runtime.ppt_select_pattern.size() > 0) {
      debug_transform.log("ignoring %s, not included in ppt_select pattern(s)%n", pptName);
      return true;
    } else {
      debug_transform.log("including %s, not included in ppt_omit pattern(s)%n", pptName);
      return false;
    }
  }

  /**
   * Given a class, return a transformed version of the class that contains "hooks" at method
   * entries and exits. Because Chicory is invoked as a javaagent, the transform method is called by
   * the Java runtime each time a new class is loaded. A return value of null leaves the byte codes
   * unchanged.
   */
  @Override
  public byte @Nullable [] transform(
      ClassLoader loader,
      @InternalForm String className,
      Class<?> classBeingRedefined,
      ProtectionDomain protectionDomain,
      byte[] classfileBuffer)
      throws IllegalClassFormatException {

    binaryClassName = Signatures.internalFormToBinaryName(className);

    // for debugging
    // new Throwable().printStackTrace();

    debug_transform.log("In chicory.Instrument.transform(): class = %s%n", className);

    // Don't instrument boot classes.  They are uninteresting and will not be able to access
    // daikon.chicory.Runtime (because it is not on the boot classpath).  Previously this code
    // skipped classes that started with java, com, javax, or sun, but this is not correct in many
    // cases.  Most boot classes have the null loader, but some generated classes (such as those in
    // sun.reflect) will have a non-null loader.  Some of these have a null parent loader, but some
    // do not.  The check for the sun.reflect package is a hack to catch all of these.  A more
    // consistent mechanism to determine boot classes would be preferrable.
    if (Chicory.boot_classes != null) {
      Matcher matcher = Chicory.boot_classes.matcher(binaryClassName);
      if (matcher.find()) {
        debug_transform.log(
            "ignoring boot class %s, matches boot_classes regex%n", binaryClassName);
        return null;
      }
    } else if (loader == null) {
      debug_transform.log("ignoring system class %s, class loader == null%n", binaryClassName);
      return null;
    } else if (loader.getParent() == null) {
      debug_transform.log("ignoring system class %s, parent loader == null%n", binaryClassName);
      return null;
    } else if (binaryClassName.startsWith("sun.reflect")) {
      debug_transform.log("ignoring system class %s, in sun.reflect package%n", binaryClassName);
      return null;
    } else if (binaryClassName.startsWith("jdk.internal.reflect")) {
      // Starting with Java 9 sun.reflect => jdk.internal.reflect.
      debug_transform.log(
          "ignoring system class %s, in jdk.internal.reflect package", binaryClassName);
      return null;
    } else if (binaryClassName.startsWith("com.sun")) {
      debug_transform.log("Class from com.sun package %s with nonnull loaders%n", binaryClassName);
    }

    // Don't instrument our own code.
    if (isChicory(className)) {
      debug_transform.log("Not transforming Chicory class %s%n", binaryClassName);
      return null;
    }

    debug_transform.log(
        "transforming class %s, loader %s - %s%n", className, loader, loader.getParent());

    // Parse the bytes of the classfile, die on any errors.
    JavaClass c;
    try (ByteArrayInputStream bais = new ByteArrayInputStream(classfileBuffer)) {
      ClassParser parser = new ClassParser(bais, className);
      c = parser.parse();
    } catch (Throwable t) {
      System.err.printf("Unexpected error %s reading in %s%n", t, binaryClassName);
      t.printStackTrace();
      // No changes to the bytecodes
      return null;
    }

    if (Chicory.dump) {
      try {
        debugInstrument.log(
            "Dumping .class and .javap for %s to %s%n", binaryClassName, debug_orig_dir);
        // Write the byte array to a .class file.
        c.dump(new File(debug_orig_dir, c.getClassName() + ".class"));
        // Write a BCEL-like file with an extension of .javap.
        BcelUtil.dump(c, debug_orig_dir);
      } catch (Throwable t) {
        System.err.printf(
            "Unexpected error %s dumping out debug files for: %s%n", t, binaryClassName);
        t.printStackTrace();
        // proceed with instrumentation
      }
    }

    JavaClass njc;
    ClassInfo classInfo;

    try {
      // Get the class information
      ClassGen cg = new ClassGen(c);

      // Convert reach non-void method to save its result in a local before returning.
      classInfo = instrument_all_methods(cg, binaryClassName, loader);

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

          if (valString != null) {
            classInfo.staticMap.put(field.getName(), valString);
          }
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
          cg.addMethod(createClinit(cg, binaryClassName));
        }
      }

      njc = cg.getJavaClass();

    } catch (Throwable t) {
      System.out.printf("Unexpected error %s in transform of %s%n", t, binaryClassName);
      t.printStackTrace();
      // No changes to the bytecodes
      return null;
    }

    if (classInfo.shouldInclude) {
      if (Chicory.dump) {
        try {
          debugInstrument.log("Dumping %s to %s%n", binaryClassName, debug_bin_dir);
          // write .class file
          njc.dump(new File(debug_bin_dir, c.getClassName() + ".class"));
          // write .bcel file
          BcelUtil.dump(njc, debug_bin_dir);
        } catch (Throwable t) {
          System.err.printf("Unexpected error %s dumping out debug files for: %s%n", t, className);
          t.printStackTrace();
          // proceed with instrumentation
        }
      }
      return njc.getBytes();
    } else {
      // No changes to the bytecodes
      return null;
    }
  }

  /**
   * Adds a call (or calls) to the Chicory Runtime {@code initNotify} method into a given method.
   * Clients pass the class static initializer as the method.
   *
   * @param cg a class
   * @param mg the method to modify, typically the class static initializer
   * @param fullClassName fully-qualified class name
   * @return the modified method
   */
  private Method addInvokeToClinit(ClassGen cg, MethodGen mg, String fullClassName) {

    try {
      InstructionList il = mg.getInstructionList();
      setCurrentStackMapTable(mg, cg.getMajor());
      MethodContext context = new MethodContext(cg, mg);

      for (InstructionHandle ih = il.getStart(); ih != null; ) {
        Instruction inst = ih.getInstruction();

        // Get the translation for this instruction (if any)
        InstructionList new_il = xform_clinit(cg.getConstantPool(), fullClassName, inst, context);

        // Remember the next instruction to process
        InstructionHandle next_ih = ih.getNext();

        // will do nothing if new_il == null
        insertBeforeHandle(mg, ih, new_il, false);

        // Go on to the next instruction in the list
        ih = next_ih;
      }

      remove_local_variable_type_table(mg);
      createNewStackMapAttribute(mg);

      // Update the max stack and Max Locals
      mg.setMaxLocals();
      mg.setMaxStack();
      mg.update();
    } catch (Exception e) {
      System.out.printf("Unexpected exception encountered: %s", e);
      e.printStackTrace();
    }

    return mg.getMethod();
  }

  /**
   * Called by {@link #addInvokeToClinit} to obtain the instructions that represent a call to the
   * Chicory Runtime {@code initNotify} method prior to a return opcode. Returns null if the given
   * instruction is not a return.
   *
   * @param cp ConstantPoolGen for current class
   * @param fullClassName the fully-qualified class name
   * @param inst the instruction that might be a return
   * @param context MethodContext for current method
   * @return the list of instructions that call {@code initNotify}, or null if {@code inst} is not a
   *     return instruction
   */
  private @Nullable InstructionList xform_clinit(
      ConstantPoolGen cp, String fullClassName, Instruction inst, MethodContext context) {

    switch (inst.getOpcode()) {
      case Const.ARETURN:
      case Const.DRETURN:
      case Const.FRETURN:
      case Const.IRETURN:
      case Const.LRETURN:
      case Const.RETURN:
        return call_initNotify(cp, fullClassName, context.ifact);

      default:
        return null;
    }
  }

  /**
   * Create a class initializer method, if none exists. We need a class initializer to have a place
   * to insert a call to the Chicory Runtime {@code initNotifiy()} method.
   *
   * @param cg a class
   * @param fullClassName the fully-qualified name of {@code cg}
   * @return the modified method
   */
  private Method createClinit(ClassGen cg, @BinaryName String fullClassName) {
    InstructionFactory factory = new InstructionFactory(cg);

    InstructionList il = new InstructionList();
    il.append(call_initNotify(cg.getConstantPool(), fullClassName, factory));
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

    // Update the max stack and Max Locals
    newMethGen.setMaxLocals();
    newMethGen.setMaxStack();
    newMethGen.update();

    return newMethGen.getMethod();
  }

  /**
   * Create the list of instructions for a call to {@code initNotify}.
   *
   * @param cp ConstantPoolGen for current class
   * @param fullClassName the fully-qualified class name
   * @param factory InstructionFactory for current class
   * @return the instruction list
   */
  private InstructionList call_initNotify(
      ConstantPoolGen cp, String fullClassName, InstructionFactory factory) {

    InstructionList instructions = new InstructionList();

    instructions.append(new PUSH(cp, fullClassName));
    instructions.append(
        factory.createInvoke(
            runtime_classname,
            "initNotify",
            Type.VOID,
            new Type[] {Type.STRING},
            Const.INVOKESTATIC));

    // System.out.println(fullClassName + " --- " + instructions.size());
    return instructions;
  }

  /**
   * Instrument all the methods in a class. For each method, add instrumentation code at the entry
   * and at each return from the method. In addition, changes each return statement to first place
   * the value being returned into a local and then return. This allows us to work around the JDI
   * deficiency of not being able to query return values.
   *
   * @param cg ClassGen for current class
   * @param fullClassName the fully qualified class name
   * @param loader ClassLoader for current class
   * @return ClassInfo for current class
   */
  private ClassInfo instrument_all_methods(ClassGen cg, String fullClassName, ClassLoader loader) {

    ClassInfo classInfo = new ClassInfo(cg.getClassName(), loader);
    List<MethodInfo> method_infos = new ArrayList<>();

    if (cg.getMajor() < Const.MAJOR_1_6) {
      System.out.printf(
          "Chicory warning: ClassFile: %s - classfile version (%d) is out of date and may not be"
              + " processed correctly.%n",
          cg.getClassName(), cg.getMajor());
    }

    boolean shouldInclude = false;

    try {
      for (Method m : cg.getMethods()) {

        // The class data in StackMapUtils is not thread safe,
        // allow only one method at a time to be instrumented.
        // DynComp does this by creating a new instrumentation object
        // for each class - probably a cleaner solution.
        synchronized (this) {
          pool = cg.getConstantPool();
          MethodGen mgen = new MethodGen(m, cg.getClassName(), pool);
          MethodContext context = new MethodContext(cg, mgen);

          // check for the class static initializer method
          if (mgen.getName().equals("<clinit>")) {
            if (Chicory.checkStaticInit) {
              cg.replaceMethod(m, addInvokeToClinit(cg, mgen, fullClassName));
              cg.update();
            }
            if (!Chicory.instrument_clinit) {
              continue;
            }
          }

          // If method is synthetic... (default constructors and <clinit> are not synthetic)
          if ((Const.ACC_SYNTHETIC & mgen.getAccessFlags()) > 0) {
            continue;
          }

          // Get the instruction list and skip methods with no instructions
          InstructionList il = mgen.getInstructionList();
          if (il == null) {
            continue;
          }

          if (debugInstrument.enabled) {
            Type[] arg_types = mgen.getArgumentTypes();
            String[] paramNames = mgen.getArgumentNames();
            LocalVariableGen[] local_vars = mgen.getLocalVariables();
            String types = "", names = "", locals = "";

            for (int j = 0; j < arg_types.length; j++) {
              types = types + arg_types[j] + " ";
            }
            for (int j = 0; j < paramNames.length; j++) {
              names = names + paramNames[j] + " ";
            }
            for (int j = 0; j < local_vars.length; j++) {
              locals = locals + local_vars[j].getName() + " ";
            }
            debugInstrument.log("%nMethod = %s%n", mgen);
            debugInstrument.log("arg_types(%d): %s%n", arg_types.length, types);
            debugInstrument.log("paramNames(%d): %s%n", paramNames.length, names);
            debugInstrument.log("localvars(%d): %s%n", local_vars.length, locals);
            debugInstrument.log("Original code: %s%n", mgen.getMethod().getCode());
            debugInstrument.log("%n");
          }

          // Get existing StackMapTable (if present)
          setCurrentStackMapTable(mgen, cg.getMajor());

          fixLocalVariableTable(mgen);

          // Create a MethodInfo that describes this methods arguments
          // and exit line numbers (information not available via reflection)
          // and add it to the list for this class.
          MethodInfo curMethodInfo = create_method_info(classInfo, mgen);

          printStackMapTable("After create_method_info");

          if (curMethodInfo == null) { // method filtered out!
            continue;
          }

          shouldInclude = true; // at least one method not filtered out

          // Create a map of Uninitialized_variable_info offsets to
          // InstructionHandles.  We will use this map after we
          // complete instrumentation to update the offsets due
          // to code modification and expansion.
          // The offsets point to 'new' instructions; since we do
          // not modify these, their Instruction Handles will remain
          // unchanged throught the instrumentaion process.
          buildUninitializedNewMap(il);

          method_infos.add(curMethodInfo);

          synchronized (SharedData.methods) {
            cur_method_info_index = SharedData.methods.size();
            SharedData.methods.add(curMethodInfo);
          }

          // Add nonce local to matchup enter/exits
          add_entry_instrumentation(il, context);

          printStackMapTable("After add_entry_instrumentation");

          debugInstrument.log("Modified code: %s%n", mgen.getMethod().getCode());

          // Need to see if there are any switches after this location.
          // If so, we may need to update the corresponding stackmap if
          // the amount of the switch padding changed.
          modifyStackMapsForSwitches(il.getStart(), il);

          Iterator<Boolean> shouldIncludeIter = curMethodInfo.is_included.iterator();
          Iterator<Integer> exitLocationIter = curMethodInfo.exit_locations.iterator();

          // Loop through each instruction looking for the return(s)
          for (InstructionHandle ih = il.getStart(); ih != null; ) {
            Instruction inst = ih.getInstruction();

            // If this is a return instruction, insert method exit instrumentation
            InstructionList new_il =
                generate_return_instrumentation(inst, context, shouldIncludeIter, exitLocationIter);

            // Remember the next instruction to process
            InstructionHandle next_ih = ih.getNext();

            // If this instruction was modified, replace it with the new
            // instruction list. If this instruction was the target of any
            // jumps, replace it with the first instruction in the new list
            insertBeforeHandle(mgen, ih, new_il, true);

            // Go on to the next instruction in the list
            ih = next_ih;
          }

          // Update the Uninitialized_variable_info offsets before
          // we write out the new StackMapTable.
          updateUninitializedNewOffsets(il);

          createNewStackMapAttribute(mgen);

          remove_local_variable_type_table(mgen);

          // Update the instruction list
          mgen.setInstructionList(il);
          mgen.update();

          // Update the max stack
          mgen.setMaxStack();
          mgen.update();

          // Update the method in the class
          try {
            cg.replaceMethod(m, mgen.getMethod());
          } catch (Exception e) {
            if (e.getMessage().startsWith("Branch target offset too large")) {
              System.out.printf(
                  "Chicory warning: ClassFile: %s - method %s is too large to instrument and is"
                      + " being skipped.%n",
                  cg.getClassName(), mgen.getName());
              continue;
            } else {
              throw e;
            }
          }

          if (debugInstrument.enabled) {
            debugInstrument.log("Modified code: %s%n", mgen.getMethod().getCode());
            dump_code_attributes(mgen);
          }
          cg.update();
        }
      }
    } catch (Exception e) {
      System.out.printf("Unexpected exception encountered: %s", e);
      e.printStackTrace();
    }

    // Add the class and method information to runtime so it is available
    // as enter/exit ppts are processed.
    classInfo.set_method_infos(method_infos);

    if (shouldInclude) {
      debug_transform.log("Added trace info to class %s%n", classInfo);
      synchronized (SharedData.new_classes) {
        SharedData.new_classes.add(classInfo);
      }
      synchronized (SharedData.all_classes) {
        SharedData.all_classes.add(classInfo);
      }
    } else { // not included
      debug_transform.log("Trace info not added to class %s%n", classInfo);
    }

    classInfo.shouldInclude = shouldInclude;
    return classInfo;
  }

  /**
   * If this is a return instruction, generate a new instruction list to assign the result to a
   * local variable (return__$trace2_val) and then call daikon.chicory.Runtime.exit(). This
   * instruction list wil be inserted immediately before the return.
   *
   * @param inst the instruction to inspect, which might be a return instruction
   * @param c MethodContext for current method
   * @param shouldIncludeIter whether or not to instrument this return
   * @param exitLocationIter list of exit line numbers
   * @return instruction list for instrumenting the return, or null if {@code inst} is not a return
   */
  private @Nullable InstructionList generate_return_instrumentation(
      Instruction inst,
      MethodContext c,
      Iterator<Boolean> shouldIncludeIter,
      Iterator<Integer> exitLocationIter) {

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

    if (!shouldIncludeIter.hasNext()) {
      throw new RuntimeException("Not enough entries in shouldIncludeIter");
    }

    boolean shouldInclude = shouldIncludeIter.next();

    if (!shouldInclude) {
      return null;
    }

    Type type = c.mgen.getReturnType();
    InstructionList newCode = new InstructionList();
    if (type != Type.VOID) {
      LocalVariableGen return_loc = getReturnLocal(c.mgen, type);
      newCode.append(InstructionFactory.createDup(type.getSize()));
      newCode.append(InstructionFactory.createStore(type, return_loc.getIndex()));
    }

    if (!exitLocationIter.hasNext()) {
      throw new RuntimeException("Not enough exit locations in the exitLocationIter");
    }

    newCode.append(callEnterOrExit(c, "exit", exitLocationIter.next()));
    return newCode;
  }

  /**
   * Returns the local variable used to store the return result. If it is not present, creates it
   * with the specified type. If the variable is known to already exist, the type can be null.
   *
   * @param mgen describes the current method
   * @param returnType the type of the return; may be null if the variable is known to already exist
   * @return a local variable to save the return value
   */
  private LocalVariableGen getReturnLocal(MethodGen mgen, @Nullable Type returnType) {

    // Find the local used for the return value
    LocalVariableGen returnLocal = null;
    for (LocalVariableGen lv : mgen.getLocalVariables()) {
      if (lv.getName().equals("return__$trace2_val")) {
        returnLocal = lv;
        break;
      }
    }

    // If a type was specified and the variable was found, they must match.
    if (returnLocal == null) {
      assert returnType != null : " return__$trace2_val doesn't exist";
    } else {
      assert returnType.equals(returnLocal.getType())
          : " returnType = " + returnType + "current type = " + returnLocal.getType();
    }

    if (returnLocal == null) {
      debugInstrument.log("Adding return local of type %s%n", returnType);
      returnLocal = mgen.addLocalVariable("return__$trace2_val", returnType, null, null);
    }

    return returnLocal;
  }

  /**
   * Finds the nonce local variable. Returns null if not present.
   *
   * @param mgen describes the current method
   * @return a local variable to save the nonce value, or null
   */
  private @Nullable LocalVariableGen get_nonce_local(MethodGen mgen) {

    // Find the local used for the nonce value
    for (LocalVariableGen lv : mgen.getLocalVariables()) {
      if (lv.getName().equals("this_invocation_nonce")) {
        return lv;
      }
    }

    return null;
  }

  /**
   * Inserts instrumentation code at the start of the method. This includes adding a local variable
   * (this_invocation_nonce) that is initialized to Runtime.nonce++. This provides a unique id on
   * each method entry/exit that allows them to be matched up from the dtrace file. Inserts code to
   * call daikon.chicory.Runtime.enter().
   *
   * @param instructions instruction list for method
   * @param c MethodContext for method
   * @throws IOException if there is trouble with I/O
   */
  private void add_entry_instrumentation(InstructionList instructions, MethodContext c)
      throws IOException {

    String atomic_int_classname = "java.util.concurrent.atomic.AtomicInteger";
    Type atomic_int_type = new ObjectType(atomic_int_classname);

    InstructionList newCode = new InstructionList();

    // create the nonce local variable
    LocalVariableGen nonce_lv =
        create_method_scope_local(c.mgen, "this_invocation_nonce", Type.INT);

    printStackMapTable("After cln");

    if (debugInstrument.enabled) {
      debugInstrument.log("Modified code: %s%n", c.mgen.getMethod().getCode());
    }

    // The following implements:
    //     this_invocation_nonce = Runtime.nonce++;

    // getstatic Runtime.nonce (load reference to AtomicInteger daikon.chicory.Runtime.nonce)
    newCode.append(c.ifact.createGetStatic(runtime_classname, "nonce", atomic_int_type));

    // Do an atomic get and increment of nonce value.
    // This is multi-thread safe and leaves int value of nonce on stack.
    newCode.append(
        c.ifact.createInvoke(
            atomic_int_classname, "getAndIncrement", Type.INT, new Type[] {}, Const.INVOKEVIRTUAL));

    // istore <lv> (pop original value of nonce into this_invocation_nonce)
    newCode.append(InstructionFactory.createStore(Type.INT, nonce_lv.getIndex()));

    newCode.setPositions();
    InstructionHandle end = newCode.getEnd();
    int len_part1 = end.getPosition() + end.getInstruction().getLength();

    // call Runtime.enter()
    newCode.append(callEnterOrExit(c, "enter", -1));

    newCode.setPositions();
    end = newCode.getEnd();
    int len_part2 = end.getPosition() + end.getInstruction().getLength() - len_part1;

    // Add the new instructions at the start and move any LineNumbers
    // and Local variables to point to them.  Other targeters
    // (branches, exceptions) should still point to the old start
    // NOTE: Don't use insert_at_method_start as it tries to update StackMaps
    // and that will be done with special code below.
    InstructionHandle old_start = instructions.getStart();
    InstructionHandle new_start = instructions.insert(newCode);
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

    // Modify existing StackMapTable (if present)
    if (stackMapTable.length > 0) {
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

      if (stackMapTable[0].getByteCodeOffset() == 0) {
        skipFirst = true;
      } else {
        stackMapTable[0].updateByteCodeOffset(-1);
      }
    }

    // Create new StackMap entries for our instrumentation code.
    int new_table_length = stackMapTable.length + ((len_part2 > 0) ? 2 : 1) - (skipFirst ? 1 : 0);
    StackMapEntry[] new_map = new StackMapEntry[new_table_length];
    StackMapType nonce_type = new StackMapType(Const.ITEM_Integer, -1, pool.getConstantPool());
    StackMapType[] old_nonce_type = {nonce_type};
    new_map[0] =
        new StackMapEntry(
            Const.APPEND_FRAME, len_part1, old_nonce_type, null, pool.getConstantPool());

    int new_index = 1;
    if (len_part2 > 0) {
      new_map[1] =
          new StackMapEntry(
              ((len_part2 - 1) > Const.SAME_FRAME_MAX
                  ? Const.SAME_FRAME_EXTENDED
                  : Const.SAME_FRAME + len_part2 - 1),
              len_part2 - 1,
              null,
              null,
              pool.getConstantPool());
      new_index++;
    }

    // We can just copy the rest of the stack frames over as the FULL_FRAME
    // ones were already updated when the nonce variable was allocated.
    for (int i = (skipFirst ? 1 : 0); i < stackMapTable.length; i++) {
      new_map[new_index++] = stackMapTable[i];
    }
    stackMapTable = new_map;
  }

  /**
   * Pushes the object, nonce, parameters, and return value on the stack and calls the specified
   * Method (normally enter or exit) in daikon.chicory.Runtime. The parameters are passed as an
   * array of objects. Any primitive values are wrapped in the appropriate daikon.chicory.Runtime
   * wrapper (IntWrap, FloatWrap, etc).
   *
   * @param c MethodContext for a method
   * @param callMethod either "enter" or "exit"
   * @param line source line number if this is an exit
   * @return instruction list for instrumenting the enter or exit of the method
   */
  private InstructionList callEnterOrExit(MethodContext c, String callMethod, int line) {

    InstructionList newCode = new InstructionList();
    InstructionFactory ifact = c.ifact;
    MethodGen mgen = c.mgen;
    Type[] paramTypes = mgen.getArgumentTypes();

    // aload
    // Push the object.  Push null if this is a static method or a constructor.
    if (mgen.isStatic() || (callMethod.equals("enter") && isConstructor(mgen))) {
      newCode.append(new ACONST_NULL());
    } else { // must be an instance method
      newCode.append(InstructionFactory.createLoad(Type.OBJECT, 0));
    }

    // The offset of the first parameter.
    int param_offset = mgen.isStatic() ? 0 : 1;

    // iload
    // Push the nonce.
    LocalVariableGen nonce_lv = get_nonce_local(mgen);
    newCode.append(InstructionFactory.createLoad(Type.INT, nonce_lv.getIndex()));

    // iconst
    // Push the MethodInfo index.
    newCode.append(ifact.createConstant(cur_method_info_index));

    // iconst
    // anewarray
    // Create an array of objects with elements for each parameter.
    newCode.append(ifact.createConstant(paramTypes.length));
    Type object_arr_typ = new ArrayType("java.lang.Object", 1);
    newCode.append(ifact.createNewArray(Type.OBJECT, (short) 1));

    // Put each parameter into the array.
    int param_index = param_offset;
    for (int ii = 0; ii < paramTypes.length; ii++) {
      newCode.append(InstructionFactory.createDup(object_arr_typ.getSize()));
      newCode.append(ifact.createConstant(ii));
      Type at = paramTypes[ii];
      if (at instanceof BasicType) {
        newCode.append(create_wrapper(c, at, param_index));
      } else { // must be reference of some sort
        newCode.append(InstructionFactory.createLoad(Type.OBJECT, param_index));
      }
      newCode.append(InstructionFactory.createArrayStore(Type.OBJECT));
      param_index += at.getSize();
    }

    // If this is an exit, push the return value and line number.
    // The return value is stored in the local "return__$trace2_val".
    // If the return value is a primitive, wrap it in the appropriate wrapper.
    if (callMethod.equals("exit")) {
      Type ret_type = mgen.getReturnType();
      if (ret_type == Type.VOID) {
        newCode.append(new ACONST_NULL());
      } else {
        LocalVariableGen returnLocal = getReturnLocal(mgen, ret_type);
        if (ret_type instanceof BasicType) {
          newCode.append(create_wrapper(c, ret_type, returnLocal.getIndex()));
        } else {
          newCode.append(InstructionFactory.createLoad(Type.OBJECT, returnLocal.getIndex()));
        }
      }

      // push line number
      // System.out.println(mgen.getName() + " --> " + line);
      newCode.append(ifact.createConstant(line));
    }

    // Call the specified method
    Type[] methodArgs;
    if (callMethod.equals("exit")) {
      methodArgs =
          new Type[] {Type.OBJECT, Type.INT, Type.INT, object_arr_typ, Type.OBJECT, Type.INT};
    } else {
      methodArgs = new Type[] {Type.OBJECT, Type.INT, Type.INT, object_arr_typ};
    }
    newCode.append(
        c.ifact.createInvoke(
            runtime_classname, callMethod, Type.VOID, methodArgs, Const.INVOKESTATIC));

    return newCode;
  }

  /**
   * Creates code to put the local var/param at the specified var_index into a wrapper appropriate
   * for prim_type. prim_type must be a primitive type (Type.INT, Type.FLOAT, etc.). The wrappers
   * are those defined in daikon.chicory.Runtime.
   *
   * <p>The stack is left with a pointer to the newly created wrapper at the top.
   *
   * @param c MethodContext for curent method
   * @param prim_type the primitive type of the local variable or parameter
   * @param var_index the offset into the local stack of the variable or parameter
   * @return instruction list for putting the primitive in a wrapper
   */
  private InstructionList create_wrapper(MethodContext c, Type prim_type, int var_index) {

    String wrapperClassName;
    switch (prim_type.getType()) {
      case Const.T_BOOLEAN:
        wrapperClassName = "BooleanWrap";
        break;
      case Const.T_BYTE:
        wrapperClassName = "ByteWrap";
        break;
      case Const.T_CHAR:
        wrapperClassName = "CharWrap";
        break;
      case Const.T_DOUBLE:
        wrapperClassName = "DoubleWrap";
        break;
      case Const.T_FLOAT:
        wrapperClassName = "FloatWrap";
        break;
      case Const.T_INT:
        wrapperClassName = "IntWrap";
        break;
      case Const.T_LONG:
        wrapperClassName = "LongWrap";
        break;
      case Const.T_SHORT:
        wrapperClassName = "ShortWrap";
        break;
      default:
        throw new Error("unexpected type " + prim_type);
    }

    InstructionList newCode = new InstructionList();
    String classname = runtime_classname + "$" + wrapperClassName;
    newCode.append(c.ifact.createNew(classname));
    newCode.append(InstructionFactory.createDup(Type.OBJECT.getSize()));
    newCode.append(InstructionFactory.createLoad(prim_type, var_index));
    newCode.append(
        c.ifact.createInvoke(
            classname, "<init>", Type.VOID, new Type[] {prim_type}, Const.INVOKESPECIAL));

    return newCode;
  }

  /**
   * Returns true iff mgen is a constructor.
   *
   * @param mgen describes the current method
   * @return true iff mgen is a constructor
   */
  @Pure
  private boolean isConstructor(MethodGen mgen) {

    if (mgen.getName().equals("<init>") || mgen.getName().equals("")) {
      debugInstrument.log("isConstructor(%s) => true%n", mgen.getName());
      return true;
    } else {
      return false;
    }
  }

  /**
   * Return an array of strings, each corresponding to mgen's parameter types as a fully qualified
   * name: how a type is represented in Java source code.
   *
   * @param mgen describes the current method
   * @return an array of strings, each corresponding to mgen's parameter types
   */
  private @BinaryName String[] getFullyQualifiedParameterTypes(MethodGen mgen) {

    Type[] paramTypes = mgen.getArgumentTypes();
    @BinaryName String[] arg_type_strings = new @BinaryName String[paramTypes.length];

    for (int ii = 0; ii < paramTypes.length; ii++) {
      Type t = paramTypes[ii];
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

  /**
   * Creates a MethodInfo struct corresponding to {@code mgen}.
   *
   * @param classInfo a class
   * @param mgen a method in the given class
   * @return a new MethodInfo for the method, or null if the method should not be instrumented
   */
  @SuppressWarnings("unchecked")
  private @Nullable MethodInfo create_method_info(ClassInfo classInfo, MethodGen mgen) {

    // Get the parameter names for this method.
    String[] paramNames = mgen.getArgumentNames();
    LocalVariableGen[] lvs = mgen.getLocalVariables();
    int param_offset = 1;
    if (mgen.isStatic()) {
      param_offset = 0;
    }
    if (debugInstrument.enabled) {
      debugInstrument.log("create_method_info1 %s%n", paramNames.length);
      for (int ii = 0; ii < paramNames.length; ii++) {
        debugInstrument.log("param: %s%n", paramNames[ii]);
      }
    }

    int lv_start = 0;
    // If this is an inner class constructor, then its first parameter is
    // the outer class constructor.  I need to detect this and adjust the
    // parameter names appropriately.  This check is ugly.
    if (mgen.getName().equals("<init>") && mgen.getArgumentTypes().length > 0) {
      int dollarPos = mgen.getClassName().lastIndexOf("$");
      String arg0Name = mgen.getArgumentType(0).toString();
      if (dollarPos >= 0
          &&
          // type of first parameter is classname up to the "$"
          mgen.getClassName().substring(0, dollarPos).equals(arg0Name)) {
        // As a further check, for javac-generated classfiles, the
        // constant pool index #1 is "this$0", and the first 5 bytes of
        // the bytecode are:
        //   0: aload_0
        //   1: aload_1
        //   2: putfield      #1

        lv_start++;
        paramNames[0] = arg0Name + ".this";
      }
    }

    if (lvs != null) {
      for (int ii = lv_start; ii < paramNames.length; ii++) {
        if ((ii + param_offset) < lvs.length) {
          paramNames[ii] = lvs[ii + param_offset].getName();
        }
      }
    }

    if (debugInstrument.enabled) {
      debugInstrument.log("create_method_info2 %s%n", paramNames.length);
      for (int ii = 0; ii < paramNames.length; ii++) {
        debugInstrument.log("param: %s%n", paramNames[ii]);
      }
    }

    boolean shouldInclude = false;

    // It looks like DaikonWriter.methodEntryName does not use the mgen.toString argument.
    // see if we should track the entry point
    if (!shouldIgnore(
        classInfo.class_name,
        mgen.getName(),
        DaikonWriter.methodEntryName(
            classInfo.class_name,
            getFullyQualifiedParameterTypes(mgen),
            mgen.toString(),
            mgen.getName()))) {
      shouldInclude = true;
    }
    // Get the parameter types for this method.
    Type[] paramTypes = mgen.getArgumentTypes();
    @ClassGetName String[] arg_type_strings = new @ClassGetName String[paramTypes.length];
    for (int ii = 0; ii < paramTypes.length; ii++) {
      arg_type_strings[ii] = typeToClassGetName(paramTypes[ii]);
    }

    // Loop through each instruction and find the line number for each return opcode.
    List<Integer> exit_locs = new ArrayList<>();

    // Tells whether each exit loc in the method is included or not (based on filters).
    List<Boolean> isIncluded = new ArrayList<>();

    debugInstrument.log("Looking for exit points in %s%n", mgen.getName());
    InstructionList il = mgen.getInstructionList();
    int line_number = 0;
    int last_line_number = 0;
    boolean foundLine;

    for (InstructionHandle ih : il) {
      foundLine = false;

      if (ih.hasTargeters()) {
        for (InstructionTargeter it : ih.getTargeters()) {
          if (it instanceof LineNumberGen) {
            LineNumberGen lng = (LineNumberGen) it;
            // debugInstrument.log("  line number at %s: %d%n", ih, lng.getSourceLine());
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
          debugInstrument.log("Exit at line %d%n", line_number);

          // only do incremental lines if we don't have the line generator
          if (line_number == last_line_number && foundLine == false) {
            debugInstrument.log("Could not find line... at %d%n", line_number);
            line_number++;
          }

          last_line_number = line_number;

          if (!shouldIgnore(
              classInfo.class_name,
              mgen.getName(),
              DaikonWriter.methodExitName(
                  classInfo.class_name,
                  getFullyQualifiedParameterTypes(mgen),
                  mgen.toString(),
                  mgen.getName(),
                  line_number))) {
            shouldInclude = true;
            exit_locs.add(line_number);

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
          classInfo, mgen.getName(), paramNames, arg_type_strings, exit_locs, isIncluded);
    } else {
      return null;
    }
  }

  /**
   * Logs a method's attributes.
   *
   * @param mgen a method
   */
  public void dump_code_attributes(MethodGen mgen) {
    // mgen.getMethod().getCode().getAttributes() forces attributes
    // to be instantiated; mgen.getCodeAttributes() does not
    for (Attribute a : mgen.getMethod().getCode().getAttributes()) {
      int con_index = a.getNameIndex();
      Constant c = pool.getConstant(con_index);
      String att_name = ((ConstantUtf8) c).getBytes();
      debugInstrument.log("Attribute Index: %s Name: %s%n", con_index, att_name);
    }
  }

  /** Information needed by InstTransform routines about the method and class. */
  private static class MethodContext {

    /** InstructionFactory for a class. */
    public InstructionFactory ifact;

    /** MethodGen for a method of the class. */
    public MethodGen mgen;

    /**
     * Create a new MethodContext.
     *
     * @param cg ClassGen for a class
     * @param mgen a method of the class
     */
    public MethodContext(ClassGen cg, MethodGen mgen) {
      ifact = new InstructionFactory(cg);
      this.mgen = mgen;
    }
  }

  /**
   * Returns true if the specified class is part of Chicory itself (and thus should not be
   * instrumented). Some Daikon classes that are used by Chicory are included here as well.
   *
   * @param classname the name of the class to test, in internal form
   * @return true if the given class is part of Chicory itself
   */
  @Pure
  private static boolean isChicory(@InternalForm String classname) {

    if (classname.startsWith("daikon/chicory") && !classname.equals("daikon/chicory/ChicoryTest")) {
      return true;
    }
    if (classname.equals("daikon/PptTopLevel$PptType")) {
      return true;
    }
    if (classname.startsWith("daikon/plumelib")) {
      return true;
    }
    return false;
  }
}
