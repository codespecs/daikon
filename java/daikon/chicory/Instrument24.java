package daikon.chicory;

import static java.lang.constant.ConstantDescs.CD_Object;
import static java.lang.constant.ConstantDescs.CD_String;
import static java.lang.constant.ConstantDescs.CD_int;
import static java.lang.constant.ConstantDescs.CD_void;

import daikon.Chicory;
import daikon.plumelib.bcelutil.SimpleLog;
import java.io.BufferedWriter;
import java.io.File;
import java.lang.classfile.Attributes;
import java.lang.classfile.ClassBuilder;
import java.lang.classfile.ClassElement;
import java.lang.classfile.ClassFile;
import java.lang.classfile.ClassHierarchyResolver;
import java.lang.classfile.ClassModel;
import java.lang.classfile.ClassTransform;
import java.lang.classfile.CodeBuilder;
import java.lang.classfile.CodeElement;
import java.lang.classfile.CodeModel;
import java.lang.classfile.FieldModel;
import java.lang.classfile.Instruction;
import java.lang.classfile.Label;
import java.lang.classfile.MethodBuilder;
import java.lang.classfile.MethodElement;
import java.lang.classfile.MethodModel;
import java.lang.classfile.Opcode;
import java.lang.classfile.TypeKind;
import java.lang.classfile.attribute.ConstantValueAttribute;
import java.lang.classfile.constantpool.ConstantPoolBuilder;
import java.lang.classfile.constantpool.ConstantValueEntry;
import java.lang.classfile.constantpool.MethodRefEntry;
import java.lang.classfile.instruction.ArrayStoreInstruction;
import java.lang.classfile.instruction.BranchInstruction;
import java.lang.classfile.instruction.ConstantInstruction;
import java.lang.classfile.instruction.ExceptionCatch;
import java.lang.classfile.instruction.FieldInstruction;
import java.lang.classfile.instruction.InvokeInstruction;
import java.lang.classfile.instruction.LabelTarget;
import java.lang.classfile.instruction.LineNumber;
import java.lang.classfile.instruction.LoadInstruction;
import java.lang.classfile.instruction.LocalVariable;
import java.lang.classfile.instruction.LocalVariableType;
import java.lang.classfile.instruction.LookupSwitchInstruction;
import java.lang.classfile.instruction.NewObjectInstruction;
import java.lang.classfile.instruction.NewReferenceArrayInstruction;
import java.lang.classfile.instruction.ReturnInstruction;
import java.lang.classfile.instruction.StackInstruction;
import java.lang.classfile.instruction.StoreInstruction;
import java.lang.classfile.instruction.SwitchCase;
import java.lang.classfile.instruction.TableSwitchInstruction;
import java.lang.constant.ClassDesc;
import java.lang.constant.ConstantDesc;
import java.lang.constant.MethodTypeDesc;
import java.lang.instrument.ClassFileTransformer;
import java.lang.instrument.IllegalClassFormatException;
import java.lang.invoke.MethodHandles;
import java.lang.reflect.AccessFlag;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.security.ProtectionDomain;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Optional;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.checkerframework.checker.signature.qual.BinaryName;
import org.checkerframework.checker.signature.qual.ClassGetName;
import org.checkerframework.checker.signature.qual.InternalForm;
import org.checkerframework.dataflow.qual.Pure;

/**
 * Starting with JDK 24, Java has added a set of APIs for reading and modifying .class files ({@code
 * java.lang.classfile}).
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
 * APIs instead of BCEL. (We will need to continue to support Instrument.java using BCEL, as we do
 * not anticipate our clients moving from JDK 8, 11, 17 or 21 to JDK 24 for quite some time.)
 *
 * <p>The Instrument24 class is responsible for modifying another class's bytecodes. Specifically,
 * its main task is to add calls into the Chicory Runtime at method entries and exits for
 * instrumentation purposes. These added calls are sometimes referred to as "hooks".
 */
@SuppressWarnings("nullness")
public class Instrument24 implements ClassFileTransformer {

  /** A log to which to print debugging information about program instrumentation. */
  protected SimpleLog debugInstrument = new SimpleLog(false);

  /** Directory for debug output. */
  File debug_dir;

  /** Directory for debug instrumented class output. */
  File debug_bin_dir;

  /** Directory for debug original class output. */
  File debug_orig_dir;

  /** The index of this method in SharedData.methods. */
  int cur_method_info_index = 0;

  /** The MethodInfo for the current method. */
  MethodInfo curMethodInfo;

  /** The location of the runtime support class. */
  private static final String runtime_classname = "daikon.chicory.Runtime";

  /** The ClassDesc for the runtime support class. */
  private static final ClassDesc runtimeCD = ClassDesc.of(runtime_classname);

  /** The ClassDesc for the Java Object class. */
  private static final ClassDesc objectCD = ClassDesc.of("java.lang.Object");

  /** Debug information about which classes are transformed and why. */
  public static SimpleLog debug_transform = new SimpleLog(false);

  // Variables used for the entire class.

  /** Current class name in binary format. */
  @BinaryName String binaryClassName;

  /** True if the class has a class initializer. */
  private boolean hasClinit;

  /** The current ClassModel. */
  private ClassModel classModel;

  /** Stores useful information about a class. */
  private ClassInfo classInfo;

  /** ConstantPool builder for entire class. */
  private ConstantPoolBuilder poolBuilder;

  // Variables used for the entire method.
  // They are initialized in modifyCode().

  /** CodeBuilder for the current method. */
  private CodeBuilder currentCodeBuilder;

  /** Next available slot in localsTable currently always = max locals. */
  private int nextLocalIndex;

  /** Local variable table (stored as an ArrayList). */
  List<LocalVariable> localsTable;

  /** Mapping from instructions to labels. */
  // Used to associate a label with a location within the codelist
  public Map<CodeElement, Label> labelMap = new HashMap<>();

  // Note that the next three items are CodeBuilder Labels.
  // These are different from CodeModel Labels.
  /** Label for first byte code of method, used to give new locals method scope. */
  private Label startLabel;

  /** Label for last byte code of method, used to give new locals method scope. */
  private Label endLabel;

  /** Label for start of orignal code, post insertion of entry instrumentation. */
  private Label entryLabel;

  /** Label for first byte code of method, as a CodeModel Label. */
  private Label oldStartLabel;

  /** Variables used for instrumentation. */
  LocalVariable nonceLocal, returnLocal;

  // End of method variables.

  /** Create a new Instrument. Sets up debug logging. */
  public Instrument24() {
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

    // Don't instrument class if it matches an excluded regular expression
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

    // if we're here, this ppt not explicitly included or excluded
    // so keep unless there were items in the "include only" list
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

    // convert internal form to binary name
    binaryClassName = className.replace("/", ".");

    // for debugging
    // new Throwable().printStackTrace();

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
    }

    // Don't instrument our own code
    if (is_chicory(className)) {
      debug_transform.log("Not considering chicory class %s%n", binaryClassName);
      return null;
    }

    debug_transform.log(
        "transforming class %s, loader %s - %s%n", className, loader, loader.getParent());

    // Parse the bytes of the classfile, die on any errors
    ClassFile classFile =
        ClassFile.of(
            ClassFile.ClassHierarchyResolverOption.of(
                ClassHierarchyResolver.ofResourceParsing(loader)));
    try {
      classModel = classFile.parse(classfileBuffer);
    } catch (Throwable t) {
      System.err.printf("Unexpected error %s reading in %s%n", t, binaryClassName);
      t.printStackTrace();
      // No changes to the bytecodes
      return null;
    }

    if (Chicory.dump) {
      try {
        debugInstrument.log("Dumping %s to %s%n", binaryClassName, debug_orig_dir);
        // Define the output file
        Path outputFile = Paths.get(debug_orig_dir.toString(), binaryClassName + ".class");
        // Write the byte array to a .class file
        Files.write(outputFile, classFile.transformClass(classModel, ClassTransform.ACCEPT_ALL));
        // write a bcel like file with an extension of .javap

        try (BufferedWriter writer =
            Files.newBufferedWriter(
                new File(debug_orig_dir, binaryClassName + ".javap").toPath(),
                StandardCharsets.UTF_8)) {
          writer.write(classModel.toDebugString());
        }
      } catch (Throwable t) {
        System.err.printf(
            "Unexpected error %s dumping out debug files for: %s%n", t, binaryClassName);
        t.printStackTrace();
        // proceed with instrumentation
      }
    }

    hasClinit = false;
    byte[] newBytes = {};
    debugInstrument.log("%nClass: %s%n", binaryClassName);
    // Modify the classfile, die on any errors
    try {
      newBytes =
          classFile.build(
              classModel.thisClass().asSymbol(),
              classBuilder -> modifyClass(classBuilder, classModel, loader));

    } catch (Throwable t) {
      System.err.printf("Unexpected error %s in transform of %s%n", t, binaryClassName);
      t.printStackTrace();
      // No changes to the bytecodes
      return null;
    }

    if (classInfo.shouldInclude) {
      if (Chicory.dump) {
        try {
          debugInstrument.log("Dumping %s to %s%n", binaryClassName, debug_bin_dir);
          // Define the output file
          Path outputFile = Paths.get(debug_bin_dir.toString(), binaryClassName + ".class");
          // Write the byte array to a .class file
          Files.write(outputFile, newBytes);
          // write a bcel like file with an extension of .javap
          ClassModel newClassModel = classFile.parse(newBytes);
          try (BufferedWriter writer =
              Files.newBufferedWriter(
                  new File(debug_bin_dir, binaryClassName + ".javap").toPath(),
                  StandardCharsets.UTF_8)) {
            writer.write(newClassModel.toDebugString());
          }
        } catch (Throwable t) {
          System.err.printf("Unexpected error %s dumping out debug files for: %s%n", t, className);
          t.printStackTrace();
          // proceed with instrumentation
        }
      }
      return newBytes;
    } else {
      // No changes to the bytecodes
      return null;
    }
  }

  /**
   * Instrument the current class.
   *
   * @param classBuilder for the current method
   * @param classModel for the current method
   * @param loader the class loader for this class
   */
  private void modifyClass(ClassBuilder classBuilder, ClassModel classModel, ClassLoader loader) {

    // Save constant pool builder for later use
    poolBuilder = classBuilder.constantPool();
    classInfo = new ClassInfo(binaryClassName, loader);

    debugInstrument.log("Class Attributes:%n");
    for (java.lang.classfile.Attribute<?> a : classModel.attributes()) {
      debugInstrument.log("  %s%n", a);
    }

    // Have each non-void method save its result in a local before returning.
    instrument_all_methods(classModel, classBuilder);

    // remember any constant static fields
    List<FieldModel> fields = classModel.fields();
    for (FieldModel fm : fields) {
      Optional<ConstantValueAttribute> cva = fm.findAttribute(Attributes.constantValue());
      if (cva.isPresent()) {
        String name = fm.fieldName().stringValue();
        String value = formatConstantDesc(cva.get().constant().constantValue());
        // debug
        debugInstrument.log("  Constant field: %s, value: %s%n", name, value);
        classInfo.staticMap.put(name, value);
      }
    }

    if (Chicory.checkStaticInit && !hasClinit) {
      // If no clinit method need to add our own
      classBuilder.withMethod(
          "<clinit>",
          MethodTypeDesc.of(CD_void),
          ClassFile.ACC_STATIC,
          methodBuilder ->
              methodBuilder.withCode(
                  codeBuilder -> {
                    codeBuilder
                        .ldc(binaryClassName)
                        .invokestatic(
                            runtimeCD, "initNotify", MethodTypeDesc.of(CD_void, CD_String))
                        .return_();
                  }));
    }
  }

  /**
   * Adds a call (or calls) to the Chicory Runtime {@code initNotify} method into a given method.
   * Clients pass the class static initializer as the method.
   *
   * @param mgen the method to modify, typically the class static initializer
   */
  private void addInvokeToClinit(MethodGen24 mgen) {

    try {
      List<CodeElement> il = mgen.getInstructionList();
      ListIterator<CodeElement> li = il.listIterator();
      while (li.hasNext()) {

        CodeElement inst = li.next();

        // back up iterator to point to 'inst'
        li.previous();

        // Get the translation for this instruction (if any)
        List<CodeElement> new_il = xform_clinit(inst);

        // insert code prior to 'inst'
        for (CodeElement ce : new_il) {
          li.add(ce);
        }

        // skip over 'inst' we just inserted new_il in front of
        li.next();
      }
    } catch (Exception e) {
      System.err.printf("Unexpected exception encountered: %s", e);
      e.printStackTrace();
    }
  }

  /**
   * Called by addInvokeToClinit to obtain the instructions that represent a call to the Chicory
   * Runtime {@code initNotify} method prior to a return opcode. Returns null if the given
   * instruction is not a return.
   *
   * @param inst the instruction that might be a return
   * @return the list of instructions that call {@code initNotify}, or null if {@code inst} is not a
   *     return instruction
   */
  private @Nullable List<CodeElement> xform_clinit(CodeElement inst) {

    if (inst instanceof ReturnInstruction) {
      return call_initNotify();
    } else {
      // return empty list
      return new ArrayList<>();
    }
  }

  /**
   * Create the List of CodeElements for a call to {@code initNotify}.
   *
   * @return the instruction list
   */
  private List<CodeElement> call_initNotify() {

    List<CodeElement> codeList = new ArrayList<>();

    MethodRefEntry mre =
        poolBuilder.methodRefEntry(runtimeCD, "initNotify", MethodTypeDesc.of(CD_void, CD_String));
    codeList.add(buildLDCInstruction(poolBuilder.stringEntry(binaryClassName)));
    codeList.add(InvokeInstruction.of(Opcode.INVOKESTATIC, mre));

    return codeList;
  }

  /**
   * Instrument all the methods in a class. For each method, add instrumentation code at the entry
   * and at each return from the method. In addition, changes each return statement to first place
   * the value being returned into a local and then return. This allows us to work around the JDI
   * deficiency of not being able to query return values.
   *
   * @param classModel for current class
   * @param classBuilder for current class
   */
  private void instrument_all_methods(ClassModel classModel, ClassBuilder classBuilder) {

    List<MethodInfo> method_infos = new ArrayList<>();

    if (classModel.majorVersion() < ClassFile.JAVA_6_VERSION) {
      System.out.printf(
          "Chicory warning: ClassFile: %s - classfile version (%d) is out of date and may not be"
              + " processed correctly.%n",
          binaryClassName, classModel.majorVersion());
    }

    boolean shouldInclude = false;

    try {
      // Loop through each method in the class
      for (MethodModel mm : classModel.methods()) {

        // NOT SURE THIS APPLIES ANYMORE
        // don't plan to use StackMapUtils
        // The class data in StackMapUtils is not thread safe,
        // allow only one method at a time to be instrumented.
        // DynComp does this by creating a new instrumentation object
        // for each class - probably a cleaner solution.
        synchronized (this) {
          MethodGen24 mgen = new MethodGen24(mm, binaryClassName, this);

          // check for the class static initializer method
          if (mgen.getName().equals("<clinit>")) {
            hasClinit = true;
            if (Chicory.checkStaticInit) {
              addInvokeToClinit(mgen);
            }
            if (!Chicory.instrument_clinit) {
              // If we are not going to instrument this method,
              // we need to copy it to the output class now.
              classBuilder.withMethod(
                  mm.methodName().stringValue(),
                  mm.methodTypeSymbol(),
                  mm.flags().flagsMask(),
                  methodBuilder -> outputMethod(methodBuilder, mm, mgen));
              continue;
            }
          }

          // If method is synthetic... (default constructors and <clinit> are not synthetic)
          if (mgen.getAccessFlags().has(AccessFlag.SYNTHETIC)) {
            // If we are not going to instrument this method,
            // we need to copy it to the output class now.
            classBuilder.withMethod(
                mm.methodName().stringValue(),
                mm.methodTypeSymbol(),
                mm.flags().flagsMask(),
                methodBuilder -> outputMethod(methodBuilder, mm, mgen));
            continue;
          }

          // Get the instruction list and skip methods with no instructions
          List<CodeElement> il = mgen.getInstructionList();
          if (il.size() == 0) {
            // If we are not going to instrument this method,
            // we need to copy it to the output class now.
            classBuilder.withMethod(
                mm.methodName().stringValue(),
                mm.methodTypeSymbol(),
                mm.flags().flagsMask(),
                methodBuilder -> outputMethod(methodBuilder, mm, mgen));
            continue;
          }

          if (debugInstrument.enabled) {
            ClassDesc[] arg_types = mgen.getParameterTypes();
            String[] arg_names = mgen.getParameterNames();
            LocalVariable[] local_vars = mgen.getLocalVariables();
            String types = "", names = "", locals = "";

            for (int j = 0; j < arg_types.length; j++) {
              types = types + convertDescriptorToString(arg_types[j].descriptorString()) + " ";
            }
            for (int j = 0; j < arg_names.length; j++) {
              names = names + arg_names[j] + " ";
            }
            for (int j = 0; j < local_vars.length; j++) {
              locals = locals + local_vars[j].name().stringValue() + " ";
            }
            debugInstrument.log("%nMethod = %s%n", mgen);
            debugInstrument.log("arg_types(%d): %s%n", arg_types.length, types);
            debugInstrument.log("arg_names(%d): %s%n", arg_names.length, names);
            debugInstrument.log("localvars(%d): %s%n", local_vars.length, locals);
            //         debugInstrument.log("Original code: %s%n", mgen.getMethod().getCode());
            debugInstrument.log("Method Attributes:%n");
            for (java.lang.classfile.Attribute<?> a : mm.attributes()) {
              debugInstrument.log("  %s%n", a);
            }
            debugInstrument.log("mgen.getSignature: %s%n", mgen.getSignature());
            MethodTypeDesc mtd = mm.methodTypeSymbol();
            debugInstrument.log("mtd.descriptorString: %s%n", mtd.descriptorString());
            debugInstrument.log("mtd.displayDescriptor: %s%n", mtd.displayDescriptor());
          }

          // Create a MethodInfo that describes this methods arguments
          // and exit line numbers (information not available via reflection)
          // and add it to the list for this class.
          curMethodInfo = create_method_info(classInfo, mgen);

          if (curMethodInfo == null) { // method filtered out!
            // If we are not going to instrument this method,
            // we need to copy it to the output class now.
            classBuilder.withMethod(
                mm.methodName().stringValue(),
                mm.methodTypeSymbol(),
                mm.flags().flagsMask(),
                methodBuilder -> outputMethod(methodBuilder, mm, mgen));
            continue;
          }

          shouldInclude = true; // at least one method not filtered out

          method_infos.add(curMethodInfo);

          synchronized (SharedData.methods) {
            cur_method_info_index = SharedData.methods.size();
            SharedData.methods.add(curMethodInfo);
          }

          // add entry instrumentation
          // instrument return instructions
          classBuilder.withMethod(
              mm.methodName().stringValue(),
              mm.methodTypeSymbol(),
              mm.flags().flagsMask(),
              methodBuilder -> modifyMethod(methodBuilder, mm, mgen));
        }
      }
    } catch (Exception e) {
      System.err.printf("Unexpected exception encountered: %s", e);
      e.printStackTrace();
    }

    // copy all other ClassElements to output class (unchanged)
    for (ClassElement ce : classModel) {
      debugInstrument.log("ClassElement: %s%n", ce);
      switch (ce) {
        case MethodModel mm -> {}
          // copy all other ClassElements to output class (unchanged)
        default -> classBuilder.with(ce);
      }
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
  }

  /**
   * Output current method with no changes.
   *
   * @param methodBuilder for the current method
   * @param methodModel for the current method
   * @param mgen MethodGen24 for the current method
   */
  private void outputMethod(
      MethodBuilder methodBuilder, MethodModel methodModel, MethodGen24 mgen) {

    for (MethodElement me : methodModel) {
      debugInstrument.log("MethodElement: %s%n", me);
      switch (me) {
        case CodeModel codeModel ->
            methodBuilder.withCode(codeBuilder -> outputCode(codeBuilder, mgen));

          // copy all other MethodElements to output class (unchanged)
        default -> methodBuilder.with(me);
      }
    }
  }

  /**
   * Output code for current method with no changes.
   *
   * @param codeBuilder for the current method's code
   * @param mgen MethodGen24 for the current method
   */
  private void outputCode(CodeBuilder codeBuilder, MethodGen24 mgen) {

    // Copy the modified instruction list to the output class.
    for (CodeElement ce : mgen.getInstructionList()) {
      debugInstrument.log("CodeElement: %s%n", ce);
      codeBuilder.with(ce);
    }
  }

  /**
   * Instrument the current method.
   *
   * @param methodBuilder for the current method
   * @param methodModel for the current method
   * @param mgen MethodGen24 for the current method
   */
  private void modifyMethod(
      MethodBuilder methodBuilder, MethodModel methodModel, MethodGen24 mgen) {

    for (MethodElement me : methodModel) {
      debugInstrument.log("MethodElement: %s%n", me);
      switch (me) {
        case CodeModel codeModel ->
            methodBuilder.withCode(codeBuilder -> modifyCode(codeBuilder, codeModel, mgen));

          // copy all other MethodElements to output class (unchanged)
        default -> methodBuilder.with(me);
      }
    }
  }

  /**
   * Generate instrumentation code for the current method.
   *
   * @param codeBuilder for the current method's code
   * @param codeModel for the current method's code
   * @param mgen MethodGen24 for the current method
   */
  private void modifyCode(CodeBuilder codeBuilder, CodeModel codeModel, MethodGen24 mgen) {

    // ititialize all the items associated with the local variables
    nextLocalIndex = mgen.getMaxLocals();
    startLabel = codeBuilder.startLabel();
    endLabel = codeBuilder.endLabel();

    nonceLocal = null;
    returnLocal = null;
    currentCodeBuilder = codeBuilder;
    oldStartLabel = null;
    labelMap.clear();

    @SuppressWarnings("JdkObsolete")
    List<CodeElement> codeList = new LinkedList<>();

    debugInstrument.log("Code Attributes:%n");
    for (java.lang.classfile.Attribute<?> a : codeModel.attributes()) {
      debugInstrument.log("  %s%n", a);
    }

    /*
     * The localsTable was initialized in the MethodGen24 constructor.
     * Here we initialize the codeList. We also remove the local variable
     * type records. Some instrumentation changes require these to be
     * updated, but it should be safe to just delete them since the
     * LocalVariableTypeTable is optional and really only of use to a debugger.
     * We also save the CodeModel label at the start of the byte codes,
     * if there is one. If there isn't, that is okay as it means the original
     * code did not reference byte code offset 0 so inserting our instrumentation
     * code at that point will not cause a problem.
     *
     */
    for (CodeElement ce : mgen.getInstructionList()) {
      debugInstrument.log("CodeElement: %s%n", ce);
      switch (ce) {
        case LocalVariable lv -> {} // we have alreay processed these
        case LocalVariableType lvt -> {} // we can discard local variable types
        case LabelTarget l -> {
          if (mgen.getCodeAttribute().labelToBci(l.label()) == 0) oldStartLabel = l.label();
          codeList.add(ce);
        }
        default -> codeList.add(ce); // save all other elements
      }
    }

    // START modify codeList

    // Add nonce local to matchup enter/exits
    add_entry_instrumentation(codeList, mgen);

    // debugInstrument.log("Modified code: %s%n", mgen.getMethod().getCode());

    Iterator<Boolean> shouldIncIter = curMethodInfo.is_included.iterator();
    Iterator<Integer> exitIter = curMethodInfo.exit_locations.iterator();

    // instrument return instructions
    ListIterator<CodeElement> li = codeList.listIterator();
    while (li.hasNext()) {

      CodeElement inst = li.next();

      // back up iterator to point to 'inst'
      li.previous();

      // If this is a return instruction, insert method exit instrumentation
      List<CodeElement> new_il =
          generate_return_instrumentation(inst, mgen, shouldIncIter, exitIter);

      // insert code prior to 'inst'
      for (CodeElement ce : new_il) {
        li.add(ce);
      }

      // skip over 'inst' we just inserted new_il in front of
      li.next();
    }

    // END modify codeList

    // Copy the modified local variable table to the output class.
    debugInstrument.log("LocalVariableTable:%n");
    for (LocalVariable lv : localsTable) {
      codeBuilder.localVariable(
          lv.slot(), lv.name().stringValue(), lv.typeSymbol(), lv.startScope(), lv.endScope());
      debugInstrument.log(
          "  %s : %s%n", lv, convertDescriptorToString(lv.typeSymbol().descriptorString()));
    }

    // Copy the modified instruction list to the output class.
    for (CodeElement ce : codeList) {
      // If there is a new CodeBuilder label associated with this instruction, it needs to be
      // defined.
      Label l = labelMap.get(ce);
      if (l != null) {
        debugInstrument.log("Label: %s%n", l);
        currentCodeBuilder.labelBinding(l);
        // We've defined the label, remove it from the map
        labelMap.remove(ce);
      }
      // If this instruction references a Label, we need to see if it is the oldStartLabel
      // and, if so, replace the target with our new entryLabel
      ce = checkTargetLabel(ce);
      debugInstrument.log("CodeElement: %s%n", ce);
      codeBuilder.with(ce);
    }
  }

  /**
   * If this is a return instruction, generate new il to assign the result to a local variable
   * (return__$trace2_val) and then call daikon.chicory.Runtime.exit(). This il wil be inserted
   * immediately before the return.
   *
   * @param inst the instruction to inspect
   * @param mgen MethodGen24 for method
   * @param shouldIncIter whether or not to instrument this return
   * @param exitIter list of exit line numbers
   * @return instruction list for instrumenting the return
   */
  private @Nullable List<CodeElement> generate_return_instrumentation(
      CodeElement inst,
      MethodGen24 mgen,
      Iterator<Boolean> shouldIncIter,
      Iterator<Integer> exitIter) {

    List<CodeElement> newCode = new ArrayList<>();
    if (!(inst instanceof ReturnInstruction)) {
      return newCode;
    }

    if (!shouldIncIter.hasNext()) {
      throw new RuntimeException("Not enough entries in shouldIncIter");
    }

    boolean shouldInclude = shouldIncIter.next();

    if (!shouldInclude) {
      return newCode;
    }

    ClassDesc type = mgen.getReturnType();
    if (!type.equals(CD_void)) {
      TypeKind typeKind = TypeKind.from(type);
      LocalVariable returnLocal = getReturnLocal(type);
      if (typeKind.slotSize() == 1) {
        newCode.add(StackInstruction.of(Opcode.DUP));
      } else {
        newCode.add(StackInstruction.of(Opcode.DUP2));
      }
      newCode.add(StoreInstruction.of(typeKind, returnLocal.slot()));
    }

    if (!exitIter.hasNext()) {
      throw new RuntimeException("Not enough exit locations in the exitIter");
    }

    call_enter_exit(newCode, mgen, "exit", exitIter.next());
    return newCode;
  }

  /**
   * Returns the local variable used to store the return result. If it is not present, creates it
   * with the specified type. If the variable is known to already exist, the type can be null.
   *
   * @param returnType the type of the return; may be null if the variable is known to already
   *     exist.
   * @return a local variable to save the return value
   */
  private LocalVariable getReturnLocal(@Nullable ClassDesc returnType) {

    // If a type was specified and the variable was found, they must match
    if (returnLocal == null) {
      assert returnType != null : " return__$trace2_val doesn't exist";
    } else {
      assert returnType.equals(returnLocal.typeSymbol())
          : " returnType = " + returnType + "current type = " + returnLocal.typeSymbol();
    }

    if (returnLocal == null) {
      debugInstrument.log("Adding return local of type %s%n", returnType);
      returnLocal = createMethodScopeLocal("return__$trace2_val", returnType);
    }

    return returnLocal;
  }

  /**
   * Inserts instrumentation code at the start of the method. This includes adding a local variable
   * (this_invocation_nonce) that is initialized to Runtime.nonce++. This provides a unique id on
   * each method entry/exit that allows them to be matched up from the dtrace file. Inserts code to
   * call daikon.chicory.Runtime.enter().
   *
   * @param codeList instruction list for method
   * @param mgen MethodGen24 for method
   */
  private void add_entry_instrumentation(List<CodeElement> codeList, MethodGen24 mgen) {

    String atomic_int_classname = "java.util.concurrent.atomic.AtomicInteger";
    ClassDesc atomic_intClassDesc = ClassDesc.of(atomic_int_classname);

    List<CodeElement> newCode = new ArrayList<>();

    // create the nonce local variable
    nonceLocal = createMethodScopeLocal("this_invocation_nonce", CD_int);

    // The following implements:
    //     this_invocation_nonce = Runtime.nonce++;

    // getstatic Runtime.nonce (load reference to AtomicInteger daikon.chicory.Runtime.nonce)
    newCode.add(
        FieldInstruction.of(
            Opcode.GETSTATIC, poolBuilder.fieldRefEntry(runtimeCD, "nonce", atomic_intClassDesc)));

    // do an atomic get and increment of nonce value
    // this is multi-thread safe and leaves int value of nonce on stack
    MethodRefEntry mre =
        poolBuilder.methodRefEntry(
            atomic_intClassDesc, "getAndIncrement", MethodTypeDesc.of(CD_int));
    newCode.add(InvokeInstruction.of(Opcode.INVOKEVIRTUAL, mre));

    // store original value of nonce into this_invocation_nonce)
    newCode.add(StoreInstruction.of(TypeKind.INT, nonceLocal.slot()));

    call_enter_exit(newCode, mgen, "enter", -1);

    // The start of the list of CodeElements looks as follows:
    //   LocalVariable declarations (if any)
    //   Label for start of code (if present)
    //   LineNumber for start of code (if present)
    //   <the actual code for the method>
    //
    // We want to insert our instrumentation code after the LocalVariables
    // (if any) and after the inital label (if present), but before any
    // LineNumber or Instruction.
    try {
      ListIterator<CodeElement> li = codeList.listIterator();
      while (li.hasNext()) {

        CodeElement inst = li.next();
        if (!(inst instanceof LineNumber) && !(inst instanceof Instruction)) {
          continue;
        }

        // Label for new location of start of original code
        entryLabel = currentCodeBuilder.newLabel();
        debugInstrument.log("entryLabel: %s%n", entryLabel);
        labelMap.put(inst, entryLabel);

        // insert code before this LineNumber or Instruction
        // back up iterator to point to 'inst'
        li.previous();
        for (CodeElement ce : newCode) {
          li.add(ce);
        }
        break;
      }
    } catch (Exception e) {
      System.err.printf("Unexpected exception encountered: %s", e);
      e.printStackTrace();
    }
  }

  /**
   * Pushes the object, nonce, parameters, and return value on the stack and calls the specified
   * Method (normally enter or exit) in daikon.chicory.Runtime. The parameters are passed as an
   * array of objects. Any primitive values are wrapped in the appropriate daikon.chicory.Runtime
   * wrapper (IntWrap, FloatWrap, etc).
   *
   * @param newCode an instruction list to append the enter/exit code to
   * @param mgen describes the current method
   * @param callMethod either "enter" or "exit"
   * @param line source line number if this is an exit
   */
  private void call_enter_exit(
      List<CodeElement> newCode, MethodGen24 mgen, String callMethod, int line) {

    ClassDesc[] arg_types = mgen.getParameterTypes();

    // aload
    // Push the object.  Null if this is a static method or a constructor
    if (mgen.isStatic() || (callMethod.equals("enter") && is_constructor(mgen))) {
      newCode.add(ConstantInstruction.ofIntrinsic(Opcode.ACONST_NULL));
    } else { // must be an instance method
      newCode.add(LoadInstruction.of(TypeKind.REFERENCE, 0));
    }

    // Determine the offset of the first parameter
    int param_offset = 1;
    if (mgen.isStatic()) {
      param_offset = 0;
    }

    // Assumes add_entry_instrumentation has been called which sets nonceLocal.
    // iload
    // Push the nonce
    newCode.add(LoadInstruction.of(TypeKind.INT, nonceLocal.slot()));

    // iconst
    // Push the MethodInfo index
    newCode.add(loadIntegerConstant(cur_method_info_index));

    // iconst
    // anewarray
    // Create an array of objects with elements for each parameter
    newCode.add(loadIntegerConstant(arg_types.length));
    ClassDesc objectArrayCD = objectCD.arrayType(1);
    newCode.add(NewReferenceArrayInstruction.of(poolBuilder.classEntry(objectCD)));

    // Put each argument into the array
    int param_index = param_offset;
    for (int ii = 0; ii < arg_types.length; ii++) {
      newCode.add(StackInstruction.of(Opcode.DUP));
      newCode.add(loadIntegerConstant(ii));
      ClassDesc at = arg_types[ii];
      if (at.isPrimitive()) {
        create_wrapper(newCode, at, param_index);
      } else { // must be reference of some sort
        newCode.add(LoadInstruction.of(TypeKind.REFERENCE, param_index));
      }
      newCode.add(ArrayStoreInstruction.of(Opcode.AASTORE));
      param_index += TypeKind.from(at).slotSize();
    }

    // If this is an exit, push the return value and line number.
    // The return value is stored in the local "return__$trace2_val".
    // If the return value is a primitive, wrap it in the appropriate wrapper.
    if (callMethod.equals("exit")) {
      ClassDesc ret_type = mgen.getReturnType();
      if (ret_type.equals(CD_void)) {
        newCode.add(ConstantInstruction.ofIntrinsic(Opcode.ACONST_NULL));
      } else {
        LocalVariable return_local = getReturnLocal(ret_type);
        if (ret_type.isPrimitive()) {
          create_wrapper(newCode, ret_type, return_local.slot());
        } else {
          newCode.add(LoadInstruction.of(TypeKind.REFERENCE, return_local.slot()));
        }
      }

      // push line number
      newCode.add(loadIntegerConstant(line));
    }

    MethodTypeDesc methodArgs;
    // Call the specified method
    if (callMethod.equals("exit")) {
      methodArgs =
          MethodTypeDesc.of(CD_void, CD_Object, CD_int, CD_int, objectArrayCD, CD_Object, CD_int);
    } else {
      methodArgs = MethodTypeDesc.of(CD_void, CD_Object, CD_int, CD_int, objectArrayCD);
    }
    MethodRefEntry mre = poolBuilder.methodRefEntry(runtimeCD, callMethod, methodArgs);
    newCode.add(InvokeInstruction.of(Opcode.INVOKESTATIC, mre));
  }

  /** Possibly modified default switch target. */
  private Label modifiedTarget;

  /** Possibly modified switch case list. */
  private List<SwitchCase> modifiedCaseList;

  /**
   * Checks to see if the instruction targets the method's CodeModel startLabel (held in
   * oldStartLabel). If so, it replaces the target with the entryLabel. Unfortunately, the classfile
   * API does not allow us to simply replace the label, we have to replace the entire instruction.
   * Note that oldStartLabel may be null, but that is okay as any comparison to it will fail and we
   * will do nothing.
   *
   * @param inst the instruction to check
   * @return the original instruction or it's replacement
   */
  private CodeElement checkTargetLabel(CodeElement inst) {
    switch (inst) {
      case BranchInstruction bi -> {
        if (!bi.target().equals(oldStartLabel)) break;
        return BranchInstruction.of(bi.opcode(), entryLabel);
      }
      case ExceptionCatch ec -> {
        if (!ec.tryStart().equals(oldStartLabel)) break;
        return ExceptionCatch.of(ec.handler(), entryLabel, ec.tryEnd(), ec.catchType());
      }
      case LookupSwitchInstruction ls -> {
        if (!checkSwitchTargets(ls.defaultTarget(), ls.cases())) break;
        return LookupSwitchInstruction.of(modifiedTarget, modifiedCaseList);
      }
      case TableSwitchInstruction ts -> {
        if (!checkSwitchTargets(ts.defaultTarget(), ts.cases())) break;
        return TableSwitchInstruction.of(
            ts.lowValue(), ts.highValue(), modifiedTarget, modifiedCaseList);
      }
      default -> {
        return inst;
      }
    }
    return inst;
  }

  /**
   * Checks to see if a switch instruction's default target or any of the case targets refer to the
   * oldStartLabel. If so, replace those targets with the entryLabel, store the result in
   * modifiedTarget and modifiedCaseList and return true. Otherwise, return false.
   *
   * @param defaultTarget the default target for the switch instruction
   * @param caseList the case list for the switch instruction
   * @return true if either the defaultTarget or the caseList has been modified
   */
  private boolean checkSwitchTargets(Label defaultTarget, List<SwitchCase> caseList) {
    boolean modified = false;
    if (defaultTarget.equals(oldStartLabel)) {
      modifiedTarget = entryLabel;
      modified = true;
    } else {
      modifiedTarget = defaultTarget;
    }
    List<SwitchCase> newCaseList = new ArrayList<SwitchCase>();
    for (SwitchCase item : caseList) {
      if (item.target().equals(oldStartLabel)) {
        newCaseList.add(SwitchCase.of(item.caseValue(), entryLabel));
        modified = true;
      } else {
        newCaseList.add(item);
      }
    }
    modifiedCaseList = newCaseList;
    return modified;
  }

  /**
   * Creates code to put the local var/param at the specified var_index into a wrapper appropriate
   * for prim_type. prim_type must be a primitive type (Type.INT, Type.FLOAT, etc.). The wrappers
   * are those defined in daikon.chicory.Runtime.
   *
   * <p>The stack is left with a pointer to the newly created wrapper at the top.
   *
   * @param newCode an instruction list to append the wrapper code to
   * @param prim_type the primitive type of the local variable or parameter
   * @param var_index the offset into the local stack of the variable or parameter
   */
  private void create_wrapper(List<CodeElement> newCode, ClassDesc prim_type, int var_index) {

    String wrapper;
    TypeKind typeKind;
    switch (prim_type.displayName()) {
      case "boolean":
        typeKind = TypeKind.BOOLEAN;
        wrapper = "BooleanWrap";
        break;
      case "byte":
        typeKind = TypeKind.BYTE;
        wrapper = "ByteWrap";
        break;
      case "char":
        typeKind = TypeKind.CHAR;
        wrapper = "CharWrap";
        break;
      case "double":
        typeKind = TypeKind.DOUBLE;
        wrapper = "DoubleWrap";
        break;
      case "float":
        typeKind = TypeKind.FLOAT;
        wrapper = "FloatWrap";
        break;
      case "int":
        typeKind = TypeKind.INT;
        wrapper = "IntWrap";
        break;
      case "long":
        typeKind = TypeKind.LONG;
        wrapper = "LongWrap";
        break;
      case "short":
        typeKind = TypeKind.SHORT;
        wrapper = "ShortWrap";
        break;
      default:
        throw new Error("unexpected type " + prim_type);
    }

    ClassDesc wrapperCD = ClassDesc.of(runtime_classname + "$" + wrapper);
    newCode.add(NewObjectInstruction.of(poolBuilder.classEntry(wrapperCD)));
    newCode.add(StackInstruction.of(Opcode.DUP));
    newCode.add(LoadInstruction.of(typeKind, var_index));
    MethodRefEntry mre =
        poolBuilder.methodRefEntry(wrapperCD, "<init>", MethodTypeDesc.of(CD_void, prim_type));
    newCode.add(InvokeInstruction.of(Opcode.INVOKESPECIAL, mre));
  }

  /**
   * Returns true iff mgen is a constructor.
   *
   * @param mgen describes the current method
   * @return true iff mgen is a constructor
   */
  @Pure
  private boolean is_constructor(MethodGen24 mgen) {

    if (mgen.getName().equals("<init>") || mgen.getName().equals("")) {
      debugInstrument.log("method '%s' is a constructor%n", mgen.getName());
      return true;
    } else {
      return false;
    }
  }

  /**
   * Get the ClassGetName form of a ClassDesc. For a non-array type, the binary name; for an array
   * type, a format like the FieldDescriptor field descriptor, but using "." where the field
   * descriptor uses "/".
   *
   * @param t type whose name is to be converted
   * @return a String containing the class name
   */
  @SuppressWarnings("signature") // conversion method
  private static @ClassGetName String typeToClassGetName(ClassDesc t) {

    String s = t.descriptorString();
    if (s.startsWith("[")) {
      return s.replace('/', '.');
    } else {
      return convertDescriptorToString(s);
    }
  }

  /**
   * Return an array of strings, each corresponding to mgen's argument types as a fully qualified
   * name: how a type is represented in Java source code.
   *
   * @param mgen describes the current method
   * @return an array of strings, each corresponding to mgen's argument types
   */
  @SuppressWarnings("signature") // conversion method
  private @BinaryName String[] getFullyQualifiedArgTypeNames(MethodGen24 mgen) {

    ClassDesc[] arg_types = mgen.getParameterTypes();
    @BinaryName String[] arg_type_strings = new @BinaryName String[arg_types.length];

    for (int ii = 0; ii < arg_types.length; ii++) {
      String s = convertDescriptorToString(arg_types[ii].descriptorString());
      arg_type_strings[ii] = s;
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
  private @Nullable MethodInfo create_method_info(ClassInfo classInfo, MethodGen24 mgen) {

    // Get the argument names for this method
    String[] arg_names = mgen.getParameterNames();
    LocalVariable[] lvs = mgen.getLocalVariables();
    int param_offset = 1;
    if (mgen.isStatic()) {
      param_offset = 0;
    }
    if (debugInstrument.enabled) {
      debugInstrument.log("create_method_info1 %s%n", arg_names.length);
      for (int ii = 0; ii < arg_names.length; ii++) {
        debugInstrument.log("arg: %s%n", arg_names[ii]);
      }
    }

    int lv_start = 0;
    // If this is an inner class constructor, then its first parameter is
    // the outer class constructor.  I need to detect this and adjust the
    // parameter names appropriately.  This check is ugly.
    if (mgen.getName().equals("<init>") && mgen.getParameterTypes().length > 0) {
      int dollarPos = mgen.getClassName().lastIndexOf("$");
      String arg0Name = convertDescriptorToString(mgen.getParameterType(0).descriptorString());
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
        arg_names[0] = arg0Name + ".this";
      }
    }

    if (lvs != null) {
      for (int ii = lv_start; ii < arg_names.length; ii++) {
        if ((ii + param_offset) < lvs.length) {
          arg_names[ii] = lvs[ii + param_offset].name().stringValue();
        }
      }
    }

    if (debugInstrument.enabled) {
      debugInstrument.log("create_method_info2 %s%n", arg_names.length);
      for (int ii = 0; ii < arg_names.length; ii++) {
        debugInstrument.log("arg: %s%n", arg_names[ii]);
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
            getFullyQualifiedArgTypeNames(mgen),
            mgen.toString(),
            mgen.getName()))) {
      shouldInclude = true;
    }
    // Get the argument types for this method
    ClassDesc[] arg_types = mgen.getParameterTypes();
    @ClassGetName String[] arg_type_strings = new @ClassGetName String[arg_types.length];
    for (int ii = 0; ii < arg_types.length; ii++) {
      arg_type_strings[ii] = typeToClassGetName(arg_types[ii]);
    }

    // Loop through each instruction and find the line number for each
    // return opcode
    List<Integer> exit_locs = new ArrayList<>();

    // tells whether each exit loc in the method is included or not (based on filters)
    List<Boolean> isIncluded = new ArrayList<>();

    debugInstrument.log("Looking for exit points in %s%n", mgen.getName());
    List<CodeElement> il = mgen.getInstructionList();
    int line_number = 0;
    int last_line_number = 0;
    boolean foundLine;

    ListIterator<CodeElement> li = il.listIterator();
    while (li.hasNext()) {
      CodeElement inst = li.next();
      foundLine = false;

      if (inst instanceof LineNumber ln) {
        line_number = ln.line();
        foundLine = true;
      }

      if (inst instanceof ReturnInstruction) {
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
                getFullyQualifiedArgTypeNames(mgen),
                mgen.toString(),
                mgen.getName(),
                line_number))) {
          shouldInclude = true;
          exit_locs.add(line_number);

          isIncluded.add(true);
        } else {
          isIncluded.add(false);
        }
      }
    }

    if (shouldInclude) {
      return new MethodInfo(
          classInfo, mgen.getName(), arg_names, arg_type_strings, exit_locs, isIncluded);
    } else {
      return null;
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
  private static boolean is_chicory(@InternalForm String classname) {

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

  // UNFINISHED and maybe unneeded
  //  // converts a method descriptor to a Java language string
  //  public static String convertDescriptorToString(String descriptor) {
  //    StringBuilder args = new StringBuilder("(");
  //    if (descriptor.charAt(0) != '(') {
  //        throw new IllegalArgumentException("Invalid method descriptor: " + descriptor);
  //    }
  //    int paren = descriptor.indexOf(')');
  //    if (paren < 0) {
  //        throw new IllegalArgumentException("Invalid method descriptor: " + descriptor);
  //    }
  //    int end;
  //    int comma = descriptor.indexOf(',');
  //    if (comma < 0) {
  //        end = paren;
  //    } else {
  //        end = min(comma, paren);
  //    }

  /**
   * Format a field descriptor for output. The main difference between a descriptor and a signature
   * is that the later may contain type arguments. This routine was orginaly written for
   * descriptors, but some support for type arguments has been added.
   *
   * @param descriptor the object to format
   * @return a formatted string
   */
  public static String convertDescriptorToString(String descriptor) {
    StringBuilder result = new StringBuilder();

    int arrayDimensions = 0;
    while (descriptor.charAt(0) == '[') {
      arrayDimensions++;
      descriptor = descriptor.substring(1);
    }

    // Convert primitive types
    switch (descriptor.charAt(0)) {
      case 'B':
        result.append("byte");
        break;
      case 'C':
        result.append("char");
        break;
      case 'D':
        result.append("double");
        break;
      case 'F':
        result.append("float");
        break;
      case 'I':
        result.append("int");
        break;
      case 'J':
        result.append("long");
        break;
      case 'S':
        result.append("short");
        break;
      case 'Z':
        result.append("boolean");
        break;
      case 'V':
        result.append("void");
        break;
      case 'L': // Object type, starts with 'L' and ends with ';'
        result.append(parseObjectType(descriptor));
        break;
      default:
        throw new IllegalArgumentException("Invalid descriptor: " + descriptor);
    }

    // Append array brackets if applicable
    while (arrayDimensions-- > 0) {
      result.append("[]");
    }

    return result.toString();
  }

  /**
   * Format an object that may contain generics for output.
   *
   * @param descriptor the object to format
   * @return a formatted string
   */
  private static String parseObjectType(String descriptor) {
    StringBuilder result = new StringBuilder();
    int genericStart = descriptor.indexOf('<');
    int genericEnd = descriptor.lastIndexOf('>');
    int endOfBaseType = descriptor.indexOf(';');

    if (genericStart > 0 && genericEnd > genericStart) {
      // Base type with generics
      String baseType = descriptor.substring(1, genericStart).replace('/', '.');
      result.append(baseType).append('<');
      String genericPart = descriptor.substring(genericStart + 1, genericEnd);
      result.append(parseGenericParameters(genericPart));
      result.append('>');
    } else if (endOfBaseType > 0) {
      // Regular object type
      result.append(descriptor.substring(1, endOfBaseType).replace('/', '.'));
    } else {
      throw new IllegalArgumentException("Malformed object type descriptor: " + descriptor);
    }

    return result.toString();
  }

  /**
   * Format a generic parameter for output.
   *
   * @param genericPart the parameter(s) to format
   * @return a formatted string
   */
  private static String parseGenericParameters(String genericPart) {
    StringBuilder result = new StringBuilder();
    int depth = 0;
    StringBuilder current = new StringBuilder();
    List<String> params = new ArrayList<>();

    for (int i = 0; i < genericPart.length(); i++) {
      char c = genericPart.charAt(i);
      if (c == '<') {
        depth++;
        current.append(c);
      } else if (c == '>') {
        depth--;
        current.append(c);
      } else if (c == ';' && depth == 0) {
        params.add(convertDescriptorToString(current.toString()));
        current.setLength(0); // Clear the buffer
      } else {
        current.append(c);
      }
    }

    if (current.length() > 0) {
      params.add(convertDescriptorToString(current.toString()));
    }

    result.append(String.join(", ", params));
    return result.toString();
  }

  /**
   * Format a constant value for printing.
   *
   * @param item the constant to format
   * @return a string containing the constant's value
   */
  private final String formatConstantDesc(ConstantDesc item) {
    String result = "";
    try {
      result = item.resolveConstantDesc(MethodHandles.lookup()).toString();
    } catch (Exception e) {
      System.err.printf("Unexpected error %s getting constant value for: %s%n", e, item);
    }
    return result;
  }

  /**
   * Create a new local with a scope of the full method.
   *
   * @param localName name of new local
   * @param localType type of new local
   * @return a LocalVariable for the new local
   */
  protected LocalVariable createMethodScopeLocal(String localName, ClassDesc localType) {
    LocalVariable newVar =
        LocalVariable.of(nextLocalIndex, localName, localType, startLabel, endLabel);
    localsTable.add(newVar);
    nextLocalIndex += TypeKind.from(localType).slotSize();
    return newVar;
  }

  /**
   * Build a load constant instruction (LDC). Checks the offset of the constant pool element to be
   * loaded and generates a LDC or LDC_W, as needed.
   *
   * @param entry describes the constant pool element to be loaded
   * @return a LDC instruction
   */
  protected CodeElement buildLDCInstruction(ConstantValueEntry entry) {
    if (entry.index() > 255) {
      return ConstantInstruction.ofLoad(Opcode.LDC_W, entry);
    } else {
      return ConstantInstruction.ofLoad(Opcode.LDC, entry);
    }
  }

  /**
   * Build a load constant instruction for values of type int, short, char, byte
   *
   * @param value to be pushed
   * @return a push instruction
   */
  protected CodeElement loadIntegerConstant(final int value) {
    return switch (value) {
      case -1 -> ConstantInstruction.ofIntrinsic(Opcode.ICONST_M1);
      case 0 -> ConstantInstruction.ofIntrinsic(Opcode.ICONST_0);
      case 1 -> ConstantInstruction.ofIntrinsic(Opcode.ICONST_1);
      case 2 -> ConstantInstruction.ofIntrinsic(Opcode.ICONST_2);
      case 3 -> ConstantInstruction.ofIntrinsic(Opcode.ICONST_3);
      case 4 -> ConstantInstruction.ofIntrinsic(Opcode.ICONST_4);
      case 5 -> ConstantInstruction.ofIntrinsic(Opcode.ICONST_5);
      default ->
          (value >= Byte.MIN_VALUE && value <= Byte.MAX_VALUE)
              ? ConstantInstruction.ofArgument(Opcode.BIPUSH, value)
              : (value >= Short.MIN_VALUE && value <= Short.MAX_VALUE)
                  ? ConstantInstruction.ofArgument(Opcode.SIPUSH, value)
                  : buildLDCInstruction(poolBuilder.intEntry(value));
    };
  }
}
