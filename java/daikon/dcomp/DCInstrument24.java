package daikon.dcomp;

import static java.lang.constant.ConstantDescs.CD_Class;
import static java.lang.constant.ConstantDescs.CD_Object;
import static java.lang.constant.ConstantDescs.CD_String;
import static java.lang.constant.ConstantDescs.CD_Throwable;
import static java.lang.constant.ConstantDescs.CD_boolean;
import static java.lang.constant.ConstantDescs.CD_int;
import static java.lang.constant.ConstantDescs.CD_long;
import static java.lang.constant.ConstantDescs.CD_void;

import daikon.DynComp;
import daikon.chicory.ClassInfo;
import daikon.chicory.DaikonWriter;
import daikon.chicory.Instrument24;
import daikon.chicory.MethodGen24;
import daikon.chicory.MethodInfo;
import daikon.plumelib.bcelutil.BcelUtil;
import daikon.plumelib.bcelutil.SimpleLog;
import daikon.plumelib.options.Option;
import daikon.plumelib.reflection.Signatures;
import daikon.plumelib.util.EntryReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintStream;
import java.lang.classfile.Annotation;
import java.lang.classfile.ClassBuilder;
import java.lang.classfile.ClassElement;
import java.lang.classfile.ClassFile;
import java.lang.classfile.ClassModel;
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
import java.lang.classfile.attribute.CodeAttribute;
import java.lang.classfile.attribute.RuntimeVisibleAnnotationsAttribute;
import java.lang.classfile.constantpool.ClassEntry;
import java.lang.classfile.constantpool.ConstantPoolBuilder;
import java.lang.classfile.constantpool.ConstantValueEntry;
import java.lang.classfile.constantpool.MethodRefEntry;
import java.lang.classfile.constantpool.NameAndTypeEntry;
import java.lang.classfile.instruction.ArrayLoadInstruction;
import java.lang.classfile.instruction.ArrayStoreInstruction;
import java.lang.classfile.instruction.BranchInstruction;
import java.lang.classfile.instruction.ConstantInstruction;
import java.lang.classfile.instruction.ExceptionCatch;
import java.lang.classfile.instruction.FieldInstruction;
import java.lang.classfile.instruction.InvokeDynamicInstruction;
import java.lang.classfile.instruction.InvokeInstruction;
import java.lang.classfile.instruction.LabelTarget;
import java.lang.classfile.instruction.LineNumber;
import java.lang.classfile.instruction.LoadInstruction;
import java.lang.classfile.instruction.LocalVariable;
import java.lang.classfile.instruction.LocalVariableType;
import java.lang.classfile.instruction.LookupSwitchInstruction;
import java.lang.classfile.instruction.NewMultiArrayInstruction;
import java.lang.classfile.instruction.NewReferenceArrayInstruction;
import java.lang.classfile.instruction.NopInstruction;
import java.lang.classfile.instruction.ReturnInstruction;
import java.lang.classfile.instruction.StackInstruction;
import java.lang.classfile.instruction.StoreInstruction;
import java.lang.classfile.instruction.SwitchCase;
import java.lang.classfile.instruction.TableSwitchInstruction;
import java.lang.classfile.instruction.ThrowInstruction;
import java.lang.constant.ClassDesc;
import java.lang.constant.MethodTypeDesc;
import java.lang.reflect.AccessFlag;
import java.net.URL;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Deque;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Optional;
import java.util.Queue;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.regex.Pattern;
import org.apache.bcel.Const;
import org.apache.bcel.classfile.AnnotationEntry;
import org.apache.bcel.classfile.Annotations;
import org.apache.bcel.classfile.Attribute;
import org.apache.bcel.classfile.JavaClass;
import org.apache.bcel.classfile.Method;
import org.apache.bcel.classfile.RuntimeVisibleAnnotations;
import org.apache.bcel.generic.AnnotationEntryGen;
import org.apache.bcel.generic.ArrayType;
import org.apache.bcel.generic.BasicType;
// import org.apache.bcel.generic.BranchInstruction;
import org.apache.bcel.generic.ClassGen;
import org.apache.bcel.generic.ClassGenException;
import org.apache.bcel.generic.ConstantPoolGen;
// import org.apache.bcel.generic.FieldInstruction;
import org.apache.bcel.generic.IADD;
// import org.apache.bcel.generic.Instruction;
import org.apache.bcel.generic.InstructionFactory;
import org.apache.bcel.generic.InstructionHandle;
import org.apache.bcel.generic.InstructionList;
// import org.apache.bcel.generic.InvokeInstruction;
// import org.apache.bcel.generic.LoadInstruction;
import org.apache.bcel.generic.LocalVariableGen;
import org.apache.bcel.generic.MethodGen;
import org.apache.bcel.generic.ObjectType;
// import org.apache.bcel.generic.ReturnInstruction;
// import org.apache.bcel.generic.StoreInstruction;
import org.apache.bcel.generic.Type;
import org.apache.bcel.verifier.structurals.OperandStack;
import org.apache.commons.io.IOUtils;
import org.checkerframework.checker.lock.qual.GuardSatisfied;
import org.checkerframework.checker.nullness.qual.EnsuresNonNullIf;
import org.checkerframework.checker.nullness.qual.KeyFor;
import org.checkerframework.checker.nullness.qual.MonotonicNonNull;
import org.checkerframework.checker.nullness.qual.NonNull;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.checkerframework.checker.signature.qual.BinaryName;
import org.checkerframework.checker.signature.qual.ClassGetName;
import org.checkerframework.checker.signature.qual.DotSeparatedIdentifiers;
import org.checkerframework.checker.signature.qual.FieldDescriptor;
import org.checkerframework.checker.signature.qual.FqBinaryName;
import org.checkerframework.checker.signature.qual.MethodDescriptor;
import org.checkerframework.dataflow.qual.Pure;

/**
 * Instruments a class file to perform Dynamic Comparability.
 *
 * <p>The DCInstrument24 class is responsible for modifying another class's bytecodes. Specifically,
 * its main task is to add calls into the DynComp Runtime to calculate comparability values. These
 * added calls are sometimes referred to as "hooks".
 *
 * <p>Instrument24 and DCInstrument24 use Java's ({@code java.lang.classfile}) APIs for reading and
 * modifying .class files. Those APIs were added in JDK 24. Compared to BCEL, these APIs are more
 * complete and robust (no more fiddling with StackMaps) and are always up to date with any .class
 * file changes (since they are part of the JDK). (We will need to continue to support
 * Instrument.java using BCEL, as we anticipate our clients using JDK 21 or less for quite some
 * time.)
 */
@SuppressWarnings("nullness")
public class DCInstrument24 {

  /** UNDONE: bogus decl to compile old code. */
  protected ConstantPoolGen pool;

  /** A log to which to print debugging information about program instrumentation. */
  protected SimpleLog debugInstrument = new SimpleLog(false);

  /**
   * Used when testing to continue processing if an error occurs. Currently, This flag is only used
   * by BuildJDK.
   */
  @Option("Halt if an instrumentation error occurs")
  public static boolean quit_if_error = true;

  /** The index of this method in SharedData.methods. */
  int cur_method_info_index = 0;

  /** The MethodInfo for the current method. */
  @Nullable MethodInfo curMethodInfo;

  /** Start of user code for current method, prior to instrumenting, as a CodeModel Label. */
  protected @MonotonicNonNull Label oldStartLabel;

  /** Start of user code for current method, after instrumenting, as a CodeBuilder Label. */
  protected @MonotonicNonNull Label newStartLabel;

  /**
   * Index into current method's instruction list that points to where a call to the DynComp runtime
   * enter routine should be inserted (if needed) and where to define the newStartLabel.
   */
  protected int newStartIndex;

  // Variables used for calculating the state of the operand stack.

  /** Mapping from a label to its index in the method's codeList. */
  protected static Map<Label, Integer> labelIndexMap;

  /** Mapping from a label to its operand stack. */
  protected static Map<Label, OperandStack24> worklistHistory;

  /** State of operand stack prior to each byte code instruction. */
  protected static OperandStack24[] stacks;

  /** The type of each local variable. */
  protected static ClassDesc[] locals;

  /** Record containing a work item for the operand stack calculation. */
  record WorkItem(int instructionIndex, OperandStack24 stack) {}

  /** Queue of items for the operand stack calculation. */
  protected static Queue<WorkItem> worklist = new ArrayDeque<>();

  // Variables used for the entire class.

  /** The current ClassFile. */
  protected ClassFile classFile;

  /** The current ClassModel. */
  protected ClassModel classModel;

  /** ConstantPool builder for current class. */
  private ConstantPoolBuilder poolBuilder;

  /** True if we tracking any methods in the current class. */
  private boolean trackClass = false;

  /** ClassGen for the current class. */
  protected ClassGen gen;

  /** ClassGen for the current class. */
  protected ClassGen24 classGen;

  /** MethodGen for the current method. */
  protected MethodGen mgen;

  /** Is the current class a member of the JDK? */
  protected boolean in_jdk;

  /** The BCEL InstructionFactory for generating byte code instructions. */
  protected InstructionFactory ifact;

  /** The loader that loaded the Class to instrument. */
  protected @Nullable ClassLoader loader;

  /** Has an {@code <init>} method completed initialization? */
  protected boolean constructor_is_initialized;

  /** Local that stores the tag frame for the current method. */
  protected LocalVariable tagFrameLocal;

  // Argument descriptors
  /** Type array with two objects. */
  protected static ClassDesc[] two_objects_arg = new ClassDesc[] {CD_Object, CD_Object};

  /** Type array with an int. */
  protected static ClassDesc[] integer_arg = new ClassDesc[] {CD_int};

  /** Type array with an object. */
  protected static ClassDesc[] object_arg = new ClassDesc[] {CD_Object};

  /** ClassDesc for the special dcomp_marker argument. */
  protected final ClassDesc dcomp_marker;

  /** ClassDesc for an Object array. */
  protected static final ClassDesc objectArrayCD = CD_Object.arrayType(1);

  /** Type array with an Object array. */
  protected static final ClassDesc[] objectArrayCD_arg = new ClassDesc[] {objectArrayCD};

  /** Type array with no arguments. */
  protected static final ClassDesc[] noArgsCD = new ClassDesc[0];

  // Debug loggers
  /** Log file if debug_native is enabled. */
  protected static SimpleLog debug_native = new SimpleLog(false);

  /** Log file if debug_dup is enabled. */
  protected static SimpleLog debug_dup = new SimpleLog(false);

  /**
   * Debug information about which classes and/or methods are transformed and why. Use
   * debugInstrument for actual instrumentation details.
   */
  protected static SimpleLog debug_transform = new SimpleLog(false);

  // Flags to enable additional console output for debugging
  /** If true, enable JUnit analysis debugging. */
  protected static final boolean debugJUnitAnalysis = false;

  //  /** If true, enable {@link #getDefiningInterface} debugging. */
  // protected static final boolean debugGetDefiningInterface = false;

  /** If true, enable {@link #handleInvoke} debugging. */
  protected static final boolean debugHandleInvoke = false;

  /** If true, enable operand stack debugging. */
  protected static final boolean debugOperandStack = false;

  /** Keeps track of the methods that were not successfully instrumented. */
  protected List<String> skipped_methods = new ArrayList<>();

  /** Either "java.lang" or "daikon.dcomp". */
  protected @DotSeparatedIdentifiers String dcomp_prefix;

  /** Either "daikon.dcomp.DCRuntime" or "java.lang.DCRuntime". */
  private @DotSeparatedIdentifiers String dcompRuntimeClassname = "daikon.dcomp.DCRuntime";

  /** The ClassDesc for the DynComp runtime support class. */
  private ClassDesc runtimeCD;

  /** Set of JUnit test classes. */
  protected static Set<String> junitTestClasses = new HashSet<>();

  /** Possible states of JUnit test discovery. */
  protected enum JUnitState {
    /** Have not seen a junit class file. */
    NOT_SEEN,
    /** Have seen a junit class file. */
    STARTING,
    /** Have seen a junit class file that loads junit test classes. */
    TEST_DISCOVERY,
    /** Have completed identifing junit test classes. */
    RUNNING
  };

  /** Current state of JUnit test discovery. */
  protected static JUnitState junit_state = JUnitState.NOT_SEEN;

  /** Have we seen 'JUnitCommandLineParseResult.parse'? */
  protected static boolean junit_parse_seen = false;

  /**
   * Map from each static field name to its unique integer id. Note that while it's intuitive to
   * think that each static should show up exactly once, that is not the case. A static defined in a
   * superclass can be accessed through each of its subclasses. Tag accessor methods must be added
   * in each subclass and each should return the same id. We thus will lookup the same name multiple
   * times.
   */
  static Map<String, Integer> static_field_id = new LinkedHashMap<>();

  /**
   * Map from class name to its access_flags. Used to cache the results of the lookup done in {@link
   * #getAccessFlags}. If a class is marked ACC_ANNOTATION then it will not have been instrumented.
   */
  static Map<String, Integer> accessFlags = new HashMap<>();

  /** Integer constant of access_flag value of ACC_ANNOTATION. */
  static Integer Integer_ACC_ANNOTATION = Integer.valueOf(ClassFile.ACC_ANNOTATION);

  /**
   * Array of classes whose fields are not initialized from java. Since the fields are not
   * initialized from java, their tag storage is not allocated as part of a store, but rather must
   * be allocated as part of a load. We call a special runtime method for this so that we can check
   * for this in other cases.
   */
  protected static String[] uninit_classes =
      new String[] {
        "java.lang.String",
        "java.lang.Class",
        "java.lang.StringBuilder",
        "java.lang.AbstractStringBuilder",
      };

  /**
   * List of Object methods. Since we can't instrument Object, none of these can be instrumented,
   * and most of them don't provide useful comparability information anyway. The equals method and
   * the clone method are special-cased in the {@link #handleInvoke} routine.
   */
  protected static MethodDef[] obj_methods =
      new MethodDef[] {
        new MethodDef("finalize", new ClassDesc[0]),
        new MethodDef("getClass", new ClassDesc[0]),
        new MethodDef("hashCode", new ClassDesc[0]),
        new MethodDef("notify", new ClassDesc[0]),
        new MethodDef("notifyall", new ClassDesc[0]),
        new MethodDef("toString", new ClassDesc[0]),
        new MethodDef("wait", new ClassDesc[0]),
        new MethodDef("wait", new ClassDesc[] {CD_long}),
        new MethodDef("wait", new ClassDesc[] {CD_long, CD_int}),
      };

  /** Class that defines a method (by its name and argument types) */
  static class MethodDef {
    /** Name of method. */
    String name;

    /** Arument types for the method. */
    ClassDesc[] arg_types;

    /**
     * Create a new MethodDef.
     *
     * @param name of method
     * @param arg_types of method
     */
    MethodDef(String name, ClassDesc[] arg_types) {
      this.name = name;
      this.arg_types = arg_types;
    }

    /**
     * Equality test for MethodDef.
     *
     * @param name of method
     * @param arg_types of method
     */
    @EnsuresNonNullIf(result = true, expression = "#1")
    boolean equals(@GuardSatisfied MethodDef this, String name, ClassDesc[] arg_types) {
      if (!name.equals(this.name)) {
        return false;
      }
      if (this.arg_types.length != arg_types.length) {
        return false;
      }
      for (int ii = 0; ii < arg_types.length; ii++) {
        if (!arg_types[ii].equals(this.arg_types[ii])) {
          return false;
        }
      }
      return true;
    }

    @EnsuresNonNullIf(result = true, expression = "#1")
    @Pure
    @Override
    public boolean equals(@GuardSatisfied MethodDef this, @GuardSatisfied @Nullable Object obj) {
      if (!(obj instanceof MethodDef)) {
        return false;
      }
      MethodDef md = (MethodDef) obj;
      return equals(md.name, md.arg_types);
    }

    @Pure
    @Override
    public int hashCode(@GuardSatisfied MethodDef this) {
      int code = name.hashCode();
      for (ClassDesc arg : arg_types) {
        code += arg.hashCode();
      }
      return code;
    }
  }

  /** Initialize with the original class and whether or not the class is part of the JDK. */
  @SuppressWarnings({
    "nullness:StaticAssignmentInConstructor", // instrumentation_interface
    "nullness:initialization",
    "StaticAssignmentInConstructor" // ErrorProne: instrumentation_interface
  })
  public DCInstrument24(ClassFile classFile, ClassModel classModel, boolean in_jdk) {
    this.classFile = classFile;
    this.classModel = classModel;
    this.in_jdk = in_jdk;
    constructor_is_initialized = false;
    if (Premain.jdk_instrumented) {
      dcomp_prefix = "java.lang";
    } else {
      dcomp_prefix = "daikon.dcomp";
    }
    dcomp_marker = ClassDesc.of(Signatures.addPackage(dcomp_prefix, "DCompMarker"));
    if (BcelUtil.javaVersion == 8) {
      dcomp_prefix = "daikon.dcomp";
    }
    DCRuntime.instrumentation_interface = Signatures.addPackage(dcomp_prefix, "DCompInstrumented");

    // System.out.printf("DCInstrument %s%n", orig_class.getClassName());
    // Turn on some of the logging based on debug option.
    debugInstrument.enabled = DynComp.debug || Premain.debug_dcinstrument;
    // TEMPORARY
    debugInstrument.enabled = true;
    debugInstrument.enabled = false;
    debug_native.enabled = DynComp.debug;
    debug_transform.enabled = daikon.dcomp.Instrument24.debug_transform.enabled;

    if (debugOperandStack) {
      // Create a new PrintStream with autoflush enabled
      PrintStream newOut = new PrintStream(System.out, true);
      // Reassign System.out to the new PrintStream
      System.setOut(newOut);
      // Create a new PrintStream with autoflush enabled
      PrintStream newErr = new PrintStream(System.err, true);
      // Reassign System.err to the new PrintStream
      System.setErr(newErr);
    }
  }

  /**
   * Instruments the original class to perform dynamic comparabilty and returns the new class
   * definition.
   *
   * @return the modified JavaClass
   */
  public byte @Nullable [] instrument(ClassInfo classInfo) {

    // Don't know where I got this idea.  They are executed.  Don't remember why
    // adding dcomp marker causes problems.
    // Don't instrument annotations.  They aren't executed and adding
    // the marker argument causes subtle errors
    if (classModel.flags().has(AccessFlag.ANNOTATION)) {
      debug_transform.log("Not instrumenting annotation %s%n", classInfo.class_name);
      return null;
    }

    // If a class has an EvoSuite annotation it may be instrumented by Evosuite;
    // thus, we should not instrument it before Evosuite does.
    for (java.lang.classfile.Attribute<?> attribute : classModel.attributes()) {
      if (attribute instanceof RuntimeVisibleAnnotationsAttribute rvaa) {
        for (final Annotation item : rvaa.annotations()) {
          System.out.print("annotation: " + item.className().stringValue());
          if (item.className().stringValue().startsWith("Lorg/evosuite/runtime")) {
            debug_transform.log(
                "Not instrumenting possible Evosuite target: %s%n", classInfo.class_name);
            return null;
          }
        }
      }
    }

    try {
      return classFile.build(
          classModel.thisClass().asSymbol(),
          classBuilder -> instrumentClass(classBuilder, classModel, classInfo));
    } catch (Throwable t) {
      System.err.printf("Unexpected error %s in transform of %s%n", t, classInfo.class_name);
      t.printStackTrace();
      throw new RuntimeException("Unexpected error", t);
    }
  }

  /**
   * Instrument the current class.
   *
   * @param classBuilder for the class
   * @param classModel for the class
   * @param classInfo for the given class
   */
  private void instrumentClass(
      ClassBuilder classBuilder, ClassModel classModel, ClassInfo classInfo) {

    classGen = new ClassGen24(classModel, classInfo.class_name, classBuilder);
    runtimeCD = ClassDesc.of(dcompRuntimeClassname);
    poolBuilder = classBuilder.constantPool();

    debug_transform.log("%nInstrumenting class: %s, Attributes:%n", classInfo.class_name);
    for (java.lang.classfile.Attribute<?> a : classModel.attributes()) {
      debug_transform.log("  %s%n", a);
    }

    debugInstrument.log("Class Interfaces:%n");
    for (ClassEntry ce : classModel.interfaces()) {
      debugInstrument.log("  %s%n", ce.asInternalName());
    }

    debug_transform.indent();
    trackClass = false;

    // Handle object methods for this class
    handle_object(classGen);

    classInfo.isJunitTestClass = false;

    // Have all top-level classes implement our interface
    if (classGen.getSuperclassName().equals("java.lang.Object")) {
      @SuppressWarnings("signature:assignment")
      @MethodDescriptor String objectArgReturnBoolean = "(Ljava/lang/Object;)Z";
      // Add equals method if it doesn't already exist. This ensures
      // that an instrumented version, equals(Object, DCompMarker),
      // will be created in this class.
      MethodModel eq = classGen.containsMethod("equals", objectArgReturnBoolean);
      if (eq == null) {
        debugInstrument.log("Added equals method%n");
        add_equals_method(classBuilder, classGen, classInfo);
      }

      // Add DCompInstrumented interface and the required
      // equals_dcomp_instrumented method.
      add_dcomp_interface(classBuilder, classGen, classInfo);
    }

    // UNDONE MISSING JUNIT CODE! need isJunitTestClass to be calculated

    instrument_all_methods(classModel, classBuilder, classInfo);

    // Add tag accessor methods for each primitive in the class
    create_tag_accessors(classGen);

    // If no clinit method, we need to add our own.
    if (!classInfo.hasClinit) {
      createClinit(classBuilder, classInfo);
    }

    debug_transform.exdent();

    // The code that builds the list of daikon variables for each ppt
    // needs to know what classes are instrumented.  Its looks in the
    // Chicory runtime for this information.
    if (trackClass) {
      debug_transform.log("DCInstrument adding %s to all class list%n", classInfo.class_name);
      synchronized (daikon.chicory.SharedData.all_classes) {
        daikon.chicory.SharedData.all_classes.add(classInfo);
      }
    }

    debug_transform.log("Instrumentation complete: %s%n", classInfo.class_name);
  }

  /**
   * Adds a call to the DynComp Runtime {@code set_class_initialized} method at the begining of the
   * given method. Clients pass the class static initializer {@code <clinit>} as the method.
   *
   * @param mgen the method to modify, should be the class static initializer {@code <clinit>}
   * @param classInfo for the given class
   */
  private void addInvokeToClinit(MethodGen24 mgen, ClassInfo classInfo) {

    try {
      List<CodeElement> il = mgen.getInstructionList();
      // point to start of list
      ListIterator<CodeElement> li = il.listIterator();

      for (CodeElement ce : call_initNotify(poolBuilder, classInfo)) {
        li.add(ce);
      }
    } catch (Exception e) {
      System.err.printf("Unexpected exception encountered: %s", e);
      e.printStackTrace();
    }
  }

  /**
   * Create a class initializer method, if none exists. We need a class initializer to have a place
   * to insert a call to the DynComp Runtime {@code set_class_initialized} method.
   *
   * @param classBuilder for the given class
   * @param classInfo for the given class
   */
  private void createClinit(ClassBuilder classBuilder, ClassInfo classInfo) {

    List<CodeElement> instructions = call_initNotify(poolBuilder, classInfo);
    instructions.add(ReturnInstruction.of(TypeKind.VOID)); // need to return!

    classBuilder.withMethod(
        "<clinit>",
        MethodTypeDesc.of(CD_void),
        ClassFile.ACC_STATIC,
        methodBuilder ->
            methodBuilder.withCode(codeBuilder -> copyCode(codeBuilder, instructions)));
  }

  /**
   * Create the list of instructions for a call to {@code set_class_initialized}. This is used to
   * keep track of when the class is initialized (so we don't look for fields in uninitialized
   * classes).
   *
   * @param poolBuilder for the given class
   * @param classInfo for the given class
   * @return the instruction list
   */
  private List<CodeElement> call_initNotify(ConstantPoolBuilder poolBuilder, ClassInfo classInfo) {

    List<CodeElement> instructions = new ArrayList<>();

    MethodRefEntry mre =
        poolBuilder.methodRefEntry(
            runtimeCD, "set_class_initialized", MethodTypeDesc.of(CD_void, CD_String));
    instructions.add(buildLDCInstruction(poolBuilder.stringEntry(classInfo.class_name)));
    instructions.add(InvokeInstruction.of(Opcode.INVOKESTATIC, mre));

    return instructions;
  }

  /**
   * Instruments all the methods in a class. For each method, adds instrumentation code at the entry
   * and at each return from the method. In addition, changes each return statement to first place
   * the value being returned into a local and then return. This allows us to work around the JDI
   * deficiency of not being able to query return values.
   *
   * @param classModel for current class
   * @param classBuilder for current class
   * @param classInfo for the given class
   */
  private void instrument_all_methods(
      ClassModel classModel, ClassBuilder classBuilder, ClassInfo classInfo) {

    @BinaryName String classname = classInfo.class_name;

    if (classModel.majorVersion() < ClassFile.JAVA_6_VERSION) {
      System.out.printf(
          "DynComp warning: ClassFile: %s - classfile version (%d) is out of date and may not be"
              + " processed correctly.%n",
          classname, classModel.majorVersion());
    }

    // Process each method in the class
    for (MethodModel mm : classModel.methods()) {

      try {

        MethodGen24 mgen = new MethodGen24(mm, classname, classBuilder);

        if (debugInstrument.enabled) {
          ClassDesc[] paramTypes = mgen.getParameterTypes();
          String[] paramNames = mgen.getParameterNames();
          LocalVariable[] local_vars = mgen.getOriginalLocalVariables();
          String types = "", names = "", locals = "";

          for (int j = 0; j < paramTypes.length; j++) {
            @SuppressWarnings("signature:assignment") // need JDK annotations
            @FieldDescriptor String paramFD = paramTypes[j].descriptorString();
            types = types + convertDescriptorToFqBinaryName(paramFD) + " ";
          }
          for (int j = 0; j < paramNames.length; j++) {
            names = names + paramNames[j] + " ";
          }
          for (int j = 0; j < local_vars.length; j++) {
            locals = locals + local_vars[j].name().stringValue() + " ";
          }
          debugInstrument.log("%nMethod = %s%n", mgen);
          debugInstrument.log("paramTypes(%d): %s%n", paramTypes.length, types);
          debugInstrument.log("paramNames(%d): %s%n", paramNames.length, names);
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

        // Note whether we want to track the daikon variables in this method
        // boolean track = should_track(classname, mgen.getName(), methodEntryName(classname,
        // mgen));
        boolean track = should_track(classname, mgen.getName(), classname + mgen);

        // We do not want to track bridge methods the compiler has synthesized as
        // they are overloaded on return type which normal Java does not support.
        if ((mgen.getAccessFlagsMask() & ClassFile.ACC_BRIDGE) != 0) {
          track = false;
        }

        // If any one method is tracked, then the class is tracked.
        if (track) {
          trackClass = true;
        }

        // If we are tracking variables, make sure the class is public
        int access_flags = classModel.flags().flagsMask();
        if (track && (access_flags & ClassFile.ACC_PUBLIC) == 0) {
          access_flags |= ClassFile.ACC_PUBLIC;
          access_flags &= ~ClassFile.ACC_PROTECTED;
          access_flags &= ~ClassFile.ACC_PRIVATE;
        }
        // reset class access flags in case they have been changed
        classBuilder.withFlags(access_flags);

        // debug_transform.log("  Processing method %s, track=%b%n", simplify_method_name(mgen),
        // track);
        debug_transform.log("  Processing method %s, track=%b%n", mgen, track);
        debug_transform.indent();

        // check for the class static initializer method
        if (mgen.isClinit()) {
          classInfo.hasClinit = true;
          addInvokeToClinit(mgen, classInfo);
          // We are not going to add any additional instrumentation to this method.
          // We need to copy it to the output class.
          outputMethodUnchanged(classBuilder, mm, mgen);
          continue;
        }

        // local variables referenced from a lambda expression must be final or effectively final
        final boolean trackMethod = track;
        boolean addingDcompArg = true;
        if (mgen.isMain()) {
          addingDcompArg = false;
          createMainStub(mgen, classBuilder, classInfo);
        }

        boolean replacingMethod = !addingDcompArg || classInfo.isJunitTestClass;
        if (!replacingMethod) {
          // make copy of original method
          debugInstrument.log("Copying method: %s%n", mgen.getName());
          debugInstrument.indent();
          outputMethodUnchanged(classBuilder, mm, mgen);
          debugInstrument.exdent();
          debugInstrument.log("End of copy%n");
        }

        MethodTypeDesc mtd = mm.methodTypeSymbol();
        if (addingDcompArg) {
          // The original parameterList is immutable, so we need to make a copy.
          List<ClassDesc> paramList = new ArrayList<ClassDesc>(mtd.parameterList());
          paramList.add(dcomp_marker);
          mtd = MethodTypeDesc.of(mtd.returnType(), paramList);
        }
        classBuilder.withMethod(
            mm.methodName().stringValue(),
            mtd,
            mm.flags().flagsMask(),
            methodBuilder -> instrumentMethod(methodBuilder, mm, mgen, classInfo, trackMethod));

        debug_transform.exdent();
      } catch (Throwable t) {
        if (debugInstrument.enabled) {
          t.printStackTrace();
        }
        throw new Error(
            "Unexpected error processing " + classname + "." + mm.methodName().stringValue(), t);
      }
    }

    // Copy all other ClassElements to output class (unchanged).
    for (ClassElement ce : classModel) {
      debugInstrument.log("ClassElement: %s%n", ce);
      switch (ce) {
        case MethodModel mm -> {}
        // Copy all other ClassElements to output class (unchanged).
        default -> classBuilder.with(ce);
      }
    }
  }

  /**
   * Copy the given method from the input class file to the output output class with no changes.
   * Uses {@code copyMethod} to perform the actual copy.
   *
   * @param classBuilder for the output class
   * @param mm MethodModel describes the input method
   * @param mgen describes the output method
   */
  private void outputMethodUnchanged(ClassBuilder classBuilder, MethodModel mm, MethodGen24 mgen) {
    classBuilder.withMethod(
        mm.methodName().stringValue(),
        mm.methodTypeSymbol(),
        mm.flags().flagsMask(),
        methodBuilder -> copyMethod(methodBuilder, mm, mgen));
  }

  /**
   * Copy the given method from the input class file to the output output class with no changes.
   *
   * @param methodBuilder for the output class
   * @param methodModel describes the input method
   * @param mgen describes the output method
   */
  private void copyMethod(MethodBuilder methodBuilder, MethodModel methodModel, MethodGen24 mgen) {

    for (MethodElement me : methodModel) {
      debugInstrument.log("MethodElement: %s%n", me);
      switch (me) {
        case CodeModel codeModel ->
            methodBuilder.withCode(codeBuilder -> copyCode(codeBuilder, mgen.getInstructionList()));

        // copy all other MethodElements to output class (unchanged)
        default -> methodBuilder.with(me);
      }
    }
  }

  /**
   * Copy an instruction list into the given method.
   *
   * @param codeBuilder for the given method's code
   * @param instructions instruction list to copy
   */
  private void copyCode(CodeBuilder codeBuilder, List<CodeElement> instructions) {

    for (CodeElement ce : instructions) {
      debugInstrument.log("CodeElement: %s%n", ce);
      codeBuilder.with(ce);
    }
  }

  /**
   * Instrument the given method using {@link #instrumentCode}.
   *
   * @param methodBuilder for the given method
   * @param methodModel for the given method
   * @param mgen describes the given method
   * @param classInfo for the current class
   * @param trackMethod true iff we need to track the daikon variables in this method
   */
  private void instrumentMethod(
      MethodBuilder methodBuilder,
      MethodModel methodModel,
      MethodGen24 mgen,
      ClassInfo classInfo,
      boolean trackMethod) {

    for (MethodElement me : methodModel) {
      debugInstrument.log("MethodElement: %s%n", me);
      switch (me) {
        case CodeModel codeModel ->
            methodBuilder.withCode(
                codeBuilder ->
                    instrumentCode(codeBuilder, codeModel, mgen, classInfo, trackMethod));

        // copy all other MethodElements to output class (unchanged)
        default -> methodBuilder.with(me);
      }
    }
  }

  //  /**
  //   * Insert the our instrumentation code into the instruction list for the given method. This
  //   * includes adding instrumentation code at the entry and at each return from the method. In
  //   * addition, it changes each return statement to first place the value being returned into a
  // local
  //   * and then return.
  //   *
  //   * @param instructions instruction list for method
  //   * @param mgen describes the given method
  //   * @param curMethodInfo provides additional information about the method
  //   * @param minfo for the given method's code
  //   */
  //  private void insertInstrumentationCode(
  //      List<CodeElement> instructions,
  //      MethodGen24 mgen,
  //      MethodInfo curMethodInfo,
  //      MethodGen24.MInfo24 minfo) {
  //
  //    // Add nonce local to matchup enter/exits
  //    // addInstrumentationAtEntry(instructions, mgen, minfo);
  //
  //    // debugInstrument.log("Modified code: %s%n", mgen.getMethod().getCode());
  //
  //    assert curMethodInfo != null : "@AssumeAssertion(nullness): can't get here if null";
  //    Iterator<Boolean> shouldIncludeIter = curMethodInfo.is_included.iterator();
  //    Iterator<Integer> exitLocationIter = curMethodInfo.exit_locations.iterator();
  //
  //    // instrument return instructions
  //    ListIterator<CodeElement> li = instructions.listIterator();
  //    while (li.hasNext()) {
  //
  //      CodeElement inst = li.next();
  //
  //      // back up iterator to point to 'inst'
  //      li.previous();
  //
  //      // If this is a return instruction, insert method exit instrumentation
  //      // List<CodeElement> new_il =
  //      // generate_return_instrumentation(inst, mgen, minfo, shouldIncludeIter,
  // exitLocationIter);
  //
  //      // insert code prior to 'inst'
  //      // for (CodeElement ce : new_il) {
  //      // li.add(ce);
  //      // }
  //
  //      // skip over 'inst' we just inserted new_il in front of
  //      li.next();
  //    }
  //  }

  /**
   * Generate instrumentation code for the given method. This includes reading in and processing the
   * original instruction list, calling {@code insertInstrumentationCode} to add the instrumentation
   * code, and then copying the modified instruction list to the output method while updating the
   * code labels, if needed.
   *
   * @param codeBuilder for the given method's code
   * @param codeModel for the input method's code
   * @param mgen describes the output method
   * @param classInfo for the current class
   * @param trackMethod true iff we need to track the daikon variables in this method
   */
  private void instrumentCode(
      CodeBuilder codeBuilder,
      CodeModel codeModel,
      MethodGen24 mgen,
      ClassInfo classInfo,
      boolean trackMethod) {

    // method_info_index is not used at this point in DCInstrument
    MethodGen24.MInfo24 minfo = new MethodGen24.MInfo24(0, mgen.getMaxLocals(), codeBuilder);
    // Add back in any unused parameters that the Java compiler has optimized out.
    if (mgen.fixLocals(minfo)) {
      // localsTable was changed
    }

    // If the method is native
    if ((mgen.getAccessFlagsMask() & ClassFile.ACC_NATIVE) != 0) {

      // Create Java code that cleans up the tag stack and calls the real native method.
      // fix_native(gen, mgen);

      // Add the DCompMarker argument to distinguish our version
      // add_dcomp_arg(mgen);

    } else { // normal method

      if (!classInfo.isJunitTestClass) {
        // Add the DCompMarker argument to distinguish our version
        add_dcomp_arg(mgen, minfo);
      }

      debugInstrument.log("Revised LocalVariableTable:%n");
      for (LocalVariable lv : mgen.localsTable) {
        debugInstrument.log("  %s%n", lv);
      }
      int paramCount = (mgen.isStatic() ? 0 : 1) + mgen.getParameterTypes().length;
      debugInstrument.log("paramCount: %d%n", paramCount);

      // Create a MethodInfo that describes this method's arguments
      // and exit line numbers (information not available via reflection)
      // and add it to the list for this class.
      MethodInfo mi = null;
      if (trackMethod) {
        mi = create_method_info(classInfo, mgen);
        classInfo.method_infos.add(mi);
        DCRuntime.methods.add(mi);
      }

      @SuppressWarnings("JdkObsolete")
      List<CodeElement> codeList = new LinkedList<>();

      // no codeModel if DCInstrument24 generated the method
      if (codeModel != null) {
        debugInstrument.log("Code Attributes:%n");
        for (java.lang.classfile.Attribute<?> a : codeModel.attributes()) {
          debugInstrument.log("  %s%n", a);
        }
      }

      // The localsTable was initialized in the MethodGen24 constructor. Here we initialize the
      // codeList. We also remove the local variable type records. Some instrumentation changes
      // require these to be updated, but it should be safe to just delete them since the
      // LocalVariableTypeTable is optional and really only of use to a debugger. We also save the
      // CodeModel label at the start of the byte codes, if there is one. If there isn't, that is
      // okay as it means the original code did not reference byte code offset 0 so inserting our
      // instrumentation code at that point will not cause a problem.
      @SuppressWarnings("nullness:assignment") // can't have gotten here if CodeAttribute is null
      @NonNull CodeAttribute ca = mgen.getCodeAttribute();
      for (CodeElement ce : mgen.getInstructionList()) {
        debugInstrument.log("CodeElement: %s%n", ce);
        switch (ce) {
          case LocalVariable lv -> {} // we have alreay processed these
          // debugging code
          // case LocalVariable lv -> {
          // System.out.printf("Local start: %s end: %s%n", lv.startScope(), lv.endScope()); }
          case LocalVariableType lvt -> {} // we can discard local variable types
          // debuging code
          // case LocalVariableType lvt -> {
          // @FieldDescriptor String lvFD = lvt.signatureSymbol().signatureString();
          // System.out.printf("  %s : %s%n", lvt, convertDescriptorToFqBinaryName(lvFD)); }
          case LabelTarget l -> {
            if (ca.labelToBci(l.label()) == 0) {
              oldStartLabel = l.label();
            }
            codeList.add(ce);
          }
          default -> codeList.add(ce); // save all other elements
        }
      }

      // Create the local to store the tag frame for this method
      tagFrameLocal = createTagFrameLocal(mgen, minfo);

      // Create newStartLabel now so instrumentCodeList can use it.
      newStartLabel = codeBuilder.newLabel();

      // Instrument the method
      instrumentCodeList(codeModel, mgen, minfo, codeList);

      if (trackMethod) {
        add_enter(mgen, minfo, codeList, DCRuntime.methods.size() - 1);
        add_exit(mgen, mi, minfo, codeList, DCRuntime.methods.size() - 1);
      }

      // Copy the modified local variable table to the output class.
      debugInstrument.log("LocalVariableTable:%n");
      for (LocalVariable lv : mgen.localsTable) {
        codeBuilder.localVariable(
            lv.slot(), lv.name().stringValue(), lv.typeSymbol(), lv.startScope(), lv.endScope());
        @SuppressWarnings("signature:assignment") // need JDK annotations
        @FieldDescriptor String lvFD = lv.typeSymbol().descriptorString();
        debugInstrument.log("  %s : %s%n", lv, convertDescriptorToFqBinaryName(lvFD));
      }

      // Copy the modified instruction list to the output class.
      ListIterator<CodeElement> li = codeList.listIterator();
      CodeElement ce = null;
      while (li.hasNext()) {
        if (li.nextIndex() == newStartIndex) {
          codeBuilder.labelBinding(newStartLabel);
          debugInstrument.log("Label: %s%n", newStartLabel);
        }
        ce = li.next();
        // If this instruction references a Label, we need to see if it is the oldStartLabel
        // and, if so, replace the target with the newStartLabel.
        ce = retargetStartLabel(ce);
        debugInstrument.log("CodeElement: %s%n", ce);
        codeBuilder.with(ce);
      }

      // build_exception_handler returns null if there isn't one.
      List<CodeElement> handlerCode = build_exception_handler(mgen);
      if (handlerCode != null) {
        Label handlerLabel = codeBuilder.newBoundLabel();
        for (CodeElement ceh : handlerCode) {
          codeBuilder.with(ceh);
        }
        // Using handlerLabel for the end of the try region is technically one instruction
        // too many, but it shouldn't matter and is easily available.
        codeBuilder.exceptionCatch(newStartLabel, handlerLabel, handlerLabel, CD_Throwable);
      }
    }

    // UNDONE needs to be moved?
    // We do not want to copy the @HotSpotIntrinsicCandidate annotations from
    // the original method to our instrumented method as the signature will
    // not match anything in the JVM's list.  This won't cause an execution
    // problem but will produce a massive number of warnings.
    // JDK 11: @HotSpotIntrinsicCandidate
    // JDK 17: @IntrinsicCandidate
    // AnnotationEntryGen[] aes = mgen.getAnnotationEntries();
    // for (AnnotationEntryGen item : aes) {
    // String type = item.getTypeName();
    // if (type.endsWith("IntrinsicCandidate;")) {
    // mgen.removeAnnotationEntry(item);
    // }
    // }

    // UNDONE
    //   can we do this with java.lang.classfile?
    //   needs to be moved?
    //    il = mg.getInstructionList();
    //    InstructionHandle end = il.getEnd();
    //    int length = end.getPosition() + end.getInstruction().getLength();
    //    if (length >= Const.MAX_CODE_SIZE) {
    //      throw new ClassGenException(
    //          "Code array too big: must be smaller than " + Const.MAX_CODE_SIZE + " bytes.");
    //    }
  }

  /** old instrumenter - to be removed */
  public JavaClass old_instrument() {

    @BinaryName String classname = gen.getClassName();

    // Don't know where I got this idea.  They are executed.  Don't remember why
    // adding dcomp marker causes problems.
    // Don't instrument annotations.  They aren't executed and adding
    // the marker argument causes subtle errors
    if ((gen.getModifiers() & Const.ACC_ANNOTATION) != 0) {
      debug_transform.log("Not instrumenting annotation %s%n", classname);
      // WHY NOT RETURN NULL?
      return gen.getJavaClass().copy();
    }

    // If a class has an EvoSuite annotation it may be instrumented by Evosuite;
    // thus, we should not instrument it before Evosuite does.
    //    for (final Attribute attribute : orig_class.getAttributes()) {
    //      if (attribute instanceof RuntimeVisibleAnnotations) {
    //        for (final AnnotationEntry item : ((Annotations) attribute).getAnnotationEntries()) {
    //          if (item.toString().startsWith("@Lorg/evosuite/runtime")) {
    //            debug_transform.log("Not instrumenting possible Evosuite target: %s%n",
    // classname);
    //            // WHY NOT RETURN NULL?
    //            return gen.getJavaClass().copy();
    //          }
    //        }
    //      }
    //    }

    debug_transform.log("Instrumenting class %s%n", classname);
    debug_transform.indent();

    // Create the ClassInfo for this class and its list of methods
    ClassInfo class_info = new ClassInfo(classname, loader);
    boolean track_class = false;

    // Handle object methods for this class
    // handle_object(classGen);

    // Have all top-level classes implement our interface
    if (gen.getSuperclassName().equals("java.lang.Object")) {
      // Add equals method if it doesn't already exist. This ensures
      // that an instrumented version, equals(Object, DCompMarker),
      // will be created in this class.
      Method eq = gen.containsMethod("equals", "(Ljava/lang/Object;)Z");
      if (eq == null) {
        debugInstrument.log("Added equals method%n");
        // add_equals_method(gen);
      }

      // Add DCompInstrumented interface and the required
      // equals_dcomp_instrumented method.
      // add_dcomp_interface(gen);
    }

    // A very tricky special case: If JUnit is running and the current
    // class has been passed to JUnit on the command line, then this
    // is a JUnit test class and our normal instrumentation will
    // cause JUnit to complain about multiple constructors and
    // methods that should have no arguments. To work around these
    // restrictions, we replace rather than duplicate each method
    // we instrument and we do not add the dcomp marker argument.
    // We must also remember the class name so if we see a subsequent
    // call to one of its methods we do not add the dcomp argument.

    debugInstrument.log("junit_state: %s%n", junit_state);

    StackTraceElement[] stack_trace;

    switch (junit_state) {
      case NOT_SEEN:
        if (classname.startsWith("org.junit")) {
          junit_state = JUnitState.STARTING;
        }
        break;

      case STARTING:
        // Now check to see if JUnit is looking for test class(es).
        stack_trace = Thread.currentThread().getStackTrace();
        // [0] is getStackTrace
        for (int i = 1; i < stack_trace.length; i++) {
          if (debugJUnitAnalysis) {
            System.out.printf(
                "%s : %s%n", stack_trace[i].getClassName(), stack_trace[i].getMethodName());
          }
          if (isJunitTrigger(stack_trace[i].getClassName(), stack_trace[i].getMethodName())) {
            junit_parse_seen = true;
            junit_state = JUnitState.TEST_DISCOVERY;
            break;
          }
        }
        break;

      case TEST_DISCOVERY:
        // Now check to see if JUnit is done looking for test class(es).
        boolean local_junit_parse_seen = false;
        stack_trace = Thread.currentThread().getStackTrace();
        // [0] is getStackTrace
        for (int i = 1; i < stack_trace.length; i++) {
          if (debugJUnitAnalysis) {
            System.out.printf(
                "%s : %s%n", stack_trace[i].getClassName(), stack_trace[i].getMethodName());
          }
          if (isJunitTrigger(stack_trace[i].getClassName(), stack_trace[i].getMethodName())) {
            local_junit_parse_seen = true;
            break;
          }
        }
        if (junit_parse_seen && !local_junit_parse_seen) {
          junit_parse_seen = false;
          junit_state = JUnitState.RUNNING;
        } else if (!junit_parse_seen && local_junit_parse_seen) {
          junit_parse_seen = true;
        }
        break;

      case RUNNING:
        if (debugJUnitAnalysis) {
          stack_trace = Thread.currentThread().getStackTrace();
          // [0] is getStackTrace
          for (int i = 1; i < stack_trace.length; i++) {
            System.out.printf(
                "%s : %s%n", stack_trace[i].getClassName(), stack_trace[i].getMethodName());
          }
        }
        // nothing to do
        break;

      default:
        throw new Error("invalid junit_state");
    }

    debugInstrument.log("junit_state: %s%n", junit_state);

    boolean junit_test_class = false;
    if (junit_state == JUnitState.TEST_DISCOVERY) {
      // We have a possible JUnit test class.  We need to verify by
      // one of two methods.  Either the class is a subclass of
      // junit.framework.TestCase or one of its methods has a
      // RuntimeVisibleAnnotation of org/junit/Test.
      Deque<String> classnameStack = new ArrayDeque<>();
      String super_class;
      String this_class = classname;
      while (true) {
        super_class = getSuperclassName(this_class);
        if (super_class == null) {
          // something has gone wrong
          break;
        }
        if (debugJUnitAnalysis) {
          System.out.printf("this_class: %s%n", this_class);
          System.out.printf("super_class: %s%n", super_class);
        }
        if (super_class.equals("junit.framework.TestCase")) {
          // This is a junit test class and so are the
          // elements of classnameStack.
          junit_test_class = true;
          junitTestClasses.add(this_class);
          while (!classnameStack.isEmpty()) {
            junitTestClasses.add(classnameStack.pop());
          }
          break;
        } else if (super_class.equals("java.lang.Object")) {
          // We're done; not a junit test class.
          // Ignore items on classnameStack.
          break;
        }
        // Recurse and check the super_class.
        classnameStack.push(this_class);
        this_class = super_class;
      }
    }

    // Even if we have not detected that JUnit is active, any class that
    // contains a method with a RuntimeVisibleAnnotation of org/junit/Test
    // needs to be marked as a JUnit test class. (Daikon issue #536)

    if (!junit_test_class) {
      // need to check for junit Test annotation on a method
      searchloop:
      for (Method m : gen.getMethods()) {
        for (final Attribute attribute : m.getAttributes()) {
          if (attribute instanceof RuntimeVisibleAnnotations) {
            if (debugJUnitAnalysis) {
              System.out.printf("attribute: %s%n", attribute.toString());
            }
            for (final AnnotationEntry item : ((Annotations) attribute).getAnnotationEntries()) {
              if (debugJUnitAnalysis) {
                System.out.printf("item: %s%n", item.toString());
              }
              if (item.toString().endsWith("org/junit/Test;") // JUnit 4
                  || item.toString().endsWith("org/junit/jupiter/api/Test;") // JUnit 5
              ) {
                junit_test_class = true;
                junitTestClasses.add(classname);
                break searchloop;
              }
            }
          }
        }
      }
    }

    if (junit_test_class) {
      debugInstrument.log("JUnit test class: %s%n", classname);
    } else {
      debugInstrument.log("Not a JUnit test class: %s%n", classname);
    }

    // Process each method
    for (Method m : gen.getMethods()) {

      try {
        // Note whether we want to track the daikon variables in this method
        boolean track = should_track(classname, m.getName(), methodEntryName(classname, m));

        // We do not want to track bridge methods the compiler has synthesized as
        // they are overloaded on return type which normal Java does not support.
        if ((m.getAccessFlags() & Const.ACC_BRIDGE) != 0) {
          track = false;
        }

        // If any one method is tracked, then the class is tracked.
        if (track) {
          track_class = true;
        }

        // If we are tracking variables, make sure the class is public
        if (track && !gen.isPublic()) {
          gen.isPrivate(false);
          gen.isProtected(false);
          gen.isPublic(true);
        }

        debug_transform.log("  Processing method %s, track=%b%n", simplify_method_name(m), track);
        debug_transform.indent();

        MethodGen mg = new MethodGen(m, classname, pool);
        mgen = mg; // copy to global

        InstructionList il = mg.getInstructionList();
        boolean has_code = (il != null);
        //        if (has_code) {
        //          setCurrentStackMapTable(mg, gen.getMajor());
        //          buildUninitializedNewMap(il);
        //        }
        //
        //        fixLocalVariableTable(mg);

        // If the method is native
        if (mg.isNative()) {

          // Create Java code that cleans up the tag stack and calls the real native method.
          fix_native(gen, mg);
          has_code = true;
          //          setCurrentStackMapTable(mg, gen.getMajor());

          // Add the DCompMarker argument to distinguish our version
          // add_dcomp_arg(mg);

        } else { // normal method

          if (!junit_test_class) {
            // Add the DCompMarker argument to distinguish our version
            // add_dcomp_arg(mg);
          }

          // Create a MethodInfo that describes this method's arguments
          // and exit line numbers (information not available via reflection)
          // and add it to the list for this class.
          MethodInfo mi = null;
          if (track && has_code) {
            //         mi = create_method_info(class_info, mg);
            class_info.method_infos.add(mi);
            DCRuntime.methods.add(mi);
          }

          // Instrument the method
          if (has_code) {
            // Create the local to store the tag frame for this method
            // tag_frame_local = create_tag_frame_local(mg);
            // build_exception_handler(mg);
            // instrument_method(mg);
            if (track) {
              // add_enter(mg, mi, DCRuntime.methods.size() - 1);
              // add_exit(mg, mi, DCRuntime.methods.size() - 1);
            }
            // install_exception_handler(mg);
          }
        }

        if (has_code) {
          //          updateUninitializedNewOffsets(mg.getInstructionList());
          //          createNewStackMapAttribute(mg);
          mg.setMaxLocals();
          mg.setMaxStack();
        } else {
          mg.removeCodeAttributes();
          mg.removeLocalVariables();
        }

        //        remove_local_variable_type_table(mg);

        // We do not want to copy the @HotSpotIntrinsicCandidate annotations from
        // the original method to our instrumented method as the signature will
        // not match anything in the JVM's list.  This won't cause an execution
        // problem but will produce a massive number of warnings.
        // JDK 11: @HotSpotIntrinsicCandidate
        // JDK 17: @IntrinsicCandidate
        AnnotationEntryGen[] aes = mg.getAnnotationEntries();
        for (AnnotationEntryGen item : aes) {
          String type = item.getTypeName();
          if (type.endsWith("IntrinsicCandidate;")) {
            mg.removeAnnotationEntry(item);
          }
        }

        // Can't duplicate 'main' or 'clinit' or a JUnit test.
        boolean replacingMethod = BcelUtil.isMain(mg) || BcelUtil.isClinit(mg) || junit_test_class;
        try {
          if (has_code) {
            il = mg.getInstructionList();
            InstructionHandle end = il.getEnd();
            int length = end.getPosition() + end.getInstruction().getLength();
            if (length >= Const.MAX_CODE_SIZE) {
              throw new ClassGenException(
                  "Code array too big: must be smaller than " + Const.MAX_CODE_SIZE + " bytes.");
            }
          }
          if (replacingMethod) {
            gen.replaceMethod(m, mg.getMethod());
            if (BcelUtil.isMain(mg)) {
              // gen.addMethod(create_dcomp_stub(mg).getMethod());
            }
          } else {
            gen.addMethod(mg.getMethod());
          }
        } catch (Exception e) {
          String s = e.getMessage();
          if (s == null) {
            throw e;
          }
          if (s.startsWith("Branch target offset too large")
              || s.startsWith("Code array too big")) {
            System.err.printf(
                "DynComp warning: ClassFile: %s - method %s is too large to instrument and is"
                    + " being skipped.%n",
                classname, mg.getName());
            // Build a dummy instrumented method that has DCompMarker
            // argument and no instrumentation.
            // first, restore unmodified method
            mg = new MethodGen(m, classname, pool);
            // restore StackMapTable
            //            setCurrentStackMapTable(mg, gen.getMajor());
            // Add the DCompMarker argument
            // add_dcomp_arg(mg);
            //            remove_local_variable_type_table(mg);
            // try again
            if (replacingMethod) {
              gen.replaceMethod(m, mg.getMethod());
              if (BcelUtil.isMain(mg)) {
                //  gen.addMethod(create_dcomp_stub(mg).getMethod());
              }
            } else {
              gen.addMethod(mg.getMethod());
            }
          } else {
            throw e;
          }
        }
        debug_transform.exdent();
      } catch (Throwable t) {
        // debug code
        // t.printStackTrace();
        if (debugInstrument.enabled) {
          t.printStackTrace();
        }
        throw new Error("Unexpected error processing " + classname + "." + m.getName(), t);
      }
    }

    // Add tag accessor methods for each primitive in the class
    // create_tag_accessors(gen);

    // Keep track of when the class is initialized (so we don't look
    // for fields in uninitialized classes)
    // track_class_init();
    debug_transform.exdent();

    // The code that builds the list of daikon variables for each ppt
    // needs to know what classes are instrumented.  Its looks in the
    // Chicory runtime for this information.
    if (track_class) {
      debug_transform.log("DCInstrument adding %s to all class list%n", class_info);
      synchronized (daikon.chicory.SharedData.all_classes) {
        daikon.chicory.SharedData.all_classes.add(class_info);
      }
    }
    debug_transform.log("Instrumentation complete: %s%n", classname);

    return gen.getJavaClass().copy();
  }

  /**
   * Returns true if the specified classname.method_name is the root of JUnit startup code.
   *
   * @param classname class to be checked
   * @param method_name method to be checked
   * @return true if the given method is a JUnit trigger
   */
  boolean isJunitTrigger(String classname, String method_name) {
    if ((classname.contains("JUnitCommandLineParseResult")
            && method_name.equals("parse")) // JUnit 4
        || (classname.contains("EngineDiscoveryRequestResolution")
            && method_name.equals("resolve")) // JUnit 5
    ) {
      return true;
    }
    return false;
  }

  // General Java Runtime instrumentation strategy:
  //
  // <p>It is a bit of a misnomer, but the Daikon code and documentation uses the term JDK to refer
  // to the Java Runtime Environment class libraries. In Java 8 and earlier, they were usually found
  // in {@code <your java installation>/jre/lib/rt.jar}. For these versions of Java, we
  // pre-instrumented the entire rt.jar.
  //
  // <p>In Java 9 and later, the Java Runtime classes have been divided into modules that are
  // usually found in: {@code <your java installation>/jmods/*.jmod}.
  //
  // <p>With the conversion to modules for Java 9 and beyond, we have elected to pre-instrument only
  // java.base.jmod and instrument all other Java Runtime (aka JDK) classes dynamically as they are
  // loaded.
  //
  // <p>Post Java 8 there are increased security checks when loading JDK classes. In particular, the
  // core classes contained in the java.base module may not reference anything outside of java.base.
  // This means we cannot pre-instrument classes in the same manner as was done for Java 8 as this
  // would introduce external references to the DynComp runtime (DCRuntime.java).
  //
  // <p>However, we can get around this restriction in the following manner: We create a shadow
  // DynComp runtime called java.lang.DCRuntime that contains all the public methods of
  // daikon.dcomp.DCRuntime, but with method bodies that contain only a return statement. We
  // pre-instrument java.base the same as we would for JDK 8, but change all references to
  // daikon.dcomp.DCRuntime to refer to java.lang.DCRuntime instead, and thus pass the security load
  // test. During DynComp initialization (in Premain) we use java.lang.instrument.redefineClasses to
  // replace the dummy java.lang.DCRuntime with a version where each method calls the corresponding
  // method in daikon.dcomp.DCRuntime. The Java runtime does not enforce the security check in this
  // case.

  /**
   * Instruments a JDK class to perform dynamic comparability and returns the new class definition.
   * A second version of each method in the class is created which is instrumented for
   * comparability.
   *
   * @return the modified JavaClass
   */
  public JavaClass instrument_jdk() {

    String classname = gen.getClassName();

    // Don't instrument annotations.  They aren't executed and adding
    // the marker argument causes subtle errors
    if ((gen.getModifiers() & Const.ACC_ANNOTATION) != 0) {
      debug_transform.log("Not instrumenting annotation %s%n", classname);
      // MUST NOT RETURN NULL
      return gen.getJavaClass().copy();
    }

    int i = classname.lastIndexOf('.');
    if (i > 0) {
      // Don't instrument problem packages.
      // See Premain.java for a list and explainations.
      String packageName = classname.substring(0, i);
      if (Premain.problem_packages.contains(packageName)) {
        debug_transform.log("Skipping problem package %s%n", packageName);
        return gen.getJavaClass().copy();
      }
    }

    if (BcelUtil.javaVersion > 8) {
      // Don't instrument problem classes.
      // See Premain.java for a list and explainations.
      if (Premain.problem_classes.contains(classname)) {
        debug_transform.log("Skipping problem class %s%n", classname);
        return gen.getJavaClass().copy();
      }
      dcompRuntimeClassname = "java.lang.DCRuntime";
    }

    debug_transform.log("Instrumenting class(JDK) %s%n", classname);
    debug_transform.indent();

    // Handle object methods for this class
    // handle_object(classGen);

    // Have all top-level classes implement our interface
    if (gen.getSuperclassName().equals("java.lang.Object")) {
      // Add equals method if it doesn't already exist. This ensures
      // that an instrumented version, equals(Object, DCompMarker),
      // will be created in this class.
      Method eq = gen.containsMethod("equals", "(Ljava/lang/Object;)Z");
      if (eq == null) {
        debugInstrument.log("Added equals method%n");
        // add_equals_method(gen);
      }
      // Add DCompInstrumented interface and the required
      // equals_dcomp_instrumented method.
      // add_dcomp_interface(gen);
    }

    // Process each method
    for (Method m : gen.getMethods()) {

      // tag_frame_local = null;
      try {
        // Don't modify class initialization methods.  They can't affect
        // user comparability and there isn't any way to get a second
        // copy of them.
        if (BcelUtil.isClinit(m)) {
          continue;
        }

        debug_transform.log("  Processing method %s%n", simplify_method_name(m));
        debug_transform.indent();

        MethodGen mg = new MethodGen(m, classname, pool);
        mgen = mg; // copy to global

        InstructionList il = mg.getInstructionList();
        boolean has_code = (il != null);
        //        if (has_code) {
        //          setCurrentStackMapTable(mg, gen.getMajor());
        //          buildUninitializedNewMap(il);
        //        }
        //
        //        fixLocalVariableTable(mg);

        // If the method is native
        if (mg.isNative()) {

          // Create Java code that cleans up the tag stack and calls the real native method.
          fix_native(gen, mg);
          has_code = true;
          //          setCurrentStackMapTable(mg, gen.getMajor());

          // Add the DCompMarker argument to distinguish our version
          // add_dcomp_arg(mg);

        } else { // normal method

          // Add the DCompMarker argument to distinguish our version
          // add_dcomp_arg(mg);

          // Instrument the method
          if (has_code) {
            // Create the local to store the tag frame for this method
            // tag_frame_local = create_tag_frame_local(mg);
            // build_exception_handler(mg);
            // instrument_method(mg);
            // install_exception_handler(mg);
          }
        }

        if (has_code) {
          //          updateUninitializedNewOffsets(mg.getInstructionList());
          //          createNewStackMapAttribute(mg);
          mg.setMaxLocals();
          mg.setMaxStack();
        } else {
          mg.removeCodeAttributes();
          mg.removeLocalVariables();
        }

        //        remove_local_variable_type_table(mg);

        // We do not want to copy the @HotSpotIntrinsicCandidate annotations from
        // the original method to our instrumented method as the signature will
        // not match anything in the JVM's list.  This won't cause an execution
        // problem but will produce a massive number of warnings.
        // JDK 11: @HotSpotIntrinsicCandidate
        // JDK 17: @IntrinsicCandidate
        AnnotationEntryGen[] aes = mg.getAnnotationEntries();
        for (AnnotationEntryGen item : aes) {
          String type = item.getTypeName();
          if (type.endsWith("IntrinsicCandidate;")) {
            mg.removeAnnotationEntry(item);
          }
        }

        try {
          if (has_code) {
            il = mg.getInstructionList();
            InstructionHandle end = il.getEnd();
            int length = end.getPosition() + end.getInstruction().getLength();
            if (length >= Const.MAX_CODE_SIZE) {
              throw new ClassGenException(
                  "Code array too big: must be smaller than " + Const.MAX_CODE_SIZE + " bytes.");
            }
          }
          gen.addMethod(mg.getMethod());
        } catch (Exception e) {
          String s = e.getMessage();
          if (s == null) {
            throw e;
          }
          if (s.startsWith("Branch target offset too large")
              || s.startsWith("Code array too big")) {
            System.err.printf(
                "DynComp warning: ClassFile: %s - method %s is too large to instrument and is"
                    + " being skipped.%n",
                classname, mg.getName());
            // Build a dummy instrumented method that has DCompMarker
            // argument and no instrumentation.
            // first, restore unmodified method
            mg = new MethodGen(m, classname, pool);
            // restore StackMapTable
            //            setCurrentStackMapTable(mg, gen.getMajor());
            // Add the DCompMarker argument
            // add_dcomp_arg(mg);
            //            remove_local_variable_type_table(mg);
            // try again
            gen.addMethod(mg.getMethod());
          } else {
            throw e;
          }
        }

        debug_transform.exdent();
      } catch (Throwable t) {
        if (debugInstrument.enabled) {
          t.printStackTrace();
        }
        skip_method(mgen);
        if (quit_if_error) {
          throw new Error("Unexpected error processing " + classname + "." + m.getName(), t);
        } else {
          System.err.printf("Unexpected error processing %s.%s: %s%n", classname, m.getName(), t);
          System.err.printf("Method is NOT instrumented.%n");
        }
      }
    }

    // Add tag accessor methods for each primitive in the class
    // create_tag_accessors(gen);

    // We don't need to track class initialization in the JDK because
    // that is only used when printing comparability which is only done
    // for client classes
    // track_class_init();

    debug_transform.exdent();
    debug_transform.log("Instrumentation complete: %s%n", classname);

    return gen.getJavaClass().copy();
  }

  /**
   * Instrument the specified method for dynamic comparability.
   *
   * @param codeModel for the method's code
   * @param mgen describes the given method
   * @param minfo for the given method's code
   * @param instructions instruction list for method
   */
  private void instrumentCodeList(
      CodeModel codeModel,
      MethodGen24 mgen,
      MethodGen24.MInfo24 minfo,
      List<CodeElement> instructions) {

    List<CodeElement> newCode = createTagFrame(mgen);

    // The start of the list of CodeElements looks as follows:
    //   LocalVariable declarations (if any)
    //   Label for start of code (if present)
    //   LineNumber for start of code (if present)
    //   <the actual code for the method>
    //
    // We want to insert the tag_frame setup code after the LocalVariables (if any) and after the
    // inital label (if present), but before any LineNumber or Instruction.
    ListIterator<CodeElement> li = instructions.listIterator();
    CodeElement inst = null;
    try {
      while (li.hasNext()) {
        inst = li.next();
        if ((inst instanceof LineNumber) || (inst instanceof Instruction)) {
          break;
        }
      }

      // Insert the TagFrame code before the LineNumber or Instruction we just located.
      // Back up the iterator to point to just before 'inst', then copy the newCode.
      li.previous();
      for (CodeElement ce : newCode) {
        li.add(ce);
      }

      // We want to remember the current location in the instruction list. It is where a call to the
      // DynComp runtime enter routine should be inserted, if we are tracking this method.
      // This will also be used to indicate where the newStartLabel should be defined.
      // Finally, we will also return to this location for code instrumentation after calculating
      // the operand stack values.
      newStartIndex = li.nextIndex();
    } catch (Exception e) {
      System.err.printf("Unexpected exception encountered: %s", e);
      e.printStackTrace();
      // UNDONE: throw?
    }

    // The next section of code calculates the operand stack value(s) for the current method.

    // Build a map of labels to instruction list offsets
    labelIndexMap = new HashMap<>();
    li = instructions.listIterator();
    while (li.hasNext()) {
      inst = li.next();
      if (inst instanceof LabelTarget lt) {
        if (debugOperandStack) {
          System.out.println("label target: " + lt.label() + ", index: " + li.previousIndex());
        }
        // remember where this label is located within the instruction list
        labelIndexMap.put(lt.label(), li.previousIndex());
      }
    }
    if (oldStartLabel != null) {
      // change offset of oldStartLabel to where we will define newStartLabel
      labelIndexMap.put(oldStartLabel, newStartIndex);
    }
    labelIndexMap.put(newStartLabel, newStartIndex);

    // Create an array to hold the calculated operand stack
    // prior to each byte code instruction.
    stacks = new OperandStack24[instructions.size()];

    // Create an array containing the type of each local variable.
    // This will be indexed by the local variable's slot number.
    // There may be a gap for the second slot of a variable of type long or double.
    // UNDONE: do we have to save state of locals like we do statck?
    locals = new ClassDesc[mgen.getMaxLocals()];
    // UNDONE: init locals with 'this' and params only
    for (final LocalVariable lv : mgen.localsTable) {
      locals[lv.slot()] = lv.typeSymbol();
    }

    worklistHistory = new HashMap<>();

    // Create a worklist of instruction locations and operand stacks.

    // Create a work item for start of users code.
    if (oldStartLabel != null) {
      addLabelToWorklist(oldStartLabel, new OperandStack24(mgen.getMaxStack()));
    } else {
      addLabelToWorklist(newStartLabel, new OperandStack24(mgen.getMaxStack()));
    }

    if (codeModel != null) {
      // Create a work item for each exception handler.
      for (ExceptionCatch ec : codeModel.exceptionHandlers()) {
        Label l = ec.handler();
        Optional<ClassEntry> catchType = ec.catchType();
        OperandStack24 temp = new OperandStack24(mgen.getMaxStack());
        if (catchType.isPresent()) {
          temp.push(catchType.get().asSymbol());
        } else {
          temp.push(CD_Throwable);
        }
        addLabelToWorklist(l, temp);
      }
    }

    WorkItem item;
    OperandStack24 stack;
    int inst_index;
    while (!worklist.isEmpty()) {
      item = worklist.remove();
      if (debugOperandStack) {
        System.out.println(
            "pull from worklist: " + item.instructionIndex() + ", stack: " + item.stack());
      }
      li = instructions.listIterator(item.instructionIndex());
      stack = item.stack();
      // UNDONE turn it on
      boolean proceed = true;
      while (proceed) {
        if (!li.hasNext()) throw new Error("error in instruction list");
        inst_index = li.nextIndex();
        inst = li.next();
        if (debugOperandStack) {
          System.out.println("inst_index: " + inst_index);
          System.out.println("Operand Stack in: " + stack);
        }
        proceed = CalcStack24.calcOperandStack(mgen, minfo, inst, inst_index, stack);
        if (debugOperandStack) {
          System.out.println("Operand Stack out: " + stack);
          System.out.println("proceed: " + proceed);
        }
      }
    }

    // set list iterator to start of the user instructions
    li = instructions.listIterator(newStartIndex);

    // if we get index = li.nextIndex() here and increment each time through loop
    // it sould match stack index we calculate above
    while (li.hasNext()) {
      // System.out.println("inst index: " + li.nextIndex());
      inst = li.next();

      // Get the stack information
      //      stack = stack_types.get(handle_offsets[index++]);

      //      new_il = xform_inst(mg, ih, stack);
      // Get the translation for this instruction (if any)
      List<CodeElement> new_il = instrumentInstruction(mgen, minfo, inst);
      if (new_il != null) {
        // System.out.println("insts added: " + (new_il.size()-1));
        li.remove(); // remove the instruction we instrumented
        for (CodeElement ce : new_il) {
          li.add(ce);
        }
      }

      // If the modified method is now too large, we quit instrumenting the method
      // and will rediscover the problem in the main instrumentation loop above
      // and deal with it there.
      // if (ih.getPosition() >= Const.MAX_CODE_SIZE) {
      //   break;
      // }

    }
  }

  /**
   * Create a worklist item.
   *
   * @param target label where to start operand stack simulation
   * @param stack state of operand stack at target
   */
  protected static void addLabelToWorklist(Label target, OperandStack24 stack) {
    OperandStack24 existing = worklistHistory.get(target);
    if (existing == null) {
      if (debugOperandStack) {
        System.out.println(
            "push to worklist: " + target + ", " + labelIndexMap.get(target) + ", stack: " + stack);
      }
      worklistHistory.put(target, stack.getClone());
      worklist.add(new WorkItem(labelIndexMap.get(target), stack.getClone()));
    } else {
      // will throw if stacks don't match
      verifyOperandStackMatches(target, existing, stack);
      // stacks match, don't add duplicate to worklist
      if (debugOperandStack) {
        System.out.println(
            "duplicate worklist item: "
                + target
                + ", "
                + labelIndexMap.get(target)
                + ", stack: "
                + stack);
      }
    }
  }

  /**
   * Verify that the operand stacks match at a label.
   *
   * @param target label where control flow merges
   * @param existing state of operand stack at target
   * @param current state of operand stack at transfer to target
   */
  protected static void verifyOperandStackMatches(
      Label target, OperandStack24 existing, OperandStack24 current) {
    if (existing.equals(current)) {
      if (debugOperandStack) {
        System.out.println("operand stacks match at: " + target);
      }
      return;
    }
    // stacks don't match
    if (debugOperandStack) {
      System.err.flush();
      System.out.flush();
      System.out.println("operand stacks do not match at label:" + target);
      System.out.println("existing stack: " + existing);
      System.out.println("current stack: " + current);
      System.out.flush();
    }
    throw new Error("operand stacks do not match at label:" + target);
  }

  /**
   * Adds the method name and containing class name to {@code skip_methods}, the list of
   * uninstrumented methods.
   *
   * @param mg method to add to skipped_methods list
   */
  void skip_method(MethodGen mg) {
    skipped_methods.add(mg.getClassName() + "." + mg.getName());
  }

  /**
   * Returns the list of uninstrumented methods. (Note: instrument_jdk() needs to have been called
   * first.)
   */
  public List<String> get_skipped_methods() {
    return new ArrayList<String>(skipped_methods);
  }

  /**
   * Adds a try/catch block around the entire method. If an exception occurs, the tag stack is
   * cleaned up and the exception is rethrown.
   *
   * @param mgen method to add exception handler
   * @return code list for handler, or null if method should not have a handler
   */
  public List<CodeElement> build_exception_handler(MethodGen24 mgen) {

    if (mgen.getName().equals("main")) {
      return null;
    }
    // <init> methods (constructors) turn out to be problematic
    // for adding a whole method exception handler.  The start of
    // the exception handler should be after the primary object is
    // initialized - but this is hard to determine without a full
    // analysis of the code.  Hence, we just skip these methods.
    if (!mgen.isStatic()) {
      if (mgen.isConstructor()) {
        return null;
      }
    }

    List<CodeElement> instructions = new ArrayList<>();

    instructions.add(StackInstruction.of(Opcode.DUP));
    MethodRefEntry mre =
        poolBuilder.methodRefEntry(
            runtimeCD, "exception_exit", MethodTypeDesc.of(CD_void, CD_Object));
    instructions.add(InvokeInstruction.of(Opcode.INVOKESTATIC, mre));
    instructions.add(ThrowInstruction.of());
    return instructions;
  }

  /**
   * Adds the call to DCRuntime.enter to the beginning of the method.
   *
   * @param mgen method to modify
   * @param minfo for the given method's code
   * @param instructions instruction list for method
   * @param method_info_index index for MethodInfo
   */
  private void add_enter(
      MethodGen24 mgen,
      MethodGen24.MInfo24 minfo,
      List<CodeElement> instructions,
      int method_info_index) {
    List<CodeElement> newCode = callEnterOrExit(mgen, minfo, method_info_index, "enter", -1);
    instructions.addAll(newStartIndex, newCode);
    // Update the newStartIndex to account for the instrumentation code we just added.
    newStartIndex += newCode.size();
  }

  /**
   * Creates the local used to store the tag frame and returns it.
   *
   * @param mgen method to modify
   * @param minfo for the given method's code
   * @return LocalVariable for the tag_frame local
   */
  LocalVariable createTagFrameLocal(MethodGen24 mgen, MethodGen24.MInfo24 minfo) {
    return createLocalWithMethodScope(mgen, minfo, "dcomp_tag_frame$5a", objectArrayCD);
  }

  /**
   * Generates the code to create the tag frame for this method and store it in tag_frame_local.
   * This needs to be before the call to DCRuntime.enter (since it passed to that method).
   *
   * @param mgen describes the given method
   * @return InstructionList for tag_frame setup code
   */
  private List<CodeElement> createTagFrame(MethodGen24 mgen) {

    ClassDesc arg_types[] = mgen.getParameterTypes();

    // Determine the offset of the first argument in the frame
    int offset = 1;
    if (mgen.isStatic()) {
      offset = 0;
    }

    // allocate an extra slot to save the tag frame depth for debugging
    int frame_size = mgen.getMaxLocals() + 1;

    // unsigned byte max = 255.  minus the character '0' (decimal 48)
    // Largest frame size noted so far is 123.
    assert frame_size < 207 : frame_size + " " + mgen.getClassName() + "." + mgen.getName();
    String params = "" + (char) (frame_size + '0');
    // Character.forDigit (frame_size, Character.MAX_RADIX);
    List<Integer> plist = new ArrayList<>();
    for (ClassDesc argType : arg_types) {
      if (argType.isPrimitive()) {
        plist.add(offset);
      }
      offset += TypeKind.from(argType).slotSize();
    }
    for (int ii = plist.size() - 1; ii >= 0; ii--) {
      char tmpChar = (char) (plist.get(ii) + '0');
      params += tmpChar;
      // Character.forDigit (plist.get(ii), Character.MAX_RADIX);
    }

    // Create code to create and init the tag_frame and store the result in tag_frame_local
    List<CodeElement> instructions = new ArrayList<>();

    MethodRefEntry mre =
        poolBuilder.methodRefEntry(
            runtimeCD, "create_tag_frame", MethodTypeDesc.of(objectArrayCD, CD_String));
    instructions.add(buildLDCInstruction(poolBuilder.stringEntry(params)));
    instructions.add(InvokeInstruction.of(Opcode.INVOKESTATIC, mre));
    instructions.add(StoreInstruction.of(TypeKind.REFERENCE, tagFrameLocal.slot()));

    debugInstrument.log("Store Tag frame local at index %d%n", tagFrameLocal.slot());

    return instructions;
  }

  /**
   * Pushes the object, method info index, parameters, and return value on the stack and calls the
   * specified Method (normally enter or exit) in DCRuntime. The parameters are passed as an array
   * of objects.
   *
   * @param mgen method to modify
   * @param minfo for the given method's code
   * @param method_info_index index for MethodInfo
   * @param methodToCall "enter" or "exit"
   * @param line source line number if type is exit
   * @return instruction list for the enter or exit code
   */
  private List<CodeElement> callEnterOrExit(
      MethodGen24 mgen,
      MethodGen24.MInfo24 minfo,
      int method_info_index,
      String methodToCall,
      int line) {

    List<CodeElement> instructions = new ArrayList<>();
    ClassDesc paramTypes[] = mgen.getParameterTypes();

    // Push the tag frame
    instructions.add(LoadInstruction.of(TypeKind.REFERENCE, tagFrameLocal.slot()));

    // Push the object.  Push null if this is a static method or a constructor.
    if (mgen.isStatic() || (methodToCall.equals("enter") && mgen.isConstructor())) {
      instructions.add(ConstantInstruction.ofIntrinsic(Opcode.ACONST_NULL));
    } else { // must be an instance method
      instructions.add(LoadInstruction.of(TypeKind.REFERENCE, 0));
    }

    // The offset of the first parameter.
    int param_offset = mgen.isStatic() ? 0 : 1;

    // Push the MethodInfo index
    instructions.add(loadIntegerConstant(method_info_index));

    // Create an array of objects with elements for each parameter.
    instructions.add(loadIntegerConstant(paramTypes.length));
    instructions.add(NewReferenceArrayInstruction.of(poolBuilder.classEntry(CD_Object)));

    // Put each parameter into the array.
    int param_index = param_offset;
    for (int ii = 0; ii < paramTypes.length; ii++) {
      instructions.add(StackInstruction.of(Opcode.DUP));
      instructions.add(loadIntegerConstant(ii));
      ClassDesc at = paramTypes[ii];
      if (at.isPrimitive()) {
        instructions.add(ConstantInstruction.ofIntrinsic(Opcode.ACONST_NULL));
      } else { // it's a reference of some sort
        instructions.add(LoadInstruction.of(TypeKind.REFERENCE, param_index));
      }
      instructions.add(ArrayStoreInstruction.of(Opcode.AASTORE));
      param_index += TypeKind.from(at).slotSize();
    }

    // If this is an exit, push the return value and line number.
    // The return value is stored in the local "return__$trace2_val".
    // If the return value is a primitive, push a null.
    if (methodToCall.equals("exit")) {
      ClassDesc returnType = mgen.getReturnType();
      if (returnType.equals(CD_void)) {
        instructions.add(ConstantInstruction.ofIntrinsic(Opcode.ACONST_NULL));
      } else {
        LocalVariable return_local = getReturnLocal(mgen, returnType, minfo);
        if (returnType.isPrimitive()) {
          instructions.add(ConstantInstruction.ofIntrinsic(Opcode.ACONST_NULL));
        } else {
          instructions.add(LoadInstruction.of(TypeKind.REFERENCE, return_local.slot()));
        }
      }
      // push line number
      instructions.add(loadIntegerConstant(line));
    }

    MethodTypeDesc methodArgs;
    // Call the specified method.
    if (methodToCall.equals("exit")) {
      methodArgs =
          MethodTypeDesc.of(
              CD_void, objectArrayCD, CD_Object, CD_int, objectArrayCD, CD_Object, CD_int);
    } else {
      methodArgs = MethodTypeDesc.of(CD_void, objectArrayCD, CD_Object, CD_int, objectArrayCD);
    }
    MethodRefEntry mre = poolBuilder.methodRefEntry(runtimeCD, methodToCall, methodArgs);
    instructions.add(InvokeInstruction.of(Opcode.INVOKESTATIC, mre));

    return instructions;
  }

  // @param stack current contents of the stack
  /**
   * Transforms instructions to track comparability. Returns a list of instructions that replaces
   * the specified instruction. Returns null if the instruction should not be replaced.
   *
   * @param mgen method to modify
   * @param minfo for the given method's code
   * @param ce instruction to be instrumented
   * @return instrumentation for inst, or null is none
   */
  @Nullable List<CodeElement> instrumentInstruction(
      MethodGen24 mgen, MethodGen24.MInfo24 minfo, CodeElement ce) {
    switch (ce) {
      case Instruction inst -> {
        switch (inst.opcode()) {

          // Replace the object comparison instructions with a call to
          // DCRuntime.object_eq or DCRuntime.object_ne.  Those methods
          // return a boolean which is used in a ifeq/ifne instruction
          case Opcode.IF_ACMPEQ:
            return object_comparison((BranchInstruction) inst, "object_eq", Opcode.IFNE);
          case Opcode.IF_ACMPNE:
            return object_comparison((BranchInstruction) inst, "object_ne", Opcode.IFNE);

          // These instructions compare the integer on the top of the stack
          // to zero.  Nothing is made comparable by this, so we need only
          // discard the tag on the top of the stack.
          case Opcode.IFEQ:
          case Opcode.IFNE:
          case Opcode.IFLT:
          case Opcode.IFGE:
          case Opcode.IFGT:
          case Opcode.IFLE:
            {
              return discard_tag_code(inst, 1);
            }

          // Instanceof pushes either 0 or 1 on the stack depending on whether
          // the object on top of stack is of the specified type.  We push a
          // tag for a constant, since nothing is made comparable by this.
          case Opcode.INSTANCEOF:
            return build_il(dcr_call("push_const", CD_void, noArgsCD), inst);

          // all the DUP opcodes go here

          case Opcode.IF_ICMPEQ:
          case Opcode.IF_ICMPGE:
          case Opcode.IF_ICMPGT:
          case Opcode.IF_ICMPLE:
          case Opcode.IF_ICMPLT:
          case Opcode.IF_ICMPNE:
            {
              return build_il(dcr_call("cmp_op", CD_void, noArgsCD), inst);
            }

          case Opcode.GETFIELD:
          case Opcode.PUTFIELD:
          case Opcode.GETSTATIC:
          case Opcode.PUTSTATIC:
            {
              return load_store_field(mgen, ((FieldInstruction) inst));
            }

          case Opcode.DLOAD:
          case Opcode.DLOAD_0:
          case Opcode.DLOAD_1:
          case Opcode.DLOAD_2:
          case Opcode.DLOAD_3:
          case Opcode.FLOAD:
          case Opcode.FLOAD_0:
          case Opcode.FLOAD_1:
          case Opcode.FLOAD_2:
          case Opcode.FLOAD_3:
          case Opcode.ILOAD:
          case Opcode.ILOAD_0:
          case Opcode.ILOAD_1:
          case Opcode.ILOAD_2:
          case Opcode.ILOAD_3:
          case Opcode.LLOAD:
          case Opcode.LLOAD_0:
          case Opcode.LLOAD_1:
          case Opcode.LLOAD_2:
          case Opcode.LLOAD_3:
            {
              return load_local((LoadInstruction) inst, tagFrameLocal, "push_local_tag");
            }

          case Opcode.DSTORE:
          case Opcode.DSTORE_0:
          case Opcode.DSTORE_1:
          case Opcode.DSTORE_2:
          case Opcode.DSTORE_3:
          case Opcode.FSTORE:
          case Opcode.FSTORE_0:
          case Opcode.FSTORE_1:
          case Opcode.FSTORE_2:
          case Opcode.FSTORE_3:
          case Opcode.ISTORE:
          case Opcode.ISTORE_0:
          case Opcode.ISTORE_1:
          case Opcode.ISTORE_2:
          case Opcode.ISTORE_3:
          case Opcode.LSTORE:
          case Opcode.LSTORE_0:
          case Opcode.LSTORE_1:
          case Opcode.LSTORE_2:
          case Opcode.LSTORE_3:
            {
              return store_local((StoreInstruction) inst, tagFrameLocal, "pop_local_tag");
            }

          // Adjusts the tag stack for load constant opcodes. If the constant is a primitive, pushes
          // its tag
          // on the tag stack. If the constant is a reference (string, class), does nothing.
          case Opcode.LDC:
          case Opcode.LDC_W:
          case Opcode.LDC2_W:
            {
              if (((ConstantInstruction) inst).typeKind().equals(TypeKind.REFERENCE)) {
                return null;
              }
              return build_il(dcr_call("push_const", CD_void, noArgsCD), inst);
            }

          // Push the tag for the array onto the tag stack.  This causes
          // anything comparable to the length to be comparable to the array
          // as an index.
          case Opcode.ARRAYLENGTH:
            {
              return array_length(inst);
            }

          case Opcode.BIPUSH:
          case Opcode.SIPUSH:
          case Opcode.DCONST_0:
          case Opcode.DCONST_1:
          case Opcode.FCONST_0:
          case Opcode.FCONST_1:
          case Opcode.FCONST_2:
          case Opcode.ICONST_0:
          case Opcode.ICONST_1:
          case Opcode.ICONST_2:
          case Opcode.ICONST_3:
          case Opcode.ICONST_4:
          case Opcode.ICONST_5:
          case Opcode.ICONST_M1:
          case Opcode.LCONST_0:
          case Opcode.LCONST_1:
            {
              return build_il(dcr_call("push_const", CD_void, noArgsCD), inst);
            }

          // Primitive Binary operators.  Each is augmented with a call to
          // DCRuntime.binary_tag_op that merges the tags and updates the tag
          // Stack.
          case Opcode.DADD:
          case Opcode.DCMPG:
          case Opcode.DCMPL:
          case Opcode.DDIV:
          case Opcode.DMUL:
          case Opcode.DREM:
          case Opcode.DSUB:
          case Opcode.FADD:
          case Opcode.FCMPG:
          case Opcode.FCMPL:
          case Opcode.FDIV:
          case Opcode.FMUL:
          case Opcode.FREM:
          case Opcode.FSUB:
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
          case Opcode.LADD:
          case Opcode.LAND:
          case Opcode.LCMP:
          case Opcode.LDIV:
          case Opcode.LMUL:
          case Opcode.LOR:
          case Opcode.LREM:
          case Opcode.LSHL:
          case Opcode.LSHR:
          case Opcode.LSUB:
          case Opcode.LUSHR:
          case Opcode.LXOR:
            return build_il(dcr_call("binary_tag_op", CD_void, noArgsCD), inst);

          // Computed jump based on the int on the top of stack.  Since that int
          // is not made comparable to anything, we just discard its tag.  One
          // might argue that the key should be made comparable to each value in
          // the jump table.  But the tags for those values are not available.
          // And since they are all constants, its not clear how interesting it
          // would be anyway.
          case Opcode.LOOKUPSWITCH:
          case Opcode.TABLESWITCH:
            return discard_tag_code(inst, 1);

          // Make the integer argument to ANEWARRAY comparable to the new
          // array's index.
          case Opcode.ANEWARRAY:
          case Opcode.NEWARRAY:
            {
              return new_array(inst);
            }

          // If the new array has 2 dimensions, make the integer arguments
          // comparable to the corresponding indices of the new array.
          // For any other number of dimensions, discard the tags for the
          // arguments.
          case Opcode.MULTIANEWARRAY:
            {
              return multi_newarray_dc((NewMultiArrayInstruction) inst);
            }

          // Mark the array and its index as comparable.  Also for primitives,
          // push the tag of the array element on the tag stack
          case Opcode.AALOAD:
          case Opcode.BALOAD:
          case Opcode.CALOAD:
          case Opcode.DALOAD:
          case Opcode.FALOAD:
          case Opcode.IALOAD:
          case Opcode.LALOAD:
          case Opcode.SALOAD:
            {
              return array_load(mgen, (ArrayLoadInstruction) inst);
            }

          // Prefix the return with a call to the correct normal_exit method
          // to handle the tag stack
          case Opcode.ARETURN:
          case Opcode.DRETURN:
          case Opcode.FRETURN:
          case Opcode.IRETURN:
          case Opcode.LRETURN:
          case Opcode.RETURN:
            {
              return return_tag(mgen, inst);
            }

          // Throws an exception.  This clears the operand stack of the current
          // frame.  We need to clear the tag stack as well.
          case Opcode.ATHROW:
            return build_il(dcr_call("throw_op", CD_void, noArgsCD), inst);

          // Opcodes that don't need any modifications.  Here for reference
          case Opcode.ACONST_NULL:
          case Opcode.ALOAD:
          case Opcode.ALOAD_0:
          case Opcode.ALOAD_1:
          case Opcode.ALOAD_2:
          case Opcode.ALOAD_3:
          case Opcode.ASTORE:
          case Opcode.ASTORE_0:
          case Opcode.ASTORE_1:
          case Opcode.ASTORE_2:
          case Opcode.ASTORE_3:
          case Opcode.CHECKCAST:
          case Opcode.D2F: // double to float
          case Opcode.D2I: // double to integer
          case Opcode.D2L: // double to long
          case Opcode.DNEG: // Negate double on top of stack
          case Opcode.F2D: // float to double
          case Opcode.F2I: // float to integer
          case Opcode.F2L: // float to long
          case Opcode.FNEG: // Negate float on top of stack
          case Opcode.GOTO:
          case Opcode.GOTO_W:
          case Opcode.I2B: // integer to byte
          case Opcode.I2C: // integer to char
          case Opcode.I2D: // integer to double
          case Opcode.I2F: // integer to float
          case Opcode.I2L: // integer to long
          case Opcode.I2S: // integer to short
          case Opcode.IFNONNULL:
          case Opcode.IFNULL:
          case Opcode.IINC: // increment local variable by a constant
          case Opcode.INEG: // negate integer on top of stack
          case Opcode.JSR: // pushes return address on the stack, but that
          // is thought of as an object, so we don't need a tag for it.
          case Opcode.JSR_W:
          case Opcode.L2D: // long to double
          case Opcode.L2F: // long to float
          case Opcode.L2I: // long to int
          case Opcode.LNEG: // negate long on top of stack
          case Opcode.MONITORENTER:
          case Opcode.MONITOREXIT:
          case Opcode.NEW:
          case Opcode.NOP:
          case Opcode.RET: // this is the internal JSR return
            return null;

          // Handle subroutine calls.  Calls to instrumented code are modified
          // to call the instrumented version (with the DCompMarker argument).
          // Calls to uninstrumented code (rare) discard primitive arguments
          // from the tag stack and produce an arbitrary return tag.
          case Opcode.INVOKESTATIC:
          case Opcode.INVOKEVIRTUAL:
          case Opcode.INVOKESPECIAL:
          case Opcode.INVOKEINTERFACE:
            return handleInvoke((InvokeInstruction) inst, mgen);

          case Opcode.INVOKEDYNAMIC:
            return handleInvokeDynamic((InvokeDynamicInstruction) inst);

          // UNDONE default -> throw exception when done?
          default:
            //    System.out.println("Unexpected instruction opcode: " + ce);
            return null;
        }
      }

      case Label l -> {
        // UNDONE do anything?
        return null;
      }

      case LineNumber ln -> {
        // UNDONE do anything?
        return null;
      }

      // UNDONE default -> throw exception when done?
      default -> {
        System.out.println("Unexpected CodeElement: " + ce);
        return null;
      }
    }
  }

  /**
   * Transforms instructions to track comparability. Returns a list of instructions that replaces
   * the specified instruction. Returns null if the instruction should not be replaced.
   *
   * @param mg method being instrumented
   * @param ih handle of Instruction to translate
   * @param stack current contents of the stack
   */
  @Nullable InstructionList xform_inst(MethodGen mg, InstructionHandle ih, OperandStack stack) {

    org.apache.bcel.generic.Instruction inst = ih.getInstruction();

    switch (inst.getOpcode()) {

      // Duplicates the item on the top of stack.  If the value on the
      // top of the stack is a primitive, we need to do the same on the
      // tag stack.  Otherwise, we need do nothing.
      case Const.DUP:
        {
          return dup_tag(inst, stack);
        }

      // Duplicates the item on the top of the stack and inserts it 2
      // values down in the stack.  If the value at the top of the stack
      // is not a primitive, there is nothing to do here.  If the second
      // value is not a primitive, then we need only to insert the duped
      // value down 1 on the tag stack (which contains only primitives)
      case Const.DUP_X1:
        {
          return dup_x1_tag(inst, stack);
        }

      // Duplicates either the top 2 category 1 values or a single
      // category 2 value and inserts it 2 or 3 values down on the
      // stack.
      case Const.DUP2_X1:
        {
          return dup2_x1_tag(inst, stack);
        }

      // Duplicate either one category 2 value or two category 1 values.
      case Const.DUP2:
        {
          return dup2_tag(inst, stack);
        }

      // Dup the category 1 value on the top of the stack and insert it either
      // two or three values down on the stack.
      case Const.DUP_X2:
        {
          return dup_x2(inst, stack);
        }

      case Const.DUP2_X2:
        {
          return dup2_x2(inst, stack);
        }

      // Pop instructions discard the top of the stack.  We want to discard
      // the top of the tag stack iff the item on the top of the stack is a
      // primitive.
      case Const.POP:
        {
          return pop_tag(inst, stack);
        }

      // Pops either the top 2 category 1 values or a single category 2 value
      // from the top of the stack.  We must do the same to the tag stack
      // if the values are primitives.
      case Const.POP2:
        {
          return pop2_tag(inst, stack);
        }

      // Swaps the two category 1 types on the top of the stack.  We need
      // to swap the top of the tag stack if the two top elements on the
      // real stack are primitives.
      case Const.SWAP:
        {
          return swap_tag(inst, stack);
        }

      // end of dup opcodes

      // Mark the array and its index as comparable.  For primitives, store
      // the tag for the value on the top of the stack in the tag storage
      // for the array.
      case Const.AASTORE:
        return array_store(inst, "aastore", Type.OBJECT);
      case Const.BASTORE:
        // The JVM uses bastore for both byte and boolean.
        // We need to differentiate.
        Type arr_type = stack.peek(2);
        if (arr_type.getSignature().equals("[Z")) {
          return array_store(inst, "zastore", Type.BOOLEAN);
        } else {
          return array_store(inst, "bastore", Type.BYTE);
        }
      case Const.CASTORE:
        return array_store(inst, "castore", Type.CHAR);
      case Const.DASTORE:
        return array_store(inst, "dastore", Type.DOUBLE);
      case Const.FASTORE:
        return array_store(inst, "fastore", Type.FLOAT);
      case Const.IASTORE:
        return array_store(inst, "iastore", Type.INT);
      case Const.LASTORE:
        return array_store(inst, "lastore", Type.LONG);
      case Const.SASTORE:
        return array_store(inst, "sastore", Type.SHORT);

      // Make sure we didn't miss anything
      default:
        throw new Error("instruction " + inst + " unsupported");
    }
  }

  /**
   * Adds a call to DCruntime.exit() at each return from the method. This call calculates
   * comparability on the daikon variables. It is only necessary if we are tracking comparability
   * for the variables of this method.
   *
   * @param mgen method to modify
   * @param mi MethodInfo for the given method's code
   * @param minfo MInfo24 for the given method's code
   * @param instructions instruction list for method
   * @param method_info_index index for MethodInfo
   */
  private void add_exit(
      MethodGen24 mgen,
      MethodInfo mi,
      MethodGen24.MInfo24 minfo,
      List<CodeElement> instructions,
      int method_info_index) {

    // Iterator over all of the exit line numbers for this method, in order.
    // We will read one element from it each time that we encounter a
    // return instruction.
    Iterator<Integer> exitLocationIter = mi.exit_locations.iterator();

    // Loop through each instruction, looking for return instructions.
    ListIterator<CodeElement> li = instructions.listIterator();
    while (li.hasNext()) {
      CodeElement inst = li.next();

      // If this is a return instruction, Call DCRuntime.exit to calculate
      // comparability on Daikon variables
      if (inst instanceof ReturnInstruction) {
        List<CodeElement> newCode = new ArrayList<>();
        ClassDesc type = mgen.getReturnType();
        if (!type.equals(CD_void)) {
          TypeKind typeKind = TypeKind.from(type);
          LocalVariable returnLocal = getReturnLocal(mgen, type, minfo);
          if (typeKind.slotSize() == 1) {
            newCode.add(StackInstruction.of(Opcode.DUP));
          } else {
            newCode.add(StackInstruction.of(Opcode.DUP2));
          }
          newCode.add(StoreInstruction.of(typeKind, returnLocal.slot()));
        }
        if (!exitLocationIter.hasNext()) {
          throw new RuntimeException("Not enough exit locations in the exitLocationIter");
        }
        newCode.addAll(
            callEnterOrExit(mgen, minfo, method_info_index, "exit", exitLocationIter.next()));

        // back up iterator to point to 'inst', the return instruction, and insert the
        // instrumentation
        li.previous();
        for (CodeElement ce : newCode) {
          li.add(ce);
        }
        // skip over the return instruction
        li.next();
      }
    }
  }

  //  /**
  //   * Return the interface class containing the implementation of the given method. The
  // interfaces of
  //   * {@code startClass} are recursively searched.
  //   *
  //   * @param startClass the ClassModel whose interfaces are to be searched
  //   * @param methodName the target method to search for
  //   * @param argTypes the target method's argument types
  //   * @return the name of the interface class containing target method, or null if not found
  //   */
  //  private @Nullable @ClassGetName String getDefiningInterface(
  //      ClassModel startClass, String methodName, Type[] argTypes) {
  //
  //    if (debugGetDefiningInterface) {
  //      System.out.println("searching interfaces of: " + ClassGen24.getClassName(startClass));
  //    }
  //    for (@ClassGetName String interfaceName : startClass.getInterfaceNames()) {
  //      if (debugGetDefiningInterface) {
  //        System.out.println("interface: " + interfaceName);
  //      }
  //      ClassModel cm;
  //      try {
  //        cm = getClassModel(interfaceName);
  //      } catch (Throwable e) {
  //        throw new Error(String.format("Unable to load class: %s", interfaceName), e);
  //      }
  //     for (Method jm : cm.getMethods()) {
  //       if (debugGetDefiningInterface) {
  //         System.out.println("  " + jm.getName() + Arrays.toString(jm.getArgumentTypes()));
  //       }
  //       if (jm.getName().equals(methodName) && Arrays.equals(jm.getArgumentTypes(), argTypes))
  // {
  //         // We have a match.
  //         return interfaceName;
  //       }
  //     }
  //     // no match found; does this interface extend other interfaces?
  //     @ClassGetName String foundAbove = getDefiningInterface(cm, methodName, argTypes);
  //     if (foundAbove != null) {
  //       // We have a match.
  //       return foundAbove;
  //     }
  //    }
  // nothing found
  //    return null;
  //  }

  /**
   * Process an InvokeDynamic instruction. We don't instrument lambda methods, so just clean up the
   * tag stack.
   *
   * @param invoke a method invocation bytecode instruction
   * @return instructions to replace the given instruction
   */
  private List<CodeElement> handleInvokeDynamic(InvokeDynamicInstruction invoke) {

    // Get information about the call
    // There isn't really a class holding the method.
    String classname = "";
    MethodTypeDesc mtd = invoke.typeSymbol();
    ClassDesc returnType = mtd.returnType();
    ClassDesc[] argTypes = mtd.parameterArray();
    System.out.println("invoke: " + invoke);

    return cleanInvokeTagStack(invoke, classname, returnType, argTypes);
  }

  /**
   * Process an Invoke instruction. There are three cases:
   *
   * <ul>
   *   <li>convert calls to Object.equals to calls to dcomp_equals or dcomp_super_equals
   *   <li>convert calls to Object.clone to calls to dcomp_clone or dcomp_super_clone
   *   <li>otherwise, determine whether the target of the invoke is instrumented or not (this is the
   *       {@code callee_instrumented} variable)
   *       <ul>
   *         <li>If the target method is instrumented, add a DCompMarker argument to the end of the
   *             argument list.
   *         <li>If the target method is not instrumented, we must account for the fact that the
   *             instrumentation code generated up to this point has assumed that the target method
   *             is instrumented. Hence, generate code to discard a primitive tag from the
   *             DCRuntime's per-thread comparability data stack for each primitive argument. If the
   *             return type of the target method is a primitive, add code to push a tag onto the
   *             runtime comparability data stack to represent the primitive return value.
   *       </ul>
   * </ul>
   *
   * @param invoke a method invocation bytecode instruction
   * @param mgen host method of invoke
   * @return instructions to replace the given instruction
   */
  private List<CodeElement> handleInvoke(InvokeInstruction invoke, MethodGen24 mgen) {

    // Get information about the call
    String methodName = invoke.name().stringValue();
    @SuppressWarnings("signature:assignment") // JDK 24 is not annotated as yet
    @ClassGetName String classname = invoke.owner().asInternalName().replace('/', '.');
    MethodTypeDesc mtd = invoke.typeSymbol();
    ClassDesc returnType = mtd.returnType();
    ClassDesc[] argTypes = mtd.parameterArray();

    System.out.println();
    System.out.println("InvokeInst: " + invoke);
    System.out.println("returnType: " + returnType);
    System.out.println("classname: " + classname);
    // System.out.println("ref_type: " + invoke.getReferenceType(pool));

    if (is_object_equals(methodName, returnType, argTypes)) {

      // Replace calls to Object's equals method with calls to our
      // replacement, a static method in DCRuntime.
      List<CodeElement> il = new ArrayList<>();
      il.add(
          dcr_call(
              invoke.opcode().equals(Opcode.INVOKESPECIAL) ? "dcomp_super_equals" : "dcomp_equals",
              returnType,
              new ClassDesc[] {CD_Object, CD_Object}));
      return il;
    }

    if (is_object_clone(methodName, returnType, argTypes)) {

      // Replace calls to Object's clone method with calls to our
      // replacement, a static method in DCRuntime.

      List<CodeElement> il = instrument_clone_call(invoke, returnType, classname);
      return il;
    }

    boolean callee_instrumented =
        isTargetInstrumented(invoke, mgen, classname, methodName, argTypes);

    if (debugHandleInvoke) {
      System.out.printf("handleInvoke(%s)%n", invoke);
      System.out.printf("  invoke host: %s%n", classGen.getClassName() + "." + mgen.getName());
      System.out.printf("  invoke targ: %s%n", classname + "." + methodName);
      System.out.printf("  callee_instrumented: %s%n", callee_instrumented);
    }

    if (callee_instrumented) {

      List<CodeElement> il = new ArrayList<>();

      // Push the DCompMarker argument as we are calling the instrumented version.
      il.add(ConstantInstruction.ofIntrinsic(Opcode.ACONST_NULL));

      // Add the DCompMarker to the arg types list.
      List<ClassDesc> new_arg_types = new ArrayList<>(Arrays.asList(argTypes));
      new_arg_types.add(dcomp_marker);

      NameAndTypeEntry nte =
          poolBuilder.nameAndTypeEntry(methodName, MethodTypeDesc.of(returnType, new_arg_types));
      il.add(InvokeInstruction.of(invoke.opcode(), invoke.owner(), nte, invoke.isInterface()));
      return il;

    } else { // not instrumented, discard the tags before making the call
      return cleanInvokeTagStack(invoke, classname, returnType, argTypes);
    }
  }

  /**
   * Process an invoke instruction that calls a non-instrumented method. We need to clean up the tag
   * stack.
   *
   * @param invoke a method invocation bytecode instruction
   * @param classname target class of the invoke
   * @param returnType return type of method
   * @param argTypes argument types of target method
   * @return instructions to replace the given instruction
   */
  private List<CodeElement> cleanInvokeTagStack(
      Instruction invoke, String classname, ClassDesc returnType, ClassDesc[] argTypes) {
    List<CodeElement> il = new ArrayList<>();

    // JUnit test classes are a bit strange.  They are marked as not being callee_instrumented
    // because they do not have the dcomp_marker added to the argument list, but
    // they actually contain instrumentation code.  So we do not want to discard
    // the primitive tags prior to the call.
    if (!junitTestClasses.contains(classname)) {
      il = discard_primitive_tags(argTypes);
    }

    // Add a tag for the return type if it is primitive.
    if (is_primitive(returnType)) {
      if (debugHandleInvoke) {
        System.out.printf("push tag for return  type of %s%n", returnType);
      }
      il.add(dcr_call("push_const", CD_void, noArgsCD));
    }
    il.add(invoke);
    return il;
  }

  /**
   * Returns instructions that will discard any primitive tags corresponding to the specified
   * arguments. Returns an empty instruction list if there are no primitive arguments to discard.
   *
   * @param argTypes argument types of target method
   * @return an instruction list that discards primitive tags from DCRuntime's per-thread
   *     comparability data stack
   */
  private List<CodeElement> discard_primitive_tags(ClassDesc[] argTypes) {
    List<CodeElement> il = new ArrayList<>();

    int primitive_cnt = 0;
    for (ClassDesc argType : argTypes) {
      if (argType.isPrimitive()) {
        primitive_cnt++;
      }
    }
    if (primitive_cnt > 0) {
      il.addAll(discard_tag_code(NopInstruction.of(), primitive_cnt));
    }
    return il;
  }

  /**
   * Returns true if the invoke target is instrumented.
   *
   * @param invoke instruction whose target is to be checked
   * @param mgen host method of invoke
   * @param classname target class of the invoke
   * @param methodName target method of the invoke
   * @param argTypes argument types of target method
   * @return true if the target is instrumented
   */
  private boolean isTargetInstrumented(
      InvokeInstruction invoke,
      MethodGen24 mgen,
      @ClassGetName String classname,
      String methodName,
      ClassDesc[] argTypes) {

    boolean targetInstrumented = true;
    Opcode op = invoke.opcode();

    if (op.equals(Opcode.INVOKEDYNAMIC)) {
      // We don't instrument lambda methods.
      if (debugHandleInvoke) {
        System.out.printf("invokedynamic NOT the classname: %s%n", classname);
      }
      targetInstrumented = false;
    } else if (is_object_method(methodName, argTypes)) {
      targetInstrumented = false;
    } else {
      targetInstrumented = isClassnameInstrumented(classname, methodName);

      if (debugHandleInvoke) {
        System.out.printf("invoke host: %s%n", classGen.getClassName() + "." + mgen.getName());
        System.out.printf("invoke targ: %s%n", classname + "." + methodName);
      }

      if (Premain.problem_methods.contains(classname + "." + methodName)) {
        debugInstrument.log(
            "Don't call instrumented version of problem method %s.%n",
            classname + "." + methodName);
        targetInstrumented = false;
      }

      // targetInstrumented (the return value of this method) has been set.
      // Now, adjust it for some special cases.
      // Every adjustment is from `true` to `false`.

      // There are two special cases we need to detect:
      //   calls to annotations
      //   calls to functional interfaces
      //
      // Annotation classes are never instrumented so we must set
      // the targetInstrumented flag to false.
      //
      // Functional interfaces are a bit more complicated. These are primarily (only?)
      // used by Lambda functions.  Lambda methods are generated dynamically at
      // run time via the InvokeDynamic instruction.  They are not seen by our
      // ClassFileTransformer so are never instrumented.  Thus we must set the
      // targetInstrumented flag to false when we see a call to a Lambda method.
      // The heuristic we use is to assume that any InvokeInterface or InvokeVirtual
      // call to a functional interface is a call to a Lambda method.
      //
      // The Java compiler detects functional interfaces automatically, but the
      // user can declare their intent with the @FunctionInterface annotation.
      // The Java runtime is annotated in this manner.  Hence, we look for this
      // annotation to detect a call to a functional interface.  In practice, we
      // could detect functional interfaces in a manner similar to the Java
      // compiler, but for now we will go with this simpler method.
      //
      // Note that to simplify our code we set the access flags for a functional
      // interface to ANNOTATION in our accessFlags map.
      //
      if (targetInstrumented == true
          && (op.equals(Opcode.INVOKEINTERFACE) || op.equals(Opcode.INVOKEVIRTUAL))) {
        Integer access = getAccessFlags(classname);

        if ((access.intValue() & ClassFile.ACC_ANNOTATION) != 0) {
          targetInstrumented = false;
        }

        // UNDONE: New code added above should handle the case below.  Need to find a test
        // case and verify this code is no longer needed.
        // This is a bit of a hack.  An invokeinterface instruction with a
        // a target of "java.util.stream.<something>" might be calling a
        // Lambda method in which case we don't want to add the dcomp_marker.
        // Might lose something in 'normal' cases, but no easy way to detect.
        if (classname.startsWith("java.util.stream")) {
          targetInstrumented = false;
        }

        // In a similar fashion, when the Java runtime is processing annotations, there might
        // be an invoke (via reflection) of a member of the java.lang.annotation package; this
        // too should not have the dcomp_marker added.
        if (classname.startsWith("java.lang.annotation")) {
          targetInstrumented = false;
        }
      }

      // UNDONE fix up this code
      boolean doit = false;
      // If we are not using the instrumented JDK, then we need to track down the
      // actual target of an INVOKEVIRTUAL to see if it has been instrumented or not.
      if (doit && targetInstrumented == true && op.equals(Opcode.INVOKEVIRTUAL)) {
        if (!Premain.jdk_instrumented && !mgen.getName().equals("equals_dcomp_instrumented")) {

          if (debugHandleInvoke) {
            System.out.println("method: " + methodName);
            System.out.println("argTypes: " + Arrays.toString(argTypes));
            System.out.printf("invoke host: %s%n", gen.getClassName() + "." + mgen.getName());
          }

          @ClassGetName String targetClassname = classname;
          // Search this class for the target method. If not found, set targetClassname to
          // its superclass and try again.
          mainloop:
          while (true) {
            // Check that the class exists
            //        JavaClass targetClass;
            JavaClass targetClass = null;
            try {
              //        targetClass = getJavaClass(targetClassname);
            } catch (Throwable e) {
              targetClass = null;
            }
            if (targetClass == null) {
              // We cannot locate or read the .class file, better assume not instrumented.
              if (debugHandleInvoke) {
                System.out.printf("Unable to locate class: %s%n%n", targetClassname);
              }
              targetInstrumented = false;
              break;
            }
            if (debugHandleInvoke) {
              System.out.println("target class: " + targetClassname);
            }

            for (Method m : targetClass.getMethods()) {
              if (debugHandleInvoke) {
                System.out.println("  " + m.getName() + Arrays.toString(m.getArgumentTypes()));
              }
              if (m.getName().equals(methodName) && Arrays.equals(m.getArgumentTypes(), argTypes)) {
                // We have a match.
                if (debugHandleInvoke) {
                  System.out.printf("we have a match%n%n");
                }
                if (BcelUtil.inJdk(targetClassname)) {
                  targetInstrumented = false;
                }
                break mainloop;
              }
            }

            {
              // no methods match - search this class's interfaces
              @ClassGetName String found;
              try {
                found = null; // //  getDefiningInterface(targetClass, methodName, argTypes);
              } catch (Throwable e) {
                // We cannot locate or read the .class file, better assume it is not instrumented.
                targetInstrumented = false;
                break;
              }
              if (found != null) {
                // We have a match.
                if (debugHandleInvoke) {
                  System.out.printf("we have a match%n%n");
                }
                if (BcelUtil.inJdk(found)) {
                  targetInstrumented = false;
                }
                break;
              }
            }

            // Method not found; perhaps inherited from superclass.
            // Cannot use "targetClass = targetClass.getSuperClass()" because the superclass might
            // not have been loaded into BCEL yet.
            if (targetClass.getSuperclassNameIndex() == 0) {
              // The target class is Object; the search completed without finding a matching method.
              if (debugHandleInvoke) {
                System.out.printf("Unable to locate method: %s%n%n", methodName);
              }
              targetInstrumented = false;
              break;
            }
            // Recurse looking in the superclass.
            targetClassname = targetClass.getSuperclassName();
          }
        }
      }
    }

    if (op.equals(Opcode.INVOKESPECIAL)) {
      if (classname.equals(classGen.getSuperclassName()) && methodName.equals("<init>")) {
        this.constructor_is_initialized = true;
      }
    }

    return targetInstrumented;
  }

  /**
   * Returns the access flags for the given class.
   *
   * @param classname the class whose access flags to return
   * @return the access flags for the given class
   */
  private Integer getAccessFlags(String classname) {
    Integer access = accessFlags.get(classname);
    if (access == null) {
      // We have not seen this class before. Check to see if the target class is
      // an Annotation or a FunctionalInterface.
      ClassModel cm = getClassModel(classname);
      if (cm != null) {
        access = cm.flags().flagsMask();

        // Now check for FunctionalInterface
        searchloop:
        for (java.lang.classfile.Attribute<?> attribute : classModel.attributes()) {
          if (attribute instanceof RuntimeVisibleAnnotationsAttribute rvaa) {
            for (final Annotation item : rvaa.annotations()) {
              String annotation = item.className().stringValue();
              if (debugHandleInvoke) {
                System.out.println("annotation: " + annotation);
              }
              if (annotation.endsWith("FunctionalInterface;")) {
                access = Integer_ACC_ANNOTATION;
                if (debugHandleInvoke) {
                  System.out.println("FunctionalInterface is true");
                }
                break searchloop;
              }
            }
          }
        }
      } else {
        // We cannot locate or read the .class file, better pretend it is an Annotation.
        if (debugHandleInvoke) {
          System.out.printf("Unable to locate class: %s%n", classname);
        }
        access = Integer_ACC_ANNOTATION;
      }
      accessFlags.put(classname, access);
    }
    return access;
  }

  /**
   * Returns true if the specified classname is instrumented.
   *
   * @param classname class to be checked
   * @param methodName method to be checked (currently unused)
   * @return true if classname is instrumented
   */
  private boolean isClassnameInstrumented(@ClassGetName String classname, String methodName) {

    if (debugHandleInvoke) {
      System.out.printf("Checking callee instrumented on %s%n", classname);
    }

    // Our copy of daikon.plumelib is not instrumented.  It would be odd, though,
    // to see calls to this.
    if (classname.startsWith("daikon.plumelib")) {
      return false;
    }

    // Special-case JUnit test classes.
    if (junitTestClasses.contains(classname)) {
      return false;
    }

    if (daikon.dcomp.Instrument24.is_transformer(classname.replace('.', '/'))) {
      return false;
    }

    // Special case the execution trace tool.
    if (classname.startsWith("minst.Minst")) {
      return false;
    }

    // We should probably change the interface to include method name
    // and use "classname.methodname" as arg to pattern matcher.
    // If any of the omit patterns match, use the uninstrumented version of the method
    for (Pattern p : DynComp.ppt_omit_pattern) {
      if (p.matcher(classname).find()) {
        if (debugHandleInvoke) {
          System.out.printf("callee instrumented = false: %s.%s%n", classname, methodName);
        }
        return false;
      }
    }

    // If its not a JDK class, presume its instrumented.
    if (!BcelUtil.inJdk(classname)) {
      return true;
    }

    int i = classname.lastIndexOf('.');
    if (i > 0) {
      if (Premain.problem_packages.contains(classname.substring(0, i))) {
        debugInstrument.log(
            "Don't call instrumented member of problem package %s%n", classname.substring(0, i));
        return false;
      }
    }

    if (Premain.problem_classes.contains(classname)) {
      debugInstrument.log("Don't call instrumented member of problem class %s%n", classname);
      return false;
    }

    // We have decided not to use the instrumented version of Random as
    // the method generates values based on an initial seed value.
    // (Typical of random() algorithms.) This has the undesirable side
    // effect of putting all the generated values in the same comparison
    // set when they should be distinct.
    // NOTE: If we find other classes that should not use the instrumented
    // versions, we should consider making this a searchable list.
    if (classname.equals("java.util.Random")) {
      return false;
    }

    // If using the instrumented JDK, then everthing but object is instrumented
    if (Premain.jdk_instrumented && !classname.equals("java.lang.Object")) {
      return true;
    }

    return false;
  }

  /**
   * Given a classname return it's superclass name. Note that BCEL reports that the superclass of
   * 'java.lang.Object' is 'java.lang.Object' rather than saying there is no superclass.
   *
   * @param classname the fully qualified name of the class in binary form. E.g., "java.util.List"
   * @return superclass name of classname or null if there is an error
   */
  private @ClassGetName String getSuperclassName(String classname) {
    ClassModel cm = getClassModel(classname);
    if (cm != null) {
      return ClassGen24.getSuperclassName(cm);
    } else {
      return null;
    }
  }

  /** Cache for {@link #getClassModel} method. */
  private static Map<String, ClassModel> classModelCache =
      new ConcurrentHashMap<String, ClassModel>();

  /**
   * There are times when it is useful to inspect a class file other than the one we are currently
   * instrumenting. Note we cannot use classForName to do this as it might trigger a recursive call
   * to Instrument which would not work at this point.
   *
   * <p>Given a class name, we treat it as a system resource, create a {@code Path} to it and have
   * {@code java.lang.classfile} read and create a {@code ClassModel} object.
   *
   * @param classname the fully qualified name of the class in binary form, e.g., "java.util.List"
   * @return the ClassModel of the corresponding classname or null
   */
  private @Nullable ClassModel getClassModel(String classname) {
    ClassModel cached = classModelCache.get(classname);
    if (cached != null) {
      return cached;
    }

    URL class_url = ClassLoader.getSystemResource(classname.replace('.', '/') + ".class");
    if (class_url != null) {
      try (InputStream inputStream = class_url.openStream()) {
        if (inputStream != null) {
          ClassModel result = classFile.parse(IOUtils.toByteArray(inputStream));
          classModelCache.put(classname, result);
          return result;
        }
      } catch (Throwable t) {
        t.printStackTrace();
        throw new Error(String.format("Unexpected error while reading %s%n", classname), t);
      }
    }
    // Do not cache a null result, because a subsequent invocation might return non-null.
    return null;
  }

  /**
   * Returns whether or not the method is Object.equals().
   *
   * @param methodName method to check
   * @param returnType return type of method
   * @param args array of argument types to method
   * @return true if method is Object.equals()
   */
  @Pure
  boolean is_object_equals(String methodName, ClassDesc returnType, ClassDesc[] args) {
    return (methodName.equals("equals")
        && returnType.equals(CD_boolean)
        && args.length == 1
        && args[0].equals(CD_Object));
  }

  /**
   * Returns true if the specified method is Object.clone().
   *
   * @param methodName method to check
   * @param returnType return type of method
   * @param args array of argument types to method
   * @return true if method is Object.clone()
   */
  @Pure
  boolean is_object_clone(String methodName, ClassDesc returnType, ClassDesc[] args) {
    return methodName.equals("clone") && returnType.equals(CD_Object) && (args.length == 0);
  }

  /**
   * Instrument calls to the Object method clone. An instrumented version is called if it exists,
   * the non-instrumented version if it does not.
   *
   * @param invoke invoke instruction to inspect and replace
   * @param returnType return type of method
   * @param classname target class
   * @return instruction list to call the correct version of clone or toString
   */
  private List<CodeElement> instrument_clone_call(
      InvokeInstruction invoke, ClassDesc returnType, @ClassGetName String classname) {

    List<CodeElement> il = new ArrayList<>();
    if (classname.startsWith("[")) {
      // <array>.clone() is never instrumented, return original invoke.
      il.add(invoke);
      return il;
    }

    // push the target class
    il.add(buildLDCInstruction(poolBuilder.stringEntry(classname)));

    // if this is a super call
    if (invoke.opcode().equals(Opcode.INVOKESPECIAL)) {

      // Runtime will discover if the object's superclass has an instrumented clone method.
      // If so, call it; otherwise call the uninstrumented version.
      // use CD_Class
      il.add(dcr_call("dcomp_super_clone", returnType, new ClassDesc[] {CD_Object, CD_Class}));

    } else { // a regular (non-super) clone() call

      // Runtime will discover if the object has an instrumented clone method.
      // If so, call it; otherwise call the uninstrumented version.
      il.add(dcr_call("dcomp_clone", returnType, new ClassDesc[] {CD_Object, CD_Class}));
    }

    return il;
  }

  /**
   * Create the instructions that replace the object eq or ne branch instruction. They are replaced
   * by a call to the specified compare_method (which returns a boolean) followed by the specified
   * boolean ifeq or ifne instruction.
   *
   * @param branch a branch instruction
   * @param compare_method name of DCRuntime routine to call
   * @param boolean_if branch instruction to gerate of the runtime call
   * @return instruction list to do object comparison
   */
  private List<CodeElement> object_comparison(
      BranchInstruction branch, String compare_method, Opcode boolean_if) {
    List<CodeElement> il = new ArrayList<>();

    MethodRefEntry mre =
        poolBuilder.methodRefEntry(
            runtimeCD, compare_method, MethodTypeDesc.of(CD_boolean, two_objects_arg));
    il.add(InvokeInstruction.of(Opcode.INVOKESTATIC, mre));
    il.add(BranchInstruction.of(boolean_if, branch.target()));
    return il;
  }

  /**
   * Handles load and store field instructions. The instructions must be augmented to either push
   * (load) or pop (store) the tag on the tag stack. This is accomplished by calling the tag get/set
   * method for this field.
   */
  private List<CodeElement> load_store_field(MethodGen24 mgen, FieldInstruction f) {

    System.out.println();
    System.out.println("mgen: " + mgen);
    System.out.println("FieldInst: " + f);
    System.out.println("field_name: " + f.name().stringValue());
    System.out.println("owner: " + f.owner());
    System.out.println("owner: " + f.owner().asInternalName().replace('/', '.'));
    ClassDesc field_type = f.typeSymbol();
    System.out.println("field_type: " + field_type);
    int field_size = TypeKind.from(field_type).slotSize();
    if (!field_type.isPrimitive()) {
      return null;
    }

    List<CodeElement> il = new ArrayList<>();
    Opcode op = f.opcode();
    // ObjectType obj_type = (ObjectType) f.getReferenceType(pool);
    @BinaryName String classname = classGen.getClassName();
    String fieldName = f.name().stringValue();
    String owner = f.owner().asInternalName().replace('/', '.');
    ClassDesc ownerCD = f.owner().asSymbol();

    // If this class doesn't support tag fields, don't load/store them
    if (!tag_fields_ok(mgen, classname)) {
      if (op.equals(Opcode.GETFIELD) || op.equals(Opcode.GETSTATIC)) {
        il.add(dcr_call("push_const", CD_void, noArgsCD));
      } else {
        il.add(loadIntegerConstant(1));
        il.add(dcr_call("discard_tag", CD_void, integer_arg));
      }

      // Perform the normal field command
      il.add(f);
      return il;
    }

    if (op.equals(Opcode.GETSTATIC)) {
      MethodRefEntry mre =
          poolBuilder.methodRefEntry(
              ownerCD,
              Premain.tag_method_name(Premain.GET_TAG, owner, fieldName),
              MethodTypeDesc.of(CD_void, noArgsCD));
      il.add(InvokeInstruction.of(Opcode.INVOKESTATIC, mre));
    } else if (op.equals(Opcode.PUTSTATIC)) {
      MethodRefEntry mre =
          poolBuilder.methodRefEntry(
              ownerCD,
              Premain.tag_method_name(Premain.SET_TAG, owner, fieldName),
              MethodTypeDesc.of(CD_void, noArgsCD));
      il.add(InvokeInstruction.of(Opcode.INVOKESTATIC, mre));
    } else if (op.equals(Opcode.GETFIELD)) {
      // il.add(InstructionFactory.createDup(obj_type.getSize()));
      il.add(StackInstruction.of(Opcode.DUP));
      MethodRefEntry mre =
          poolBuilder.methodRefEntry(
              ownerCD,
              Premain.tag_method_name(Premain.GET_TAG, owner, fieldName),
              MethodTypeDesc.of(CD_void, noArgsCD));
      il.add(InvokeInstruction.of(Opcode.INVOKEVIRTUAL, mre));
    } else { // must be Opcode.PUTFIELD
      if (field_size == 2) {
        // LocalVariableGen lv = get_tmp2_local(mg, field_type);
        // il.add(InstructionFactory.createStore(field_type, lv.getIndex()));
        // il.add(InstructionFactory.createDup(obj_type.getSize()));
        // MethodRefEntry mre =
        // poolBuilder.methodRefEntry(
        // ownerCD, Premain.tag_method_name(Premain.SET_TAG, owner, fieldName),
        // MethodTypeDesc.of(CD_void, noArgsCD));
        // il.add(InvokeInstruction.of(Opcode.INVOKEVIRTUAL, mre));
        // il.add(InstructionFactory.createLoad(field_type, lv.getIndex()));
      } else {
        il.add(StackInstruction.of(Opcode.SWAP));
        // il.add(StackInstruction.of(obj_type.getSize()));
        il.add(StackInstruction.of(Opcode.DUP));
        MethodRefEntry mre =
            poolBuilder.methodRefEntry(
                ownerCD,
                Premain.tag_method_name(Premain.SET_TAG, owner, fieldName),
                MethodTypeDesc.of(CD_void, noArgsCD));
        il.add(InvokeInstruction.of(Opcode.INVOKEVIRTUAL, mre));
        il.add(StackInstruction.of(Opcode.SWAP));
      }
    }

    // Perform the normal field command
    il.add(f);

    return il;
  }

  /**
   * Handles load local instructions. The instructions must be augmented to push the tag on the tag
   * stack. This is accomplished by calling the specified method in DCRuntime and passing that
   * method the tag_frame frame and the offset of local/parameter.
   *
   * @param load a load instruction
   * @param tagFrameLocal local variable for the tag_frame
   * @param method name of DCRuntime routine to call
   * @return instruction list to do object comparison
   */
  private List<CodeElement> load_local(
      LoadInstruction load, LocalVariable tagFrameLocal, String method) {
    List<CodeElement> il = new ArrayList<>();

    debugInstrument.log("CreateLoad %s %d%n", load.opcode(), tagFrameLocal.slot());

    // Push the tag_frame frame
    il.add(LoadInstruction.of(TypeKind.REFERENCE, tagFrameLocal.slot()));

    // push index of local
    il.add(loadIntegerConstant(load.slot()));

    // Call the runtime method to handle loading the local/parameter
    MethodRefEntry mre =
        poolBuilder.methodRefEntry(
            runtimeCD, method, MethodTypeDesc.of(CD_void, objectArrayCD, CD_int));
    il.add(InvokeInstruction.of(Opcode.INVOKESTATIC, mre));

    // the original load instruction
    il.add(load);
    return il;
  }

  /**
   * Handles store local instructions. The instructions must be augmented to pop the tag off the tag
   * stack. This is accomplished by calling the specified method in DCRuntime and passing that
   * method the tag_frame frame and the offset of local/parameter.
   *
   * @param store a store instruction
   * @param tagFrameLocal local variable for the tag_frame
   * @param method name of DCRuntime routine to call
   * @return instruction list to do object comparison
   */
  private List<CodeElement> store_local(
      StoreInstruction store, LocalVariable tagFrameLocal, String method) {
    List<CodeElement> il = new ArrayList<>();

    debugInstrument.log("CreateStore %s %d%n", store.opcode(), tagFrameLocal.slot());

    // Push the tag_frame frame
    il.add(LoadInstruction.of(TypeKind.REFERENCE, tagFrameLocal.slot()));

    // push index of local
    il.add(loadIntegerConstant(store.slot()));

    // Call the runtime method to handle storeing the local/parameter
    MethodRefEntry mre =
        poolBuilder.methodRefEntry(
            runtimeCD, method, MethodTypeDesc.of(CD_void, objectArrayCD, CD_int));
    il.add(InvokeInstruction.of(Opcode.INVOKESTATIC, mre));

    // the original store instruction
    il.add(store);
    return il;
  }

  /** Returns the number of the specified field in the primitive fields of obj_type. */
  int get_field_num(String name, ObjectType obj_type) {

    // If this is the current class, get the information directly
    // if (obj_type.getClassName().equals(orig_class.getClassName())) {
    // int fcnt = 0;
    // for (Field f : orig_class.getFields()) {
    // if (f.getName().equals(name)) {
    // return fcnt;
    // }
    // if (f.getType() instanceof BasicType) {
    // fcnt++;
    // }
    // }
    // throw new Error("Can't find " + name + " in " + obj_type);
    // }

    // Look up the class using this classes class loader.  This may
    // not be the best way to accomplish this.
    Class<?> obj_class;
    try {
      obj_class = Class.forName(obj_type.getClassName(), false, loader);
    } catch (Exception e) {
      throw new Error("can't find class " + obj_type.getClassName(), e);
    }

    // Loop through all of the fields, counting the number of primitive fields
    int fcnt = 0;
    for (java.lang.reflect.Field f : obj_class.getDeclaredFields()) {
      if (f.getName().equals(name)) {
        return fcnt;
      }
      if (f.getType().isPrimitive()) {
        fcnt++;
      }
    }
    throw new Error("Can't find " + name + " in " + obj_class);
  }

  /**
   * Gets the local variable used to store a category2 temporary. This is used in the PUTFIELD code
   * to temporarily store the value being placed in the field.
   */
  LocalVariableGen get_tmp2_local(MethodGen mg, Type typ) {

    // UNDONE - not converted

    String name = "dcomp_$tmp_" + typ;
    // System.out.printf("local var name = %s%n", name);

    // See if the local has already been created
    for (LocalVariableGen lv : mg.getLocalVariables()) { // should be localsTable?
      if (lv.getName().equals(name)) {
        assert lv.getType().equals(typ) : lv + " " + typ;
        return lv;
      }
    }

    // Create the variable
    return mg.addLocalVariable(name, typ, null, null);
  }

  /**
   * Returns the local variable used to store the return result. If it is not present, creates it
   * with the specified type. If the variable is known to already exist, the type can be null.
   *
   * @param mgen describes the given method
   * @param returnType the type of the return; may be null if the variable is known to already exist
   * @param minfo for the given method's code
   * @return a local variable to save the return value
   */
  @SuppressWarnings("nullness")
  private LocalVariable getReturnLocal(
      MethodGen24 mgen, @Nullable ClassDesc returnType, MethodGen24.MInfo24 minfo) {

    // If a type was specified and the variable was found, they must match.
    if (minfo.returnLocal == null) {
      assert returnType != null : " return__$trace2_val doesn't exist";
    } else {
      assert minfo.returnLocal.typeSymbol().equals((Object) returnType)
          : " returnType = " + returnType + "; current type = " + minfo.returnLocal.typeSymbol();
    }

    if (minfo.returnLocal == null) {
      debugInstrument.log("Adding return local of type %s%n", returnType);
      minfo.returnLocal =
          createLocalWithMethodScope(mgen, minfo, "return__$trace2_val", returnType);
    }

    return minfo.returnLocal;
  }

  /**
   * Returns the local variable used to store the return result. If it is not present, creates it
   * with the specified type. If the variable is known to already exist, the type can be null.
   */
  LocalVariableGen get_return_local(MethodGen mg, @Nullable Type return_type) {

    // UNDONE - not converted

    // Find the local used for the return value
    LocalVariableGen return_local = null;
    for (LocalVariableGen lv : mg.getLocalVariables()) { // should be localsTable?
      if (lv.getName().equals("return__$trace2_val")) {
        return_local = lv;
        break;
      }
    }

    // If a type was specified and the variable was found, they must match
    if (return_local == null) {
      assert (return_type != null) : " return__$trace2_val doesn't exist";
    } else {
      assert return_type.equals(return_local.getType())
          : " return_type = " + return_type + "; current type = " + return_local.getType();
    }

    if (return_local == null) {
      // log ("Adding return local of type %s%n", return_type);
      return_local = mg.addLocalVariable("return__$trace2_val", return_type, null, null);
    }

    return return_local;
  }

  /**
   * Creates a MethodInfo corresponding to the specified method. The exit locations are filled in,
   * but the reflection information is not generated. Returns null if there are no instructions.
   *
   * @param classInfo class containing the method
   * @param mgen method to inspect
   * @return MethodInfo for the method
   */
  private @Nullable MethodInfo create_method_info(ClassInfo classInfo, MethodGen24 mgen) {

    // Get the argument names for this method
    String[] paramNames = mgen.getParameterNames();

    if (debugInstrument.enabled) {
      debugInstrument.log("create_method_info: %s%n", paramNames.length);
      for (int i = 0; i < paramNames.length; i++) {
        debugInstrument.log("param: %s%n", paramNames[i]);
      }
    }

    // Get the parameter types for this method.
    ClassDesc[] paramTypes = mgen.getParameterTypes();
    @ClassGetName String[] arg_type_strings = new @ClassGetName String[paramTypes.length];
    for (int i = 0; i < paramTypes.length; i++) {
      arg_type_strings[i] = Instrument24.typeToClassGetName(paramTypes[i]);
    }

    // Loop through each instruction and find the line number for each return opcode.
    List<Integer> exit_locs = new ArrayList<>();

    // Tells whether each exit loc in the method is included or not (based on filters).
    List<Boolean> isIncluded = new ArrayList<>();

    debugInstrument.log("Looking for exit points in %s%n", mgen.getName());
    List<CodeElement> il = mgen.getInstructionList();
    if (il.isEmpty()) {
      return null;
    }

    int line_number = 0;
    int last_line_number = 0;

    for (CodeElement inst : il) {
      boolean foundLine = false;

      if (inst instanceof LineNumber ln) {
        line_number = ln.line();
        foundLine = true;
      }

      if (inst instanceof ReturnInstruction) {
        debugInstrument.log("Exit at line %d%n", line_number);

        // Only do incremental lines if we don't have the line generator.
        if (line_number == last_line_number && foundLine == false) {
          debugInstrument.log("Could not find line %d%n", line_number);
          line_number++;
        }
        last_line_number = line_number;

        exit_locs.add(line_number);
        isIncluded.add(true);
      }
    }

    return new MethodInfo(
        classInfo, mgen.getName(), paramNames, arg_type_strings, exit_locs, isIncluded);
  }

  /**
   * Creates code that makes the index comparable (for indexing purposes) with the array in array
   * load instructions. First the arrayref and its index are duplicated on the stack. Then the
   * appropriate array load method is called to mark them as comparable and update the tag stack.
   * Finally the original load instruction is performed.
   *
   * @param mgen method to check
   * @param inst an array load instruction
   * @return instruction list that calls the runtime to handle the array load instruction
   */
  private List<CodeElement> array_load(MethodGen24 mgen, ArrayLoadInstruction inst) {
    List<CodeElement> il = new ArrayList<>();

    // Duplicate the array ref and index and pass them to DCRuntime
    // which will make the index comparable with the array.  In the case
    // of primtives it will also get the tag for the primitive and push
    // it on the tag stack.
    il.add(StackInstruction.of(Opcode.DUP2));
    String method = "primitive_array_load";
    if (inst.typeKind().equals(TypeKind.REFERENCE)) {
      method = "ref_array_load";
    } else if (is_uninit_class(mgen.getClassName())) {
      method = "primitive_array_load_null_ok";
    }

    il.add(dcr_call(method, CD_void, new ClassDesc[] {CD_Object, CD_int}));

    // Perform the original instruction
    il.add(inst);

    return il;
  }

  /**
   * Creates code to make the index comparable (for indexing purposes) with the array in the array
   * store instruction. This is accomplished by calling the specified method and passing it the
   * array reference, index, and value (of base_type). The method will mark the array and index as
   * comparable and perform the array store.
   *
   * @param inst an array store instruction
   * @param method runtime method to call
   * @param base_type type of array store
   * @return instruction list that calls the runtime to handle the array store instruction
   */
  InstructionList array_store(
      org.apache.bcel.generic.Instruction inst, String method, Type base_type) {

    InstructionList il = new InstructionList();
    Type arr_type = new ArrayType(base_type, 1);
    il.append(dcr_call(method, Type.VOID, new Type[] {arr_type, Type.INT, base_type}));
    return il;
  }

  /**
   * Creates code that pushes the array's tag onto the tag stack, so that the index is comparable to
   * the array length. First, the arrayref is duplicated on the stack. Then a method is called to
   * push the array's tag onto the tag stack. Finally the original arraylength instruction is
   * performed.
   *
   * @param inst an arraylength instruction
   * @return instruction list that calls the runtime to handle the arraylength instruction
   */
  private List<CodeElement> array_length(Instruction inst) {
    List<CodeElement> il = new ArrayList<>();

    // Duplicate the array ref and pass it to DCRuntime which will push
    // it onto the tag stack.
    il.add(StackInstruction.of(Opcode.DUP));
    il.add(dcr_call("push_array_tag", CD_void, object_arg));

    // Perform the original instruction
    il.add(inst);

    return il;
  }

  /**
   * Creates code to make the declared length of a new array comparable to its index.
   *
   * @param inst a anewarray or newarray instruction
   * @return instruction list that calls the runtime to handle the newarray instruction
   */
  private List<CodeElement> new_array(Instruction inst) {
    List<CodeElement> il = new ArrayList<>();

    // Perform the original instruction
    il.add(inst);

    // Duplicate the array ref from the top of the stack and pass it
    // to DCRuntime which will push it onto the tag stack.
    il.add(StackInstruction.of(Opcode.DUP));
    il.add(dcr_call("push_array_tag", CD_void, object_arg));

    // Make the array and the count comparable. Also, pop the tags for
    // the array and the count off the tag stack.
    il.add(dcr_call("cmp_op", CD_void, noArgsCD));

    return il;
  }

  /**
   * Creates code to make the declared lengths of a new two-dimensional array comparable to the
   * corresponding indices.
   *
   * @param inst a multianewarray instruction
   * @return instruction list that calls the runtime to handle the multianewarray instruction
   */
  private List<CodeElement> multiarray2(Instruction inst) {
    List<CodeElement> il = new ArrayList<>();

    // Duplicate both count arguments
    il.add(StackInstruction.of(Opcode.DUP2));

    // Perform the original instruction
    il.add(inst);

    // Duplicate the new arrayref and put it below the count arguments
    // Stack is now: ..., arrayref, count1, count2, arrayref
    il.add(StackInstruction.of(Opcode.DUP_X2));

    il.add(dcr_call("multianewarray2", CD_void, new ClassDesc[] {CD_int, CD_int, objectArrayCD}));

    return il;
  }

  /**
   * Returns whether or not this ppt should be included. A ppt is included if it matches ones of the
   * select patterns and doesn't match any of the omit patterns.
   *
   * @param className class to test
   * @param methodName method to test
   * @param pptName ppt to look for
   * @return true if this ppt should be included
   */
  boolean should_track(@BinaryName String className, String methodName, String pptName) {

    debugInstrument.log(
        "Considering tracking (24) ppt: %s, %s, %s%n", className, methodName, pptName);

    // Don't track any JDK classes
    if (BcelUtil.inJdk(className)) {
      debug_transform.log("ignoring %s, is a JDK class%n", className);
      return false;
    }

    // Don't track toString methods because we call them in
    // our debug statements.
    if (pptName.contains("toString")) {
      debug_transform.log("ignoring %s, is a toString method%n", pptName);
      return false;
    }

    // call shouldIgnore to check ppt-omit-pattern(s) and ppt-select-pattern(s)
    return !daikon.chicory.Instrument24.shouldIgnore(className, methodName, pptName);
  }

  /**
   * Constructs a ppt entry name from a Method.
   *
   * @param fullClassName class name
   * @param m method
   * @return corresponding ppt name
   */
  static String methodEntryName(String fullClassName, Method m) {

    // System.out.printf("classname = %s, method = %s, short_name = %s%n",
    //                   fullClassName, m, m.getName());

    // Get an array of the type names
    Type[] argTypes = m.getArgumentTypes();
    String[] type_names = new String[argTypes.length];
    for (int ii = 0; ii < argTypes.length; ii++) {
      type_names[ii] = argTypes[ii].toString();
    }

    // Remove exceptions from the name
    String full_name = m.toString();
    full_name = full_name.replaceFirst("\\s*throws.*", "");

    return fullClassName
        + "."
        + DaikonWriter.methodEntryName(fullClassName, type_names, full_name, m.getName());
  }

  /**
   * Convenience function to construct a call to a static method in DCRuntime.
   *
   * @param methodName method to call
   * @param returnType type of method return
   * @param argTypes array of method argument types
   * @return InvokeInstruction for the call
   */
  InvokeInstruction dcr_call(String methodName, ClassDesc returnType, ClassDesc[] argTypes) {
    MethodRefEntry mre =
        poolBuilder.methodRefEntry(runtimeCD, methodName, MethodTypeDesc.of(returnType, argTypes));
    return InvokeInstruction.of(Opcode.INVOKESTATIC, mre);
  }

  /**
   * Convenience function to construct a call to a static method in DCRuntime.
   *
   * @param methodName method to call
   * @param returnType type of method return
   * @param argTypes array of method argument types
   * @return InvokeInstruction for the call
   */
  org.apache.bcel.generic.InvokeInstruction dcr_call(
      String methodName, Type returnType, Type[] argTypes) {

    return ifact.createInvoke(
        dcompRuntimeClassname, methodName, returnType, argTypes, Const.INVOKESTATIC);
  }

  /**
   * Create the code to call discard_tag(tag_count) and append inst to the end of that code.
   *
   * @param inst instruction to be replaced
   * @param tag_count number of tags to discard
   * @return instruction list to discard tag(s)
   */
  List<CodeElement> discard_tag_code(CodeElement inst, int tag_count) {
    List<CodeElement> il = new ArrayList<>();
    il.add(loadIntegerConstant(tag_count));
    il.add(dcr_call("discard_tag", CD_void, integer_arg));
    il.add(inst);
    return il;
  }

  /**
   * Duplicates the item on the top of stack. If the value on the top of the stack is a primitive,
   * we need to do the same on the tag stack. Otherwise, we need do nothing.
   */
  InstructionList dup_tag(org.apache.bcel.generic.Instruction inst, OperandStack stack) {
    // Type top = stack.peek();
    if (debug_dup.enabled) {
      debug_dup.log("DUP -> %s [... %s]%n", "dup", stack_contents(stack, 2));
    }
    // if (is_primitive(top)) {
    //      return build_il(dcr_call("dup", Type.VOID, Type.NO_ARGS), inst);
    // }
    return null;
  }

  /**
   * Duplicates the item on the top of the stack and inserts it 2 values down in the stack. If the
   * value at the top of the stack is not a primitive, there is nothing to do here. If the second
   * value is not a primitive, then we need only to insert the duped value down 1 on the tag stack
   * (which contains only primitives).
   */
  InstructionList dup_x1_tag(org.apache.bcel.generic.Instruction inst, OperandStack stack) {
    // Type top = stack.peek();
    if (debug_dup.enabled) {
      debug_dup.log("DUP -> %s [... %s]%n", "dup_x1", stack_contents(stack, 2));
    }
    // if (!is_primitive(top)) {
    // return null;
    // }
    //    String method = "dup_x1";
    // if (!is_primitive(stack.peek(1))) {
    //      method = "dup";
    // }
    //    return build_il(dcr_call(method, Type.VOID, Type.NO_ARGS), inst);
    return null;
  }

  /**
   * Duplicates either the top 2 category 1 values or a single category 2 value and inserts it 2 or
   * 3 values down on the stack.
   */
  InstructionList dup2_x1_tag(org.apache.bcel.generic.Instruction inst, OperandStack stack) {
    // String op;
    // Type top = stack.peek();
    // if (is_category2(top)) {
    // if (is_primitive(stack.peek(1))) {
    // op = "dup_x1";
    // } else { // not a primitive, so just dup
    // op = "dup";
    // }
    // } else if (is_primitive(top)) {
    // if (is_primitive(stack.peek(1)) && is_primitive(stack.peek(2))) op = "dup2_x1";
    // else if (is_primitive(stack.peek(1))) op = "dup2";
    // else if (is_primitive(stack.peek(2))) op = "dup_x1";
    // else {
    //// neither value 1 nor value 2 is primitive
    // op = "dup";
    // }
    // } else { // top is not primitive
    // if (is_primitive(stack.peek(1)) && is_primitive(stack.peek(2))) {
    // op = "dup_x1";
    // } else if (is_primitive(stack.peek(1))) {
    // op = "dup";
    // } else { // neither of the top two values is primitive
    // op = null;
    // }
    // }
    // if (debug_dup.enabled) {
    // debug_dup.log("DUP2_X1 -> %s [... %s]%n", op, stack_contents(stack, 3));
    // }

    // if (op != null) {
    //      return build_il(dcr_call(op, Type.VOID, Type.NO_ARGS), inst);
    // }
    return null;
  }

  /**
   * Duplicate either one category 2 value or two category 1 values. The instruction is implemented
   * as necessary on the tag stack.
   */
  InstructionList dup2_tag(org.apache.bcel.generic.Instruction inst, OperandStack stack) {
    Type top = stack.peek();
    String op;
    if (is_category2(top)) {
      op = "dup";
    } // else if (is_primitive(top) && is_primitive(stack.peek(1))) op = "dup2";
    // else if (is_primitive(top) || is_primitive(stack.peek(1))) op = "dup";
    else {
      // both of the top two items are not primitive, nothing to dup
      op = null;
    }
    if (debug_dup.enabled) {
      debug_dup.log("DUP2 -> %s [... %s]%n", op, stack_contents(stack, 2));
    }
    if (op != null) {
      //      return build_il(dcr_call(op, Type.VOID, Type.NO_ARGS), inst);
    }
    return null;
  }

  /**
   * Dup the category 1 value on the top of the stack and insert it either two or three values down
   * on the stack.
   */
  InstructionList dup_x2(org.apache.bcel.generic.Instruction inst, OperandStack stack) {
    // Type top = stack.peek();
    String op = null;
    // if (is_primitive(top)) {
    // if (is_category2(stack.peek(1))) op = "dup_x1";
    // else if (is_primitive(stack.peek(1)) && is_primitive(stack.peek(2))) op = "dup_x2";
    // else if (is_primitive(stack.peek(1)) || is_primitive(stack.peek(2))) op = "dup_x1";
    // else {
    // op = "dup";
    // }
    // }
    if (debug_dup.enabled) {
      debug_dup.log("DUP_X2 -> %s [... %s]%n", op, stack_contents(stack, 3));
    }
    if (op != null) {
      //      return build_il(dcr_call(op, Type.VOID, Type.NO_ARGS), inst);
    }
    return null;
  }

  /**
   * Duplicate the top one or two operand stack values and insert two, three, or four values down.
   */
  InstructionList dup2_x2(org.apache.bcel.generic.Instruction inst, OperandStack stack) {
    // Type top = stack.peek();
    // String op;
    // if (is_category2(top)) {
    // if (is_category2(stack.peek(1))) op = "dup_x1";
    // else if (is_primitive(stack.peek(1)) && is_primitive(stack.peek(2))) op = "dup_x2";
    // else if (is_primitive(stack.peek(1)) || is_primitive(stack.peek(2))) op = "dup_x1";
    // else {
    //// both values are references
    // op = "dup";
    // }
    // } else if (is_primitive(top)) {
    // if (is_category2(stack.peek(1))) {
    // throw new Error("not supposed to happen " + stack_contents(stack, 3));
    // } else if (is_category2(stack.peek(2))) {
    // if (is_primitive(stack.peek(1))) {
    // op = "dup2_x1";
    // } else {
    // op = "dup_x1";
    // }
    // } else if (is_primitive(stack.peek(1))) {
    // if (is_primitive(stack.peek(2)) && is_primitive(stack.peek(3))) op = "dup2_x2";
    // else if (is_primitive(stack.peek(2)) || is_primitive(stack.peek(3))) op = "dup2_x1";
    // else {
    //// both 2 and 3 are references
    // op = "dup2";
    // }
    // } else { // 1 is a reference
    // if (is_primitive(stack.peek(2)) && is_primitive(stack.peek(3))) op = "dup_x2";
    // else if (is_primitive(stack.peek(2)) || is_primitive(stack.peek(3))) op = "dup_x1";
    // else {
    //// both 2 and 3 are references
    // op = "dup";
    // }
    // }
    // } else { // top is a reference
    // if (is_category2(stack.peek(1))) {
    // throw new Error("not supposed to happen " + stack_contents(stack, 3));
    // } else if (is_category2(stack.peek(2))) {
    // if (is_primitive(stack.peek(1))) {
    // op = "dup_x1";
    // } else {
    // op = null; // nothing to dup
    // }
    // } else if (is_primitive(stack.peek(1))) {
    // if (is_primitive(stack.peek(2)) && is_primitive(stack.peek(3))) op = "dup_x2";
    // else if (is_primitive(stack.peek(2)) || is_primitive(stack.peek(3))) op = "dup_x1";
    // else {
    //// both 2 and 3 are references
    // op = "dup";
    // }
    // } else { // 1 is a reference
    // op = null; // nothing to dup
    // }
    // }
    // if (debug_dup.enabled) {
    // debug_dup.log("DUP_X2 -> %s [... %s]%n", op, stack_contents(stack, 3));
    // }
    // if (op != null) {
    //      return build_il(dcr_call(op, Type.VOID, Type.NO_ARGS), inst);
    // }
    return null;
  }

  /**
   * Pop instructions discard the top of the stack. We want to discard the top of the tag stack iff
   * the item on the top of the stack is a primitive.
   */
  InstructionList pop_tag(org.apache.bcel.generic.Instruction inst, OperandStack stack) {
    // Type top = stack.peek();
    // if (is_primitive(top)) {
    // return discard_tag_code(inst, 1);
    // }
    return null;
  }

  /**
   * Pops either the top 2 category 1 values or a single category 2 value from the top of the stack.
   * We must do the same to the tag stack if the values are primitives.
   */
  InstructionList pop2_tag(org.apache.bcel.generic.Instruction inst, OperandStack stack) {
    Type top = stack.peek();
    if (is_category2(top)) {
      // return discard_tag_code(inst, 1);
    } else {
      int cnt = 0;
      // if (is_primitive(top)) {
      // cnt++;
      // }
      // if (is_primitive(stack.peek(1))) {
      // cnt++;
      // }
      if (cnt > 0) {
        // return discard_tag_code(inst, cnt);
      }
    }
    return null;
  }

  /**
   * Swaps the two category 1 types on the top of the stack. We need to swap the top of the tag
   * stack if the two top elements on the real stack are primitives.
   */
  InstructionList swap_tag(org.apache.bcel.generic.Instruction inst, OperandStack stack) {
    // Type type1 = stack.peek();
    // Type type2 = stack.peek(1);
    // if (is_primitive(type1) && is_primitive(type2)) {
    //      return build_il(dcr_call("swap", Type.VOID, Type.NO_ARGS), inst);
    // }
    return null;
  }

  /**
   * Handle the instruction that allocates multi-dimensional arrays. If the new array has 2
   * dimensions, make the integer arguments comparable to the corresponding indices of the new
   * array. For any other number of dimensions, discard the tags for the arguments. Higher
   * dimensions should really be handled as well, but there are very few cases of this and the
   * resulting code would be quite complex (see multiarray2 for details).
   */
  private List<CodeElement> multi_newarray_dc(NewMultiArrayInstruction inst) {
    int dims = inst.dimensions();
    if (dims == 2) {
      return multiarray2(inst);
    } else {
      return discard_tag_code(inst, dims);
    }
  }

  /**
   * Create an instruction list that calls the runtime to handle returns for the tag stack follow by
   * the original return instruction.
   *
   * @param mgen method to check
   * @param inst return instruction to be replaced
   * @return the instruction list
   */
  private List<CodeElement> return_tag(MethodGen24 mgen, Instruction inst) {
    List<CodeElement> il = new ArrayList<>();

    ClassDesc type = mgen.getReturnType();

    // Push the tag frame
    il.add(LoadInstruction.of(TypeKind.REFERENCE, tagFrameLocal.slot()));

    if (is_primitive(type)) {
      il.add(dcr_call("normal_exit_primitive", CD_void, objectArrayCD_arg));
    } else {
      il.add(dcr_call("normal_exit", CD_void, objectArrayCD_arg));
    }
    il.add(inst);
    return il;
  }

  /**
   * Returns whether or not the specified type is a primitive (int, float, double, etc).
   *
   * @param type type to check
   * @return true if type is primitive
   */
  @Pure
  boolean is_primitive(ClassDesc type) {
    return type.isPrimitive() && !type.equals(CD_void);
  }

  /**
   * Returns whether or not the specified type is a category 2 (8 byte) type.
   *
   * @param type type to check
   * @return true if type requires 8 bytes
   */
  @Pure
  boolean is_category2(Type type) {
    return (type == Type.DOUBLE) || (type == Type.LONG);
  }

  /**
   * Converts a BCEL type to a Class. The class referenced will be loaded but not initialized. The
   * specified loader must be able to find it. If load is null, the default loader will be used.
   *
   * @param t type to get class for
   * @param loader to use to locate class
   * @return instance of class
   */
  static Class<?> type_to_class(Type t, ClassLoader loader) {

    if (loader == null) {
      loader = DCInstrument24.class.getClassLoader();
    }

    if (t == Type.BOOLEAN) {
      return Boolean.TYPE;
    } else if (t == Type.BYTE) {
      return Byte.TYPE;
    } else if (t == Type.CHAR) {
      return Character.TYPE;
    } else if (t == Type.DOUBLE) {
      return Double.TYPE;
    } else if (t == Type.FLOAT) {
      return Float.TYPE;
    } else if (t == Type.INT) {
      return Integer.TYPE;
    } else if (t == Type.LONG) {
      return Long.TYPE;
    } else if (t == Type.SHORT) {
      return Short.TYPE;
    } else if (t instanceof ObjectType || t instanceof ArrayType) {
      //      @ClassGetName String sig = typeToClassGetName(t);
      try {
        //        return Class.forName(sig, false, loader);
        return null;
      } catch (Exception e) {
        //        throw new Error("can't get class " + sig, e);
      }
    } else {
      throw new Error("unexpected type " + t);
    }
    return null;
  }

  /**
   * Modify a doubled native method to call its original method. It pops all of the parameter tags
   * off of the tag stack. If there is a primitive return value it puts a new tag value on the stack
   * for it.
   *
   * <p>TODO: add a way to provide a synopsis for native methods that affect comparability.
   *
   * @param gen current class
   * @param mg the interface method. Must be native.
   */
  void fix_native(ClassGen gen, MethodGen mg) {

    InstructionList il = new InstructionList();
    Type[] argTypes = mg.getArgumentTypes();
    String[] argNames = mg.getArgumentNames();

    debug_native.log("Native call %s%n", mg);

    // Build local variables for each argument to the method
    if (!mg.isStatic()) {
      mg.addLocalVariable("this", new ObjectType(mg.getClassName()), null, null);
    }
    for (int ii = 0; ii < argTypes.length; ii++) {
      mg.addLocalVariable(argNames[ii], argTypes[ii], null, null);
    }

    // Discard the tags for any primitive arguments passed to system
    // methods
    int primitive_cnt = 0;
    for (Type argType : argTypes) {
      if (argType instanceof BasicType) {
        primitive_cnt++;
      }
    }
    if (primitive_cnt > 0) {
      // il.append(discard_tag_code(new NOP(), primitive_cnt));
    }

    // push a tag if there is a primitive return value
    Type returnType = mg.getReturnType();
    // UNDONE change to:
    // if (is_primitive(returnType)) {
    if ((returnType instanceof BasicType) && (returnType != Type.VOID)) {
      il.append(dcr_call("push_const", Type.VOID, Type.NO_ARGS));
    }

    // If the method is not static, push the instance on the stack
    if (!mg.isStatic()) {
      il.append(InstructionFactory.createLoad(new ObjectType(gen.getClassName()), 0));
    }

    // System.out.printf("%s: atc = %d, anc = %d%n", mg.getName(), argTypes.length,
    // argNames.length);

    // if call is sun.reflect.Reflection.getCallerClass (realFramesToSkip)
    if (mg.getName().equals("getCallerClass")
        && (argTypes.length == 1)
        && gen.getClassName().equals("sun.reflect.Reflection")) {

      // The call returns the class realFramesToSkip up on the stack. Since we
      // have added this call in between, we need to increment that number by 1.
      il.append(InstructionFactory.createLoad(Type.INT, 0));
      il.append(ifact.createConstant(1));
      il.append(new IADD());
      // System.out.printf("adding 1 in %s.%s%n", gen.getClassName(),
      //                   mg.getName());

    } else { // normal call

      // push each argument on the stack
      int param_index = 1;
      if (mg.isStatic()) {
        param_index = 0;
      }
      for (Type argType : argTypes) {
        il.append(InstructionFactory.createLoad(argType, param_index));
        param_index += argType.getSize();
      }
    }

    // Call the method
    il.append(
        ifact.createInvoke(
            gen.getClassName(),
            mg.getName(),
            mg.getReturnType(),
            argTypes,
            (mg.isStatic() ? Const.INVOKESTATIC : Const.INVOKEVIRTUAL)));

    // If there is a return value, return it
    il.append(InstructionFactory.createReturn(mg.getReturnType()));

    // We've created new il; we need to set the instruction handle positions.
    il.setPositions();

    // Add the instructions to the method
    mg.setInstructionList(il);
    mg.setMaxStack();
    mg.setMaxLocals();

    // turn off the native flag
    mg.setAccessFlags(mg.getAccessFlags() & ~Const.ACC_NATIVE);
  }

  /**
   * Convenience function to build an instruction list.
   *
   * @param instructions a variable number of instructions
   * @return an instruction list
   */
  private List<CodeElement> build_il(CodeElement... instructions) {
    List<CodeElement> il = new ArrayList<>();

    for (CodeElement inst : instructions) {
      il.add(inst);
    }
    return il;
  }

  /**
   * Returns whether or not tag fields are used within the specified method of the specified class.
   * We can safely use class fields except in Object, String, and Class. If checking a class, mgen
   * is null.
   *
   * @param mgen method to check
   * @param classname class to check
   * @return true if tag fields may be used in class for method
   */
  boolean tag_fields_ok(@Nullable MethodGen24 mgen, @BinaryName String classname) {

    // Prior to Java 8 an interface could not contain any implementations.
    if (classGen.isInterface()) {
      if (classModel.majorVersion() < ClassFile.JAVA_8_VERSION) {
        return false;
      }
    }

    if (mgen != null) {
      if (mgen.getName().equals("<init>")) {
        if (!this.constructor_is_initialized) {
          return false;
        }
      }
    }

    if (!Premain.jdk_instrumented) {
      if (BcelUtil.inJdk(classname)) {
        return false;
      }
    }

    if (!classname.startsWith("java.lang")) {
      return true;
    }

    if (classname.equals("java.lang.String")
        || classname.equals("java.lang.Class")
        || classname.equals("java.lang.Object")
        || classname.equals("java.lang.ClassLoader")) {
      return false;
    }

    return true;
  }

  /**
   * Returns a string describing the top max_items items on the stack.
   *
   * @param stack OperandStack
   * @param max_items number of items to describe
   * @return string describing the top max_items on the operand stack
   */
  static String stack_contents(OperandStack stack, int max_items) {
    String contents = "";
    if (max_items >= stack.size()) {
      max_items = stack.size() - 1;
    }
    for (int ii = max_items; ii >= 0; ii--) {
      if (contents.length() != 0) {
        contents += ", ";
      }
      contents += stack.peek(ii);
    }
    return contents;
  }

  /**
   * Creates tag get and set accessor methods for each field in gen. An accessor is created for each
   * field (including final, static, and private fields). The accessors share the modifiers of their
   * field (except that all are final). Accessors are named {@code <field>_<class>__$get_tag} and
   * {@code <field>_<class>__$set_tag}. The class name must be included because field names can
   * shadow one another.
   *
   * <p>If tag_fields_ok is true for the class, then tag fields are created and the accessor uses
   * the tag fields. If not, tag storage is created separately and accessed via the field number.
   *
   * <p>Accessors are also created for each visible superclass field that is not hidden by a field
   * in this class. These accessors just call the superclasses accessor.
   *
   * <p>Any accessors created are added to the class.
   *
   * @param classGen class to check for fields
   */
  void create_tag_accessors(ClassGen24 classGen) {

    // If this class doesn't support tag fields, don't create them
    if (!tag_fields_ok(null, classGen.getClassName())) return;

    Set<String> field_set = new HashSet<>();
    Map<FieldModel, Integer> field_map = build_field_map(classModel);

    // Build accessors for all fields declared in this class
    for (FieldModel fm : classModel.fields()) {

      String fieldName = fm.fieldName().stringValue();
      assert !field_set.contains(fieldName) : fieldName + "-" + classGen.getClassName();
      field_set.add(fieldName);

      // skip primitive fields
      // MLR: skip non primitive fields?
      if (!fm.fieldTypeSymbol().isPrimitive()) {
        continue;
      }

      if (fm.flags().has(AccessFlag.STATIC)) {
        String full_name = full_name(classModel, fm);
        create_get_tag(classGen, fm, static_field_id.get(full_name));
        create_set_tag(classGen, fm, static_field_id.get(full_name));
      } else {
        create_get_tag(classGen, fm, field_map.get(fm));
        create_set_tag(classGen, fm, field_map.get(fm));
      }
    }

    // Build accessors for each field declared in a superclass that is
    // is not shadowed in a subclass
    //    JavaClass[] super_classes;
    //    try {
    //      // super_classes = gen.getJavaClass().getSuperClasses();
    //    } catch (Exception e) {
    //      throw new Error(e);
    //    }
    //    for (JavaClass super_class : super_classes) {
    //      for (Field f : super_class.getFields()) {
    //        if (f.isPrivate()) {
    //          continue;
    //        }
    //        if (field_set.contains(f.getName())) {
    //          continue;
    //        }
    //        if (!is_primitive(f.getType())) {
    //          continue;
    //        }
    //
    //        field_set.add(f.getName());
    //        MethodGen get_method;
    //        MethodGen set_method;
    //        if (f.isStatic()) {
    //          String full_name = full_name(super_class, fm);
    //          get_method = create_get_tag(classGen, fm, static_field_id.get(full_name));
    //          set_method = create_set_tag(classGen, fm, static_field_id.get(full_name));
    //        } else {
    //          get_method = create_get_tag(classGen, fm, field_map.get(f));
    //          set_method = create_set_tag(classGen, fm, field_map.get(f));
    //        }
    //        gen.addMethod(get_method.getMethod());
    //        gen.addMethod(set_method.getMethod());
    //      }
    //    }
  }

  /**
   * Builds a Map that relates each field in jc and each of its superclasses to a unique offset. The
   * offset can be used to index into a tag array for this class. Instance fields are placed in the
   * returned map and static fields are placed in static map (shared between all classes).
   *
   * @param classModel class to check for fields
   * @return field offset map
   */
  Map<FieldModel, Integer> build_field_map(ClassModel classModel) {

    Optional<ClassEntry> ce = classModel.superclass();
    if (!ce.isPresent()) {
      // class is java.lang.Object, no primitive fields
      return new LinkedHashMap<>();
    }

    // Get the offsets for each field in the superclass.
    String superclassName = ce.get().asInternalName().replace('/', '.');
    ClassModel super_cm = getClassModel(superclassName);
    if (super_cm == null) {
      throw new Error("Can't get superclass for " + superclassName);
    }

    Map<FieldModel, Integer> field_map = build_field_map(super_cm);
    int offset = field_map.size();

    // Determine the offset for each primitive field in the class
    // Also make sure the static_tags list is large enough for all
    // of the tags.
    for (FieldModel fm : classModel.fields()) {
      if (!fm.fieldTypeSymbol().isPrimitive()) {
        continue;
      }
      if (fm.flags().has(AccessFlag.STATIC)) {
        if (!in_jdk) {
          int min_size = static_field_id.size() + DCRuntime.max_jdk_static;
          while (DCRuntime.static_tags.size() <= min_size) DCRuntime.static_tags.add(null);
          static_field_id.put(full_name(classModel, fm), min_size);
        } else { // building jdk
          String full_name = full_name(classModel, fm);
          if (static_field_id.containsKey(full_name)) {
            // System.out.printf("Reusing static field %s value %d%n",
            //                    full_name, static_field_id.get(full_name));
          } else {
            // System.out.printf("Allocating new static field %s%n",
            //                    full_name);
            static_field_id.put(full_name, static_field_id.size() + 1);
          }
        }
      } else {
        field_map.put(fm, offset);
        offset++;
      }
    }

    return field_map;
  }

  /**
   * Creates a get tag method for field fm. The tag corresponding to field fm will be pushed on the
   * tag stack.
   *
   * <pre>{@code
   * void <field>_<class>__$get_tag() {
   *   #if fm.isStatic()
   *     DCRuntime.push_static_tag (tag_offset)
   *   #else
   *     DCRuntime.push_field_tag (this, tag_offset);
   * }
   * }</pre>
   *
   * @param classGen class whose accessors are being built. Not necessarily the class declaring fm
   *     (if fm is inherited).
   * @param fm field to build an accessor for
   * @param tag_offset offset of fm in the tag storage for this field
   */
  @SuppressWarnings("EnumOrdinal")
  void create_get_tag(ClassGen24 classGen, FieldModel fm, int tag_offset) {

    // Determine the method to call in DCRuntime.  Instance fields and static
    // fields are handled separately.  Also instance fields in special
    // classes that are created by the JVM are handled separately since only
    // in those classes can fields be read without being written (in java)
    String methodname = "push_field_tag";
    String classname = classGen.getClassName();
    ClassDesc[] args = {CD_Object, CD_int};
    if (fm.flags().has(AccessFlag.STATIC)) {
      methodname = "push_static_tag";
      args = new ClassDesc[] {CD_int};
    } else if (is_uninit_class(classname)) {
      methodname = "push_field_tag_null_ok";
    }

    String accessor_name =
        Premain.tag_method_name(Premain.GET_TAG, classname, fm.fieldName().stringValue());

    List<CodeElement> newCode = new ArrayList<>();

    if (!fm.flags().has(AccessFlag.STATIC)) {
      newCode.add(LoadInstruction.of(TypeKind.REFERENCE, 0)); // aload_0 (load this)
    }
    newCode.add(loadIntegerConstant(tag_offset));
    newCode.add(dcr_call(methodname, CD_void, args));
    newCode.add(ReturnInstruction.of(TypeKind.VOID));

    int access_flags = fm.flags().flagsMask();
    if (classGen.isInterface()) {
      // method in interface cannot be final
      access_flags &= ~AccessFlag.FINAL.ordinal();
      if (classModel.majorVersion() < ClassFile.JAVA_8_VERSION) {
        // If class file version is prior to 8 then a method in an interface
        // cannot be static (it's implicit) and must be abstract.
        access_flags &= ~AccessFlag.STATIC.ordinal();
        access_flags |= AccessFlag.ABSTRACT.ordinal();
      }
    } else {
      access_flags |= AccessFlag.FINAL.ordinal();
    }

    access_flags |= ClassFile.ACC_PUBLIC;
    access_flags &= ~ClassFile.ACC_PROTECTED;
    access_flags &= ~ClassFile.ACC_PRIVATE;

    // Create the get accessor method
    classGen
        .getClassBuilder()
        .withMethod(
            accessor_name,
            MethodTypeDesc.of(CD_void),
            access_flags,
            methodBuilder -> methodBuilder.withCode(codeBuilder -> copyCode(codeBuilder, newCode)));
  }

  /**
   * Creates a set tag method for field fm. The tag on the top of the tag stack will be popped off
   * and placed in the tag storeage corresponding to field
   *
   * <pre>{@code
   * void <field>_<class>__$set_tag() {
   *   #if fm.isStatic()
   *     DCRuntime.pop_static_tag (tag_offset)
   *   #else
   *     DCRuntime.pop_field_tag (this, tag_offset);
   * }
   * }</pre>
   *
   * @param classGen class whose accessors are being built. Not necessarily the class declaring fm
   *     (if fm is inherited).
   * @param fm field to build an accessor for
   * @param tag_offset offset of fm in the tag storage for this field
   */
  @SuppressWarnings("EnumOrdinal")
  void create_set_tag(ClassGen24 classGen, FieldModel fm, int tag_offset) {

    String methodname = "pop_field_tag";
    String classname = classGen.getClassName();
    ClassDesc[] args = {CD_Object, CD_int};
    if (fm.flags().has(AccessFlag.STATIC)) {
      methodname = "pop_static_tag";
      args = new ClassDesc[] {CD_int};
    }

    String setter_name =
        Premain.tag_method_name(Premain.SET_TAG, classname, fm.fieldName().stringValue());

    List<CodeElement> newCode = new ArrayList<>();

    if (!fm.flags().has(AccessFlag.STATIC)) {
      newCode.add(LoadInstruction.of(TypeKind.REFERENCE, 0)); // aload_0 (load this)
    }
    newCode.add(loadIntegerConstant(tag_offset));
    newCode.add(dcr_call(methodname, CD_void, args));
    newCode.add(ReturnInstruction.of(TypeKind.VOID));

    int access_flags = fm.flags().flagsMask();
    if (classGen.isInterface()) {
      // method in interface cannot be final
      access_flags &= ~AccessFlag.FINAL.ordinal();
      if (classModel.majorVersion() < ClassFile.JAVA_8_VERSION) {
        // If class file version is prior to 8 then a method in an interface
        // cannot be static (it's implicit) and must be abstract.
        access_flags &= ~AccessFlag.STATIC.ordinal();
        access_flags |= AccessFlag.ABSTRACT.ordinal();
      }
    } else {
      access_flags |= AccessFlag.FINAL.ordinal();
    }

    // make field public
    access_flags |= ClassFile.ACC_PUBLIC;
    access_flags &= ~ClassFile.ACC_PROTECTED;
    access_flags &= ~ClassFile.ACC_PRIVATE;

    // Create the setter method
    classGen
        .getClassBuilder()
        .withMethod(
            setter_name,
            MethodTypeDesc.of(CD_void),
            access_flags,
            methodBuilder -> methodBuilder.withCode(codeBuilder -> copyCode(codeBuilder, newCode)));
  }

  /**
   * Adds the DCompInstrumented interface to the given class. Adds the following method to the
   * class, so that it implements the DCompInstrumented interface:
   *
   * <pre>{@code
   * public boolean equals_dcomp_instrumented(Object o) {
   *   return this.equals(o, null);
   * }
   * }</pre>
   *
   * The method does nothing except call the instrumented equals method (boolean equals(Object,
   * DCompMarker)).
   *
   * @param classBuilder for the class
   * @param classGen class to add method to
   */
  void add_dcomp_interface(ClassBuilder classBuilder, ClassGen24 classGen, ClassInfo classInfo) {
    classGen.addInterface(DCRuntime.instrumentation_interface);
    debugInstrument.log("Added interface DCompInstrumented%n");

    List<CodeElement> instructions = new ArrayList<>();

    int access_flags = ClassFile.ACC_PUBLIC;
    if (classGen.isInterface()) {
      access_flags |= ClassFile.ACC_ABSTRACT;
    }

    MethodTypeDesc mtd = MethodTypeDesc.of(CD_boolean, CD_Object);
    instructions.add(LoadInstruction.of(TypeKind.REFERENCE, 0)); // load this
    instructions.add(LoadInstruction.of(TypeKind.REFERENCE, 1)); // load obj
    instructions.add(ConstantInstruction.ofIntrinsic(Opcode.ACONST_NULL)); // use null for marker

    MethodRefEntry mre =
        poolBuilder.methodRefEntry(
            ClassDesc.of(classGen.getClassName()),
            "equals",
            MethodTypeDesc.of(CD_boolean, CD_Object, dcomp_marker));
    instructions.add(InvokeInstruction.of(Opcode.INVOKEVIRTUAL, mre));
    instructions.add(ReturnInstruction.of(TypeKind.BOOLEAN));

    // build the equals_dcomp_instrumented method
    classBuilder.withMethod(
        "equals_dcomp_instrumented",
        mtd,
        access_flags,
        methodBuilder ->
            methodBuilder.withCode(codeBuilder -> copyCode(codeBuilder, instructions)));

    // now build the instrumented version of the equals_dcomp_instrumented method
    @BinaryName String classname = classInfo.class_name;
    // create pseudo MethodGen24
    MethodGen24 mgen =
        new MethodGen24(
            classname,
            classBuilder,
            "equals_dcomp_instrumented",
            access_flags,
            mtd,
            instructions,
            3, // maxStack
            2); // maxLocals
    boolean track = should_track(classname, mgen.getName(), classname + mgen);
    classBuilder.withMethod(
        "equals_dcomp_instrumented",
        MethodTypeDesc.of(CD_boolean, CD_Object, dcomp_marker),
        access_flags,
        methodBuilder ->
            methodBuilder.withCode(
                codeBuilder -> instrumentCode(codeBuilder, null, mgen, classInfo, track)));
  }

  /**
   * Adds the following method to a class:
   *
   * <pre>{@code
   * public boolean equals (Object obj) {
   *   return super.equals(obj);
   * }
   * }</pre>
   *
   * Must only be called if the Object equals method has not been overridden; if the equals method
   * is already defined in the class, a ClassFormatError will result because of the duplicate
   * method.
   *
   * @param classBuilder for the class
   * @param classGen class to add method to
   */
  void add_equals_method(ClassBuilder classBuilder, ClassGen24 classGen, ClassInfo classInfo) {
    List<CodeElement> instructions = new ArrayList<>();

    int access_flags = ClassFile.ACC_PUBLIC;
    if (classGen.isInterface()) {
      access_flags |= ClassFile.ACC_ABSTRACT;
    }

    MethodTypeDesc mtd = MethodTypeDesc.of(CD_boolean, CD_Object);
    instructions.add(LoadInstruction.of(TypeKind.REFERENCE, 0)); // load this
    instructions.add(LoadInstruction.of(TypeKind.REFERENCE, 1)); // load obj
    MethodRefEntry mre =
        poolBuilder.methodRefEntry(ClassDesc.of(classGen.getSuperclassName()), "equals", mtd);
    instructions.add(InvokeInstruction.of(Opcode.INVOKESPECIAL, mre));
    instructions.add(ReturnInstruction.of(TypeKind.BOOLEAN));

    // build the equals method
    classBuilder.withMethod(
        "equals",
        mtd,
        access_flags,
        methodBuilder ->
            methodBuilder.withCode(codeBuilder -> copyCode(codeBuilder, instructions)));

    // now build the instrumented version of the equals method
    @BinaryName String classname = classInfo.class_name;
    // create pseudo MethodGen24
    MethodGen24 mgen =
        new MethodGen24(classname, classBuilder, "equals", access_flags, mtd, instructions, 2, 2);
    boolean track = should_track(classname, mgen.getName(), classname + mgen);
    classBuilder.withMethod(
        "equals",
        MethodTypeDesc.of(CD_boolean, CD_Object, dcomp_marker),
        access_flags,
        methodBuilder ->
            methodBuilder.withCode(
                codeBuilder -> instrumentCode(codeBuilder, null, mgen, classInfo, track)));
  }

  /**
   * Marks the class as implementing various object methods (currently clone and toString). Callers
   * will call the instrumented version of the method if it exists, otherwise they will call the
   * uninstrumented version.
   *
   * @param classGen class to check
   */
  void handle_object(ClassGen24 classGen) {

    @SuppressWarnings("signature:assignment")
    @MethodDescriptor String noArgsReturnObject = "()Ljava/lang/Object;";
    MethodModel cl = classGen.containsMethod("clone", noArgsReturnObject);
    if (cl != null) {
      classGen.addInterface(Signatures.addPackage(dcomp_prefix, "DCompClone"));
    }

    @SuppressWarnings("signature:assignment")
    @MethodDescriptor String noArgsReturnString = "()Ljava/lang/String;";
    MethodModel ts = classGen.containsMethod("toString", noArgsReturnString);
    if (ts != null) {
      classGen.addInterface(Signatures.addPackage(dcomp_prefix, "DCompToString"));
    }
  }

  /**
   * Add a dcomp marker argument to indicate this is the instrumented version of the method.
   *
   * @param mgen method to add dcomp marker to
   * @param minfo for the given method's code
   */
  void add_dcomp_arg(MethodGen24 mgen, MethodGen24.MInfo24 minfo) {

    // Don't modify main or the JVM won't be able to find it.
    if (mgen.isMain()) {
      return;
    }

    // Don't modify class init methods, they don't take arguments
    if (mgen.isClinit()) {
      return;
    }

    // Add the dcomp marker argument to indicate this is the
    // instrumented version of the method.
    BcelUtils24.addNewParameter(mgen, "marker", dcomp_marker, minfo);
  }

  /**
   * Returns whether or not the method is defined in Object.
   *
   * @param methodName method to check
   * @param argTypes array of argument types to method
   * @return true if method is member of Object
   */
  @Pure
  boolean is_object_method(String methodName, ClassDesc[] argTypes) {
    for (MethodDef md : obj_methods) {
      if (md.equals(methodName, argTypes)) {
        return true;
      }
    }
    return false;
  }

  /**
   * Returns whether or not the class is one of those that has values initialized by the JVM or
   * native methods.
   *
   * @param classname class to check
   * @return true if classname has members that are uninitialized
   */
  @Pure
  boolean is_uninit_class(String classname) {

    for (String u_name : uninit_classes) {
      if (u_name.equals(classname)) {
        return true;
      }
    }

    return false;
  }

  /**
   * Creates a pseudo 'main' method with a DcompMarker argument that does nothing but call the
   * original 'main' method without the DCompMarker argument.
   *
   * @param mgen describes the 'main' method
   * @param classBuilder for current class
   * @param classInfo for the given class
   */
  void createMainStub(MethodGen24 mgen, ClassBuilder classBuilder, ClassInfo classInfo) {
    List<CodeElement> instructions = new ArrayList<>();
    instructions.add(LoadInstruction.of(TypeKind.REFERENCE, 0)); // load arg0

    MethodRefEntry mre =
        poolBuilder.methodRefEntry(
            ClassDesc.of(classInfo.class_name),
            "main",
            MethodTypeDesc.of(CD_void, CD_String.arrayType(1)));
    instructions.add(InvokeInstruction.of(Opcode.INVOKESTATIC, mre)); // call real main
    instructions.add(ReturnInstruction.of(TypeKind.VOID));

    classBuilder.withMethod(
        "main",
        MethodTypeDesc.of(CD_void, CD_Object, dcomp_marker),
        mgen.getAccessFlagsMask(),
        methodBuilder ->
            methodBuilder.withCode(codeBuilder -> copyCode(codeBuilder, instructions)));
  }

  /**
   * Writes the static map from field names to their integer ids to the specified file. Can be read
   * with restore_static_field_id. Each line contains a key/value combination with a blank
   * separating them.
   *
   * @param file where to write the static field ids
   * @throws IOException if unable to find or open the file
   */
  static void save_static_field_id(File file) throws IOException {

    PrintStream ps = new PrintStream(file);
    for (Map.Entry<@KeyFor("static_field_id") String, Integer> entry : static_field_id.entrySet()) {
      ps.printf("%s  %d%n", entry.getKey(), entry.getValue());
    }
    ps.close();
  }

  /**
   * Restores the static map from the specified file.
   *
   * @param file where to read the static field ids
   * @throws IOException if unable to create an EntryReader
   * @see #save_static_field_id(File)
   */
  static void restore_static_field_id(File file) throws IOException {
    try (EntryReader er = new EntryReader(file, "UTF-8")) {
      for (String line : er) {
        String[] key_val = line.split("  *");
        assert !static_field_id.containsKey(key_val[0]) : key_val[0] + " " + key_val[1];
        static_field_id.put(key_val[0], Integer.valueOf(key_val[1]));
        // System.out.printf("Adding %s %s to static map%n", key_val[0],
        //                   key_val[1]);
      }
    }
  }

  /**
   * Return the fully qualified fieldname of the specified field.
   *
   * @param cm class containing the field
   * @param fm the field
   * @return string containing the fully qualified name
   */
  protected String full_name(ClassModel cm, FieldModel fm) {
    String temp = ClassGen24.getClassName(cm) + "." + fm.fieldName().stringValue();
    return temp;
  }

  /**
   * Return simplified name of a method. Both exceptions and annotations are removed.
   *
   * @param m the method
   * @return string containing the simplified method name
   */
  protected String simplify_method_name(Method m) {
    // Remove exceptions from the full method name
    String full_name = m.toString().replaceFirst("\\s*throws.*", "");
    // Remove annotations from full method name
    return full_name.replaceFirst("(?s) \\[.*", "");
  }

  // UNDONE consider making this a Java record

  /** Variables used for processing a switch instruction. */
  private static class ModifiedSwitchInfo {

    /** Possibly modified default switch target. */
    public Label modifiedTarget;

    /** Possibly modified switch case list. */
    public List<SwitchCase> modifiedCaseList;

    /**
     * Creates a ModifiedSwitchInfo.
     *
     * @param modifiedTarget possibly modified default swith target
     * @param modifiedCaseList possibly modified switch case list
     */
    public ModifiedSwitchInfo(Label modifiedTarget, List<SwitchCase> modifiedCaseList) {
      this.modifiedTarget = modifiedTarget;
      this.modifiedCaseList = modifiedCaseList;
    }
  }

  /**
   * Checks to see if the instruction targets the method's CodeModel startLabel (held in
   * oldStartLabel). If so, it replaces the target with the newStartLabel. Unfortunately, the
   * classfile API does not allow us to simply replace the label, we have to replace the entire
   * instruction. Note that oldStartLabel may be null, but that is okay as any comparison to it will
   * fail and we will do nothing.
   *
   * @param inst the instruction to check
   * @return the original instruction or its replacement
   */
  private CodeElement retargetStartLabel(CodeElement inst) {
    ModifiedSwitchInfo info;
    switch (inst) {
      case BranchInstruction bi -> {
        if (bi.target().equals(oldStartLabel)) {
          return BranchInstruction.of(bi.opcode(), newStartLabel);
        }
      }
      case ExceptionCatch ec -> {
        if (ec.tryStart().equals(oldStartLabel)) {
          return ExceptionCatch.of(ec.handler(), newStartLabel, ec.tryEnd(), ec.catchType());
        }
      }
      case LookupSwitchInstruction ls -> {
        info = retargetStartLabel(ls.defaultTarget(), ls.cases());
        if (info != null) {
          return LookupSwitchInstruction.of(info.modifiedTarget, info.modifiedCaseList);
        }
      }
      case TableSwitchInstruction ts -> {
        info = retargetStartLabel(ts.defaultTarget(), ts.cases());
        if (info != null) {
          return TableSwitchInstruction.of(
              ts.lowValue(), ts.highValue(), info.modifiedTarget, info.modifiedCaseList);
        }
      }
      default -> {}
    }
    return inst;
  }

  /**
   * Checks to see if a switch instruction's default target or any of the case targets refer to
   * oldStartLabel. If so, replace those targets with the newStartLabel, and return the result in a
   * ModifiedSwitchInfo. Otherwise, return null.
   *
   * @param defaultTarget the default target for the switch instruction
   * @param caseList the case list for the switch instruction
   * @return a ModifiedSwitchInfo with the changed values, or null if no changes
   */
  private @Nullable ModifiedSwitchInfo retargetStartLabel(
      Label defaultTarget, List<SwitchCase> caseList) {
    Label modifiedTarget;
    boolean modified = false;

    if (defaultTarget.equals(oldStartLabel)) {
      modifiedTarget = newStartLabel;
      modified = true;
    } else {
      modifiedTarget = defaultTarget;
    }

    List<SwitchCase> newCaseList = new ArrayList<SwitchCase>();
    for (SwitchCase item : caseList) {
      if (item.target().equals(oldStartLabel)) {
        newCaseList.add(SwitchCase.of(item.caseValue(), newStartLabel));
        modified = true;
      } else {
        newCaseList.add(item);
      }
    }

    if (modified) {
      return new ModifiedSwitchInfo(modifiedTarget, newCaseList);
    } else {
      return null;
    }
  }

  /**
   * Format a field descriptor for output. The main difference between a descriptor and a signature
   * is that the latter may contain type arguments. This routine was orginaly written for
   * descriptors, but some support for type arguments has been added.
   *
   * <p>The output format is an extension of binary name format that includes primitives and arrays.
   * It is almost identical to a fully qualified name, but using “$” instead of “.” to separate
   * nested classes from their enclosing classes.
   *
   * @param descriptor the object to format
   * @return a @FqBinaryName formatted string
   */
  @SuppressWarnings("signature") // conversion method
  public static @FqBinaryName String convertDescriptorToFqBinaryName(
      @FieldDescriptor String descriptor) {
    StringBuilder result = new StringBuilder();

    int arrayDimensions = 0;
    while (descriptor.charAt(0) == '[') {
      arrayDimensions++;
      @SuppressWarnings("signature") // string manipulation
      @FieldDescriptor String descriptorFd = descriptor.substring(1);
      descriptor = descriptorFd;
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
        result.append(descriptorToFqBinaryName(descriptor));
        break;
      default:
        throw new IllegalArgumentException("Invalid descriptor: " + descriptor);
    }

    // Append array brackets if applicable
    for (int i = 0; i < arrayDimensions; i++) {
      result.append("[]");
    }

    return result.toString();
  }

  /**
   * Format a class name that may contain type arguments.
   *
   * @param descriptor the object to format
   * @return a @FqBinaryName formatted string
   */
  @SuppressWarnings("signature") // conversion method
  private static @FqBinaryName String descriptorToFqBinaryName(String descriptor) {
    StringBuilder result = new StringBuilder();
    int genericStart = descriptor.indexOf('<');
    int genericEnd = descriptor.lastIndexOf('>');
    int endOfBaseType = descriptor.indexOf(';');

    if (genericStart > 0 && genericEnd > genericStart) {
      // Base type with generics
      String baseType = descriptor.substring(1, genericStart).replace('/', '.');
      result.append(baseType).append('<');
      String genericPart = descriptor.substring(genericStart + 1, genericEnd);
      result.append(typeArgumentsToBinaryNames(genericPart));
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
   * Format one or more type parameters.
   *
   * @param genericPart the type parameter(s) to format
   * @return a string containing a list of types as binary names
   */
  @SuppressWarnings("signature") // string manipulation
  private static String typeArgumentsToBinaryNames(String genericPart) {
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
        current.append(c);
        params.add(convertDescriptorToFqBinaryName(current.toString()));
        current.setLength(0); // Clear the buffer
      } else {
        current.append(c);
      }
    }

    if (current.length() > 0) {
      params.add(convertDescriptorToFqBinaryName(current.toString()));
    }

    result.append(String.join(", ", params));
    return result.toString();
  }

  //  /**
  //   * Format a constant value for printing.
  //   *
  //   * @param item the constant to format
  //   * @return a string containing the constant's value
  //   */
  //  private final String formatConstantDesc(ConstantDesc item) {
  //    try {
  //      return item.resolveConstantDesc(MethodHandles.lookup()).toString();
  //    } catch (Exception e) {
  //      System.err.printf("Unexpected error %s getting constant value for: %s%n", e, item);
  //      return "";
  //    }
  //  }

  // UNDONE - this routine should probably be moved to MethodGen24.

  /**
   * Create a new local variable whose scope is the full method.
   *
   * @param mgen describes the given method
   * @param minfo for the given method's code
   * @param localName name of new local variable
   * @param localType type of new local variable
   * @return the new local variable
   */
  protected LocalVariable createLocalWithMethodScope(
      MethodGen24 mgen, MethodGen24.MInfo24 minfo, String localName, ClassDesc localType) {
    LocalVariable newVar =
        LocalVariable.of(
            minfo.nextLocalIndex, localName, localType, minfo.startLabel, minfo.endLabel);
    mgen.localsTable.add(newVar);
    minfo.nextLocalIndex += TypeKind.from(localType).slotSize();
    mgen.setMaxLocals(minfo.nextLocalIndex);
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
