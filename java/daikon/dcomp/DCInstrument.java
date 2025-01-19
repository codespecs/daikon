package daikon.dcomp;

import daikon.DynComp;
import daikon.chicory.ClassInfo;
import daikon.chicory.DaikonWriter;
import daikon.chicory.Instrument;
import daikon.chicory.MethodInfo;
import daikon.plumelib.bcelutil.BcelUtil;
import daikon.plumelib.bcelutil.InstructionListUtils;
import daikon.plumelib.bcelutil.SimpleLog;
import daikon.plumelib.bcelutil.StackTypes;
import daikon.plumelib.options.Option;
import daikon.plumelib.reflection.Signatures;
import daikon.plumelib.util.EntryReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintStream;
import java.net.URL;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Deque;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.regex.Pattern;
import org.apache.bcel.Const;
import org.apache.bcel.classfile.AnnotationEntry;
import org.apache.bcel.classfile.Annotations;
import org.apache.bcel.classfile.Attribute;
import org.apache.bcel.classfile.ClassParser;
import org.apache.bcel.classfile.Constant;
import org.apache.bcel.classfile.ConstantInterfaceMethodref;
import org.apache.bcel.classfile.Field;
import org.apache.bcel.classfile.JavaClass;
import org.apache.bcel.classfile.Method;
import org.apache.bcel.classfile.RuntimeVisibleAnnotations;
import org.apache.bcel.classfile.StackMapEntry;
import org.apache.bcel.classfile.StackMapType;
import org.apache.bcel.generic.AALOAD;
import org.apache.bcel.generic.ACONST_NULL;
import org.apache.bcel.generic.ALOAD;
import org.apache.bcel.generic.ASTORE;
import org.apache.bcel.generic.ATHROW;
import org.apache.bcel.generic.AnnotationEntryGen;
import org.apache.bcel.generic.ArrayType;
import org.apache.bcel.generic.BasicType;
import org.apache.bcel.generic.BranchInstruction;
import org.apache.bcel.generic.ClassGen;
import org.apache.bcel.generic.ClassGenException;
import org.apache.bcel.generic.CodeExceptionGen;
import org.apache.bcel.generic.DUP;
import org.apache.bcel.generic.DUP2;
import org.apache.bcel.generic.DUP_X2;
import org.apache.bcel.generic.FieldInstruction;
import org.apache.bcel.generic.GETFIELD;
import org.apache.bcel.generic.GETSTATIC;
import org.apache.bcel.generic.IADD;
import org.apache.bcel.generic.INVOKEDYNAMIC;
import org.apache.bcel.generic.INVOKEINTERFACE;
import org.apache.bcel.generic.INVOKESPECIAL;
import org.apache.bcel.generic.INVOKEVIRTUAL;
import org.apache.bcel.generic.Instruction;
import org.apache.bcel.generic.InstructionFactory;
import org.apache.bcel.generic.InstructionHandle;
import org.apache.bcel.generic.InstructionList;
import org.apache.bcel.generic.InstructionTargeter;
import org.apache.bcel.generic.InvokeInstruction;
import org.apache.bcel.generic.LDC;
import org.apache.bcel.generic.LDC2_W;
import org.apache.bcel.generic.LineNumberGen;
import org.apache.bcel.generic.LoadInstruction;
import org.apache.bcel.generic.LocalVariableGen;
import org.apache.bcel.generic.LocalVariableInstruction;
import org.apache.bcel.generic.MULTIANEWARRAY;
import org.apache.bcel.generic.MethodGen;
import org.apache.bcel.generic.NOP;
import org.apache.bcel.generic.ObjectType;
import org.apache.bcel.generic.PUTFIELD;
import org.apache.bcel.generic.PUTSTATIC;
import org.apache.bcel.generic.ReferenceType;
import org.apache.bcel.generic.ReturnInstruction;
import org.apache.bcel.generic.SWAP;
import org.apache.bcel.generic.StoreInstruction;
import org.apache.bcel.generic.Type;
import org.apache.bcel.verifier.structurals.OperandStack;
import org.checkerframework.checker.lock.qual.GuardSatisfied;
import org.checkerframework.checker.nullness.qual.EnsuresNonNullIf;
import org.checkerframework.checker.nullness.qual.KeyFor;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.checkerframework.checker.signature.qual.BinaryName;
import org.checkerframework.checker.signature.qual.ClassGetName;
import org.checkerframework.checker.signature.qual.DotSeparatedIdentifiers;
import org.checkerframework.dataflow.qual.Pure;

/** Instruments a class file to perform Dynamic Comparability. */
@SuppressWarnings({"nullness"}) //
public class DCInstrument extends InstructionListUtils {

  /**
   * Used when testing to continue processing if an error occurs. Currently, This flag is only used
   * by BuildJDK.
   */
  @Option("Halt if an instrumentation error occurs")
  public static boolean quit_if_error = true;

  /** Unmodified version of input class. */
  protected JavaClass orig_class;

  /** ClassGen for the current class. */
  protected ClassGen gen;

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
  protected LocalVariableGen tag_frame_local;

  // Argument descriptors
  /** Type array with two objects. */
  protected static Type[] two_objects = new Type[] {Type.OBJECT, Type.OBJECT};

  /** Type array with an object and an int. */
  protected static Type[] object_int = new Type[] {Type.OBJECT, Type.INT};

  /** Type array with a string. */
  protected static Type[] string_arg = new Type[] {Type.STRING};

  /** Type array with an int. */
  protected static Type[] integer_arg = new Type[] {Type.INT};

  /** Type array with an object. */
  protected static Type[] object_arg = new Type[] {Type.OBJECT};

  /** ObjectType for "java.lang.Class". */
  protected static Type javalangClass = new ObjectType("java.lang.Class");

  // Type descriptors
  protected static Type object_arr = new ArrayType(Type.OBJECT, 1);
  // private Type int_arr = new ArrayType (Type.INT, 1);
  protected static ObjectType throwable = new ObjectType("java.lang.Throwable");
  protected ObjectType dcomp_marker;
  protected static ObjectType javalangObject = new ObjectType("java.lang.Object");

  // Debug loggers
  /** Log file if debug_native is enabled. */
  protected static SimpleLog debug_native = new SimpleLog(false);

  /** Log file if debug_dup is enabled. */
  protected static SimpleLog debug_dup = new SimpleLog(false);

  // Flags to enable additional console output for debugging
  /** If true, enable JUnit analysis debugging. */
  protected static final boolean debugJUnitAnalysis = false;

  /** If true, enable {@link #getDefiningInterface} debugging. */
  protected static final boolean debugGetDefiningInterface = false;

  /** If true, enable {@link #handleInvoke} debugging. */
  protected static final boolean debugHandleInvoke = false;

  /** Keeps track of the methods that were not successfully instrumented. */
  protected List<String> skipped_methods = new ArrayList<>();

  /**
   * Specifies if we are to use an instrumented version of the JDK. Calls into the JDK must be
   * modified to remove the arguments from the tag stack if the JDK is not instrumented. This flag
   * is set/reset in daikon.dcomp.Premain.
   */
  protected static boolean jdk_instrumented = true;

  /** Either "java.lang.DCompInstrumented" or "daikon.dcomp.DCompInstrumented". */
  // Static because used in DCRuntime
  protected static @BinaryName String instrumentation_interface;

  /** Either "java.lang" or "daikon.dcomp". */
  protected @DotSeparatedIdentifiers String dcomp_prefix;

  /** Either "daikon.dcomp.DCRuntime" or "java.lang.DCRuntime". */
  protected @DotSeparatedIdentifiers String dcompRuntimeClassName = "daikon.dcomp.DCRuntime";

  /** Name prefix for tag setter methods. */
  protected static final String SET_TAG = "set_tag";

  /** Name prefix for tag getter methods. */
  protected static final String GET_TAG = "get_tag";

  /** Set of JUnit test classes. */
  protected static Set<String> junitTestClasses = new HashSet<>();

  /** Possible states of JUnit test discovery. */
  protected enum JUnitState {
    NOT_SEEN,
    STARTING,
    TEST_DISCOVERY,
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
  static Integer Integer_ACC_ANNOTATION = Integer.valueOf(Const.ACC_ANNOTATION);

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
        new MethodDef("finalize", new Type[0]),
        new MethodDef("getClass", new Type[0]),
        new MethodDef("hashCode", new Type[0]),
        new MethodDef("notify", new Type[0]),
        new MethodDef("notifyall", new Type[0]),
        new MethodDef("toString", new Type[0]),
        new MethodDef("wait", new Type[0]),
        new MethodDef("wait", new Type[] {Type.LONG}),
        new MethodDef("wait", new Type[] {Type.LONG, Type.INT}),
      };

  protected static InstructionList global_catch_il;
  protected static CodeExceptionGen global_exception_handler;
  private InstructionHandle insertion_placeholder;

  /** Class that defines a method (by its name and argument types) */
  static class MethodDef {
    String name;
    Type[] arg_types;

    MethodDef(String name, Type[] arg_types) {
      this.name = name;
      this.arg_types = arg_types;
    }

    @EnsuresNonNullIf(result = true, expression = "#1")
    boolean equals(@GuardSatisfied MethodDef this, String name, Type[] arg_types) {
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
      for (Type arg : arg_types) {
        code += arg.hashCode();
      }
      return code;
    }
  }

  /** Initialize with the original class and whether or not the class is part of the JDK. */
  @SuppressWarnings("StaticAssignmentInConstructor") // instrumentation_interface
  public DCInstrument(JavaClass orig_class, boolean in_jdk, @Nullable ClassLoader loader) {
    super();
    this.orig_class = orig_class;
    this.in_jdk = in_jdk;
    this.loader = loader;
    gen = new ClassGen(orig_class);
    pool = gen.getConstantPool();
    ifact = new InstructionFactory(gen);
    constructor_is_initialized = false;
    if (jdk_instrumented) {
      dcomp_prefix = "java.lang";
    } else {
      dcomp_prefix = "daikon.dcomp";
    }
    dcomp_marker = new ObjectType(Signatures.addPackage(dcomp_prefix, "DCompMarker"));
    if (BcelUtil.javaVersion == 8) {
      dcomp_prefix = "daikon.dcomp";
    }
    instrumentation_interface = Signatures.addPackage(dcomp_prefix, "DCompInstrumented");

    // System.out.printf("DCInstrument %s%n", orig_class.getClassName());
    // Turn on some of the logging based on debug option.
    debugInstrument.enabled = DynComp.debug || Premain.debug_dcinstrument;
    debug_native.enabled = DynComp.debug;
  }

  /**
   * Instruments the original class to perform dynamic comparabilty and returns the new class
   * definition.
   *
   * @return the modified JavaClass
   */
  public JavaClass instrument() {

    String classname = gen.getClassName();

    // Don't know where I got this idea.  They are executed.  Don't remember why
    // adding dcomp marker causes problems.
    // Don't instrument annotations.  They aren't executed and adding
    // the marker argument causes subtle errors
    if ((gen.getModifiers() & Const.ACC_ANNOTATION) != 0) {
      Instrument.debug_transform.log("Not instrumenting annotation %s%n", classname);
      // WHY NOT RETURN NULL?
      return gen.getJavaClass().copy();
    }

    // If a class has an EvoSuite annotation it may be instrumented by Evosuite;
    // thus, we should not instrument it before Evosuite does.
    for (final Attribute attribute : orig_class.getAttributes()) {
      if (attribute instanceof RuntimeVisibleAnnotations) {
        for (final AnnotationEntry item : ((Annotations) attribute).getAnnotationEntries()) {
          if (item.toString().startsWith("@Lorg/evosuite/runtime")) {
            Instrument.debug_transform.log(
                "Not instrumenting possible Evosuite target: %s%n", classname);
            // WHY NOT RETURN NULL?
            return gen.getJavaClass().copy();
          }
        }
      }
    }

    Instrument.debug_transform.log("Instrumenting class %s%n", classname);
    Instrument.debug_transform.indent();

    // Create the ClassInfo for this class and its list of methods
    ClassInfo class_info = new ClassInfo(classname, loader);
    boolean track_class = false;

    // Handle object methods for this class
    handle_object(gen);

    // Have all top-level classes implement our interface
    if (gen.getSuperclassName().equals("java.lang.Object")) {
      // Add equals method if it doesn't already exist. This ensures
      // that an instrumented version, equals(Object, DCompMarker),
      // will be created in this class.
      Method eq = gen.containsMethod("equals", "(Ljava/lang/Object;)Z");
      if (eq == null) {
        debugInstrument.log("Added equals method%n");
        add_equals_method(gen);
      }

      // Add DCompInstrumented interface and the required
      // equals_dcomp_instrumented method.
      add_dcomp_interface(gen);
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

    Instrument.debug_transform.log("junit_state: %s%n", junit_state);

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

    Instrument.debug_transform.log("junit_state: %s%n", junit_state);

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
      Instrument.debug_transform.log("JUnit test class: %s%n", classname);
    } else {
      Instrument.debug_transform.log("Not a JUnit test class: %s%n", classname);
    }

    // Process each method
    for (Method m : gen.getMethods()) {

      tag_frame_local = null;
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

        Instrument.debug_transform.log(
            "  Processing method %s, track=%b%n", simplify_method_name(m), track);
        Instrument.debug_transform.indent();

        MethodGen mg = new MethodGen(m, classname, pool);
        mgen = mg; // copy to global

        InstructionList il = mg.getInstructionList();
        boolean has_code = (il != null);
        if (has_code) {
          setCurrentStackMapTable(mg, gen.getMajor());
          buildUninitializedNewMap(il);
        }

        fixLocalVariableTable(mg);

        // If the method is native
        if (mg.isNative()) {

          // Create Java code that cleans up the tag stack and calls the real native method.
          fix_native(gen, mg);
          has_code = true;
          setCurrentStackMapTable(mg, gen.getMajor());

          // Add the DCompMarker argument to distinguish our version
          add_dcomp_arg(mg);

        } else { // normal method

          if (!junit_test_class) {
            // Add the DCompMarker argument to distinguish our version
            add_dcomp_arg(mg);
          }

          // Create a MethodInfo that describes this method's arguments
          // and exit line numbers (information not available via reflection)
          // and add it to the list for this class.
          MethodInfo mi = null;
          if (track && has_code) {
            mi = create_method_info(class_info, mg);
            class_info.method_infos.add(mi);
            DCRuntime.methods.add(mi);
          }

          // Instrument the method
          if (has_code) {
            // Create the local to store the tag frame for this method
            tag_frame_local = create_tag_frame_local(mg);
            build_exception_handler(mg);
            instrument_method(mg);
            if (track) {
              add_enter(mg, mi, DCRuntime.methods.size() - 1);
              add_exit(mg, mi, DCRuntime.methods.size() - 1);
            }
            install_exception_handler(mg);
          }
        }

        if (has_code) {
          updateUninitializedNewOffsets(mg.getInstructionList());
          createNewStackMapAttribute(mg);
          mg.setMaxLocals();
          mg.setMaxStack();
        } else {
          mg.removeCodeAttributes();
          mg.removeLocalVariables();
        }

        remove_local_variable_type_table(mg);

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
              gen.addMethod(create_dcomp_stub(mg).getMethod());
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
            setCurrentStackMapTable(mg, gen.getMajor());
            // Add the DCompMarker argument
            add_dcomp_arg(mg);
            remove_local_variable_type_table(mg);
            // try again
            if (replacingMethod) {
              gen.replaceMethod(m, mg.getMethod());
              if (BcelUtil.isMain(mg)) {
                gen.addMethod(create_dcomp_stub(mg).getMethod());
              }
            } else {
              gen.addMethod(mg.getMethod());
            }
          } else {
            throw e;
          }
        }
        Instrument.debug_transform.exdent();
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
    create_tag_accessors(gen);

    // Keep track of when the class is initialized (so we don't look
    // for fields in uninitialized classes)
    track_class_init();
    Instrument.debug_transform.exdent();

    // The code that builds the list of daikon variables for each ppt
    // needs to know what classes are instrumented.  Its looks in the
    // Chicory runtime for this information.
    if (track_class) {
      Instrument.debug_transform.log("DCInstrument adding %s to all class list%n", class_info);
      synchronized (daikon.chicory.SharedData.all_classes) {
        daikon.chicory.SharedData.all_classes.add(class_info);
      }
    }
    Instrument.debug_transform.log("Instrumentation complete: %s%n", classname);

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
      Instrument.debug_transform.log("Not instrumenting annotation %s%n", classname);
      // MUST NOT RETURN NULL
      return gen.getJavaClass().copy();
    }

    int i = classname.lastIndexOf('.');
    if (i > 0) {
      // Don't instrument problem packages.
      // See Premain.java for a list and explainations.
      String packageName = classname.substring(0, i);
      if (Premain.problem_packages.contains(packageName)) {
        Instrument.debug_transform.log("Skipping problem package %s%n", packageName);
        return gen.getJavaClass().copy();
      }
    }

    if (BcelUtil.javaVersion > 8) {
      // Don't instrument problem classes.
      // See Premain.java for a list and explainations.
      if (Premain.problem_classes.contains(classname)) {
        Instrument.debug_transform.log("Skipping problem class %s%n", classname);
        return gen.getJavaClass().copy();
      }
      dcompRuntimeClassName = "java.lang.DCRuntime";
    }

    Instrument.debug_transform.log("Instrumenting class(JDK) %s%n", classname);
    Instrument.debug_transform.indent();

    // Handle object methods for this class
    handle_object(gen);

    // Have all top-level classes implement our interface
    if (gen.getSuperclassName().equals("java.lang.Object")) {
      // Add equals method if it doesn't already exist. This ensures
      // that an instrumented version, equals(Object, DCompMarker),
      // will be created in this class.
      Method eq = gen.containsMethod("equals", "(Ljava/lang/Object;)Z");
      if (eq == null) {
        debugInstrument.log("Added equals method%n");
        add_equals_method(gen);
      }
      // Add DCompInstrumented interface and the required
      // equals_dcomp_instrumented method.
      add_dcomp_interface(gen);
    }

    // Process each method
    for (Method m : gen.getMethods()) {

      tag_frame_local = null;
      try {
        // Don't modify class initialization methods.  They can't affect
        // user comparability and there isn't any way to get a second
        // copy of them.
        if (BcelUtil.isClinit(m)) {
          continue;
        }

        Instrument.debug_transform.log("  Processing method %s%n", simplify_method_name(m));
        Instrument.debug_transform.indent();

        MethodGen mg = new MethodGen(m, classname, pool);
        mgen = mg; // copy to global

        InstructionList il = mg.getInstructionList();
        boolean has_code = (il != null);
        if (has_code) {
          setCurrentStackMapTable(mg, gen.getMajor());
          buildUninitializedNewMap(il);
        }

        fixLocalVariableTable(mg);

        // If the method is native
        if (mg.isNative()) {

          // Create Java code that cleans up the tag stack and calls the real native method.
          fix_native(gen, mg);
          has_code = true;
          setCurrentStackMapTable(mg, gen.getMajor());

          // Add the DCompMarker argument to distinguish our version
          add_dcomp_arg(mg);

        } else { // normal method

          // Add the DCompMarker argument to distinguish our version
          add_dcomp_arg(mg);

          // Instrument the method
          if (has_code) {
            // Create the local to store the tag frame for this method
            tag_frame_local = create_tag_frame_local(mg);
            build_exception_handler(mg);
            instrument_method(mg);
            install_exception_handler(mg);
          }
        }

        if (has_code) {
          updateUninitializedNewOffsets(mg.getInstructionList());
          createNewStackMapAttribute(mg);
          mg.setMaxLocals();
          mg.setMaxStack();
        } else {
          mg.removeCodeAttributes();
          mg.removeLocalVariables();
        }

        remove_local_variable_type_table(mg);

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
            setCurrentStackMapTable(mg, gen.getMajor());
            // Add the DCompMarker argument
            add_dcomp_arg(mg);
            remove_local_variable_type_table(mg);
            // try again
            gen.addMethod(mg.getMethod());
          } else {
            throw e;
          }
        }

        Instrument.debug_transform.exdent();
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
    create_tag_accessors(gen);

    // We don't need to track class initialization in the JDK because
    // that is only used when printing comparability which is only done
    // for client classes
    // track_class_init();

    Instrument.debug_transform.exdent();
    Instrument.debug_transform.log("Instrumentation complete: %s%n", classname);

    return gen.getJavaClass().copy();
  }

  /**
   * Instrument the specified method for dynamic comparability.
   *
   * @param mg MethodGen for the method to be instrumented
   */
  public void instrument_method(MethodGen mg) {

    // Because the tag_frame_local is active for the entire method
    // and its creation will change the state of the locals layout,
    // we need to insert the code to initialize it now so that the
    // stack anaylsis we are about to do is correct for potential
    // code replacements we might make later.
    InstructionHandle orig_start = mg.getInstructionList().getStart();
    add_create_tag_frame(mg);
    // Calculate the operand stack value(s) for revised code.
    mg.setMaxStack();
    // Calculate stack types information
    StackTypes stack_types = bcelCalcStackTypes(mg);
    if (stack_types == null) {
      skip_method(mg);
      return;
    }

    InstructionList il = mg.getInstructionList();
    OperandStack stack = null;

    // Prior to adding support for Stack Maps, the position field
    // of each InstructionHandle was not updated until the modified
    // method was written out.  Hence, it could be used as the
    // index into stack_types.  To support StackMaps we need to
    // update the position field as we modify the code bytes.  So
    // we need a mapping from InstructionHandle to orignal offset.
    // I beleive we always visit the InstructionHandle nodes of
    // the method's InstructionList in order - hence, we will use
    // a simple array for now.  If this turns out to not be the
    // case we will need to use a hash map.

    int[] handle_offsets = new int[il.getLength()];
    InstructionHandle ih = orig_start;
    int index = 0;
    // Loop through each instruction, building up offset map.
    while (ih != null) {
      handle_offsets[index++] = ih.getPosition();

      if (debugInstrument.enabled) {
        debugInstrument.log("inst: %s %n", ih);
        for (InstructionTargeter it : ih.getTargeters()) {
          debugInstrument.log("targeter: %s %n", it);
        }
      }

      ih = ih.getNext();
    }

    index = 0;
    // Loop through each instruction, making substitutions
    for (ih = orig_start; ih != null; ) {
      debugInstrument.log("instrumenting instruction %s%n", ih);
      InstructionList new_il = null;

      // Remember the next instruction to process
      InstructionHandle next_ih = ih.getNext();

      // Get the stack information
      stack = stack_types.get(handle_offsets[index++]);

      // Get the translation for this instruction (if any)
      new_il = xform_inst(mg, ih, stack);

      // If this instruction was modified, replace it with the new
      // instruction list. If this instruction was the target of any
      // jumps or line numbers, replace them with the first
      // instruction in the new list.
      replaceInstructions(mg, il, ih, new_il);

      // If the modified method is now too large, we quit instrumenting the method
      // and will rediscover the problem in the main instrumentation loop above
      // and deal with it there.
      if (ih.getPosition() >= Const.MAX_CODE_SIZE) {
        break;
      }

      ih = next_ih;
    }
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
   */
  public void build_exception_handler(MethodGen mg) {

    if (mg.getName().equals("main")) {
      global_catch_il = null;
      global_exception_handler = null;
      return;
    }

    InstructionList il = new InstructionList();
    il.append(new DUP());
    il.append(
        ifact.createInvoke(
            dcompRuntimeClassName, "exception_exit", Type.VOID, object_arg, Const.INVOKESTATIC));
    il.append(new ATHROW());

    add_exception_handler(mg, il);
  }

  /** Adds a try/catch block around the entire method. */
  public void add_exception_handler(MethodGen mg, InstructionList catch_il) {

    // <init> methods (constructors) turn out to be problematic
    // for adding a whole method exception handler.  The start of
    // the exception handler should be after the primary object is
    // initialized - but this is hard to determine without a full
    // analysis of the code.  Hence, we just skip these methods.
    if (!mg.isStatic()) {
      if (BcelUtil.isConstructor(mg)) {
        global_catch_il = null;
        global_exception_handler = null;
        return;
      }
    }

    InstructionList cur_il = mg.getInstructionList();
    InstructionHandle start = cur_il.getStart();
    InstructionHandle end = cur_il.getEnd();

    // This is just a temporary handler to get the start and end
    // address tracked as we make code modifications.
    global_catch_il = catch_il;
    global_exception_handler = new CodeExceptionGen(start, end, null, throwable);
  }

  /** Adds a try/catch block around the entire method. */
  public void install_exception_handler(MethodGen mg) {

    if (global_catch_il == null) {
      return;
    }

    InstructionList cur_il = mg.getInstructionList();
    InstructionHandle start = global_exception_handler.getStartPC();
    InstructionHandle end = global_exception_handler.getEndPC();
    InstructionHandle exc = cur_il.append(global_catch_il);
    cur_il.setPositions();
    mg.addExceptionHandler(start, end, exc, throwable);
    // discard temporary handler
    global_catch_il = null;
    global_exception_handler = null;

    if (!needStackMap) {
      return;
    }

    int exc_offset = exc.getPosition();

    debugInstrument.log(
        "New ExceptionHandler: %x %x %x %n", start.getPosition(), end.getPosition(), exc_offset);

    // This is a trick to get runningOffset set to
    // value of last stack map entry.
    updateStackMapOffset(exc_offset, 0);
    int map_offset = exc_offset - runningOffset - 1;

    // Get the argument types for this method
    Type[] arg_types = mg.getArgumentTypes();

    int arg_index = (mg.isStatic() ? 0 : 1);
    StackMapType[] arg_map_types = new StackMapType[arg_types.length + arg_index];
    if (!mg.isStatic()) {
      arg_map_types[0] =
          new StackMapType(
              Const.ITEM_Object, pool.addClass(mg.getClassName()), pool.getConstantPool());
    }
    for (int ii = 0; ii < arg_types.length; ii++) {
      arg_map_types[arg_index++] = generateStackMapTypeFromType(arg_types[ii]);
    }

    StackMapEntry map_entry;
    StackMapType stack_map_type =
        new StackMapType(
            Const.ITEM_Object, pool.addClass(throwable.getClassName()), pool.getConstantPool());
    StackMapType[] stack_map_types = {stack_map_type};
    map_entry =
        new StackMapEntry(
            Const.FULL_FRAME, map_offset, arg_map_types, stack_map_types, pool.getConstantPool());

    int orig_size = stackMapTable.length;
    StackMapEntry[] new_stack_map_table = new StackMapEntry[orig_size + 1];
    System.arraycopy(stackMapTable, 0, new_stack_map_table, 0, orig_size);
    new_stack_map_table[orig_size] = map_entry;
    stackMapTable = new_stack_map_table;
  }

  /**
   * Adds the code to create the tag frame to the beginning of the method. This needs to be before
   * the call to DCRuntime.enter (since it passed to that method).
   */
  public void add_create_tag_frame(MethodGen mg) {

    InstructionList nl = create_tag_frame(mg, tag_frame_local);

    // We add a temporary NOP at the end of the create_tag_frame
    // code that we will replace with runtime initization code
    // later.  We do this so that any existing stack map at
    // instruction offset 0 is not replaced by the one we are
    // about to add for initializing the tag_frame variable.
    insertion_placeholder = nl.append(new NOP());

    byte[] code = nl.getByteCode();
    // -1 because of the NOP we inserted.
    int len_code = code.length - 1;

    insertAtMethodStart(mg, nl);

    if (!needStackMap) {
      return;
    }

    // For Java 7 and beyond the StackMapTable is part of the
    // verification process.  We need to create and or update it to
    // account for instrumentation code we have inserted as well as
    // adjustments for the new 'tag_frame' local.

    // Get existing StackMapTable (if present)
    if (stackMapTable.length > 0) {
      // Each stack map frame specifies (explicity or implicitly) an
      // offset_delta that is used to calculate the actual bytecode
      // offset at which the frame applies.  This is caluclated by
      // by adding offset_delta + 1 to the bytecode offset of the
      // previous frame, unless the previous frame is the initial
      // frame of the method, in which case the bytecode offset is
      // offset_delta. (From the Java Virual Machine Specification,
      // Java SE 7 Edition, section 4.7.4)

      // Since we are inserting a new stack map frame at the
      // beginning of the stack map table, we need to adjust the
      // offset_delta of the original first stack map frame due to
      // the fact that it will no longer be the first entry.  We
      // must subtract (len_code + 1).
      // (We don't have to worry about the special case of the
      // original first entry having an offset of 0 because of the
      // NOP we inserted above.

      stackMapTable[0].updateByteCodeOffset(-(len_code + 1));
    }

    int new_table_length = stackMapTable.length + 1;
    StackMapEntry[] new_stack_map_table = new StackMapEntry[new_table_length];

    // Insert a new StackMapEntry at the beginning of the table
    // that adds the tag_frame variable.
    StackMapType tag_frame_type = generateStackMapTypeFromType(object_arr);
    StackMapType[] stack_map_type_arr = {tag_frame_type};
    new_stack_map_table[0] =
        new StackMapEntry(
            Const.APPEND_FRAME, len_code, stack_map_type_arr, null, pool.getConstantPool());

    // We can just copy the rest of the stack frames over as the FULL_FRAME
    // ones were already updated when the tag_frame variable was allocated.
    for (int i = 0; i < stackMapTable.length; i++) {
      new_stack_map_table[i + 1] = stackMapTable[i];
    }
    stackMapTable = new_stack_map_table;
    // print_stackMapTable ("add_create_tag_frame");
  }

  /**
   * Adds the call to DCRuntime.enter to the beginning of the method.
   *
   * @param mg method to modify
   * @param mi MethodInfo for method
   * @param method_info_index index for MethodInfo
   */
  public void add_enter(MethodGen mg, MethodInfo mi, int method_info_index) {
    InstructionList il = mg.getInstructionList();
    replaceInstructions(
        mg, il, insertion_placeholder, call_enter_exit(mg, method_info_index, "enter", -1));
  }

  /**
   * Creates the local used to store the tag frame and returns it.
   *
   * @param mg method to modify
   * @return LocalVariableGen for the tag_frame local
   */
  LocalVariableGen create_tag_frame_local(MethodGen mg) {
    return create_method_scope_local(mg, "dcomp_tag_frame$5a", object_arr);
  }

  /**
   * Creates code to create the tag frame for this method and store it in tag_frame_local.
   *
   * @param mg method to modify
   * @param tag_frame_local LocalVariableGen for the tag_frame local
   * @return InstructionList for tag_frame setup code
   */
  InstructionList create_tag_frame(MethodGen mg, LocalVariableGen tag_frame_local) {

    Type arg_types[] = mg.getArgumentTypes();

    // Determine the offset of the first argument in the frame
    int offset = 1;
    if (mg.isStatic()) {
      offset = 0;
    }

    // allocate an extra slot to save the tag frame depth for debugging
    int frame_size = mg.getMaxLocals() + 1;

    // unsigned byte max = 255.  minus the character '0' (decimal 48)
    // Largest frame size noted so far is 123.
    assert frame_size < 207 : frame_size + " " + mg.getClassName() + "." + mg.getName();
    String params = "" + (char) (frame_size + '0');
    // Character.forDigit (frame_size, Character.MAX_RADIX);
    List<Integer> plist = new ArrayList<>();
    for (Type argType : arg_types) {
      if (argType instanceof BasicType) {
        plist.add(offset);
      }
      offset += argType.getSize();
    }
    for (int ii = plist.size() - 1; ii >= 0; ii--) {
      char tmpChar = (char) (plist.get(ii) + '0');
      params += tmpChar;
      // Character.forDigit (plist.get(ii), Character.MAX_RADIX);
    }

    // Create code to create/init the tag frame and store in tag_frame_local
    InstructionList il = new InstructionList();
    il.append(ifact.createConstant(params));
    il.append(
        ifact.createInvoke(
            dcompRuntimeClassName, "create_tag_frame", object_arr, string_arg, Const.INVOKESTATIC));
    il.append(InstructionFactory.createStore(object_arr, tag_frame_local.getIndex()));
    debugInstrument.log("Store Tag frame local at index %d%n", tag_frame_local.getIndex());

    return il;
  }

  /**
   * Pushes the object, method info index, parameters, and return value on the stack and calls the
   * specified Method (normally enter or exit) in DCRuntime. The parameters are passed as an array
   * of objects.
   *
   * @param mg method to modify
   * @param method_info_index index for MethodInfo
   * @param method_name "enter" or "exit"
   * @param line source line number if type is exit
   * @return InstructionList for the enter or exit code
   */
  InstructionList call_enter_exit(
      MethodGen mg, int method_info_index, String method_name, int line) {

    InstructionList il = new InstructionList();
    Type[] arg_types = mg.getArgumentTypes();

    // Push the tag frame
    il.append(InstructionFactory.createLoad(object_arr, tag_frame_local.getIndex()));

    // Push the object.  Null if this is a static method or a constructor
    if (mg.isStatic() || (method_name.equals("enter") && BcelUtil.isConstructor(mg))) {
      il.append(new ACONST_NULL());
    } else { // must be an instance method
      il.append(InstructionFactory.createLoad(Type.OBJECT, 0));
    }

    // Determine the offset of the first parameter
    int param_offset = 1;
    if (mg.isStatic()) {
      param_offset = 0;
    }

    // Push the MethodInfo index
    il.append(ifact.createConstant(method_info_index));

    // Create an array of objects with elements for each parameter
    il.append(ifact.createConstant(arg_types.length));
    il.append(ifact.createNewArray(Type.OBJECT, (short) 1));

    // Put each argument into the array
    int param_index = param_offset;
    for (int ii = 0; ii < arg_types.length; ii++) {
      il.append(InstructionFactory.createDup(object_arr.getSize()));
      il.append(ifact.createConstant(ii));
      Type at = arg_types[ii];
      if (at instanceof BasicType) {
        il.append(new ACONST_NULL());
        // il.append (create_wrapper (c, at, param_index));
      } else { // must be reference of some sort
        il.append(InstructionFactory.createLoad(Type.OBJECT, param_index));
      }
      il.append(InstructionFactory.createArrayStore(Type.OBJECT));
      param_index += at.getSize();
    }

    // If this is an exit, push the return value and line number.
    // The return value
    // is stored in the local "return__$trace2_val"  If the return
    // value is a primitive, wrap it in the appropriate run-time wrapper
    if (method_name.equals("exit")) {
      Type returnType = mg.getReturnType();
      if (returnType == Type.VOID) {
        il.append(new ACONST_NULL());
      } else {
        LocalVariableGen return_local = get_return_local(mg, returnType);
        if (returnType instanceof BasicType) {
          il.append(new ACONST_NULL());
          // il.append (create_wrapper (c, returnType, return_local.getIndex()));
        } else {
          il.append(InstructionFactory.createLoad(Type.OBJECT, return_local.getIndex()));
        }
      }

      // push line number
      il.append(ifact.createConstant(line));
    }

    // Call the specified method
    Type[] method_args;
    if (method_name.equals("exit")) {
      method_args =
          new Type[] {object_arr, Type.OBJECT, Type.INT, object_arr, Type.OBJECT, Type.INT};
    } else {
      method_args = new Type[] {object_arr, Type.OBJECT, Type.INT, object_arr};
    }
    il.append(
        ifact.createInvoke(
            dcompRuntimeClassName, method_name, Type.VOID, method_args, Const.INVOKESTATIC));

    return il;
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

    Instruction inst = ih.getInstruction();

    switch (inst.getOpcode()) {

        // Replace the object comparison instructions with a call to
        // DCRuntime.object_eq or DCRuntime.object_ne.  Those methods
        // return a boolean which is used in a ifeq/ifne instruction
      case Const.IF_ACMPEQ:
        return object_comparison((BranchInstruction) inst, "object_eq", Const.IFNE);
      case Const.IF_ACMPNE:
        return object_comparison((BranchInstruction) inst, "object_ne", Const.IFNE);

        // These instructions compare the integer on the top of the stack
        // to zero.  Nothing is made comparable by this, so we need only
        // discard the tag on the top of the stack.
      case Const.IFEQ:
      case Const.IFNE:
      case Const.IFLT:
      case Const.IFGE:
      case Const.IFGT:
      case Const.IFLE:
        {
          return discard_tag_code(inst, 1);
        }

        // Instanceof pushes either 0 or 1 on the stack depending on whether
        // the object on top of stack is of the specified type.  We push a
        // tag for a constant, since nothing is made comparable by this.
      case Const.INSTANCEOF:
        return build_il(dcr_call("push_const", Type.VOID, Type.NO_ARGS), inst);

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

      case Const.IF_ICMPEQ:
      case Const.IF_ICMPGE:
      case Const.IF_ICMPGT:
      case Const.IF_ICMPLE:
      case Const.IF_ICMPLT:
      case Const.IF_ICMPNE:
        {
          return build_il(dcr_call("cmp_op", Type.VOID, Type.NO_ARGS), inst);
        }

      case Const.GETFIELD:
        {
          return load_store_field(mg, (GETFIELD) inst);
        }

      case Const.PUTFIELD:
        {
          return load_store_field(mg, (PUTFIELD) inst);
        }

      case Const.GETSTATIC:
        {
          return load_store_field(mg, ((GETSTATIC) inst));
        }

      case Const.PUTSTATIC:
        {
          return load_store_field(mg, ((PUTSTATIC) inst));
        }

      case Const.DLOAD:
      case Const.DLOAD_0:
      case Const.DLOAD_1:
      case Const.DLOAD_2:
      case Const.DLOAD_3:
      case Const.FLOAD:
      case Const.FLOAD_0:
      case Const.FLOAD_1:
      case Const.FLOAD_2:
      case Const.FLOAD_3:
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
        {
          return load_store_local((LoadInstruction) inst, tag_frame_local, "push_local_tag");
        }

      case Const.DSTORE:
      case Const.DSTORE_0:
      case Const.DSTORE_1:
      case Const.DSTORE_2:
      case Const.DSTORE_3:
      case Const.FSTORE:
      case Const.FSTORE_0:
      case Const.FSTORE_1:
      case Const.FSTORE_2:
      case Const.FSTORE_3:
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
        {
          return load_store_local((StoreInstruction) inst, tag_frame_local, "pop_local_tag");
        }

      case Const.LDC:
      case Const.LDC_W:
      case Const.LDC2_W:
        {
          return ldc_tag(inst, stack);
        }

        // Push the tag for the array onto the tag stack.  This causes
        // anything comparable to the length to be comparable to the array
        // as an index.
      case Const.ARRAYLENGTH:
        {
          return array_length(inst);
        }

      case Const.BIPUSH:
      case Const.SIPUSH:
      case Const.DCONST_0:
      case Const.DCONST_1:
      case Const.FCONST_0:
      case Const.FCONST_1:
      case Const.FCONST_2:
      case Const.ICONST_0:
      case Const.ICONST_1:
      case Const.ICONST_2:
      case Const.ICONST_3:
      case Const.ICONST_4:
      case Const.ICONST_5:
      case Const.ICONST_M1:
      case Const.LCONST_0:
      case Const.LCONST_1:
        {
          return build_il(dcr_call("push_const", Type.VOID, Type.NO_ARGS), inst);
        }

        // Primitive Binary operators.  Each is augmented with a call to
        // DCRuntime.binary_tag_op that merges the tags and updates the tag
        // Stack.
      case Const.DADD:
      case Const.DCMPG:
      case Const.DCMPL:
      case Const.DDIV:
      case Const.DMUL:
      case Const.DREM:
      case Const.DSUB:
      case Const.FADD:
      case Const.FCMPG:
      case Const.FCMPL:
      case Const.FDIV:
      case Const.FMUL:
      case Const.FREM:
      case Const.FSUB:
      case Const.IADD:
      case Const.IAND:
      case Const.IDIV:
      case Const.IMUL:
      case Const.IOR:
      case Const.IREM:
      case Const.ISHL:
      case Const.ISHR:
      case Const.ISUB:
      case Const.IUSHR:
      case Const.IXOR:
      case Const.LADD:
      case Const.LAND:
      case Const.LCMP:
      case Const.LDIV:
      case Const.LMUL:
      case Const.LOR:
      case Const.LREM:
      case Const.LSHL:
      case Const.LSHR:
      case Const.LSUB:
      case Const.LUSHR:
      case Const.LXOR:
        return build_il(dcr_call("binary_tag_op", Type.VOID, Type.NO_ARGS), inst);

        // Computed jump based on the int on the top of stack.  Since that int
        // is not made comparable to anything, we just discard its tag.  One
        // might argue that the key should be made comparable to each value in
        // the jump table.  But the tags for those values are not available.
        // And since they are all constants, its not clear how interesting it
        // would be anyway.
      case Const.LOOKUPSWITCH:
      case Const.TABLESWITCH:
        return discard_tag_code(inst, 1);

        // Make the integer argument to ANEWARRAY comparable to the new
        // array's index.
      case Const.ANEWARRAY:
      case Const.NEWARRAY:
        {
          return new_array(inst);
        }

        // If the new array has 2 dimensions, make the integer arguments
        // comparable to the corresponding indices of the new array.
        // For any other number of dimensions, discard the tags for the
        // arguments.
      case Const.MULTIANEWARRAY:
        {
          return multi_newarray_dc(inst);
        }

        // Mark the array and its index as comparable.  Also for primitives,
        // push the tag of the array element on the tag stack
      case Const.AALOAD:
      case Const.BALOAD:
      case Const.CALOAD:
      case Const.DALOAD:
      case Const.FALOAD:
      case Const.IALOAD:
      case Const.LALOAD:
      case Const.SALOAD:
        {
          return array_load(inst);
        }

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

        // Prefix the return with a call to the correct normal_exit method
        // to handle the tag stack
      case Const.ARETURN:
      case Const.DRETURN:
      case Const.FRETURN:
      case Const.IRETURN:
      case Const.LRETURN:
      case Const.RETURN:
        {
          return return_tag(mg, inst);
        }

        // Handle subroutine calls.  Calls to instrumented code are modified
        // to call the instrumented version (with the DCompMarker argument).
        // Calls to uninstrumented code (rare) discard primitive arguments
        // from the tag stack and produce an arbitrary return tag.
      case Const.INVOKESTATIC:
      case Const.INVOKEVIRTUAL:
      case Const.INVOKESPECIAL:
      case Const.INVOKEINTERFACE:
      case Const.INVOKEDYNAMIC:
        return handleInvoke((InvokeInstruction) inst);

        // Throws an exception.  This clears the operand stack of the current
        // frame.  We need to clear the tag stack as well.
      case Const.ATHROW:
        return build_il(dcr_call("throw_op", Type.VOID, Type.NO_ARGS), inst);

        // Opcodes that don't need any modifications.  Here for reference
      case Const.ACONST_NULL:
      case Const.ALOAD:
      case Const.ALOAD_0:
      case Const.ALOAD_1:
      case Const.ALOAD_2:
      case Const.ALOAD_3:
      case Const.ASTORE:
      case Const.ASTORE_0:
      case Const.ASTORE_1:
      case Const.ASTORE_2:
      case Const.ASTORE_3:
      case Const.CHECKCAST:
      case Const.D2F: // double to float
      case Const.D2I: // double to integer
      case Const.D2L: // double to long
      case Const.DNEG: // Negate double on top of stack
      case Const.F2D: // float to double
      case Const.F2I: // float to integer
      case Const.F2L: // float to long
      case Const.FNEG: // Negate float on top of stack
      case Const.GOTO:
      case Const.GOTO_W:
      case Const.I2B: // integer to byte
      case Const.I2C: // integer to char
      case Const.I2D: // integer to double
      case Const.I2F: // integer to float
      case Const.I2L: // integer to long
      case Const.I2S: // integer to short
      case Const.IFNONNULL:
      case Const.IFNULL:
      case Const.IINC: // increment local variable by a constant
      case Const.INEG: // negate integer on top of stack
      case Const.JSR: // pushes return address on the stack, but that
        // is thought of as an object, so we don't need
        // a tag for it.
      case Const.JSR_W:
      case Const.L2D: // long to double
      case Const.L2F: // long to float
      case Const.L2I: // long to int
      case Const.LNEG: // negate long on top of stack
      case Const.MONITORENTER:
      case Const.MONITOREXIT:
      case Const.NEW:
      case Const.NOP:
      case Const.RET: // this is the internal JSR return
        return null;

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
   * @param mg method to modify
   * @param mi MethodInfo for method
   * @param method_info_index index for MethodInfo
   */
  void add_exit(MethodGen mg, MethodInfo mi, int method_info_index) {

    // Iterator over all of the exit line numbers for this method, in order.
    // We will read one element from it each time that we encounter a
    // return instruction.
    Iterator<Integer> exit_iter = mi.exit_locations.iterator();

    // Loop through each instruction, looking for return instructions.
    InstructionList il = mg.getInstructionList();
    for (InstructionHandle ih = il.getStart(); ih != null; ) {

      // Remember the next instruction to process
      InstructionHandle next_ih = ih.getNext();

      // If this is a return instruction, Call DCRuntime.exit to calculate
      // comparability on Daikon variables
      Instruction inst = ih.getInstruction();
      if (inst instanceof ReturnInstruction) {
        Type type = mg.getReturnType();
        InstructionList new_il = new InstructionList();
        if (type != Type.VOID) {
          LocalVariableGen return_loc = get_return_local(mg, type);
          new_il.append(InstructionFactory.createDup(type.getSize()));
          new_il.append(InstructionFactory.createStore(type, return_loc.getIndex()));
        }
        new_il.append(call_enter_exit(mg, method_info_index, "exit", exit_iter.next()));
        new_il.append(inst);
        replaceInstructions(mg, il, ih, new_il);
      }

      ih = next_ih;
    }
  }

  /**
   * Return the interface class containing the implementation of the given method. The interfaces of
   * {@code startClass} are recursively searched.
   *
   * @param startClass the JavaClass whose interfaces are to be searched
   * @param methodName the target method to search for
   * @param argTypes the target method's argument types
   * @return the name of the interface class containing target method, or null if not found
   */
  private @Nullable @ClassGetName String getDefiningInterface(
      JavaClass startClass, String methodName, Type[] argTypes) {

    if (debugGetDefiningInterface) {
      System.out.println("searching interfaces of: " + startClass.getClassName());
    }
    for (@ClassGetName String interfaceName : startClass.getInterfaceNames()) {
      if (debugGetDefiningInterface) {
        System.out.println("interface: " + interfaceName);
      }
      JavaClass ji;
      try {
        ji = getJavaClass(interfaceName);
      } catch (Throwable e) {
        throw new Error(String.format("Unable to load class: %s", interfaceName), e);
      }
      for (Method jm : ji.getMethods()) {
        if (debugGetDefiningInterface) {
          System.out.println("  " + jm.getName() + Arrays.toString(jm.getArgumentTypes()));
        }
        if (jm.getName().equals(methodName) && Arrays.equals(jm.getArgumentTypes(), argTypes)) {
          // We have a match.
          return interfaceName;
        }
      }
      // no match found; does this interface extend other interfaces?
      @ClassGetName String foundAbove = getDefiningInterface(ji, methodName, argTypes);
      if (foundAbove != null) {
        // We have a match.
        return foundAbove;
      }
    }
    // nothing found
    return null;
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
   * @return instructions to replace the given instruction
   */
  private InstructionList handleInvoke(InvokeInstruction invoke) {

    // Get information about the call
    String methodName = invoke.getMethodName(pool);
    // getClassName does not work properly if invoke is INVOKEDYNAMIC.
    // We will deal with this later.
    @ClassGetName String classname = invoke.getClassName(pool);
    Type returnType = invoke.getReturnType(pool);
    Type[] argTypes = invoke.getArgumentTypes(pool);

    if (is_object_equals(methodName, returnType, argTypes)) {

      // Replace calls to Object's equals method with calls to our
      // replacement, a static method in DCRuntime.

      Type[] new_arg_types = new Type[] {javalangObject, javalangObject};

      InstructionList il = new InstructionList();
      il.append(
          ifact.createInvoke(
              dcompRuntimeClassName,
              (invoke.getOpcode() == Const.INVOKESPECIAL) ? "dcomp_super_equals" : "dcomp_equals",
              returnType,
              new_arg_types,
              Const.INVOKESTATIC));
      return il;
    }

    if (is_object_clone(methodName, returnType, argTypes)) {

      // Replace calls to Object's clone method with calls to our
      // replacement, a static method in DCRuntime.

      InstructionList il = instrument_clone_call(invoke);
      return il;
    }

    boolean callee_instrumented = isTargetInstrumented(invoke, classname, methodName, argTypes);

    if (debugHandleInvoke) {
      System.out.printf("handleInvoke(%s)%n", invoke);
      System.out.printf("  invoke host: %s%n", gen.getClassName() + "." + mgen.getName());
      System.out.printf("  invoke targ: %s%n", classname + "." + methodName);
      System.out.printf("  callee_instrumented: %s%n", callee_instrumented);
    }

    if (callee_instrumented) {

      InstructionList il = new InstructionList();
      // Add the DCompMarker argument so that it calls the instrumented version.
      il.append(new ACONST_NULL());
      Type[] new_arg_types = BcelUtil.postpendToArray(argTypes, dcomp_marker);
      Constant methodref = pool.getConstant(invoke.getIndex());
      il.append(
          ifact.createInvoke(
              classname,
              methodName,
              returnType,
              new_arg_types,
              invoke.getOpcode(),
              methodref instanceof ConstantInterfaceMethodref));
      return il;

    } else { // not instrumented, discard the tags before making the call

      InstructionList il = new InstructionList();
      // JUnit test classes are a bit strange.  They are marked as not being callee_instrumented
      // because they do not have the dcomp_marker added to the argument list, but
      // they actually contain instrumentation code.  So we do not want to discard
      // the primitive tags prior to the call.
      if (!junitTestClasses.contains(classname)) {
        il.append(discard_primitive_tags(argTypes));
      }

      // Add a tag for the return type if it is primitive.
      if ((returnType instanceof BasicType) && (returnType != Type.VOID)) {
        if (debugHandleInvoke) {
          System.out.printf("push tag for return  type of %s%n", invoke.getReturnType(pool));
        }
        il.append(dcr_call("push_const", Type.VOID, Type.NO_ARGS));
      }
      il.append(invoke);
      return il;
    }
  }

  /**
   * Returns instructions that will discard any primitive tags corresponding to the specified
   * arguments. Returns an empty instruction list if there are no primitive arguments to discard.
   *
   * @param argTypes argument types of target method
   * @return an instruction list that discards primitive tags from DCRuntime's per-thread
   *     comparability data stack
   */
  private InstructionList discard_primitive_tags(Type[] argTypes) {

    InstructionList il = new InstructionList();
    int primitive_cnt = 0;
    for (Type argType : argTypes) {
      if (argType instanceof BasicType) {
        primitive_cnt++;
      }
    }
    if (primitive_cnt > 0) {
      il.append(discard_tag_code(new NOP(), primitive_cnt));
    }
    return il;
  }

  /**
   * Returns true if the invoke target is instrumented.
   *
   * @param invoke instruction whose target is to be checked
   * @param classname target class of the invoke
   * @param methodName target method of the invoke
   * @param argTypes argument types of target method
   * @return true if the target is instrumented
   */
  private boolean isTargetInstrumented(
      InvokeInstruction invoke,
      @ClassGetName String classname,
      String methodName,
      Type[] argTypes) {
    boolean targetInstrumented;

    if (invoke instanceof INVOKEDYNAMIC) {
      // We don't instrument lambda methods.
      // BUG: BCEL doesn't know how to get classname from an INVOKEDYNAMIC instruction.
      if (debugHandleInvoke) {
        System.out.printf("invokedynamic NOT the classname: %s%n", classname);
      }
      targetInstrumented = false;
    } else if (is_object_method(methodName, invoke.getArgumentTypes(pool))) {
      targetInstrumented = false;
    } else {
      targetInstrumented = isClassnameInstrumented(classname, methodName);

      if (debugHandleInvoke) {
        System.out.printf("invoke host: %s%n", gen.getClassName() + "." + mgen.getName());
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
          && (invoke instanceof INVOKEINTERFACE || invoke instanceof INVOKEVIRTUAL)) {
        Integer access = getAccessFlags(classname);

        if ((access.intValue() & Const.ACC_ANNOTATION) != 0) {
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

      // If we are not using the instrumented JDK, then we need to track down the
      // actual target of an INVOKEVIRTUAL to see if it has been instrumented or not.
      if (targetInstrumented == true && invoke instanceof INVOKEVIRTUAL) {
        if (!jdk_instrumented && !mgen.getName().equals("equals_dcomp_instrumented")) {

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
            JavaClass targetClass;
            try {
              targetClass = getJavaClass(targetClassname);
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
                found = getDefiningInterface(targetClass, methodName, argTypes);
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

    if (invoke instanceof INVOKESPECIAL) {
      if (classname.equals(gen.getSuperclassName()) && methodName.equals("<init>")) {
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
      JavaClass c = getJavaClass(classname);
      if (c != null) {
        access = c.getAccessFlags();

        // Now check for FunctionalInterface
        for (final AnnotationEntry item : c.getAnnotationEntries()) {
          if (item.getAnnotationType().endsWith("FunctionalInterface;")) {
            access = Integer_ACC_ANNOTATION;
            if (debugHandleInvoke) {
              System.out.println(item.getAnnotationType());
            }
            break;
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

    if (daikon.dcomp.Instrument.is_transformer(classname.replace('.', '/'))) {
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
    if (jdk_instrumented && !classname.equals("java.lang.Object")) {
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
    JavaClass jc = getJavaClass(classname);
    if (jc != null) {
      return jc.getSuperclassName();
    } else {
      return null;
    }
  }

  /** Cache for {@link #getJavaClass} method. */
  private static Map<String, JavaClass> javaClasses = new ConcurrentHashMap<String, JavaClass>();

  /**
   * There are times when it is useful to inspect a class file other than the one we are currently
   * instrumenting. Note we cannot use classForName to do this as it might trigger a recursive call
   * to Instrument which would not work at this point.
   *
   * <p>Given a class name, we treat it as a system resource and try to open it as an input stream
   * that we can pass to BCEL to read and convert to a JavaClass object.
   *
   * @param classname the fully qualified name of the class in binary form, e.g., "java.util.List"
   * @return the JavaClass of the corresponding classname or null
   */
  private @Nullable JavaClass getJavaClass(String classname) {
    JavaClass cached = javaClasses.get(classname);
    if (cached != null) {
      return cached;
    }

    URL class_url = ClassLoader.getSystemResource(classname.replace('.', '/') + ".class");
    if (class_url != null) {
      try (InputStream inputStream = class_url.openStream()) {
        if (inputStream != null) {
          // Parse the bytes of the classfile, die on any errors
          ClassParser parser = new ClassParser(inputStream, classname + "<internal>");
          JavaClass result = parser.parse();
          javaClasses.put(classname, result);
          return result;
        }
      } catch (Throwable t) {
        throw new Error("Unexpected error reading " + class_url, t);
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
  boolean is_object_equals(String methodName, Type returnType, Type[] args) {
    return (methodName.equals("equals")
        && returnType == Type.BOOLEAN
        && args.length == 1
        && args[0].equals(javalangObject));
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
  boolean is_object_clone(String methodName, Type returnType, Type[] args) {
    return methodName.equals("clone") && returnType.equals(javalangObject) && (args.length == 0);
  }

  /**
   * Instrument calls to the Object method clone. An instrumented version is called if it exists,
   * the non-instrumented version if it does not.
   *
   * @param invoke invoke instruction to inspect and replace
   * @return InstructionList to call the correct version of clone or toString
   */
  InstructionList instrument_clone_call(InvokeInstruction invoke) {

    InstructionList il = new InstructionList();
    Type returnType = invoke.getReturnType(pool);
    String classname = invoke.getClassName(pool);
    ReferenceType ref_type = invoke.getReferenceType(pool);
    if (ref_type instanceof ArrayType) {
      // <array>.clone() is never instrumented, return original invoke.
      il.append(invoke);
      return il;
    }

    // push the target class
    il.append(new LDC(pool.addClass(classname)));

    // if this is a super call
    if (invoke.getOpcode() == Const.INVOKESPECIAL) {

      // Runtime will discover if the object's superclass has an instrumented clone method.
      // If so, call it; otherwise call the uninstrumented version.
      il.append(dcr_call("dcomp_super_clone", returnType, new Type[] {Type.OBJECT, javalangClass}));

    } else { // a regular (non-super) clone() call

      // Runtime will discover if the object has an instrumented clone method.
      // If so, call it; otherwise call the uninstrumented version.
      il.append(dcr_call("dcomp_clone", returnType, new Type[] {Type.OBJECT, javalangClass}));
    }

    return il;
  }

  /**
   * Create the instructions that replace the object eq or ne branch instruction. They are replaced
   * by a call to the specified compare_method (which returns a boolean) followed by the specified
   * boolean ifeq or ifne instruction.
   */
  InstructionList object_comparison(
      BranchInstruction branch, String compare_method, short boolean_if) {

    InstructionList il = new InstructionList();
    il.append(
        ifact.createInvoke(
            dcompRuntimeClassName, compare_method, Type.BOOLEAN, two_objects, Const.INVOKESTATIC));
    assert branch.getTarget() != null;
    il.append(InstructionFactory.createBranchInstruction(boolean_if, branch.getTarget()));
    return il;
  }

  /**
   * Handles load and store field instructions. The instructions must be augmented to either push
   * (load) or pop (store) the tag on the tag stack. This is accomplished by calling the tag get/set
   * method for this field.
   */
  InstructionList load_store_field(MethodGen mg, FieldInstruction f) {

    Type field_type = f.getFieldType(pool);
    if (field_type instanceof ReferenceType) {
      return null;
    }
    ObjectType obj_type = (ObjectType) f.getReferenceType(pool);
    InstructionList il = new InstructionList();
    String classname = obj_type.getClassName();

    // If this class doesn't support tag fields, don't load/store them
    if (!tag_fields_ok(mg, classname)) {
      if ((f instanceof GETFIELD) || (f instanceof GETSTATIC)) {
        il.append(dcr_call("push_const", Type.VOID, Type.NO_ARGS));
      } else {
        il.append(ifact.createConstant(1));
        il.append(dcr_call("discard_tag", Type.VOID, integer_arg));
      }

      // Perform the normal field command
      il.append(f);
      return il;
    }

    if (f instanceof GETSTATIC) {
      il.append(
          ifact.createInvoke(
              classname,
              tag_method_name(GET_TAG, classname, f.getFieldName(pool)),
              Type.VOID,
              Type.NO_ARGS,
              Const.INVOKESTATIC));
    } else if (f instanceof PUTSTATIC) {
      il.append(
          ifact.createInvoke(
              classname,
              tag_method_name(SET_TAG, classname, f.getFieldName(pool)),
              Type.VOID,
              Type.NO_ARGS,
              Const.INVOKESTATIC));
    } else if (f instanceof GETFIELD) {
      il.append(InstructionFactory.createDup(obj_type.getSize()));
      il.append(
          ifact.createInvoke(
              classname,
              tag_method_name(GET_TAG, classname, f.getFieldName(pool)),
              Type.VOID,
              Type.NO_ARGS,
              Const.INVOKEVIRTUAL));
    } else { // must be put field
      if (field_type.getSize() == 2) {
        LocalVariableGen lv = get_tmp2_local(mg, field_type);
        il.append(InstructionFactory.createStore(field_type, lv.getIndex()));
        il.append(InstructionFactory.createDup(obj_type.getSize()));
        il.append(
            ifact.createInvoke(
                classname,
                tag_method_name(SET_TAG, classname, f.getFieldName(pool)),
                Type.VOID,
                Type.NO_ARGS,
                Const.INVOKEVIRTUAL));
        il.append(InstructionFactory.createLoad(field_type, lv.getIndex()));
      } else {
        il.append(new SWAP());
        il.append(InstructionFactory.createDup(obj_type.getSize()));
        il.append(
            ifact.createInvoke(
                classname,
                tag_method_name(SET_TAG, classname, f.getFieldName(pool)),
                Type.VOID,
                Type.NO_ARGS,
                Const.INVOKEVIRTUAL));
        il.append(new SWAP());
      }
    }

    // Perform the normal field command
    il.append(f);

    return il;
  }

  /**
   * Handles load and store local instructions. The instructions must be augmented to either push
   * (load) or pop (store) the tag on the tag stack. This is accomplished by calling the specified
   * method in DCRuntime and passing that method the tag frame and the offset of local/parameter.
   */
  InstructionList load_store_local(
      LocalVariableInstruction lvi, LocalVariableGen tag_frame_local, String method) {

    // Don't need tags for objects
    assert !(lvi instanceof ALOAD) && !(lvi instanceof ASTORE) : "lvi " + lvi;

    InstructionList il = new InstructionList();

    // Push the tag frame and the index of this local
    il.append(InstructionFactory.createLoad(object_arr, tag_frame_local.getIndex()));
    debugInstrument.log("CreateLoad %s %d%n", object_arr, tag_frame_local.getIndex());
    il.append(ifact.createConstant(lvi.getIndex()));

    // Call the runtime method to handle loading/storing the local/parameter
    il.append(
        ifact.createInvoke(
            dcompRuntimeClassName,
            method,
            Type.VOID,
            new Type[] {object_arr, Type.INT},
            Const.INVOKESTATIC));
    il.append(lvi);
    return il;
  }

  /** Returns the number of the specified field in the primitive fields of obj_type. */
  int get_field_num(String name, ObjectType obj_type) {

    // If this is the current class, get the information directly
    if (obj_type.getClassName().equals(orig_class.getClassName())) {
      int fcnt = 0;
      for (Field f : orig_class.getFields()) {
        if (f.getName().equals(name)) {
          return fcnt;
        }
        if (f.getType() instanceof BasicType) {
          fcnt++;
        }
      }
      throw new Error("Can't find " + name + " in " + obj_type);
    }

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

    String name = "dcomp_$tmp_" + typ;
    // System.out.printf("local var name = %s%n", name);

    // See if the local has already been created
    for (LocalVariableGen lv : mg.getLocalVariables()) {
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
   */
  LocalVariableGen get_return_local(MethodGen mg, @Nullable Type return_type) {

    // Find the local used for the return value
    LocalVariableGen return_local = null;
    for (LocalVariableGen lv : mg.getLocalVariables()) {
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
          : " return_type = " + return_type + "current type = " + return_local.getType();
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
   * @param class_info class containing the method
   * @param mg method to inspect
   * @return MethodInfo for the method
   */
  @Nullable MethodInfo create_method_info(ClassInfo class_info, MethodGen mg) {

    // if (mg.getName().equals("<clinit>")) {
    //   // This case DOES occur at run time.  -MDE 1/22/2010
    // }

    // Get the argument names for this method
    String[] argNames = mg.getArgumentNames();
    LocalVariableGen[] lvs = mg.getLocalVariables();
    int param_offset = 1;
    if (mg.isStatic()) {
      param_offset = 0;
    }
    if (lvs != null) {
      for (int ii = 0; ii < argNames.length; ii++) {
        if ((ii + param_offset) < lvs.length) {
          argNames[ii] = lvs[ii + param_offset].getName();
        }
      }
    }

    // Get the argument types for this method
    Type[] argTypes = mg.getArgumentTypes();
    @ClassGetName String[] arg_type_strings = new @ClassGetName String[argTypes.length];
    for (int ii = 0; ii < argTypes.length; ii++) {
      arg_type_strings[ii] = typeToClassGetName(argTypes[ii]);
      // System.out.printf("DCI arg types: %s %s%n", argTypes[ii], arg_type_strings[ii]);
    }

    // Loop through each instruction and find the line number for each
    // return opcode
    List<Integer> exit_locs = new ArrayList<>();

    // Tells whether each exit loc in the method is included or not
    // (based on filters)
    List<Boolean> isIncluded = new ArrayList<>();

    // log ("Looking for exit points in %s%n", mg.getName());
    InstructionList il = mg.getInstructionList();
    int line_number = 0;
    int last_line_number = 0;
    boolean foundLine;

    if (il == null) {
      return null;
    }

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
        case Const.ARETURN:
        case Const.DRETURN:
        case Const.FRETURN:
        case Const.IRETURN:
        case Const.LRETURN:
        case Const.RETURN:
          // log ("Exit at line %d%n", line_number);
          // only do incremental lines if we don't have the line generator
          if (line_number == last_line_number && foundLine == false) {
            line_number++;
          }
          last_line_number = line_number;

          exit_locs.add(line_number);
          isIncluded.add(true);
          break;

        default:
          break;
      }
    }

    return new MethodInfo(
        class_info, mg.getName(), argNames, arg_type_strings, exit_locs, isIncluded);
  }

  /**
   * Adds a call to DCRuntime.set_class_initialized (String classname) to the class initializer for
   * this class. Creates a class initializer if one is not currently present.
   */
  void track_class_init() {

    // Look for the class init method.  If not found, create an empty one.
    Method cinit = null;
    for (Method m : gen.getMethods()) {
      if (m.getName().equals("<clinit>")) {
        cinit = m;
        break;
      }
    }
    if (cinit == null) {
      InstructionList il = new InstructionList();
      il.append(InstructionFactory.createReturn(Type.VOID));
      MethodGen cinit_gen =
          new MethodGen(
              Const.ACC_STATIC,
              Type.VOID,
              Type.NO_ARGS,
              new String[0],
              "<clinit>",
              gen.getClassName(),
              il,
              pool);
      cinit_gen.setMaxLocals();
      cinit_gen.setMaxStack();
      cinit_gen.update();
      cinit = cinit_gen.getMethod();
      gen.addMethod(cinit);
    }

    try {
      MethodGen cinit_gen = new MethodGen(cinit, gen.getClassName(), pool);
      setCurrentStackMapTable(cinit_gen, gen.getMajor());

      // Add a call to DCRuntime.set_class_initialized to the beginning of the method
      InstructionList il = new InstructionList();
      il.append(ifact.createConstant(gen.getClassName()));
      il.append(
          ifact.createInvoke(
              dcompRuntimeClassName,
              "set_class_initialized",
              Type.VOID,
              string_arg,
              Const.INVOKESTATIC));

      insertAtMethodStart(cinit_gen, il);
      createNewStackMapAttribute(cinit_gen);
      cinit_gen.setMaxLocals();
      cinit_gen.setMaxStack();
      gen.replaceMethod(cinit, cinit_gen.getMethod());
    } catch (Throwable t) {
      if (debugInstrument.enabled) {
        t.printStackTrace();
      }
      throw new Error(
          "Unexpected error processing " + gen.getClassName() + "." + cinit.getName(), t);
    }
  }

  /**
   * Creates code that makes the index comparable (for indexing purposes) with the array in array
   * load instructions. First the arrayref and its index are duplicated on the stack. Then the
   * appropriate array load method is called to mark them as comparable and update the tag stack.
   * Finally the original load instruction is performed.
   *
   * @param inst an array load instruction
   * @return instruction list that calls the runtime to handle the array load instruction
   */
  InstructionList array_load(Instruction inst) {

    InstructionList il = new InstructionList();

    // Duplicate the array ref and index and pass them to DCRuntime
    // which will make the index comparable with the array.  In the case
    // of primtives it will also get the tag for the primitive and push
    // it on the tag stack.
    il.append(new DUP2());
    String method = "primitive_array_load";
    if (inst instanceof AALOAD) {
      method = "ref_array_load";
    } else if (is_uninit_class(gen.getClassName())) {
      method = "primitive_array_load_null_ok";
    }

    il.append(dcr_call(method, Type.VOID, new Type[] {Type.OBJECT, Type.INT}));

    // Perform the original instruction
    il.append(inst);

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
  InstructionList array_store(Instruction inst, String method, Type base_type) {

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
  InstructionList array_length(Instruction inst) {

    InstructionList il = new InstructionList();

    // Duplicate the array ref and pass it to DCRuntime which will push
    // it onto the tag stack.
    il.append(new DUP());
    il.append(dcr_call("push_array_tag", Type.VOID, new Type[] {Type.OBJECT}));

    // Perform the original instruction
    il.append(inst);

    return il;
  }

  /**
   * Creates code to make the declared length of a new array comparable to its index.
   *
   * @param inst a anewarray or newarray instruction
   * @return instruction list that calls the runtime to handle the newarray instruction
   */
  InstructionList new_array(Instruction inst) {
    InstructionList il = new InstructionList();

    // Perform the original instruction
    il.append(inst);

    // Duplicate the array ref from the top of the stack and pass it
    // to DCRuntime which will push it onto the tag stack.
    il.append(new DUP());
    il.append(dcr_call("push_array_tag", Type.VOID, new Type[] {Type.OBJECT}));

    // Make the array and the count comparable. Also, pop the tags for
    // the array and the count off the tag stack.
    il.append(dcr_call("cmp_op", Type.VOID, Type.NO_ARGS));

    return il;
  }

  /**
   * Creates code to make the declared lengths of a new two-dimensional array comparable to the
   * corresponding indices.
   *
   * @param inst a multianewarray instruction
   * @return instruction list that calls the runtime to handle the multianewarray instruction
   */
  InstructionList multiarray2(Instruction inst) {
    InstructionList il = new InstructionList();

    // Duplicate both count arguments
    il.append(new DUP2());

    // Perform the original instruction
    il.append(inst);

    // Duplicate the new arrayref and put it below the count arguments
    // Stack is now: ..., arrayref, count1, count2, arrayref
    il.append(new DUP_X2());

    Type objArray = new ArrayType(Type.OBJECT, 1);
    il.append(dcr_call("multianewarray2", Type.VOID, new Type[] {Type.INT, Type.INT, objArray}));

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
  boolean should_track(@ClassGetName String className, String methodName, String pptName) {

    Instrument.debug_transform.log(
        "Considering tracking ppt: %s, %s, %s%n", className, methodName, pptName);

    // Don't track any JDK classes
    if (BcelUtil.inJdk(className)) {
      Instrument.debug_transform.log("ignoring %s, is a JDK class%n", className);
      return false;
    }

    // Don't track toString methods because we call them in
    // our debug statements.
    if (pptName.contains("toString")) {
      Instrument.debug_transform.log("ignoring %s, is a toString method%n", pptName);
      return false;
    }

    // call shouldIgnore to check ppt-omit-pattern(s) and ppt-select-pattern(s)
    return !daikon.chicory.Instrument.shouldIgnore(className, methodName, pptName);
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
  InvokeInstruction dcr_call(String methodName, Type returnType, Type[] argTypes) {

    return ifact.createInvoke(
        dcompRuntimeClassName, methodName, returnType, argTypes, Const.INVOKESTATIC);
  }

  /**
   * Create the code to call discard_tag(tag_count) and append inst to the end of that code.
   *
   * @param inst instruction to be replaced
   * @param tag_count number of tags to discard
   * @return InstructionList
   */
  InstructionList discard_tag_code(Instruction inst, int tag_count) {
    InstructionList il = new InstructionList();
    il.append(ifact.createConstant(tag_count));
    il.append(dcr_call("discard_tag", Type.VOID, integer_arg));
    append_inst(il, inst);
    return il;
  }

  /**
   * Duplicates the item on the top of stack. If the value on the top of the stack is a primitive,
   * we need to do the same on the tag stack. Otherwise, we need do nothing.
   */
  InstructionList dup_tag(Instruction inst, OperandStack stack) {
    Type top = stack.peek();
    if (debug_dup.enabled) {
      debug_dup.log("DUP -> %s [... %s]%n", "dup", stack_contents(stack, 2));
    }
    if (is_primitive(top)) {
      return build_il(dcr_call("dup", Type.VOID, Type.NO_ARGS), inst);
    }
    return null;
  }

  /**
   * Duplicates the item on the top of the stack and inserts it 2 values down in the stack. If the
   * value at the top of the stack is not a primitive, there is nothing to do here. If the second
   * value is not a primitive, then we need only to insert the duped value down 1 on the tag stack
   * (which contains only primitives).
   */
  InstructionList dup_x1_tag(Instruction inst, OperandStack stack) {
    Type top = stack.peek();
    if (debug_dup.enabled) {
      debug_dup.log("DUP -> %s [... %s]%n", "dup_x1", stack_contents(stack, 2));
    }
    if (!is_primitive(top)) {
      return null;
    }
    String method = "dup_x1";
    if (!is_primitive(stack.peek(1))) {
      method = "dup";
    }
    return build_il(dcr_call(method, Type.VOID, Type.NO_ARGS), inst);
  }

  /**
   * Duplicates either the top 2 category 1 values or a single category 2 value and inserts it 2 or
   * 3 values down on the stack.
   */
  InstructionList dup2_x1_tag(Instruction inst, OperandStack stack) {
    String op;
    Type top = stack.peek();
    if (is_category2(top)) {
      if (is_primitive(stack.peek(1))) {
        op = "dup_x1";
      } else { // not a primitive, so just dup
        op = "dup";
      }
    } else if (is_primitive(top)) {
      if (is_primitive(stack.peek(1)) && is_primitive(stack.peek(2))) op = "dup2_x1";
      else if (is_primitive(stack.peek(1))) op = "dup2";
      else if (is_primitive(stack.peek(2))) op = "dup_x1";
      else {
        // neither value 1 nor value 2 is primitive
        op = "dup";
      }
    } else { // top is not primitive
      if (is_primitive(stack.peek(1)) && is_primitive(stack.peek(2))) {
        op = "dup_x1";
      } else if (is_primitive(stack.peek(1))) {
        op = "dup";
      } else { // neither of the top two values is primitive
        op = null;
      }
    }
    if (debug_dup.enabled) {
      debug_dup.log("DUP2_X1 -> %s [... %s]%n", op, stack_contents(stack, 3));
    }

    if (op != null) {
      return build_il(dcr_call(op, Type.VOID, Type.NO_ARGS), inst);
    }
    return null;
  }

  /**
   * Duplicate either one category 2 value or two category 1 values. The instruction is implemented
   * as necessary on the tag stack.
   */
  InstructionList dup2_tag(Instruction inst, OperandStack stack) {
    Type top = stack.peek();
    String op;
    if (is_category2(top)) {
      op = "dup";
    } else if (is_primitive(top) && is_primitive(stack.peek(1))) op = "dup2";
    else if (is_primitive(top) || is_primitive(stack.peek(1))) op = "dup";
    else {
      // both of the top two items are not primitive, nothing to dup
      op = null;
    }
    if (debug_dup.enabled) {
      debug_dup.log("DUP2 -> %s [... %s]%n", op, stack_contents(stack, 2));
    }
    if (op != null) {
      return build_il(dcr_call(op, Type.VOID, Type.NO_ARGS), inst);
    }
    return null;
  }

  /**
   * Dup the category 1 value on the top of the stack and insert it either two or three values down
   * on the stack.
   */
  InstructionList dup_x2(Instruction inst, OperandStack stack) {
    Type top = stack.peek();
    String op = null;
    if (is_primitive(top)) {
      if (is_category2(stack.peek(1))) op = "dup_x1";
      else if (is_primitive(stack.peek(1)) && is_primitive(stack.peek(2))) op = "dup_x2";
      else if (is_primitive(stack.peek(1)) || is_primitive(stack.peek(2))) op = "dup_x1";
      else {
        op = "dup";
      }
    }
    if (debug_dup.enabled) {
      debug_dup.log("DUP_X2 -> %s [... %s]%n", op, stack_contents(stack, 3));
    }
    if (op != null) {
      return build_il(dcr_call(op, Type.VOID, Type.NO_ARGS), inst);
    }
    return null;
  }

  /**
   * Duplicate the top one or two operand stack values and insert two, three, or four values down.
   */
  InstructionList dup2_x2(Instruction inst, OperandStack stack) {
    Type top = stack.peek();
    String op;
    if (is_category2(top)) {
      if (is_category2(stack.peek(1))) op = "dup_x1";
      else if (is_primitive(stack.peek(1)) && is_primitive(stack.peek(2))) op = "dup_x2";
      else if (is_primitive(stack.peek(1)) || is_primitive(stack.peek(2))) op = "dup_x1";
      else {
        // both values are references
        op = "dup";
      }
    } else if (is_primitive(top)) {
      if (is_category2(stack.peek(1))) {
        throw new Error("not supposed to happen " + stack_contents(stack, 3));
      } else if (is_category2(stack.peek(2))) {
        if (is_primitive(stack.peek(1))) {
          op = "dup2_x1";
        } else {
          op = "dup_x1";
        }
      } else if (is_primitive(stack.peek(1))) {
        if (is_primitive(stack.peek(2)) && is_primitive(stack.peek(3))) op = "dup2_x2";
        else if (is_primitive(stack.peek(2)) || is_primitive(stack.peek(3))) op = "dup2_x1";
        else {
          // both 2 and 3 are references
          op = "dup2";
        }
      } else { // 1 is a reference
        if (is_primitive(stack.peek(2)) && is_primitive(stack.peek(3))) op = "dup_x2";
        else if (is_primitive(stack.peek(2)) || is_primitive(stack.peek(3))) op = "dup_x1";
        else {
          // both 2 and 3 are references
          op = "dup";
        }
      }
    } else { // top is a reference
      if (is_category2(stack.peek(1))) {
        throw new Error("not supposed to happen " + stack_contents(stack, 3));
      } else if (is_category2(stack.peek(2))) {
        if (is_primitive(stack.peek(1))) {
          op = "dup_x1";
        } else {
          op = null; // nothing to dup
        }
      } else if (is_primitive(stack.peek(1))) {
        if (is_primitive(stack.peek(2)) && is_primitive(stack.peek(3))) op = "dup_x2";
        else if (is_primitive(stack.peek(2)) || is_primitive(stack.peek(3))) op = "dup_x1";
        else {
          // both 2 and 3 are references
          op = "dup";
        }
      } else { // 1 is a reference
        op = null; // nothing to dup
      }
    }
    if (debug_dup.enabled) {
      debug_dup.log("DUP_X2 -> %s [... %s]%n", op, stack_contents(stack, 3));
    }
    if (op != null) {
      return build_il(dcr_call(op, Type.VOID, Type.NO_ARGS), inst);
    }
    return null;
  }

  /**
   * Pop instructions discard the top of the stack. We want to discard the top of the tag stack iff
   * the item on the top of the stack is a primitive.
   */
  InstructionList pop_tag(Instruction inst, OperandStack stack) {
    Type top = stack.peek();
    if (is_primitive(top)) {
      return discard_tag_code(inst, 1);
    }
    return null;
  }

  /**
   * Pops either the top 2 category 1 values or a single category 2 value from the top of the stack.
   * We must do the same to the tag stack if the values are primitives.
   */
  InstructionList pop2_tag(Instruction inst, OperandStack stack) {
    Type top = stack.peek();
    if (is_category2(top)) {
      return discard_tag_code(inst, 1);
    } else {
      int cnt = 0;
      if (is_primitive(top)) {
        cnt++;
      }
      if (is_primitive(stack.peek(1))) {
        cnt++;
      }
      if (cnt > 0) {
        return discard_tag_code(inst, cnt);
      }
    }
    return null;
  }

  /**
   * Swaps the two category 1 types on the top of the stack. We need to swap the top of the tag
   * stack if the two top elements on the real stack are primitives.
   */
  InstructionList swap_tag(Instruction inst, OperandStack stack) {
    Type type1 = stack.peek();
    Type type2 = stack.peek(1);
    if (is_primitive(type1) && is_primitive(type2)) {
      return build_il(dcr_call("swap", Type.VOID, Type.NO_ARGS), inst);
    }
    return null;
  }

  /**
   * Adjusts the tag stack for load constant opcodes. If the constant is a primitive, pushes its tag
   * on the tag stack. If the constant is a reference (string, class), does nothing.
   */
  @Nullable InstructionList ldc_tag(Instruction inst, OperandStack stack) {
    Type type;
    if (inst instanceof LDC) { // LDC_W extends LDC
      type = ((LDC) inst).getType(pool);
    } else {
      type = ((LDC2_W) inst).getType(pool);
    }
    if (!(type instanceof BasicType)) {
      return null;
    }
    return build_il(dcr_call("push_const", Type.VOID, Type.NO_ARGS), inst);
  }

  /**
   * Handle the instruction that allocates multi-dimensional arrays. If the new array has 2
   * dimensions, make the integer arguments comparable to the corresponding indices of the new
   * array. For any other number of dimensions, discard the tags for the arguments. Higher
   * dimensions should really be handled as well, but there are very few cases of this and the
   * resulting code would be quite complex (see multiarray2 for details).
   */
  InstructionList multi_newarray_dc(Instruction inst) {
    int dims = ((MULTIANEWARRAY) inst).getDimensions();
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
   * @param mg method to modify
   * @param inst return instruction to be replaced
   * @return the instruction list
   */
  InstructionList return_tag(MethodGen mg, Instruction inst) {
    Type type = mg.getReturnType();
    InstructionList il = new InstructionList();

    // Push the tag frame
    il.append(InstructionFactory.createLoad(object_arr, tag_frame_local.getIndex()));

    if ((type instanceof BasicType) && (type != Type.VOID)) {
      il.append(dcr_call("normal_exit_primitive", Type.VOID, new Type[] {object_arr}));
    } else {
      il.append(dcr_call("normal_exit", Type.VOID, new Type[] {object_arr}));
    }
    il.append(inst);
    return il;
  }

  /**
   * Returns whether or not the specified type is a primitive (int, float, double, etc).
   *
   * @param type type to check
   * @return true if type is primitive
   */
  @Pure
  boolean is_primitive(Type type) {
    return (type instanceof BasicType) && (type != Type.VOID);
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
      loader = DCInstrument.class.getClassLoader();
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
      @ClassGetName String sig = typeToClassGetName(t);
      try {
        return Class.forName(sig, false, loader);
      } catch (Exception e) {
        throw new Error("can't get class " + sig, e);
      }
    } else {
      throw new Error("unexpected type " + t);
    }
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
      il.append(discard_tag_code(new NOP(), primitive_cnt));
    }

    // push a tag if there is a primitive return value
    Type returnType = mg.getReturnType();
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
   * Returns whether or not tag fields are used within the specified method of the specified class.
   * We can safely use class fields except in Object, String, and Class.
   *
   * @param mg method to check
   * @param classname class to check
   * @return true if tag fields may be used in class for method
   */
  boolean tag_fields_ok(MethodGen mg, @ClassGetName String classname) {

    // Prior to Java 8 an interface could not contain any implementations.
    if (gen.isInterface()) {
      if (gen.getMajor() < Const.MAJOR_1_8) {
        return false;
      }
    }

    if (BcelUtil.isConstructor(mg)) {
      if (!this.constructor_is_initialized) {
        return false;
      }
    }

    if (!jdk_instrumented) {
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
   * ISSUE? This flag is not currently tested. (markro)
   *
   * <p>Accessors are also created for each visible superclass field that is not hidden by a field
   * in this class. These accessors just call the superclasses accessor.
   *
   * <p>Any accessors created are added to the class.
   *
   * @param gen class to check for fields
   */
  void create_tag_accessors(ClassGen gen) {

    String classname = gen.getClassName();

    // If this class doesn't support tag fields, don't create them
    if (!tag_fields_ok(mgen, classname)) return;

    Set<String> field_set = new HashSet<>();
    Map<Field, Integer> field_map = build_field_map(gen.getJavaClass());

    // Build accessors for all fields declared in this class
    for (Field f : gen.getFields()) {

      assert !field_set.contains(f.getName()) : f.getName() + "-" + classname;
      field_set.add(f.getName());

      // skip primitive fields
      if (!is_primitive(f.getType())) {
        continue;
      }

      MethodGen get_method;
      MethodGen set_method;
      if (f.isStatic()) {
        String full_name = full_name(orig_class, f);
        get_method = create_get_tag(gen, f, static_field_id.get(full_name));
        set_method = create_set_tag(gen, f, static_field_id.get(full_name));
      } else {
        get_method = create_get_tag(gen, f, field_map.get(f));
        set_method = create_set_tag(gen, f, field_map.get(f));
      }
      gen.addMethod(get_method.getMethod());
      gen.addMethod(set_method.getMethod());
    }

    // Build accessors for each field declared in a superclass that is
    // is not shadowed in a subclass
    JavaClass[] super_classes;
    try {
      super_classes = gen.getJavaClass().getSuperClasses();
    } catch (Exception e) {
      throw new Error(e);
    }
    for (JavaClass super_class : super_classes) {
      for (Field f : super_class.getFields()) {
        if (f.isPrivate()) {
          continue;
        }
        if (field_set.contains(f.getName())) {
          continue;
        }
        if (!is_primitive(f.getType())) {
          continue;
        }

        field_set.add(f.getName());
        MethodGen get_method;
        MethodGen set_method;
        if (f.isStatic()) {
          String full_name = full_name(super_class, f);
          get_method = create_get_tag(gen, f, static_field_id.get(full_name));
          set_method = create_set_tag(gen, f, static_field_id.get(full_name));
        } else {
          get_method = create_get_tag(gen, f, field_map.get(f));
          set_method = create_set_tag(gen, f, field_map.get(f));
        }
        gen.addMethod(get_method.getMethod());
        gen.addMethod(set_method.getMethod());
      }
    }
  }

  /**
   * Builds a Map that relates each field in jc and each of its superclasses to a unique offset. The
   * offset can be used to index into a tag array for this class. Instance fields are placed in the
   * returned map and static fields are placed in static map (shared between all classes).
   *
   * @param jc class to check for fields
   * @return field offset map
   */
  Map<Field, Integer> build_field_map(JavaClass jc) {

    // Object doesn't have any primitive fields
    if (jc.getClassName().equals("java.lang.Object")) {
      return new LinkedHashMap<>();
    }

    // Get the offsets for each field in the superclasses.
    JavaClass super_jc;
    try {
      super_jc = jc.getSuperClass();
    } catch (Exception e) {
      throw new Error("can't get superclass for " + jc, e);
    }
    Map<Field, Integer> field_map = build_field_map(super_jc);
    int offset = field_map.size();

    // Determine the offset for each primitive field in the class
    // Also make sure the static_tags list is large enough for
    // of the tags.
    for (Field f : jc.getFields()) {
      if (!is_primitive(f.getType())) {
        continue;
      }
      if (f.isStatic()) {
        if (!in_jdk) {
          int min_size = static_field_id.size() + DCRuntime.max_jdk_static;
          while (DCRuntime.static_tags.size() <= min_size) DCRuntime.static_tags.add(null);
          static_field_id.put(full_name(jc, f), min_size);
        } else { // building jdk
          String full_name = full_name(jc, f);
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
        field_map.put(f, offset);
        offset++;
      }
    }

    return field_map;
  }

  /**
   * Creates a get tag method for field f. The tag corresponding to field f will be pushed on the
   * tag stack.
   *
   * <pre>{@code
   * void <field>_<class>__$get_tag() {
   *   #if f.isStatic()
   *     DCRuntime.push_static_tag (tag_offset)
   *   #else
   *     DCRuntime.push_field_tag (this, tag_offset);
   * }
   * }</pre>
   *
   * @param gen class whose accessors are being built. Not necessarily the class declaring f (if f
   *     is inherited).
   * @param f field to build an accessor for
   * @param tag_offset offset of f in the tag storage for this field
   * @return the get tag method
   */
  MethodGen create_get_tag(ClassGen gen, Field f, int tag_offset) {

    // Determine the method to call in DCRuntime.  Instance fields and static
    // fields are handled separately.  Also instance fields in special
    // classes that are created by the JVM are handled separately since only
    // in those classes can fields be read without being written (in java)
    String methodname = "push_field_tag";
    Type[] args = object_int;
    if (f.isStatic()) {
      methodname = "push_static_tag";
      args = integer_arg;
    } else if (is_uninit_class(gen.getClassName())) {
      methodname = "push_field_tag_null_ok";
    }

    String classname = gen.getClassName();
    String accessor_name = tag_method_name(GET_TAG, classname, f.getName());

    InstructionList il = new InstructionList();

    if (!f.isStatic()) {
      il.append(InstructionFactory.createThis());
    }
    il.append(ifact.createConstant(tag_offset));
    il.append(dcr_call(methodname, Type.VOID, args));
    il.append(InstructionFactory.createReturn(Type.VOID));

    int access_flags = f.getAccessFlags();
    if (gen.isInterface()) {
      // method in interface cannot be final
      access_flags &= ~Const.ACC_FINAL;
      if (gen.getMajor() < Const.MAJOR_1_8) {
        // If class file version is prior to 8 then a method in an interface
        // cannot be static (it's implicit) and must be abstract.
        access_flags &= ~Const.ACC_STATIC;
        access_flags |= Const.ACC_ABSTRACT;
      }
    } else {
      access_flags |= Const.ACC_FINAL;
    }

    // Create the get accessor method
    MethodGen get_method =
        new MethodGen(
            access_flags,
            Type.VOID,
            Type.NO_ARGS,
            new String[] {},
            accessor_name,
            classname,
            il,
            pool);
    get_method.isPrivate(false);
    get_method.isProtected(false);
    get_method.isPublic(true);
    get_method.setMaxLocals();
    get_method.setMaxStack();
    // add_line_numbers(get_method, il);

    return get_method;
  }

  /**
   * Creates a set tag method for field f. The tag on the top of the tag stack will be popped off
   * and placed in the tag storeage corresponding to field
   *
   * <pre>{@code
   * void <field>_<class>__$set_tag() {
   *   #if f.isStatic()
   *     DCRuntime.pop_static_tag (tag_offset)
   *   #else
   *     DCRuntime.pop_field_tag (this, tag_offset);
   * }
   * }</pre>
   *
   * @param gen class whose accessors are being built. Not necessarily the class declaring f (if f
   *     is inherited).
   * @param f field to build an accessor for
   * @param tag_offset offset of f in the tag storage for this field
   * @return the set tag method
   */
  MethodGen create_set_tag(ClassGen gen, Field f, int tag_offset) {

    String methodname = "pop_field_tag";
    Type[] args = object_int;
    if (f.isStatic()) {
      methodname = "pop_static_tag";
      args = integer_arg;
    }

    String classname = gen.getClassName();
    String setter_name = tag_method_name(SET_TAG, classname, f.getName());

    InstructionList il = new InstructionList();

    if (!f.isStatic()) {
      il.append(InstructionFactory.createThis());
    }
    il.append(ifact.createConstant(tag_offset));
    il.append(dcr_call(methodname, Type.VOID, args));
    il.append(InstructionFactory.createReturn(Type.VOID));

    int access_flags = f.getAccessFlags();
    if (gen.isInterface()) {
      // method in interface cannot be final
      access_flags &= ~Const.ACC_FINAL;
      if (gen.getMajor() < Const.MAJOR_1_8) {
        // If class file version is prior to 8 then a method in an interface
        // cannot be static (it's implicit) and must be abstract.
        access_flags &= ~Const.ACC_STATIC;
        access_flags |= Const.ACC_ABSTRACT;
      }
    } else {
      access_flags |= Const.ACC_FINAL;
    }

    // Create the setter method
    MethodGen set_method =
        new MethodGen(
            access_flags,
            Type.VOID,
            Type.NO_ARGS,
            new String[] {},
            setter_name,
            classname,
            il,
            pool);
    set_method.setMaxLocals();
    set_method.setMaxStack();
    // add_line_numbers(set_method, il);

    return set_method;
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
   * @param gen class to add interface to
   */
  void add_dcomp_interface(ClassGen gen) {
    gen.addInterface(instrumentation_interface);
    debugInstrument.log("Added interface DCompInstrumented%n");

    InstructionList il = new InstructionList();
    int access_flags = Const.ACC_PUBLIC;
    if (gen.isInterface()) {
      access_flags |= Const.ACC_ABSTRACT;
    }
    MethodGen method =
        new MethodGen(
            access_flags,
            Type.BOOLEAN,
            new Type[] {Type.OBJECT},
            new String[] {"obj"},
            "equals_dcomp_instrumented",
            gen.getClassName(),
            il,
            pool);

    il.append(InstructionFactory.createLoad(Type.OBJECT, 0)); // load this
    il.append(InstructionFactory.createLoad(Type.OBJECT, 1)); // load obj
    il.append(new ACONST_NULL()); // use null for marker
    il.append(
        ifact.createInvoke(
            gen.getClassName(),
            "equals",
            Type.BOOLEAN,
            new Type[] {Type.OBJECT, dcomp_marker},
            Const.INVOKEVIRTUAL));
    il.append(InstructionFactory.createReturn(Type.BOOLEAN));
    method.setMaxStack();
    method.setMaxLocals();
    gen.addMethod(method.getMethod());
    il.dispose();
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
   * @param gen class to add method to
   */
  void add_equals_method(ClassGen gen) {
    InstructionList il = new InstructionList();
    int access_flags = Const.ACC_PUBLIC;
    if (gen.isInterface()) {
      access_flags |= Const.ACC_ABSTRACT;
    }
    MethodGen method =
        new MethodGen(
            access_flags,
            Type.BOOLEAN,
            new Type[] {Type.OBJECT},
            new String[] {"obj"},
            "equals",
            gen.getClassName(),
            il,
            pool);

    il.append(InstructionFactory.createLoad(Type.OBJECT, 0)); // load this
    il.append(InstructionFactory.createLoad(Type.OBJECT, 1)); // load obj
    il.append(
        ifact.createInvoke(
            gen.getSuperclassName(),
            "equals",
            Type.BOOLEAN,
            new Type[] {Type.OBJECT},
            Const.INVOKESPECIAL));
    il.append(InstructionFactory.createReturn(Type.BOOLEAN));
    method.setMaxStack();
    method.setMaxLocals();
    gen.addMethod(method.getMethod());
    il.dispose();
  }

  /**
   * Marks the class as implementing various object methods (currently clone and toString). Callers
   * will call the instrumented version of the method if it exists, otherwise they will call the
   * uninstrumented version.
   *
   * @param gen class to check
   */
  void handle_object(ClassGen gen) {
    Method cl = gen.containsMethod("clone", "()Ljava/lang/Object;");
    if (cl != null) {
      gen.addInterface(Signatures.addPackage(dcomp_prefix, "DCompClone"));
    }

    Method ts = gen.containsMethod("toString", "()Ljava/lang/String;");
    if (ts != null) {
      gen.addInterface(Signatures.addPackage(dcomp_prefix, "DCompToString"));
    }
  }

  /**
   * Returns a field tag accessor method name.
   *
   * @param type "get_tag" or "set_tag"
   * @param classname name of class
   * @param fname name of field
   * @return name of tag accessor method
   */
  static String tag_method_name(String type, String classname, String fname) {
    return fname + "_" + classname.replace('.', '_') + "__$" + type;
  }

  /**
   * Add a dcomp marker argument to indicate this is the instrumented version of the method.
   *
   * @param mg method to ard dcomp marker to
   */
  void add_dcomp_arg(MethodGen mg) {

    // Don't modify main or the JVM won't be able to find it.
    if (BcelUtil.isMain(mg)) {
      return;
    }

    // Don't modify class init methods, they don't take arguments
    if (BcelUtil.isClinit(mg)) {
      return;
    }

    // Add the dcomp marker argument to indicate this is the
    // instrumented version of the method.
    addNewParameter(mg, "marker", dcomp_marker);
  }

  /**
   * Returns whether or not the method is defined in Object.
   *
   * @param methodName method to check
   * @param argTypes array of argument types to method
   * @return true if method is member of Object
   */
  @Pure
  boolean is_object_method(String methodName, Type[] argTypes) {
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
   * Creates a method with a DcompMarker argument that does nothing but call the corresponding
   * method without the DCompMarker argument. (Currently, only used for ? va main.)
   *
   * @param mg MethodGen of method to create stub for
   * @return the stub
   */
  MethodGen create_dcomp_stub(MethodGen mg) {

    InstructionList il = new InstructionList();
    Type returnType = mg.getReturnType();

    // if mg is dynamic, Push 'this' on the stack
    int offset = 0;
    if (!mg.isStatic()) {
      il.append(InstructionFactory.createThis());
      offset = 1;
    }

    // push each argument on the stack
    for (Type argType : mg.getArgumentTypes()) {
      il.append(InstructionFactory.createLoad(argType, offset));
      offset += argType.getSize();
    }

    // Call the method
    short kind = Const.INVOKEVIRTUAL;
    if (mg.isStatic()) {
      kind = Const.INVOKESTATIC;
    }
    il.append(
        ifact.createInvoke(
            mg.getClassName(), mg.getName(), returnType, mg.getArgumentTypes(), kind));

    il.append(InstructionFactory.createReturn(returnType));

    // Create the method
    Type[] argTypes = BcelUtil.postpendToArray(mg.getArgumentTypes(), dcomp_marker);
    String[] argNames = addString(mg.getArgumentNames(), "marker");
    MethodGen dcomp_mg =
        new MethodGen(
            mg.getAccessFlags(),
            returnType,
            argTypes,
            argNames,
            mg.getName(),
            mg.getClassName(),
            il,
            pool);
    dcomp_mg.setMaxLocals();
    dcomp_mg.setMaxStack();

    return dcomp_mg;
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
   * @param jc class containing the field
   * @param f the field
   * @return string containing the fully qualified name
   */
  protected String full_name(JavaClass jc, Field f) {
    return jc.getClassName() + "." + f.getName();
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
    return full_name.replaceAll(" \\[.*\\]", "");
  }
}
