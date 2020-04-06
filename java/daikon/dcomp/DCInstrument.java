package daikon.dcomp;

import daikon.DynComp;
import daikon.chicory.ClassInfo;
import daikon.chicory.DaikonWriter;
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
import java.io.PrintStream;
import java.lang.reflect.Constructor;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;
import org.apache.bcel.*;
import org.apache.bcel.classfile.*;
import org.apache.bcel.generic.*;
import org.apache.bcel.verifier.*;
import org.apache.bcel.verifier.structurals.*;
import org.checkerframework.checker.lock.qual.GuardSatisfied;
import org.checkerframework.checker.nullness.qual.EnsuresNonNullIf;
import org.checkerframework.checker.nullness.qual.KeyFor;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.checkerframework.checker.signature.qual.ClassGetName;
import org.checkerframework.checker.signature.qual.DotSeparatedIdentifiers;
import org.checkerframework.dataflow.qual.Pure;
import org.checkerframework.dataflow.qual.SideEffectFree;

/** Instruments a class file to perform Dynamic Comparability. */
@SuppressWarnings({"nullness"}) //
public class DCInstrument extends InstructionListUtils {

  /**
   * Used when testing to terminate processing if an error occurs. Currently, This flag is only used
   * by BuildJDK.
   */
  @Option("Halt if an instrumentation error occurs")
  public static boolean quit_if_error = false;

  /** Unmodified version of input class. */
  protected JavaClass orig_class;
  /** ClassGen for the current class. */
  protected ClassGen gen;
  /** MethodGen for the current method. */
  protected MethodGen mgen;
  /** Is the current class a member of the JDK? */
  protected boolean in_jdk;
  /**
   * Is this the first phase of instrumenting a member of the JDK (via BuildJDK)? This flag is
   * set/reset in daikon.dcomp.DCInstrument. This flag will never be true for Java 8.
   */
  protected boolean initial_jdk_instrument;
  /**
   * Is this the second phase of instrumenting a member of the JDK? This flag is set/reset in
   * daikon.dcomp.Instrument. This flag will never be true for Java 8.
   */
  protected static boolean retransforming = false;

  /** The BCEL InstructionFactory for generating byte code instructions. */
  protected InstructionFactory ifact;
  /** The loader that loaded DCInstrument.class. */
  protected @Nullable ClassLoader loader;
  /** Has an &lt;init&gt; method completed initialization? */
  protected boolean constructor_is_initialized;

  /** Local that stores the tag frame for the current method. */
  protected LocalVariableGen tag_frame_local;

  // Argument descriptors
  protected static Type[] two_objects = new Type[] {Type.OBJECT, Type.OBJECT};
  protected static Type[] object_string = new Type[] {Type.OBJECT, Type.STRING};
  protected static Type[] two_ints = new Type[] {Type.INT, Type.INT};
  protected static Type[] object_int = new Type[] {Type.OBJECT, Type.INT};
  protected static Type[] string_arg = new Type[] {Type.STRING};
  protected static Type[] integer_arg = new Type[] {Type.INT};
  protected static Type[] float_arg = new Type[] {Type.FLOAT};
  protected static Type[] double_arg = new Type[] {Type.DOUBLE};
  protected static Type[] boolean_arg = new Type[] {Type.BOOLEAN};
  protected static Type[] long_arg = new Type[] {Type.LONG};
  protected static Type[] short_arg = new Type[] {Type.SHORT};
  protected static Type[] object_arg = new Type[] {Type.OBJECT};
  protected static Type[] CharSequence_arg = new Type[] {new ObjectType("java.lang.CharSequence")};
  protected static Type javalangClass = new ObjectType("java.lang.Class");
  protected static Type[] class_str = new Type[] {javalangClass, Type.STRING};

  // Type descriptors
  protected static Type object_arr = new ArrayType(Type.OBJECT, 1);
  // private Type int_arr = new ArrayType (Type.INT, 1);
  protected static ObjectType throwable = new ObjectType("java.lang.Throwable");
  protected static ObjectType dcomp_marker = null;
  protected static ObjectType javalangObject = new ObjectType("java.lang.Object");

  // Debug loggers
  protected static SimpleLog debug_transform = new SimpleLog(false);
  protected static SimpleLog debug_native = new SimpleLog(false);
  protected static SimpleLog debug_dup = new SimpleLog(false);
  protected static SimpleLog debug_track = new SimpleLog(false);

  /** Keeps track of the methods that were not successfully instrumented. */
  protected List<String> skipped_methods = new ArrayList<>();

  /**
   * Specifies if we are to use an instrumented version of the JDK. Calls into the JDK must be
   * modified to remove the arguments from the tag stack if the JDK is not instrumented. This flag
   * is set/reset in daikon.dcomp.Premain.
   */
  protected static boolean jdk_instrumented = true;

  /** Either "java.lang.DCompInstrumented" or "daikon.dcomp.DCompInstrumented". */
  protected static String instrumentation_interface;
  /** Either "java.lang.DCompMarker" or "daikon.dcomp.DCompMarker". */
  protected static @DotSeparatedIdentifiers String dcomp_prefix;
  /**
   * We add a dummy local variable to JDK methods during the initial jdk instrumentation (via
   * BuildJDK) as a flag to indicate that the method needs to be re-instrumented at runtime. This is
   * never done for Java 8.
   */
  protected static final String instrumentation_marker_variable = "DaIkOn_instrumented";

  /**
   * Don't instrument toString functions. Useful in debugging since we call toString on objects from
   * our code (which then triggers (recursive) instrumentation). No longer necessary as we always
   * double client methods?
   */
  protected static boolean ignore_toString = true;

  /** Name prefix for tag setter methods. */
  protected static final String SET_TAG = "set_tag";
  /** Name prefix for tag getter methods. */
  protected static final String GET_TAG = "get_tag";

  /**
   * Map from each static field name to its unique integer id. Note that while it's intuitive to
   * think that each static should show up exactly once, that is not the case. A static defined in a
   * superclass can be accessed through each of its subclasses. Tag accessor methods must be added
   * in each subclass and each should return the same id. We thus will lookup the same name multiple
   * times.
   */
  static Map<String, Integer> static_field_id = new LinkedHashMap<>();

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
   * and most of them don't provide useful comparability information anyway. I've also added
   * newInstance because of a problem with code that the JDK generates for newInstance. The equals
   * method IS instrumented. Why does equals work? Why is clone not mentioned? (Implies it is
   * instrumented.)
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
        // new MethodDef("newInstance", new Type[] {object_arr}),
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

  /** Class that defines a range of byte code within a method. */
  static class CodeRange {
    int start_pc;
    int len;

    CodeRange(int start_pc, int len) {
      this.start_pc = start_pc;
      this.len = len;
    }

    public boolean contains(int offset) {
      return (offset >= start_pc) && (offset < (start_pc + len));
    }

    @SideEffectFree
    @Override
    public String toString(@GuardSatisfied CodeRange this) {
      return String.format("Code range: %d..%d", start_pc, start_pc + len - 1);
    }
  }

  /** Initialize with the original class and whether or not the class is part of the JDK. */
  public DCInstrument(JavaClass orig_class, boolean in_jdk, @Nullable ClassLoader loader) {
    super();
    this.orig_class = orig_class;
    this.in_jdk = in_jdk;
    this.loader = loader;
    initial_jdk_instrument = false;
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
    debug_transform.enabled = DynComp.debug_transform || Premain.debug_dcinstrument;
    debug_instrument.enabled = DynComp.debug || Premain.debug_dcinstrument;
    debug_native.enabled = DynComp.debug;
    debug_track.enabled = Premain.debug_dcinstrument;
  }

  /**
   * Instruments the original class to perform dynamic comparabilty and returns the new class
   * definition.
   *
   * @return the modified JavaClass
   */
  public JavaClass instrument() {

    String classname = gen.getClassName();

    // Don't instrument annotations.  They aren't executed and adding
    // the marker argument causes subtle errors
    if ((gen.getModifiers() & Const.ACC_ANNOTATION) != 0) {
      debug_transform.log("Not instrumenting annotation %s%n", classname);
      return gen.getJavaClass().copy();
    }

    if (retransforming) {
      debug_transform.log("Re-Instrumenting class %s%n", classname);
      remove_dummy_instrumentation();
    }

    debug_transform.log("Instrumenting class %s%n", classname);
    debug_transform.indent();

    // Create the ClassInfo for this class and its list of methods
    ClassInfo class_info = new ClassInfo(classname, loader);
    boolean track_class = false;

    if (!retransforming) {
      // Handle object methods for this class
      handle_object(gen);

      // Have all top-level classes implement our interface
      if (gen.getSuperclassName().equals("java.lang.Object")) {
        // Add equals method if it doesn't already exist. This ensures
        // that an instrumented version, equals(Object, DCompMarker),
        // will be created in this class.
        Method eq = gen.containsMethod("equals", "(Ljava/lang/Object;)Z");
        if (eq == null) {
          debug_transform.log("Added equals method%n");
          add_equals_method(gen);
        }

        // Add DCompInstrumented interface and the required
        // equals_dcomp_instrumented method.
        add_dcomp_interface(gen);
      }
    }

    // Process each method
    for (Method m : gen.getMethods()) {

      tag_frame_local = null;
      try {
        // Note whether we want to track the daikon variables in this method
        boolean track = should_track(classname, methodEntryName(classname, m));
        // If any one method is tracked, then the class is tracked.
        if (track) track_class = true;

        // If we are tracking variables, make sure the class is public
        if (track && !gen.isPublic()) {
          gen.isPrivate(false);
          gen.isProtected(false);
          gen.isPublic(true);
        }

        debug_transform.log("  Processing method %s, track=%b%n", m, track);
        debug_transform.indent();

        MethodGen mg = new MethodGen(m, classname, pool);
        mgen = mg; // copy to global

        InstructionList il = mg.getInstructionList();
        boolean has_code = (il != null);
        if (has_code) {
          set_current_stack_map_table(mg, gen.getMajor());
          if (retransforming) {
            // If we are retransforming a JDK class, then
            // the JVM has removed any StackMapTable.
            needStackMap = false;
          }
          build_unitialized_NEW_map(il);
        }

        fix_local_variable_table(mg);

        // If the method is native
        if (mg.isNative()) {

          // Create Java code that cleans up the tag stack and calls the real native method.
          fix_native(gen, mg);
          has_code = true;
          set_current_stack_map_table(mg, gen.getMajor());

          // Add the DCompMarker argument to distinguish our version
          add_dcomp_arg(mg);

        } else { // normal method

          // Add the DCompMarker argument to distinguish our version
          add_dcomp_arg(mg);

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
          update_uninitialized_NEW_offsets(mg.getInstructionList());
          create_new_stack_map_attribute(mg);
          mg.setMaxLocals();
          mg.setMaxStack();
        } else {
          mg.removeCodeAttributes();
          mg.removeLocalVariables();
        }

        remove_local_variable_type_table(mg);
        // Remove all RuntimeAnnotations (if any), as they do not apply
        // to the instrumented version of the method.
        mg.removeAnnotationEntries();

        if (!BcelUtil.isMain(mg) && !BcelUtil.isClinit(mg)) {
          // doubling
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
            if (s == null) throw e;
            if (s.startsWith("Branch target offset too large")
                || s.startsWith("Code array too big")) {
              System.out.printf(
                  "DynComp warning: ClassFile: %s - method %s is too large to instrument and is being skipped.%n",
                  classname, mg.getName());
              // Build a dummy instrumented method that has DCompMarker
              // argument and no instrumentation.
              // first, restore unmodified method
              mg = new MethodGen(m, classname, pool);
              // Add the DCompMarker argument
              add_dcomp_arg(mg);
              remove_local_variable_type_table(mg);
              // try again
              gen.addMethod(mg.getMethod());
            } else {
              throw e;
            }
          }
        } else {
          // replacing
          gen.replaceMethod(m, mg.getMethod());
          if (BcelUtil.isMain(mg)) gen.addMethod(create_dcomp_stub(mg).getMethod());
        }
        debug_transform.exdent();
      } catch (Throwable t) {
        if (debug_instrument.enabled) t.printStackTrace();
        throw new Error("Unexpected error processing " + classname + "." + m.getName(), t);
      }
    }

    // Add tag accessor methods for each primitive in the class
    create_tag_accessors(gen);

    if (!retransforming) {
      // Keep track of when the class is initialized (so we don't look
      // for fields in uninitialized classes)
      track_class_init();
    }
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

    return (gen.getJavaClass().copy());
  }

  /**
   * Instruments the original class to perform dynamic comparabilty and returns the new class
   * definition. Only tracks references; ignores primitive comparability.
   *
   * @return the modified JavaClass
   */
  public JavaClass instrument_refs_only() {

    String classname = gen.getClassName();

    // Don't instrument annotations.  They aren't executed and adding
    // the marker argument causes subtle errors
    if ((gen.getModifiers() & Const.ACC_ANNOTATION) != 0) {
      debug_transform.log("Not instrumenting annotation(refs_only) %s%n", classname);
      return gen.getJavaClass().copy();
    }

    if (retransforming) {
      debug_transform.log("Re-Instrumenting class(refs_only) %s%n", classname);
      remove_dummy_instrumentation();
    }

    debug_transform.log("Instrumenting class(refs_only) %s%n", classname);
    debug_transform.indent();

    // Create the ClassInfo for this class and its list of methods
    ClassInfo class_info = new ClassInfo(classname, loader);
    boolean track_class = false;

    if (!retransforming) {
      // Handle object methods for this class
      handle_object(gen);

      // Have all top-level classes implement our interface
      if (gen.getSuperclassName().equals("java.lang.Object")) {
        // Add equals method if it doesn't already exist. This ensures
        // that an instrumented version, equals(Object, DCompMarker),
        // will be created in this class.
        Method eq = gen.containsMethod("equals", "(Ljava/lang/Object;)Z");
        if (eq == null) {
          debug_transform.log("Added equals method(refs_only)%n");
          add_equals_method(gen);
        }
        // Add DCompInstrumented interface and the required
        // equals_dcomp_instrumented method.
        add_dcomp_interface(gen);
      }
    }

    // Process each method
    for (Method m : gen.getMethods()) {

      tag_frame_local = null;
      try {
        // Note whether we want to track the daikon variables in this method
        boolean track = should_track(classname, methodEntryName(classname, m));
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

        debug_transform.log("  Processing method %s, track=%b%n", m, track);
        debug_transform.indent();

        MethodGen mg = new MethodGen(m, classname, pool);
        mgen = mg; // copy to global

        InstructionList il = mg.getInstructionList();
        boolean has_code = (il != null);
        if (has_code) {
          set_current_stack_map_table(mg, gen.getMajor());
          if (retransforming) {
            // If we are retransforming a JDK class, then
            // the JVM has removed any StackMapTable.
            needStackMap = false;
          }
          build_unitialized_NEW_map(il);
        }

        fix_local_variable_table(mg);

        // If the method is native
        if (mg.isNative()) {

          // Create Java code that cleans up the tag stack and calls the real native method.
          fix_native_refs_only(gen, mg);
          has_code = true;
          set_current_stack_map_table(mg, gen.getMajor());

          // Add the DCompMarker argument to distinguish our version
          add_dcomp_arg(mg);

        } else { // normal method

          // Add the DCompMarker argument to distinguish our version
          add_dcomp_arg(mg);

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
            build_exception_handler_refs_only(mg);
            instrument_method_refs_only(mg);
            if (track) {
              add_enter_refs_only(mg, mi, DCRuntime.methods.size() - 1);
              add_exit_refs_only(mg, mi, DCRuntime.methods.size() - 1);
            }
            install_exception_handler(mg);
          }
        }

        if (has_code) {
          update_uninitialized_NEW_offsets(mg.getInstructionList());
          create_new_stack_map_attribute(mg);
          mg.setMaxLocals();
          mg.setMaxStack();
        } else {
          mg.removeCodeAttributes();
          mg.removeLocalVariables();
        }

        remove_local_variable_type_table(mg);
        // Remove all RuntimeAnnotations (if any), as they do not apply
        // to the instrumented version of the method.
        mg.removeAnnotationEntries();

        if (!BcelUtil.isMain(mg) && !BcelUtil.isClinit(mg)) {
          // doubling
          gen.addMethod(mg.getMethod());
        } else {
          // replacing
          gen.replaceMethod(m, mg.getMethod());
          if (BcelUtil.isMain(mg)) gen.addMethod(create_dcomp_stub(mg).getMethod());
        }
        debug_transform.exdent();
      } catch (Throwable t) {
        if (debug_instrument.enabled) t.printStackTrace();
        throw new Error("Unexpected error processing " + classname + "." + m.getName(), t);
      }
    }

    // Add tag accessor methods for each primitive in the class
    create_tag_accessors(gen);

    if (!retransforming) {
      // Keep track of when the class is initialized (so we don't look
      // for fields in uninitialized classes)
      track_class_init();
    }
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

    return (gen.getJavaClass().copy());
  }

  /**
   * General Java Runtime instrumentation strategy:
   *
   * <p>It is a bit of a misnomer, but the Daikon code and documentation uses the term JDK to refer
   * to the Java Runtime Environment class libraries. In Java 8 and earlier, they were usually found
   * in {@code <your java installation>/jre/lib/rt.jar}. For these versions of Java, we
   * pre-instrumented the entire rt.jar.
   *
   * <p>In Java 9 and later, the Java Runtime classes have been divided into modules that are
   * usually found in: {@code <your java installation>/jmods/*.jmod}.
   *
   * <p>With the conversion to modules for Java 9 and beyond, we have elected to pre-instrument only
   * java.base.jmod and instrument all other Java Runtime (aka JDK) classes dynamically as they are
   * loaded.
   *
   * <p>Post Java 8 there are increased security checks when loading JDK classes. In particular, the
   * core classes contained in the java.base module may not reference anything outside of java.base.
   * This means we cannot pre-instrument classes in the same manner as was done for Java 8 as this
   * would introduce references to the DynComp runtime (DCRuntime.java).
   *
   * <p>As an additional complication, the {@code retransformClasses} method of the Java
   * Instrumentation Interface does not allow adding additional methods or changing method
   * signatures. Hence, we have adopted the following strategy: We will pre-instrument java.base in
   * order to add the instrumented versions of the JDK methods. However, these instrumented methods
   * are dummies that are missing external references to DCRuntime and will never be executed. At
   * DynComp runtime we reinstrument these methods, discarding the bodies of the dummy versions and
   * replacing them with the correctly instrumented versions.
   *
   * <p>A simpler approach would be to just instrument JDK classes as they are loaded. However, a
   * significant number of JDK classes (>800) are loaded prior to DynComp receiving control.
   * Experiments have shown that not having these classes instrumented can lead to DynComp producing
   * comparability sets quite different than those provided by the Java 8 version of Daikon.
   */

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
      return gen.getJavaClass().copy();
    }

    if (BcelUtil.javaVersion > 8) {
      // Don't instrument problem classes.
      // See Premain.java for a list and explainations.
      if (Premain.problem_classes.contains(classname)) {
        if (DynComp.verbose) System.out.printf("Skipping problem class %s%n", classname);
        return gen.getJavaClass().copy();
      }
      initial_jdk_instrument = true;
    }

    debug_transform.log("Instrumenting class(JDK) %s%n", classname);
    debug_transform.indent();

    // Handle object methods for this class
    handle_object(gen);

    // Have all top-level classes implement our interface
    if (gen.getSuperclassName().equals("java.lang.Object")) {
      // Add equals method if it doesn't already exist. This ensures
      // that an instrumented version, equals(Object, DCompMarker),
      // will be created in this class.
      Method eq = gen.containsMethod("equals", "(Ljava/lang/Object;)Z");
      if (eq == null) {
        debug_transform.log("Added equals method%n");
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

        debug_transform.log("  Processing method %s%n", m);
        debug_transform.indent();

        MethodGen mg = new MethodGen(m, classname, pool);
        mgen = mg; // copy to global

        InstructionList il = mg.getInstructionList();
        boolean has_code = (il != null);
        if (has_code) {
          set_current_stack_map_table(mg, gen.getMajor());
          build_unitialized_NEW_map(il);
        }

        fix_local_variable_table(mg);

        // If the method is native
        if (mg.isNative()) {

          // Create Java code that cleans up the tag stack and calls the real native method.
          fix_native(gen, mg);
          has_code = true;
          set_current_stack_map_table(mg, gen.getMajor());

          // Add the DCompMarker argument to distinguish our version
          add_dcomp_arg(mg);

        } else { // normal method

          // Add the DCompMarker argument to distinguish our version
          add_dcomp_arg(mg);

          if (initial_jdk_instrument) {
            // Do not make any other changes to method at this time.
            // Retransformation during DynComp will do instrumentation.
          } else {
            // Instrument the method
            if (has_code) {
              // Create the local to store the tag frame for this method
              tag_frame_local = create_tag_frame_local(mg);
              build_exception_handler(mg);
              instrument_method(mg);
              install_exception_handler(mg);
            }
          }
        }

        if (has_code) {
          update_uninitialized_NEW_offsets(mg.getInstructionList());
          create_new_stack_map_attribute(mg);
          mg.setMaxLocals();
          mg.setMaxStack();
        } else {
          mg.removeCodeAttributes();
          mg.removeLocalVariables();
        }

        remove_local_variable_type_table(mg);

        // Remove all RuntimeAnnotations (if any), as they do not apply
        // to the instrumented version of the method.
        mg.removeAnnotationEntries();

        if (initial_jdk_instrument && has_code) {
          // mark this method as needing to be reinstrumented at runtime
          mg.addLocalVariable(instrumentation_marker_variable, Type.INT, 0, null, null);
        }
        gen.addMethod(mg.getMethod());

        debug_transform.exdent();
      } catch (Throwable t) {
        if (debug_instrument.enabled) t.printStackTrace();
        skip_method(mgen);
        if (quit_if_error) {
          throw new Error("Unexpected error processing " + classname + "." + m.getName(), t);
        } else {
          System.out.printf("Unexpected error processing %s.%s: %s%n", classname, m.getName(), t);
          System.out.printf("Method is NOT instrumented.%n");
        }
      }
    }

    // Add tag accessor methods for each primitive in the class
    create_tag_accessors(gen);

    // We don't need to track class initialization in the JDK because
    // that is only used when printing comparability which is only done
    // for client classes
    // track_class_init();

    debug_transform.exdent();
    debug_transform.log("Instrumentation complete: %s%n", classname);

    return (gen.getJavaClass().copy());
  }

  /**
   * Instruments a JDK class to perform dynamic comparabilty and returns the new class definition. A
   * second version of each method in the class is created which is instrumented for comparability
   * (Reference comparability only).
   *
   * @return the modified JavaClass
   */
  public JavaClass instrument_jdk_refs_only() {

    String classname = gen.getClassName();

    // Don't instrument annotations.  They aren't executed and adding
    // the marker argument causes subtle errors
    if ((gen.getModifiers() & Const.ACC_ANNOTATION) != 0) {
      debug_transform.log("Not instrumenting annotation(refs_only) %s%n", classname);
      return gen.getJavaClass().copy();
    }

    if (BcelUtil.javaVersion > 8) {
      // Don't instrument problem classes.
      // See Premain.java for a list and explainations.
      if (Premain.problem_classes.contains(classname)) {
        if (DynComp.verbose) System.out.printf("Skipping problem class %s%n", classname);
        return gen.getJavaClass().copy();
      }
      initial_jdk_instrument = true;
    }

    debug_transform.log("Instrumenting class(JDK refs_only) %s%n", classname);
    debug_transform.indent();

    // Handle object methods for this class
    handle_object(gen);

    // Have all top-level classes implement our interface
    if (gen.getSuperclassName().equals("java.lang.Object")) {
      // Add equals method if it doesn't already exist. This ensures
      // that an instrumented version, equals(Object, DCompMarker),
      // will be created in this class.
      Method eq = gen.containsMethod("equals", "(Ljava/lang/Object;)Z");
      if (eq == null) {
        debug_transform.log("Added equals method(refs_only)%n");
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

        debug_transform.log("  Processing method %s%n", m);
        debug_transform.indent();

        MethodGen mg = new MethodGen(m, classname, pool);
        mgen = mg; // copy to global

        InstructionList il = mg.getInstructionList();
        boolean has_code = (il != null);
        if (has_code) {
          set_current_stack_map_table(mg, gen.getMajor());
          build_unitialized_NEW_map(il);
        }

        fix_local_variable_table(mg);

        // If the method is native
        if (mg.isNative()) {

          // Create Java code that cleans up the tag stack and calls the real native method.
          fix_native_refs_only(gen, mg);
          has_code = true;
          set_current_stack_map_table(mg, gen.getMajor());

          // Add the DCompMarker argument to distinguish our version
          add_dcomp_arg(mg);

        } else { // normal method

          // Add the DCompMarker argument to distinguish our version
          add_dcomp_arg(mg);

          if (initial_jdk_instrument) {
            // Do not make any other changes to method at this time.
            // Retransformation during DynComp will do instrumentation.
          } else {
            // Instrument the method
            if (has_code) {
              build_exception_handler_refs_only(mg);
              instrument_method_refs_only(mg);
              install_exception_handler(mg);
            }
          }
        }

        if (has_code) {
          update_uninitialized_NEW_offsets(mg.getInstructionList());
          create_new_stack_map_attribute(mg);
          mg.setMaxLocals();
          mg.setMaxStack();
        } else {
          mg.removeCodeAttributes();
          mg.removeLocalVariables();
        }

        remove_local_variable_type_table(mg);

        // Remove all RuntimeAnnotations (if any), as they do not apply
        // to the instrumented version of the method.
        mg.removeAnnotationEntries();

        if (initial_jdk_instrument && has_code) {
          // mark this method as needing to be reinstrumented at runtime
          mg.addLocalVariable(instrumentation_marker_variable, Type.INT, 0, null, null);
        }
        gen.addMethod(mg.getMethod());

        debug_transform.exdent();
      } catch (Throwable t) {
        if (debug_instrument.enabled) t.printStackTrace();
        skip_method(mgen);
        if (quit_if_error) {
          throw new Error("Unexpected error processing " + classname + "." + m.getName(), t);
        } else {
          System.out.printf("Unexpected error processing %s.%s: %s%n", classname, m.getName(), t);
          System.out.printf("Method is NOT instrumented.%n");
        }
      }
    }

    // Add tag accessor methods for each primitive in the class
    create_tag_accessors(gen);

    // We don't need to track class initialization in the JDK because
    // that is only used when printing comparability which is only done
    // for client classes
    // track_class_init();

    debug_transform.exdent();
    debug_transform.log("Instrumentation complete: %s%n", classname);

    return (gen.getJavaClass().copy());
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
    StackTypes stack_types = bcel_calc_stack_types(mg);
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

      if (debug_instrument.enabled) {
        debug_instrument.log("inst: %s %n", ih);
        for (InstructionTargeter it : ih.getTargeters()) {
          debug_instrument.log("targeter: %s %n", it);
        }
      }

      ih = ih.getNext();
    }

    index = 0;
    // Loop through each instruction, making substitutions
    for (ih = orig_start; ih != null; ) {
      debug_instrument.log("instrumenting instruction %s%n", ih);
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
      replace_instructions(mg, il, ih, new_il);

      ih = next_ih;
    }
  }

  /**
   * Instrument the specified method for dynamic comparability (reference comparability only).
   *
   * @param mg MethodGen for the method to be instrumented
   */
  public void instrument_method_refs_only(MethodGen mg) {

    // Get Stack information
    StackTypes stack_types = bcel_calc_stack_types(mg);
    if (stack_types == null) {
      skip_method(mg);
      return;
    }

    // Loop through each instruction, making substitutions
    InstructionList il = mg.getInstructionList();
    OperandStack stack = null;
    for (InstructionHandle ih = il.getStart(); ih != null; ) {
      debug_instrument.log("(refs_only)instrumenting instruction %s%n", ih);
      InstructionList new_il = null;

      // Remember the next instruction to process
      InstructionHandle next_ih = ih.getNext();

      // Get the stack information
      stack = stack_types.get(ih.getPosition());

      // Get the translation for this instruction (if any)
      new_il = xform_inst_refs_only(mg, ih, stack);
      debug_instrument.log("  (refs_only)new inst: %s%n", new_il);

      // If this instruction was modified, replace it with the new
      // instruction list. If this instruction was the target of any
      // jumps or line numbers , replace them with the first
      // instruction in the new list
      replace_instructions(mg, il, ih, new_il);

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
            DCRuntime.class.getName(),
            "exception_exit",
            Type.VOID,
            object_arg,
            Const.INVOKESTATIC));
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

    debug_instrument.log(
        "New ExceptionHandler: %x %x %x %n", start.getPosition(), end.getPosition(), exc_offset);

    // This is a trick to get running_offset set to
    // value of last stack map entry.
    update_stack_map_offset(exc_offset, 0);
    int map_offset = exc_offset - running_offset - 1;

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
      arg_map_types[arg_index++] = generate_StackMapType_from_Type(arg_types[ii]);
    }

    StackMapEntry map_entry;
    StackMapType stack_map_type =
        new StackMapType(
            Const.ITEM_Object, pool.addClass(throwable.getClassName()), pool.getConstantPool());
    StackMapType[] stack_map_types = {stack_map_type};
    map_entry =
        new StackMapEntry(
            Const.FULL_FRAME, map_offset, arg_map_types, stack_map_types, pool.getConstantPool());

    int orig_size = stack_map_table.length;
    StackMapEntry[] new_stack_map_table = new StackMapEntry[orig_size + 1];
    System.arraycopy(stack_map_table, 0, new_stack_map_table, 0, orig_size);
    new_stack_map_table[orig_size] = map_entry;
    stack_map_table = new_stack_map_table;
  }

  /**
   * Adds a try/catch block around the entire method. If an exception occurs, the exception is
   * rethrown. (Reference comparability only.)
   */
  public void build_exception_handler_refs_only(MethodGen mg) {

    InstructionList il = new InstructionList();
    il.append(
        ifact.createInvoke(
            DCRuntime.class.getName(),
            "exception_exit_refs_only",
            Type.VOID,
            Type.NO_ARGS,
            Const.INVOKESTATIC));
    il.append(new ATHROW());

    add_exception_handler(mg, il);
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

    insert_at_method_start(mg, nl);

    if (!needStackMap) {
      return;
    }

    // For Java 7 and beyond the StackMapTable is part of the
    // verification process.  We need to create and or update it to
    // account for instrumentation code we have inserted as well as
    // adjustments for the new 'tag_frame' local.

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

      // Since we are inserting a new stack map frame at the
      // beginning of the stack map table, we need to adjust the
      // offset_delta of the original first stack map frame due to
      // the fact that it will no longer be the first entry.  We
      // must subtract (len_code + 1).
      // (We don't have to worry about the special case of the
      // original first entry having an offset of 0 because of the
      // NOP we inserted above.

      stack_map_table[0].updateByteCodeOffset(-(len_code + 1));
    }

    int new_table_length = stack_map_table.length + 1;
    StackMapEntry[] new_stack_map_table = new StackMapEntry[new_table_length];

    // Insert a new StackMapEntry at the beginning of the table
    // that adds the tag_frame variable.
    StackMapType tag_frame_type = generate_StackMapType_from_Type(object_arr);
    StackMapType[] stack_map_type_arr = {tag_frame_type};
    new_stack_map_table[0] =
        new StackMapEntry(
            Const.APPEND_FRAME, len_code, stack_map_type_arr, null, pool.getConstantPool());

    // We can just copy the rest of the stack frames over as the FULL_FRAME
    // ones were already updated when the tag_frame variable was allocated.
    for (int i = 0; i < stack_map_table.length; i++) {
      new_stack_map_table[i + 1] = stack_map_table[i];
    }
    stack_map_table = new_stack_map_table;
    // print_stack_map_table ("add_create_tag_frame");
  }

  /** Adds the call to DCRuntime.enter to the beginning of the method. */
  public void add_enter(MethodGen mg, MethodInfo mi, int method_info_index) {
    InstructionList il = mg.getInstructionList();
    replace_instructions(
        mg, il, insertion_placeholder, call_enter_exit(mg, method_info_index, "enter", -1));
  }

  /**
   * Adds the call to DCRuntime.enter_refs_only to the beginning of the method. (Reference
   * comparability only.)
   */
  public void add_enter_refs_only(MethodGen mg, MethodInfo mi, int method_info_index) {
    insert_at_method_start(
        mg, call_enter_exit_refs_only(mg, method_info_index, "enter_refs_only", -1));
  }

  /** Creates the local used to store the tag frame and returns it. */
  LocalVariableGen create_tag_frame_local(MethodGen mg) {
    return create_method_scope_local(mg, "dcomp_tag_frame$5a", object_arr);
  }

  /** Creates code to create the tag frame for this method and store it in tag_frame_local. */
  InstructionList create_tag_frame(MethodGen mg, LocalVariableGen tag_frame_local) {

    Type arg_types[] = mg.getArgumentTypes();

    // Determine the offset of the first argument in the frame
    int offset = 1;
    if (mg.isStatic()) offset = 0;

    // allocate an extra slot to save the tag frame depth for debugging
    int frame_size = mg.getMaxLocals() + 1;

    // unsigned byte max = 255.  minus the character '0' (decimal 48)
    // Largest frame size noted so far is 123.
    assert frame_size < 207 : frame_size + " " + mg.getClassName() + "." + mg.getName();
    String params = "" + (char) (frame_size + '0');
    // Character.forDigit (frame_size, Character.MAX_RADIX);
    List<Integer> plist = new ArrayList<>();
    for (Type arg_type : arg_types) {
      if (arg_type instanceof BasicType) {
        plist.add(offset);
      }
      offset += arg_type.getSize();
    }
    for (int ii = plist.size() - 1; ii >= 0; ii--) {
      params += (char) (plist.get(ii) + '0');
      // Character.forDigit (plist.get(ii), Character.MAX_RADIX);
    }

    // Create code to create/init the tag frame and store in tag_frame_local
    InstructionList il = new InstructionList();
    il.append(ifact.createConstant(params));
    il.append(
        ifact.createInvoke(
            DCRuntime.class.getName(),
            "create_tag_frame",
            object_arr,
            string_arg,
            Const.INVOKESTATIC));
    il.append(InstructionFactory.createStore(object_arr, tag_frame_local.getIndex()));
    debug_instrument.log("Store Tag frame local at index %d%n", tag_frame_local.getIndex());

    return il;
  }

  /**
   * Pushes the object, method info index, parameters, and return value on the stack and calls the
   * specified Method (normally enter or exit) in DCRuntime. The parameters are passed as an array
   * of objects.
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
    if (mg.isStatic()) param_offset = 0;

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
    // value is a primitive, wrap it in the appropriate runtime wrapper
    if (method_name.equals("exit")) {
      Type ret_type = mg.getReturnType();
      if (ret_type == Type.VOID) {
        il.append(new ACONST_NULL());
      } else {
        LocalVariableGen return_local = get_return_local(mg, ret_type);
        if (ret_type instanceof BasicType) {
          il.append(new ACONST_NULL());
          // il.append (create_wrapper (c, ret_type, return_local.getIndex()));
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
            DCRuntime.class.getName(), method_name, Type.VOID, method_args, Const.INVOKESTATIC));

    return il;
  }

  /**
   * Pushes the object, method info index, parameters, and return value on the stack and calls the
   * specified Method (normally enter or exit) in DCRuntime. The parameters are passed as an array
   * of objects. This version does reference comparability only, so the tag frame is NOT pushed.
   */
  InstructionList call_enter_exit_refs_only(
      MethodGen mg, int method_info_index, String method_name, int line) {

    InstructionList il = new InstructionList();
    Type[] arg_types = mg.getArgumentTypes();

    // Push the tag frame
    //    il.append (InstructionFactory.createLoad (tag_frame_local.getType(),
    //                                 tag_frame_local.getIndex()));

    // Push the object.  Null if this is a static method or a constructor
    if (mg.isStatic() || (method_name.equals("enter_refs_only") && BcelUtil.isConstructor(mg))) {
      il.append(new ACONST_NULL());
    } else { // must be an instance method
      il.append(InstructionFactory.createLoad(Type.OBJECT, 0));
    }

    // Determine the offset of the first parameter
    int param_offset = 1;
    if (mg.isStatic()) param_offset = 0;

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
    // value is a primitive, wrap it in the appropriate runtime wrapper
    if (method_name.equals("exit_refs_only")) {
      Type ret_type = mg.getReturnType();
      if (ret_type == Type.VOID) {
        il.append(new ACONST_NULL());
      } else {
        LocalVariableGen return_local = get_return_local(mg, ret_type);
        if (ret_type instanceof BasicType) {
          il.append(new ACONST_NULL());
          // il.append (create_wrapper (c, ret_type, return_local.getIndex()));
        } else {
          il.append(InstructionFactory.createLoad(Type.OBJECT, return_local.getIndex()));
        }
      }

      // push line number
      il.append(ifact.createConstant(line));
    }

    // Call the specified method
    Type[] method_args;
    if (method_name.equals("exit_refs_only")) {
      method_args =
          new Type[] {
            /*object_arr, */
            Type.OBJECT, Type.INT, object_arr, Type.OBJECT, Type.INT
          };
    } else {
      method_args =
          new Type[] {
            /*object_arr, */
            Type.OBJECT, Type.INT, object_arr
          };
    }
    il.append(
        ifact.createInvoke(
            DCRuntime.class.getName(), method_name, Type.VOID, method_args, Const.INVOKESTATIC));

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
        return (object_comparison((BranchInstruction) inst, "object_eq", Const.IFNE));
      case Const.IF_ACMPNE:
        return (object_comparison((BranchInstruction) inst, "object_ne", Const.IFNE));

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
        return handle_invoke((InvokeInstruction) inst);

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
   * Like xform_inst, but transforms instructions to track comparability of references only (i.e.
   * primitives are skipped).
   *
   * @param mg method being instrumented
   * @param ih handle of Instruction to translate
   * @param stack current contents of the stack
   */
  InstructionList xform_inst_refs_only(MethodGen mg, InstructionHandle ih, OperandStack stack) {

    Instruction inst = ih.getInstruction();

    switch (inst.getOpcode()) {

        // Replace the object comparison instructions with a call to
        // DCRuntime.object_eq or DCRuntime.object_ne.  Those methods
        // return a boolean which is used in a ifeq/ifne instruction
      case Const.IF_ACMPEQ:
        return (object_comparison((BranchInstruction) inst, "object_eq", Const.IFNE));
      case Const.IF_ACMPNE:
        return (object_comparison((BranchInstruction) inst, "object_ne", Const.IFNE));

        // Prefix the return with a call to the correct normal_exit
        // method.  normal_exit_primitive is not needed because nothing
        // happens to the tag stack.
      case Const.ARETURN:
      case Const.DRETURN:
      case Const.FRETURN:
      case Const.IRETURN:
      case Const.LRETURN:
      case Const.RETURN:
        {
          InstructionList il = new InstructionList();
          il.append(dcr_call("normal_exit_refs_only", Type.VOID, Type.NO_ARGS));
          il.append(inst);
          return il;
        }

        // Adds the extra argument to calls to instrumented methods, or
        // does nothing for calls to uninstrumented methods.
      case Const.INVOKESTATIC:
      case Const.INVOKEVIRTUAL:
      case Const.INVOKESPECIAL:
      case Const.INVOKEINTERFACE:
      case Const.INVOKEDYNAMIC:
        return handle_invoke_refs_only((InvokeInstruction) inst);

        // All other instructions need no instrumentation
      default:
        return null;
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
        replace_instructions(mg, il, ih, new_il);
      }

      ih = next_ih;
    }
  }

  /**
   * Adds a call to DCruntime.exit_refs_only() at each return from the method. This call calculates
   * comparability on the daikon variables. It is only necessary if we are tracking comparability
   * for the variables of this method (Reference comparability only.)
   *
   * @param mg method to modify
   * @param mi MethodInfo for method
   * @param method_info_index index for MethodInfo
   */
  void add_exit_refs_only(MethodGen mg, MethodInfo mi, int method_info_index) {

    // Iterator over all of the exit line numbers for this method
    Iterator<Integer> exit_iter = mi.exit_locations.iterator();

    // Loop through each instruction
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
        new_il.append(
            call_enter_exit_refs_only(mg, method_info_index, "exit_refs_only", exit_iter.next()));
        new_il.append(inst);
        replace_instructions(mg, il, ih, new_il);
      }

      ih = next_ih;
    }
  }

  /**
   * Discards primitive tags for each primitive argument to a non-instrumented method and adds a tag
   * for a primitive return value. Insures that the tag stack is correct for non-instrumented
   * methods.
   */
  InstructionList handle_invoke(InvokeInstruction invoke) {
    boolean callee_instrumented;

    // Get information about the call
    String method_name = invoke.getMethodName(pool);
    String classname = null;
    Type ret_type = invoke.getReturnType(pool);
    Type[] arg_types = invoke.getArgumentTypes(pool);

    InstructionList il = new InstructionList();

    if (invoke instanceof INVOKEDYNAMIC) {
      // we don't instrument lambda methods
      // BUG: BCEL doesn't know how to get classname from an
      // INVOKEDYNAMIC instruction.
      callee_instrumented = false;
    } else {
      classname = invoke.getClassName(pool);
      callee_instrumented = callee_instrumented(classname);

      if (BcelUtil.javaVersion > 8) {
        if (Premain.problem_methods.contains(classname + "." + method_name)) {
          if (DynComp.verbose)
            System.out.printf(
                "Don't call instrumented version of problem method %s%n",
                classname + "." + method_name);
          callee_instrumented = false;
        }
      }

      // This is a bit of a hack.  An invokeinterface instruction with a
      // a target of "java.util.stream.<something>" might be calling a
      // Lambda method in which case we don't want to add the dcomp_marker.
      // Might lose something in 'normal' cases, but no easy way to detect.
      if (invoke instanceof INVOKEINTERFACE) {
        // System.out.printf("invoke interface host: %s%n", gen.getClassName()+"."+mgen.getName());
        // System.out.printf("invoke interface targ: %s%n", classname+"."+method_name);
        if (classname.startsWith("java.util.stream")) {
          callee_instrumented = false;
        }
        // In a similar fashion, when the Java runtime is processing annotations, there might
        // be an invoke (via reflection) of java.lang.annotation.Annotations.annotationType
        // that should not have the dcomp_marker added.
        if (classname.startsWith("java.lang.annotation")) {
          callee_instrumented = false;
        }
      }

      if (invoke instanceof INVOKEVIRTUAL) {
        if (!jdk_instrumented) {
          // System.out.printf("invoke virtual: %s : %s%n", classname, method_name);
          // System.out.printf("super class: %s%n", gen.getSuperclassName());
          // Technically, we should verify the target class has super class
          // of java.lang.Enum. But that can be difficult if we haven't already
          // processed that class. And since the worst that happens is we
          // loose some tag interactions, we just go ahead.
          if (method_name.equals("ordinal")) {
            callee_instrumented = false;
          }

          // TODO:
          // This is actually a general problem.  Correct solution would seem
          // to be a variation of "has_instrumented" to find target of virtual
          // call at runtime.
          // This is just a hack to get through PASCALI corpus.
          String super_class = gen.getSuperclassName();
          if (!super_class.equals("java.lang.Object") && BcelUtil.inJdk(super_class)) {
            callee_instrumented = false;
          }
        }
      }
    }

    if (invoke instanceof INVOKESPECIAL) {
      if (classname.equals(gen.getSuperclassName()) && method_name.equals("<init>")) {
        constructor_is_initialized = true;
      }
    }

    if (is_object_method(method_name, invoke.getArgumentTypes(pool))) callee_instrumented = false;

    // System.out.printf("handle invoke %s, method = %s, ignore_toString = %b%n",
    //                     invoke, method_name, ignore_toString);

    // Replace calls to Object's equals method with calls to our
    // replacement, a static method in DCRuntime
    if (is_object_equals(method_name, ret_type, arg_types)) {

      Type[] new_arg_types = new Type[] {javalangObject, javalangObject};

      if (invoke.getOpcode() == Const.INVOKESPECIAL) {
        // this is a super.equals(Object) call
        il.append(
            ifact.createInvoke(
                "daikon.dcomp.DCRuntime",
                "dcomp_super_equals",
                ret_type,
                new_arg_types,
                Const.INVOKESTATIC));
      } else {
        // just a regular equals(Object) call
        il.append(
            ifact.createInvoke(
                "daikon.dcomp.DCRuntime",
                "dcomp_equals",
                ret_type,
                new_arg_types,
                Const.INVOKESTATIC));
      }

    } else if (is_object_clone(method_name, ret_type, arg_types)
        || (is_object_toString(method_name, ret_type, arg_types) && !ignore_toString)) {

      il = instrument_object_call(invoke, "");

    } else if (callee_instrumented) {

      // Add the DCompMarker argument so that it calls the instrumented version
      il.append(new ACONST_NULL());
      Type[] new_arg_types = BcelUtil.postpendToArray(arg_types, dcomp_marker);
      Constant methodref = pool.getConstant(invoke.getIndex());
      il.append(
          ifact.createInvoke(
              classname,
              method_name,
              ret_type,
              new_arg_types,
              invoke.getOpcode(),
              methodref instanceof ConstantInterfaceMethodref));

    } else { // not instrumented, discard the tags before making the call

      // Discard the tags for any primitive arguments passed to system
      // methods
      il.append(discard_primitive_tags(arg_types));

      // Add a tag for the return type if it is primitive
      if ((ret_type instanceof BasicType) && (ret_type != Type.VOID)) {
        // System.out.printf("push tag for return  type of %s%n",
        //                   invoke.getReturnType(pool));
        il.append(dcr_call("push_const", Type.VOID, Type.NO_ARGS));
      }
      il.append(invoke);
    }
    return il;
  }

  /**
   * Return instructions that will discard any primitive tags corresponding to the specified
   * arguments. An empty instruction list will be returned if there are no primitive arguments.
   */
  InstructionList discard_primitive_tags(Type[] arg_types) {

    InstructionList il = new InstructionList();
    int primitive_cnt = 0;
    for (Type arg_type : arg_types) {
      if (arg_type instanceof BasicType) primitive_cnt++;
    }
    if (primitive_cnt > 0) il.append(discard_tag_code(new NOP(), primitive_cnt));
    return il;
  }

  /** Returns whether or not the specified classname is instrumented. */
  boolean callee_instrumented(@ClassGetName String classname) {

    // System.out.printf("Checking callee instrumented on %s%n", classname);

    // Our copy of daikon.plumelib is not instrumented.  It would be odd, though,
    // to see calls to this.
    if (classname.startsWith("daikon.plumelib")) {
      return false;
    }

    // Special case the execution trace tool.
    if (classname.startsWith("minst.Minst")) {
      return false;
    }

    // If its not a JDK class, presume its instrumented.
    if (!BcelUtil.inJdk(classname)) {
      return true;
    }

    if (BcelUtil.javaVersion > 8) {
      int i = classname.lastIndexOf('.');
      if (i > 0) {
        if (Premain.problem_packages.contains(classname.substring(0, i))) {
          if (DynComp.verbose)
            System.out.printf(
                "Don't call instrumented member of problem package %s%n",
                classname.substring(0, i));
          return false;
        }
      }

      if (Premain.problem_classes.contains(classname)) {
        if (DynComp.verbose)
          System.out.printf("Don't call instrumented member of problem class %s%n", classname);
        return false;
      }
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

    // We should probably change the interface to include method name
    // and use "classname.methodname" as arg to pattern matcher.
    // Also, perhaps check this for all methods not just jdk?

    // If any of the omit patterns match, use the uninstrumented version
    // of the JDK method
    for (Pattern p : DynComp.ppt_omit_pattern) {
      // System.out.printf("pattern: %s, classname: %s%n", p.pattern(), classname);
      if (p.matcher(classname).find()) {
        return false;
      }
    }

    // If the JDK is instrumented, then everthing but object is instrumented
    if (jdk_instrumented && !classname.equals("java.lang.Object")) {
      return true;
    }

    return false;
  }

  /** Returns true if the specified method is Object.equals() */
  @Pure
  boolean is_object_equals(String method_name, Type ret_type, Type[] args) {
    return (method_name.equals("equals")
        && ret_type == Type.BOOLEAN
        && args.length == 1
        && args[0].equals(javalangObject));
  }

  /** Returns true if the specified method is Object.clone() */
  @Pure
  boolean is_object_clone(String method_name, Type ret_type, Type[] args) {
    return method_name.equals("clone") && ret_type.equals(javalangObject) && (args.length == 0);
  }

  /** Returns true if the specified method is Object.toString() */
  @Pure
  boolean is_object_toString(String method_name, Type ret_type, Type[] args) {
    return method_name.equals("toString") && ret_type.equals(Type.STRING) && (args.length == 0);
  }

  /**
   * Instrument calls to the Object methods clone and toString. In each case, an instrumented
   * version is called if it exists, the non-instrumented version if it does not. Could be used for
   * other Object methods without arguments.
   *
   * @param invoke invoke instruction to inspect and replace
   * @param dcr_suffix (unknown now always "")
   * @return InstructionList to call the correct version of clone or toString
   */
  InstructionList instrument_object_call(InvokeInstruction invoke, String dcr_suffix) {

    InstructionList il = new InstructionList();

    Type[] arg_types = invoke.getArgumentTypes(pool);
    Type[] new_arg_types = BcelUtil.postpendToArray(arg_types, dcomp_marker);
    String method_name = invoke.getMethodName(pool);
    Type ret_type = invoke.getReturnType(pool);
    String classname = invoke.getClassName(pool);
    String dcr_method_name = "uninstrumented_" + method_name + dcr_suffix;

    // Create the interface type that indicates whether or not this method exists
    String cap_method_name = method_name.substring(0, 1).toUpperCase() + method_name.substring(1);
    @SuppressWarnings("signature") // string manipulation
    ObjectType dcomp_interface =
        new ObjectType(Signatures.addPackage(dcomp_prefix, "DComp" + cap_method_name));

    // For now only handle methods without any arguments
    assert arg_types.length == 0 : invoke;

    // if this is a super call
    if (invoke.getOpcode() == Const.INVOKESPECIAL) {

      // If the superclass has an instrumented method, call it
      // otherwise call the uninstrumented method.  This has to be
      // done inline, because the call to super can only take place
      // in this class.  We check at runtime to see if the superclass
      // has an instrumented version of the method.  This is safe because
      // at runtime the superclass must already be loaded.

      // push the class of the superclass
      il.append(new LDC(pool.addClass(classname)));

      // push the name of the method
      il.append(ifact.createConstant(method_name));

      // Call the method that tests to see if an instrumented version of
      // method_name exists in the superclass
      il.append(dcr_call("has_instrumented", Type.BOOLEAN, class_str));

      // Test result and jump if there is no instrumented version of the method
      BranchHandle ifeq_branch = il.append(new IFEQ(null));

      // Call the instrumented version of the method
      il.append(new ACONST_NULL());
      il.append(
          ifact.createInvoke(classname, method_name, ret_type, new_arg_types, invoke.getOpcode()));

      // Jump to the end
      BranchHandle goto_branch = il.append(new GOTO(null));

      // Call the uninstrumented version of the method and handle the interaction
      InstructionHandle else_target = il.append(new DUP());
      il.append(invoke);
      il.append(dcr_call(dcr_method_name, ret_type, new Type[] {Type.OBJECT, ret_type}));

      InstructionHandle endif_target = il.append(new NOP());

      // Fixup the jump targets
      ifeq_branch.setTarget(else_target);
      goto_branch.setTarget(endif_target);

    } else { // a regular (non-super) clone() call

      // If the object has an instrumented clone method, call it
      // otherwise call the uninstrumented method.  This has to be
      // done inline, because clone is protected.  Other calls don't require
      // this, but can be handled in the same fashion.

      // Duplicate the object reference whose method is being called
      il.append(new DUP());

      // Test object and jump if there is no instrumented version of the method
      il.append(ifact.createInstanceOf(dcomp_interface));
      BranchHandle ifeq_branch = il.append(new IFEQ(null));

      // Call the instrumented version of the method
      il.append(new ACONST_NULL());
      il.append(
          ifact.createInvoke(classname, method_name, ret_type, new_arg_types, invoke.getOpcode()));

      // Jump to the end
      BranchHandle goto_branch = il.append(new GOTO(null));

      // Call the uninstrumented version of the method and handle the interaction
      InstructionHandle else_target = il.append(new DUP());
      il.append(invoke);
      il.append(dcr_call(dcr_method_name, ret_type, new Type[] {Type.OBJECT, ret_type}));

      InstructionHandle endif_target = il.append(new NOP());

      // Fixup the jump targets
      ifeq_branch.setTarget(else_target);
      goto_branch.setTarget(endif_target);
    }

    return il;
  }

  /**
   * Similar to handle_invoke, but doesn't perform special handling for primitives. (That is, it
   * does just about nothing.) Currently, does not treat equals or clone specially.
   */
  @Nullable InstructionList handle_invoke_refs_only(InvokeInstruction invoke) {
    boolean callee_instrumented;
    String classname = null;

    // Get information about the call
    String method_name = invoke.getMethodName(pool);
    Type ret_type = invoke.getReturnType(pool);
    Type[] arg_types = invoke.getArgumentTypes(pool);

    InstructionList il = new InstructionList();

    if (invoke instanceof INVOKEDYNAMIC) {
      // we don't instrument lambda methods
      callee_instrumented = false;
    } else {
      classname = invoke.getClassName(pool);
      callee_instrumented = callee_instrumented(classname);
      if (invoke instanceof INVOKEVIRTUAL) {
        // Technically, we should verify the target class has super class
        // of java.lang.Enum. But that can be difficult if we haven't already
        // processed that class. And since the worst that happens is we
        // loose some tag interactions, we just go ahead.
        if (method_name.equals("ordinal") && !jdk_instrumented) {
          callee_instrumented = false;
        }
      }
    }

    // We don't instrument any of the Object methods
    if (is_object_method(method_name, invoke.getArgumentTypes(pool))) callee_instrumented = false;

    // Replace calls to Object's equals method with calls to our
    // replacement, a static method in DCRuntime
    ObjectType javalangObject = new ObjectType("java.lang.Object");
    if (is_object_equals(method_name, ret_type, arg_types)) {

      Type[] new_arg_types = new Type[] {javalangObject, javalangObject};

      if (invoke.getOpcode() == Const.INVOKESPECIAL) {
        // this is a super.equals(Object) call
        il.append(
            ifact.createInvoke(
                "daikon.dcomp.DCRuntime",
                "dcomp_super_equals",
                ret_type,
                new_arg_types,
                Const.INVOKESTATIC));
      } else {
        // just a regular equals(Object) call
        il.append(
            ifact.createInvoke(
                "daikon.dcomp.DCRuntime",
                "dcomp_equals",
                ret_type,
                new_arg_types,
                Const.INVOKESTATIC));
      }

    } else if (method_name.equals("clone")
        && ret_type.equals(javalangObject)
        && arg_types.length == 0) {

      // XXX TEMPORARILY DISABLE INSTRUMENTATION OF CLONE It currently
      // doesn't work quite right -- it causes an
      // IllegalAccessException when attempting to invoke
      // java.util.ResourceBundle.findBundle
      // Hopefully this will be resolved soon, but in the meantime,
      // don't instrument clone() so that results are still produced
      return null;

      /*
      Type[] new_arg_types = new Type[] {javalangObject};

      if (invoke.getOpcode() == Const.INVOKESPECIAL) {
        // this is a super.clone() call
        il.append (ifact.createInvoke ("daikon.dcomp.DCRuntime",
                                       "dcomp_super_clone",
                                       ret_type, new_arg_types,
                                       Const.INVOKESTATIC));
      } else {
        // just a regular clone() call
        il.append (ifact.createInvoke ("daikon.dcomp.DCRuntime",
                                       "dcomp_clone",
                                       ret_type, new_arg_types,
                                       Const.INVOKESTATIC));
      }
      */

    } else if (callee_instrumented) {
      // If the callee is instrumented then, add the dcomp argument

      // Add the DCompMarker argument so that the instrumented version
      // will be used
      il.append(new ACONST_NULL());
      Type[] new_arg_types = BcelUtil.postpendToArray(arg_types, dcomp_marker);
      Constant methodref = pool.getConstant(invoke.getIndex());
      il.append(
          ifact.createInvoke(
              classname,
              method_name,
              ret_type,
              new_arg_types,
              invoke.getOpcode(),
              methodref instanceof ConstantInterfaceMethodref));

    } else {
      // Not instrumented, don't append dcomp argument
      // There are no tags to mess around with
      il.append(invoke);
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
            DCRuntime.class.getName(),
            compare_method,
            Type.BOOLEAN,
            two_objects,
            Const.INVOKESTATIC));
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
   * Handles load and store static instructions. The instructions must be augmented to either push
   * (load) or pop (store) the tag on the tag stack. This is accomplished by calling the specified
   * method in DCRuntime and passing that method the object containing the the field and the offset
   * of that field within the object
   *
   * @deprecated use load_store_field
   */
  @java.lang.Deprecated
  InstructionList load_store_static(FieldInstruction f, String method) {

    Type field_type = f.getFieldType(pool);
    if (field_type instanceof ReferenceType) {
      return null;
    }
    String name = f.getClassName(pool) + "." + f.getFieldName(pool);
    // System.out.printf("static field name for %s = %s%n", f, name);

    // Get the index of this static in the list of all statics and allocate
    // a tag for it.
    Integer index = null; // DCRuntime.static_field_id.get (name);
    if (index == null) {
      // index = DCRuntime.static_field_id.size();
      // DCRuntime.static_field_id.put (name, index);
      DCRuntime.static_tags.add(new Object());
    }

    // Create code to call the method passing it the static's index
    InstructionList il = new InstructionList();
    il.append(ifact.createConstant(index));
    il.append(
        ifact.createInvoke(
            DCRuntime.class.getName(),
            method,
            Type.VOID,
            new Type[] {Type.INT},
            Const.INVOKESTATIC));
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
    debug_instrument.log("CreateLoad %s %d%n", object_arr, tag_frame_local.getIndex());
    il.append(ifact.createConstant(lvi.getIndex()));

    // Call the runtime method to handle loading/storing the local/parameter
    il.append(
        ifact.createInvoke(
            DCRuntime.class.getName(),
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
        if (f.getType() instanceof BasicType) fcnt++;
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
      if (f.getType().isPrimitive()) fcnt++;
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
      assert (return_type.equals(return_local.getType()))
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
    String[] arg_names = mg.getArgumentNames();
    LocalVariableGen[] lvs = mg.getLocalVariables();
    int param_offset = 1;
    if (mg.isStatic()) param_offset = 0;
    if (lvs != null) {
      for (int ii = 0; ii < arg_names.length; ii++) {
        if ((ii + param_offset) < lvs.length) arg_names[ii] = lvs[ii + param_offset].getName();
      }
    }

    // Get the argument types for this method
    Type[] arg_types = mg.getArgumentTypes();
    @ClassGetName String[] arg_type_strings = new @ClassGetName String[arg_types.length];
    for (int ii = 0; ii < arg_types.length; ii++) {
      arg_type_strings[ii] = typeToClassGetName(arg_types[ii]);
      // System.out.printf("DCI arg types: %s %s%n", arg_types[ii], arg_type_strings[ii]);
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
        class_info, mg.getName(), arg_names, arg_type_strings, exit_locs, isIncluded);
  }

  /**
   * Adds a call to DCRuntime.class_init (String classname) to the class initializer for this class.
   * Creates a class initializer if one is not currently present.
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

    if (initial_jdk_instrument) {
      return;
    }

    try {
      MethodGen cinit_gen = new MethodGen(cinit, gen.getClassName(), pool);
      set_current_stack_map_table(cinit_gen, gen.getMajor());

      // Add a call to DCRuntime.class_init to the beginning of the method
      InstructionList il = new InstructionList();
      il.append(ifact.createConstant(gen.getClassName()));
      il.append(
          ifact.createInvoke(
              DCRuntime.class.getName(), "class_init", Type.VOID, string_arg, Const.INVOKESTATIC));

      insert_at_method_start(cinit_gen, il);
      create_new_stack_map_attribute(cinit_gen);
      cinit_gen.setMaxLocals();
      cinit_gen.setMaxStack();
      gen.replaceMethod(cinit, cinit_gen.getMethod());
    } catch (Throwable t) {
      if (debug_instrument.enabled) t.printStackTrace();
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
   * Returns true if this method is the method identified by method_id. The method is encoded as
   * 'classname:method'. The classname is the fully qualified class name. The method is this simple
   * method name (no signature).
   *
   * @param method_id classname:method to check
   * @param classname class to check for
   * @param m method to check for
   * @return true if they match
   */
  boolean has_specified_method(String method_id, String classname, Method m) {

    // Get the classname and method name
    String[] sa = method_id.split(":");
    String m_classname = sa[0];
    String m_name = sa[1];
    // System.out.printf("has_specified_method: %s:%s - %s.%s%n", m_classname,
    //                    m_name, classname, m.getName());

    if (!m_classname.equals(classname)) {
      return false;
    }

    if (!m_name.equals(m.getName())) {
      return false;
    }

    return true;
  }

  /**
   * Returns whether or not this ppt should be included. A ppt is included if it matches ones of the
   * select patterns and doesn't match any of the omit patterns.
   *
   * @param classname class to test
   * @param pptname ppt to look for
   * @return true if this ppt should be included
   */
  boolean should_track(@ClassGetName String classname, String pptname) {

    debug_track.log("Considering tracking ppt %s %s%n", classname, pptname);

    // Don't track any JDK classes
    if (BcelUtil.inJdk(classname)) {
      debug_track.log("  jdk class, return false%n");
      return false;
    }

    // Don't track toString methods because we call them in
    // our debug statements.
    if (ignore_toString && pptname.contains("toString")) {
      return false;
    }

    // If any of the omit patterns match, exclude the ppt
    for (Pattern p : DynComp.ppt_omit_pattern) {
      // System.out.printf("should_track: pattern '%s' on ppt '%s'%n",
      //                    p, pptname);
      if (p.matcher(pptname).find()) {
        debug_track.log("  Omitting program point %s%n", pptname);
        return false;
      }
    }

    // If there are no select patterns, everything matches
    if (DynComp.ppt_select_pattern.size() == 0) {
      return true;
    }

    // One of the select patterns must match the ppt or the class to include
    for (Pattern p : DynComp.ppt_select_pattern) {
      if (p.matcher(pptname).find()) {
        debug_track.log("  matched pptname%n");
        return true;
      }
      if (p.matcher(classname).find()) {
        debug_track.log(" matched classname%n");
        return true;
      }
    }
    debug_track.log(" No Match%n");
    return false;
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
    Type[] arg_types = m.getArgumentTypes();
    String[] type_names = new String[arg_types.length];
    for (int ii = 0; ii < arg_types.length; ii++) {
      type_names[ii] = arg_types[ii].toString();
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
   * @param method_name method to call
   * @param ret_type type of method return
   * @param arg_types array of method argument types
   * @return InvokeInstruction for the call
   */
  InvokeInstruction dcr_call(String method_name, Type ret_type, Type[] arg_types) {

    return ifact.createInvoke(
        DCRuntime.class.getName(), method_name, ret_type, arg_types, Const.INVOKESTATIC);
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
    if (debug_dup.enabled) debug_dup.log("DUP -> %s [... %s]%n", "dup", stack_contents(stack, 2));
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
    if (!is_primitive(stack.peek(1))) method = "dup";
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
    if (debug_dup.enabled) debug_dup.log("DUP2_X1 -> %s [... %s]%n", op, stack_contents(stack, 3));

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
    if (is_category2(top)) op = "dup";
    else if (is_primitive(top) && is_primitive(stack.peek(1))) op = "dup2";
    else if (is_primitive(top) || is_primitive(stack.peek(1))) op = "dup";
    else {
      // both of the top two items are not primitive, nothing to dup
      op = null;
    }
    if (debug_dup.enabled) debug_dup.log("DUP2 -> %s [... %s]%n", op, stack_contents(stack, 2));
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
    if (debug_dup.enabled) debug_dup.log("DUP_X2 -> %s [... %s]%n", op, stack_contents(stack, 3));
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
    if (debug_dup.enabled) debug_dup.log("DUP_X2 -> %s [... %s]%n", op, stack_contents(stack, 3));
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
      if (is_primitive(top)) cnt++;
      if (is_primitive(stack.peek(1))) cnt++;
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
    if (inst instanceof LDC) // LDC_W extends LDC
    type = ((LDC) inst).getType(pool);
    else {
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
    return ((type instanceof BasicType) && (type != Type.VOID));
  }

  /**
   * Returns whether or not the specified type is a category 2 (8 byte) type.
   *
   * @param type type to check
   * @return true if type requires 8 bytes
   */
  @Pure
  boolean is_category2(Type type) {
    return ((type == Type.DOUBLE) || (type == Type.LONG));
  }

  /**
   * Returns the type of the last instruction that modified the top of stack. A gross attempt to
   * figure out what is on the top of stack.
   *
   * @param ih search backward from this instruction
   * @return type of last instruction that modified the top of the stack
   */
  @Nullable Type find_last_push(InstructionHandle ih) {

    for (ih = ih.getPrev(); ih != null; ih = ih.getPrev()) {
      Instruction inst = ih.getInstruction();
      if (inst instanceof InvokeInstruction) {
        return ((InvokeInstruction) inst).getReturnType(pool);
      }
      if (inst instanceof TypedInstruction) {
        return ((TypedInstruction) inst).getType(pool);
      }
    }
    throw new Error("couldn't find any typed instructions");
  }

  /**
   * Returns whether or not the invoke specified invokes a native method. This requires that the
   * class that contains the method to be loaded.
   *
   * @param invoke instruction to check
   * @return true if the invoke calls a native method
   */
  @Pure
  boolean is_native(InvokeInstruction invoke) {

    // Get the class of the method
    ClassLoader loader = getClass().getClassLoader();
    Class<?> clazz;
    try {
      clazz = Class.forName(invoke.getClassName(pool), false, loader);
    } catch (Exception e) {
      throw new Error("can't get class " + invoke.getClassName(pool), e);
    }

    // Get the arguments to the method
    Type[] arg_types = invoke.getArgumentTypes(pool);
    Class<?>[] arg_classes = new Class<?>[arg_types.length];
    for (int ii = 0; ii < arg_types.length; ii++) {
      arg_classes[ii] = type_to_class(arg_types[ii], loader);
    }

    // Find the method and determine if its native
    int modifiers = 0;
    String method_name = invoke.getMethodName(pool);
    String classes = clazz.getName();
    try {
      if (method_name.equals("<init>")) {
        Constructor<?> c = clazz.getDeclaredConstructor(arg_classes);
        modifiers = c.getModifiers();
      } else if (clazz.isInterface()) {
        return false; // presume interfaces aren't native...
      } else {

        java.lang.reflect.Method m = null;
        while (m == null) {
          try {
            m = clazz.getDeclaredMethod(method_name, arg_classes);
            modifiers = m.getModifiers();
          } catch (NoSuchMethodException e) {
            clazz = clazz.getSuperclass();
            classes += ", " + clazz.getName();
          }
        }
      }
    } catch (Exception e) {
      throw new Error(
          "can't find method "
              + method_name
              + " "
              + Arrays.toString(arg_classes)
              + " "
              + classes
              + " "
              + invoke.toString(pool.getConstantPool()),
          e);
    }

    return (Modifier.isNative(modifiers));
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

    if (loader == null) loader = DCInstrument.class.getClassLoader();

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
   * Modify a doubled native method to call its original method. It pops all of the paramter tags
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
    Type[] arg_types = mg.getArgumentTypes();
    String[] arg_names = mg.getArgumentNames();

    debug_native.log("Native call %s%n", mg);

    // Build local variables for each argument to the method
    if (!mg.isStatic()) mg.addLocalVariable("this", new ObjectType(mg.getClassName()), null, null);
    for (int ii = 0; ii < arg_types.length; ii++) {
      mg.addLocalVariable(arg_names[ii], arg_types[ii], null, null);
    }

    if (!initial_jdk_instrument) {

      // Discard the tags for any primitive arguments passed to system
      // methods
      int primitive_cnt = 0;
      for (Type arg_type : arg_types) {
        if (arg_type instanceof BasicType) primitive_cnt++;
      }
      if (primitive_cnt > 0) il.append(discard_tag_code(new NOP(), primitive_cnt));

      // push a tag if there is a primitive return value
      Type ret_type = mg.getReturnType();
      if ((ret_type instanceof BasicType) && (ret_type != Type.VOID)) {
        il.append(dcr_call("push_const", Type.VOID, Type.NO_ARGS));
      }

      // If the method is not static, push the instance on the stack
      if (!mg.isStatic()) {
        il.append(InstructionFactory.createLoad(new ObjectType(gen.getClassName()), 0));
      }

      // System.out.printf("%s: atc = %d, anc = %d%n", mg.getName(), arg_types.length,
      // arg_names.length);

      // if call is sun.reflect.Reflection.getCallerClass (realFramesToSkip)
      if (mg.getName().equals("getCallerClass")
          && (arg_types.length == 1)
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
        if (mg.isStatic()) param_index = 0;
        for (Type arg_type : arg_types) {
          il.append(InstructionFactory.createLoad(arg_type, param_index));
          param_index += arg_type.getSize();
        }
      }
    }

    // Call the method
    il.append(
        ifact.createInvoke(
            gen.getClassName(),
            mg.getName(),
            mg.getReturnType(),
            arg_types,
            (mg.isStatic() ? Const.INVOKESTATIC : Const.INVOKEVIRTUAL)));

    // If there is a return value, return it
    il.append(InstructionFactory.createReturn(mg.getReturnType()));

    // Add the instructions to the method
    mg.setInstructionList(il);
    mg.setMaxStack();
    mg.setMaxLocals();

    // turn off the native flag
    mg.setAccessFlags(mg.getAccessFlags() & ~Const.ACC_NATIVE);
  }

  /**
   * Modify a doubled native method to call its original method. It pops all of the paramter tags
   * off of the tag stack. If there is a primitive return value it puts a new tag value on the stack
   * for it.
   *
   * <p>TODO: add a way to provide a synopsis for native methods that affect comparability.
   *
   * <p>(Reference comparability only.)
   *
   * @param gen current class
   * @param mg the interface method. Must be native.
   */
  void fix_native_refs_only(ClassGen gen, MethodGen mg) {

    InstructionList il = new InstructionList();
    Type[] arg_types = mg.getArgumentTypes();
    String[] arg_names = mg.getArgumentNames();

    debug_native.log("Native call %s%n", mg);

    // Build local variables for each argument to the method
    if (!mg.isStatic()) mg.addLocalVariable("this", new ObjectType(mg.getClassName()), null, null);
    for (int ii = 0; ii < arg_types.length; ii++) {
      mg.addLocalVariable(arg_names[ii], arg_types[ii], null, null);
    }

    if (!initial_jdk_instrument) {

      // If the method is not static, push the instance on the stack
      if (!mg.isStatic()) {
        il.append(InstructionFactory.createLoad(new ObjectType(gen.getClassName()), 0));
      }

      // if call is sun.reflect.Reflection.getCallerClass (realFramesToSkip)
      if (mg.getName().equals("getCallerClass")
          && (arg_types.length == 1)
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
        if (mg.isStatic()) param_index = 0;
        for (Type arg_type : arg_types) {
          il.append(InstructionFactory.createLoad(arg_type, param_index));
          param_index += arg_type.getSize();
        }
      }
    }

    // Call the method
    il.append(
        ifact.createInvoke(
            gen.getClassName(),
            mg.getName(),
            mg.getReturnType(),
            arg_types,
            (mg.isStatic() ? Const.INVOKESTATIC : Const.INVOKEVIRTUAL)));

    // If there is a return value, return it
    il.append(InstructionFactory.createReturn(mg.getReturnType()));

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

    if (BcelUtil.isConstructor(mg)) {
      if (!constructor_is_initialized) {
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
   * Adds a tag field that parallels each primitive field in the class. The tag field is of type
   * object and holds the tag associated with that primitive.
   */
  void add_tag_fields() {

    // Add fields for tag storage for each primitive field
    for (Field field : gen.getFields()) {
      if (is_primitive(field.getType()) && !field.isStatic()) {
        FieldGen tag_field =
            new FieldGen(
                field.getAccessFlags() | Const.ACC_SYNTHETIC,
                Type.OBJECT,
                DCRuntime.tag_field_name(field.getName()),
                pool);
        gen.addField(tag_field.getField());
      }
    }
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
    if (max_items >= stack.size()) max_items = stack.size() - 1;
    for (int ii = max_items; ii >= 0; ii--) {
      if (contents.length() != 0) contents += ", ";
      contents += stack.peek(ii);
    }
    return contents;
  }

  /**
   * Creates tag get and set accessor methods for each field in gen. An accessor is created for each
   * field (including final, static, and private fields). The accessors share the modifiers of their
   * field (except that all are final). Accessors are named <field>_<class>__$get_tag and
   * <field>_<class>__$set_tag. The class name must be included because field names can shadow one
   * another.
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

    if (gen.isInterface()) {
      return;
    }

    String classname = gen.getClassName();

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
      if (initial_jdk_instrument) {
        // mark this method as needing to be reinstrumented at runtime
        get_method.addLocalVariable(instrumentation_marker_variable, Type.INT, 0, null, null);
      }
      gen.addMethod(get_method.getMethod());
      if (initial_jdk_instrument) {
        // mark this method as needing to be reinstrumented at runtime
        set_method.addLocalVariable(instrumentation_marker_variable, Type.INT, 0, null, null);
      }
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
        if (initial_jdk_instrument) {
          // mark this method as needing to be reinstrumented at runtime
          get_method.addLocalVariable(instrumentation_marker_variable, Type.INT, 0, null, null);
        }
        gen.addMethod(get_method.getMethod());
        if (initial_jdk_instrument) {
          // mark this method as needing to be reinstrumented at runtime
          set_method.addLocalVariable(instrumentation_marker_variable, Type.INT, 0, null, null);
        }
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
    // Also make sure the the static_tags list is large enough for
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

    // No references to DCRuntime if initial instrument of JDK.
    // We will fix when we reinstrument.
    if (!initial_jdk_instrument) {
      if (!f.isStatic()) il.append(InstructionFactory.createThis());
      il.append(ifact.createConstant(tag_offset));
      il.append(dcr_call(methodname, Type.VOID, args));
    }
    il.append(InstructionFactory.createReturn(Type.VOID));

    // Create the get accessor method
    MethodGen get_method =
        new MethodGen(
            f.getAccessFlags() | Const.ACC_FINAL,
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
    String accessor_name = tag_method_name(SET_TAG, classname, f.getName());

    InstructionList il = new InstructionList();

    // No references to DCRuntime if initial instrument of JDK.
    // We will fix when we reinstrument.
    if (!initial_jdk_instrument) {
      if (!f.isStatic()) il.append(InstructionFactory.createThis());
      il.append(ifact.createConstant(tag_offset));
      il.append(dcr_call(methodname, Type.VOID, args));
    }
    il.append(InstructionFactory.createReturn(Type.VOID));

    // Create the get accessor method
    MethodGen set_method =
        new MethodGen(
            f.getAccessFlags() | Const.ACC_FINAL,
            Type.VOID,
            Type.NO_ARGS,
            new String[] {},
            accessor_name,
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
    if (!retransforming) {
      gen.addInterface(instrumentation_interface);
      debug_transform.log("Added interface DCompInstrumented%n");
    }

    InstructionList il = new InstructionList();
    int flags = Const.ACC_PUBLIC;
    if (gen.isInterface()) flags = Const.ACC_PUBLIC | Const.ACC_ABSTRACT;
    MethodGen method =
        new MethodGen(
            flags,
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
    int flags = Const.ACC_PUBLIC;
    if (gen.isInterface()) flags = flags | Const.ACC_ABSTRACT;
    MethodGen method =
        new MethodGen(
            flags,
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
    if (cl != null) gen.addInterface(Signatures.addPackage(dcomp_prefix, "DCompClone"));

    Method ts = gen.containsMethod("toString", "()Ljava/lang/String;");
    if (ts != null) gen.addInterface(Signatures.addPackage(dcomp_prefix, "DCompToString"));
  }

  // NOT USED
  /**
   * Adds the following method to a class:
   *
   * <pre>{@code
   * protected Object clone() throws CloneNotSupportedException {
   *   return super.clone();
   * }
   * }</pre>
   *
   * Must only be called if the Object clone method has not been overridden; if the clone method is
   * already defined in the class, a ClassFormatError will result because of the duplicate method.
   *
   * @param gen class to add method to
   */
  void add_clone_method(ClassGen gen) {
    InstructionList il = new InstructionList();
    int flags = Const.ACC_PROTECTED;
    if (gen.isInterface()) flags = Const.ACC_PUBLIC | Const.ACC_ABSTRACT;
    MethodGen method =
        new MethodGen(
            flags,
            Type.OBJECT,
            Type.NO_ARGS,
            new String[] {},
            "clone",
            gen.getClassName(),
            il,
            pool);

    il.append(InstructionFactory.createLoad(Type.OBJECT, 0)); // load this
    il.append(
        ifact.createInvoke(
            gen.getSuperclassName(), "clone", Type.OBJECT, Type.NO_ARGS, Const.INVOKESPECIAL));
    il.append(InstructionFactory.createReturn(Type.OBJECT));
    method.setMaxStack();
    method.setMaxLocals();
    gen.addMethod(method.getMethod());
    il.dispose();
  }

  /**
   * Removes the placeholder instrumentation methods added by BuildJDK. This method is never called
   * for Java 8.
   */
  void remove_dummy_instrumentation() {
    for (Method method : gen.getMethods()) {
      // first check for a DCompMarker argument
      if (method.getSignature().contains("DCompMarker")) {
        gen.removeMethod(method);
        continue;
      }
      // get_tag and set_tag methods don't have DCompMarker arguments
      // check for local variable marker
      LocalVariableTable lvt = method.getLocalVariableTable();
      if (lvt == null) {
        continue;
      }
      boolean found = false;
      for (LocalVariable lv : lvt.getLocalVariableTable()) {
        if (instrumentation_marker_variable.equals(lv.getName())) {
          found = true;
          break;
        }
      }
      if (found) {
        gen.removeMethod(method);
      }
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
    add_new_parameter(mg, "marker", dcomp_marker);
  }

  /**
   * Returns whether or not the method is defined in Object.
   *
   * @param method_name method to check
   * @param arg_types array of argument types to method
   * @return true if method is member of Object
   */
  @Pure
  boolean is_object_method(String method_name, Type[] arg_types) {
    for (MethodDef md : obj_methods) {
      if (md.equals(method_name, arg_types)) {
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
    Type ret_type = mg.getReturnType();

    // if mg is dynamic, Push 'this' on the stack
    int offset = 0;
    if (!mg.isStatic()) {
      il.append(InstructionFactory.createThis());
      offset = 1;
    }

    // push each argument on the stack
    for (Type arg_type : mg.getArgumentTypes()) {
      il.append(InstructionFactory.createLoad(arg_type, offset));
      offset += arg_type.getSize();
    }

    // Call the method
    short kind = Const.INVOKEVIRTUAL;
    if (mg.isStatic()) kind = Const.INVOKESTATIC;
    il.append(
        ifact.createInvoke(mg.getClassName(), mg.getName(), ret_type, mg.getArgumentTypes(), kind));

    il.append(InstructionFactory.createReturn(ret_type));

    // Create the method
    Type[] arg_types = BcelUtil.postpendToArray(mg.getArgumentTypes(), dcomp_marker);
    String[] arg_names = add_string(mg.getArgumentNames(), "marker");
    MethodGen dcomp_mg =
        new MethodGen(
            mg.getAccessFlags(),
            ret_type,
            arg_types,
            arg_names,
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
    for (String line : new EntryReader(file, "UTF-8")) {
      String[] key_val = line.split("  *");
      assert !static_field_id.containsKey(key_val[0]) : key_val[0] + " " + key_val[1];
      static_field_id.put(key_val[0], Integer.valueOf(key_val[1]));
      // System.out.printf("Adding %s %s to static map%n", key_val[0],
      //                   key_val[1]);
    }
  }

  /** Return the fully qualified fieldname of the specified field. */
  protected String full_name(JavaClass jc, Field f) {
    return jc.getClassName() + "." + f.getName();
  }
}
