package daikon.chicory;

import static java.lang.constant.ConstantDescs.CD_Object;
import static java.lang.constant.ConstantDescs.CD_String;
import static java.lang.constant.ConstantDescs.CD_int;
import static java.lang.constant.ConstantDescs.CD_void;

import daikon.Chicory;
import daikon.plumelib.bcelutil.BcelUtil;
import daikon.plumelib.bcelutil.SimpleLog;
import daikon.plumelib.reflection.Signatures;
import daikon.plumelib.util.ArraysPlume;
import java.io.ByteArrayInputStream;
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
import java.lang.classfile.attribute.CodeAttribute;
import java.lang.classfile.attribute.ConstantValueAttribute;
import java.lang.classfile.constantpool.ClassEntry;
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
import java.nio.file.Files;
import java.security.ProtectionDomain;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;
import java.util.Optional;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.bcel.classfile.ClassParser;
import org.apache.bcel.classfile.JavaClass;
import org.checkerframework.checker.nullness.qual.NonNull;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.checkerframework.checker.signature.qual.BinaryName;
import org.checkerframework.checker.signature.qual.ClassGetName;
import org.checkerframework.checker.signature.qual.FieldDescriptor;
import org.checkerframework.checker.signature.qual.FqBinaryName;
import org.checkerframework.checker.signature.qual.InternalForm;
import org.checkerframework.dataflow.qual.Pure;

/**
 * This class modifies another class's bytecodes. It adds calls into the Chicory runtime at method
 * entries and exits for instrumentation purposes. These added calls are sometimes referred to as
 * "hooks".
 *
 * <p>This class is loaded by ChicoryPremain at startup. It is a ClassFileTransformer which means
 * that its {@link #transform} method gets called each time the JVM loads a class.
 *
 * <p>Instrument24 uses Java's {@code java.lang.classfile} APIs for reading and modifying .class
 * files. Those APIs were added in JDK 24. Compared to BCEL, these APIs are more complete and robust
 * (no more fiddling with StackMaps) and are always up to date with any .class file changes (since
 * they are part of the JDK). (We will need to continue to support Instrument.java using BCEL, as we
 * anticipate our clients using JDK 21 or less for quite some time.)
 */
public class Instrument24 implements ClassFileTransformer {

  /** The name of the Chicory runtime support class. */
  private static final String runtime_classname = "daikon.chicory.Runtime";

  /** The ClassDesc for the Chicory runtime support class. */
  private static final ClassDesc runtimeCD = ClassDesc.of(runtime_classname);

  /** A log for debug information about which classes and/or methods are transformed and why. */
  protected static final SimpleLog debug_transform = new SimpleLog(false);

  // Public so daikon.dcomp.Instrument24 can enable it.
  /** A log for debug information about ppt-omit and ppt-select. */
  public static final SimpleLog debug_ppt_omit = new SimpleLog(false);

  /** A log to which to print debugging information about program instrumentation. */
  protected final SimpleLog debugInstrument = new SimpleLog(false);

  /** Directory for debug output. */
  final File debug_dir;

  /** Directory into which to dump instrumented classes. */
  final File debug_instrumented_dir;

  /** Directory into which to dump original classes. */
  final File debug_uninstrumented_dir;

  /** Create an instrumenter. Setup debug directories, if needed. */
  public Instrument24() {
    debug_transform.enabled = Chicory.debug_transform || Chicory.debug || Chicory.verbose;
    debug_ppt_omit.enabled = Chicory.debug;
    debugInstrument.enabled = Chicory.debug;

    debug_dir = Chicory.debug_dir;
    debug_instrumented_dir = new File(debug_dir, "instrumented");
    debug_uninstrumented_dir = new File(debug_dir, "uninstrumented");

    if (Chicory.dump) {
      debug_instrumented_dir.mkdirs();
      debug_uninstrumented_dir.mkdirs();
    }
  }

  /**
   * Returns true if the given ppt should be ignored. Uses the patterns in {@link
   * daikon.chicory.Runtime#ppt_omit_pattern} and {@link daikon.chicory.Runtime#ppt_select_pattern}.
   * This method is called by both Chicory and DynComp.
   *
   * @param className class name to be checked
   * @param methodName method name to be checked
   * @param pptName ppt name to be checked
   * @return true if the item should be filtered out
   */
  public static boolean shouldIgnore(
      @BinaryName String className, String methodName, String pptName) {

    // Because this comes first, exclusion takes precedence.
    // Don't instrument the class if it matches an excluded regular expression.
    for (Pattern pattern : Runtime.ppt_omit_pattern) {
      if (pattern.matcher(pptName).find()
          || pattern.matcher(className).find()
          || pattern.matcher(methodName).find()) {
        debug_ppt_omit.log("ignoring %s, it matches ppt_omit regex %s%n", pptName, pattern);
        return true;
      }
    }

    // If any include regular expressions are specified, only instrument
    // classes that match them.
    for (Pattern pattern : Runtime.ppt_select_pattern) {
      if (pattern.matcher(pptName).find()
          || pattern.matcher(className).find()
          || pattern.matcher(methodName).find()) {
        debug_ppt_omit.log("including %s, it matches ppt_select regex %s%n", pptName, pattern);
        return false;
      }
    }

    // If we're here, this ppt is not explicitly included or excluded.
    // Keep unless there were items in the "include only" list.
    if (!Runtime.ppt_select_pattern.isEmpty()) {
      debug_ppt_omit.log("ignoring %s, not included in ppt_select patterns%n", pptName);
      return true;
    } else {
      debug_ppt_omit.log("including %s, not included in ppt_omit patterns%n", pptName);
      return false;
    }
  }

  /**
   * Don't instrument boot classes. They are not relevant to the user and cannot access
   * daikon.chicory.Runtime (because it is not on the boot classpath).
   *
   * <p>Most boot classes have the null loader, but some generated classes (such as those in
   * sun.reflect) will have a non-null loader. Some of these have a null parent loader, but some do
   * not. The check for the sun.reflect package is a hack to catch all of these. A more consistent
   * mechanism to determine boot classes would be preferable.
   *
   * @param className class name to be checked
   * @param loader the class loader for the class
   * @return true if this is a boot class
   */
  private boolean isBootClass(@BinaryName String className, @Nullable ClassLoader loader) {
    // Chicory.boot_classes is extra classes specified by the user.
    if (Chicory.boot_classes != null) {
      Matcher matcher = Chicory.boot_classes.matcher(className);
      if (matcher.find()) {
        debug_transform.log("Ignoring boot class %s, matches boot_classes regex%n", className);
        return true;
      }
    } else if (loader == null) {
      debug_transform.log("Ignoring system class %s, class loader == null%n", className);
      return true;
    } else if (loader.getParent() == null) {
      debug_transform.log("Ignoring system class %s, parent loader == null%n", className);
      return true;
    } else if (className.startsWith("sun.reflect.")) {
      debug_transform.log("Ignoring system class %s, in sun.reflect package%n", className);
      return true;
    } else if (className.startsWith("jdk.internal.reflect.")) {
      // Starting with Java 9 sun.reflect => jdk.internal.reflect.
      debug_transform.log("Ignoring system class %s, in jdk.internal.reflect package", className);
      return true;
    }
    return false;
  }

  /**
   * Write a .class file and a .bcel version of the class file.
   *
   * @param classBytes a byte array of the class file to output. This is the whole classfile, not
   *     just the bytecodes part of it.
   * @param directory output location for the files
   * @param className the current class
   */
  private void writeDebugClassFiles(
      byte[] classBytes, File directory, @BinaryName String className) {
    // UNDONE: Should we stop using bcel and use classModel.toDebugString() instead?
    // Convert the classBytes to a BCEL JavaClass
    JavaClass c = null;
    try (ByteArrayInputStream bais = new ByteArrayInputStream(classBytes)) {
      ClassParser parser = new ClassParser(bais, className);
      c = parser.parse();
    } catch (Throwable t) {
      System.err.printf("Unexpected error %s while parsing the bytes of %s%n", t, className);
      if (debug_transform.enabled) {
        t.printStackTrace();
      }
      // Ignore the error, it shouldn't affect the instrumentation.
    }

    try {
      debug_transform.log("Dumping .class and .bcel for %s to %s%n", className, directory);
      // Write the byte array to a .class file.
      File outputFile = new File(directory, className + ".class");
      Files.write(outputFile.toPath(), classBytes);
      // Write a BCEL-like file.
      if (c != null) {
        BcelUtil.dump(c, directory);
      }
    } catch (Throwable t) {
      System.err.printf("Unexpected error %s writing debug files for: %s%n", t, className);
      if (debug_transform.enabled) {
        t.printStackTrace();
      }
      // Ignore the error, it shouldn't affect the instrumentation.
    }
  }

  /**
   * Given a class, return a transformed version of the class that contains instrumentation code.
   * Because Chicory is invoked as a javaagent, the transform method is called by the Java runtime
   * each time a new class is loaded. A return value of null leaves the byte codes unchanged.
   *
   * <p>{@inheritDoc}
   */
  @Override
  public byte @Nullable [] transform(
      @Nullable ClassLoader loader,
      @InternalForm String className,
      @Nullable Class<?> classBeingRedefined,
      ProtectionDomain protectionDomain,
      byte[] classfileBuffer)
      throws IllegalClassFormatException {

    // For debugging.
    // new Throwable().printStackTrace();

    debug_transform.log("%nEntering chicory.Instrument24.transform(): class = %s%n", className);

    if (className == null) {
      // most likely a lambda related class
      return null;
    }

    @BinaryName String binaryClassName = Signatures.internalFormToBinaryName(className);

    if (isBootClass(binaryClassName, loader)) {
      return null;
    }

    if (className.contains("/$Proxy")) {
      debug_transform.log("Skipping proxy class %s%n", binaryClassName);
      return null;
    }

    // Don't instrument our own code.
    if (isChicoryClass(className)) {
      debug_transform.log("Not transforming Chicory class %s%n", binaryClassName);
      return null;
    }

    ClassLoader cfLoader;
    if (loader == null) {
      cfLoader = ClassLoader.getSystemClassLoader();
      debug_transform.log("Transforming class %s, loaders %s, %s%n", className, loader, cfLoader);
    } else {
      cfLoader = loader;
      debug_transform.log(
          "Transforming class %s, loaders %s, %s%n", className, loader, loader.getParent());
    }

    // Parse the bytes of the classfile, die on any errors.
    ClassFile classFile =
        ClassFile.of(
            ClassFile.ClassHierarchyResolverOption.of(
                ClassHierarchyResolver.ofResourceParsing(cfLoader)));

    ClassModel classModel;
    try {
      classModel = classFile.parse(classfileBuffer);
    } catch (Throwable t) {
      System.err.printf("Unexpected error %s while parsing bytes of %s%n", t, binaryClassName);
      if (debug_transform.enabled) {
        t.printStackTrace();
      }
      // No changes to the bytecodes.
      return null;
    }

    if (Chicory.dump) {
      writeDebugClassFiles(
          classFile.transformClass(classModel, ClassTransform.ACCEPT_ALL),
          debug_uninstrumented_dir,
          binaryClassName);
    }

    // Instrument the classfile, die on any errors.
    ClassInfo classInfo = new ClassInfo(binaryClassName, cfLoader);
    byte[] newBytes;
    try {
      newBytes =
          classFile.build(
              classModel.thisClass().asSymbol(),
              classBuilder -> instrumentClass(classBuilder, classModel, classInfo));
    } catch (Throwable t) {
      System.err.printf("Unexpected error %s in transform of %s%n", t, binaryClassName);
      if (debug_transform.enabled) {
        t.printStackTrace();
      }
      // No changes to the bytecodes.
      return null;
    }

    if (classInfo.shouldInclude) {
      if (Chicory.dump) {
        writeDebugClassFiles(newBytes, debug_instrumented_dir, binaryClassName);
      }
      return newBytes;
    } else {
      debug_transform.log("Didn't instrument %s%n", binaryClassName);
      // No changes to the bytecodes.
      return null;
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

    debug_transform.log("%nInstrumenting class: %s%n", classInfo.class_name);

    debugInstrument.log("Class Attributes:%n");
    for (java.lang.classfile.Attribute<?> a : classModel.attributes()) {
      debugInstrument.log("  %s%n", a);
    }

    debugInstrument.log("Class Interfaces:%n");
    for (ClassEntry ce : classModel.interfaces()) {
      debugInstrument.log("  %s%n", ce.asInternalName());
    }

    // Modify each non-void method to save its result in a local variable before returning.
    instrument_all_methods(classModel, classBuilder, classInfo);

    // Copy all other ClassElements to output class unchanged.
    for (ClassElement ce : classModel) {
      debugInstrument.log("ClassElement: %s%n", ce);
      switch (ce) {
        case MethodModel mm -> {}
        // Copy all other ClassElements to output class unchanged.
        default -> classBuilder.with(ce);
      }
    }

    if (classInfo.shouldInclude) {
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

    // Store constant static fields in `classInfo`. This ought to be a method of ClassInfo,
    // but that wouldn't work with both Instrument.java and Instrument24.java.
    List<FieldModel> fields = classModel.fields();
    for (FieldModel fm : fields) {
      Optional<ConstantValueAttribute> cva = fm.findAttribute(Attributes.constantValue());
      if (cva.isPresent()) {
        String name = fm.fieldName().stringValue();
        String value = formatConstantDesc(cva.get().constant().constantValue());
        debugInstrument.log("  Constant field: %s, value: %s%n", name, value);
        classInfo.staticMap.put(name, value);
      }
    }

    // If no clinit method, we need to add our own.
    if (Chicory.checkStaticInit && !classInfo.hasClinit) {
      createClinit(classBuilder, classInfo);
    }
  }

  /**
   * Adds a call to the Chicory Runtime {@code initNotify} method prior to each return in the given
   * method. Clients pass the class static initializer {@code <clinit>} as the method.
   *
   * @param mgen the method to modify, typically the class static initializer {@code <clinit>}
   * @param classInfo for the given class
   */
  private void addInitNotifyCalls(MethodGen24 mgen, ClassInfo classInfo) {

    try {
      List<CodeElement> il = mgen.getInstructionList();
      ListIterator<CodeElement> li = il.listIterator();
      while (li.hasNext()) {

        CodeElement inst = li.next();

        // Get the translation for this instruction (if any).
        if (inst instanceof ReturnInstruction) {
          // Back up iterator to point before 'inst'.
          li.previous();
          // Insert code prior to 'inst'.
          for (CodeElement ce : call_initNotify(mgen.getPoolBuilder(), classInfo)) {
            li.add(ce);
          }
          // Skip over 'inst' we just inserted new_il in front of.
          li.next();
        }
      }
    } catch (Exception e) {
      System.err.printf("Unexpected exception encountered: %s", e);
      e.printStackTrace();
    }
  }

  /**
   * Create a class initializer method, if none exists. We need a class initializer to have a place
   * to insert a call to the Chicory Runtime {@code initNotifiy()} method.
   *
   * @param classBuilder for the given class
   * @param classInfo for the given class
   */
  private void createClinit(ClassBuilder classBuilder, ClassInfo classInfo) {

    List<CodeElement> instructions = call_initNotify(classBuilder.constantPool(), classInfo);
    instructions.add(ReturnInstruction.of(TypeKind.VOID)); // need to return!

    classBuilder.withMethod(
        "<clinit>",
        MethodTypeDesc.of(CD_void),
        ClassFile.ACC_STATIC,
        methodBuilder ->
            methodBuilder.withCode(codeBuilder -> copyCode(codeBuilder, instructions)));
  }

  /**
   * Create the list of instructions for a call to {@code initNotify}.
   *
   * @param poolBuilder for the given class
   * @param classInfo for the given class
   * @return the instruction list
   */
  private List<CodeElement> call_initNotify(ConstantPoolBuilder poolBuilder, ClassInfo classInfo) {

    List<CodeElement> instructions = new ArrayList<>(2);

    MethodRefEntry mre =
        poolBuilder.methodRefEntry(runtimeCD, "initNotify", MethodTypeDesc.of(CD_void, CD_String));
    instructions.add(buildLDCInstruction(poolBuilder.stringEntry(classInfo.class_name)));
    instructions.add(InvokeInstruction.of(Opcode.INVOKESTATIC, mre));

    return instructions;
  }

  /**
   * Instruments all the methods in a class. For each method, adds instrumentation code at the entry
   * and at each return from the method. In addition, changes each return statement to first place
   * the value being returned into a local and then return. Note that {@link #callEnterOrExit}
   * special cases the instrumentation for constructor entry.
   *
   * @param classModel for current class
   * @param classBuilder for current class
   * @param classInfo for the given class
   */
  private void instrument_all_methods(
      ClassModel classModel, ClassBuilder classBuilder, ClassInfo classInfo) {

    if (classModel.majorVersion() < ClassFile.JAVA_6_VERSION) {
      String output =
          String.format(
              "Chicory warning: ClassFile: %s - classfile version (%d) is out of date and may not"
                  + " be processed correctly.",
              classInfo.class_name, classModel.majorVersion());
      System.out.printf("%s%n", output);
      debugInstrument.log("%s%n", output);
    }

    List<MethodModel> methods = classModel.methods();
    List<MethodInfo> method_infos = new ArrayList<>(methods.size());
    boolean shouldInclude = false;
    try {
      for (MethodModel mm : methods) {

        // Allow only one method at a time to be instrumented.
        // DynComp does this by creating a new instrumentation object
        // for each class - probably a cleaner solution.
        synchronized (this) {
          MethodGen24 mgen = new MethodGen24(mm, classInfo.class_name, classBuilder);

          // Check for the class static initializer method.
          if (mgen.getName().equals("<clinit>")) {
            classInfo.hasClinit = true;
            if (Chicory.checkStaticInit) {
              addInitNotifyCalls(mgen, classInfo);
            }
            if (!Chicory.instrument_clinit) {
              // We are not going to instrument this method.
              // We need to copy it to the output class.
              copyMethodToOutputUnchanged(classBuilder, mm, mgen);
              continue;
            }
          }

          // If method is synthetic... (default constructors and <clinit> are not synthetic).
          if ((mgen.getAccessFlagsMask() & ClassFile.ACC_SYNTHETIC) != 0) {
            // We are not going to instrument this method.
            // We need to copy it to the output class.
            copyMethodToOutputUnchanged(classBuilder, mm, mgen);
            continue;
          }

          // Skip methods with no instructions.
          if (mgen.getInstructionList().isEmpty()) {
            // We are not going to instrument this method.
            // We need to copy it to the output class.
            copyMethodToOutputUnchanged(classBuilder, mm, mgen);
            continue;
          }

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
            debugInstrument.log("mm.methodName: %s%n", mm.methodName().stringValue());
            debugInstrument.log("mm.methodType: %s%n", mm.methodType().stringValue());

            debugInstrument.log("mgen.getSignature: %s%n", mgen.getSignature());
            MethodTypeDesc mtd = mm.methodTypeSymbol();
            debugInstrument.log("mtd.descriptorString: %s%n", mtd.descriptorString());
            debugInstrument.log("mtd.displayDescriptor: %s%n", mtd.displayDescriptor());
          }

          // Create a MethodInfo that describes this method's arguments and exit line numbers
          // (information not available via reflection) and add it to the list for this class.
          MethodInfo curMethodInfo = create_method_info(classInfo, mgen);

          if (curMethodInfo == null) { // method filtered out!
            // We are not going to instrument this method.
            // We need to copy it to the output class.
            copyMethodToOutputUnchanged(classBuilder, mm, mgen);
            continue;
          }

          shouldInclude = true; // at least one method not filtered out

          method_infos.add(curMethodInfo);

          int method_info_index;
          synchronized (SharedData.methods) {
            method_info_index = SharedData.methods.size();
            assert curMethodInfo != null : "@AssumeAssertion(nullness): just checked above";
            SharedData.methods.add(curMethodInfo);
          }

          // Instrument entry and exits (return instructions).
          classBuilder.withMethod(
              mm.methodName().stringValue(),
              mm.methodTypeSymbol(),
              mm.flags().flagsMask(),
              methodBuilder ->
                  instrumentMethod(methodBuilder, mm, mgen, curMethodInfo, method_info_index));
        }
      }
    } catch (Exception e) {
      System.err.printf("Unexpected exception encountered: %s", e);
      e.printStackTrace();
    }

    classInfo.shouldInclude = shouldInclude;

    // Add the class and method information to runtime so it is available
    // as enter/exit ppts are processed.
    classInfo.set_method_infos(method_infos);
  }

  /**
   * Copy the given method from the input class file to the output class with no changes. Uses
   * {@link #copyMethod} to perform the actual copy.
   *
   * @param classBuilder for the output class
   * @param mm the input method
   * @param mgen describes the output method
   */
  private void copyMethodToOutputUnchanged(
      ClassBuilder classBuilder, MethodModel mm, MethodGen24 mgen) {
    classBuilder.withMethod(
        mm.methodName().stringValue(),
        mm.methodTypeSymbol(),
        mm.flags().flagsMask(),
        methodBuilder -> copyMethod(methodBuilder, mm, mgen));
  }

  /**
   * Copy the given method from the input class file to the output class with no changes.
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

        // Copy all other MethodElements to output class unchanged.
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
   * Instrument a method using {@link #instrumentCode}.
   *
   * @param methodBuilder for the given method
   * @param methodModel for the given method
   * @param mgen describes the given method
   * @param curMethodInfo additional information about the method
   * @param method_info_index the index of the method in SharedData.methods
   */
  private void instrumentMethod(
      MethodBuilder methodBuilder,
      MethodModel methodModel,
      MethodGen24 mgen,
      MethodInfo curMethodInfo,
      int method_info_index) {

    for (MethodElement me : methodModel) {
      debugInstrument.log("MethodElement: %s%n", me);
      switch (me) {
        case CodeModel codeModel ->
            methodBuilder.withCode(
                codeBuilder ->
                    instrumentCode(codeBuilder, codeModel, mgen, curMethodInfo, method_info_index));

        // Copy all other MethodElements to output class unchanged.
        default -> methodBuilder.with(me);
      }
    }
  }

  /**
   * Generate instrumentation code for the given method. The first step is to read and process the
   * original instruction list to create the initial version of the new instruction list for the
   * method (codeList). It then calls {@link #instrumentInstructionList} to add the instrumentation
   * code to the codeList. Finally, it inserts this modified instruction list into the output method
   * while updating the any code labels.
   *
   * @param codeBuilder for the given method's code
   * @param codeModel for the input method's code
   * @param mgen describes the output method
   * @param curMethodInfo provides additional information about the method
   * @param method_info_index the index of the method in SharedData.methods
   */
  private void instrumentCode(
      CodeBuilder codeBuilder,
      CodeModel codeModel,
      MethodGen24 mgen,
      MethodInfo curMethodInfo,
      int method_info_index) {

    MethodGen24.MInfo24 minfo =
        new MethodGen24.MInfo24(method_info_index, mgen.getMaxLocals(), codeBuilder);

    @SuppressWarnings("JdkObsolete")
    List<CodeElement> codeList = new LinkedList<>();

    debugInstrument.log("Code Attributes:%n");
    for (java.lang.classfile.Attribute<?> a : codeModel.attributes()) {
      debugInstrument.log("  %s%n", a);
    }

    // The localsTable was initialized in the MethodGen24 constructor. Here we initialize the
    // codeList. We also remove the local variable type records. Some instrumentation changes
    // require these to be updated, but it should be safe to just delete them since the
    // LocalVariableTypeTable is optional and really only of use to a debugger.  We also save the
    // CodeModel label at the start of the byte codes, if there is one. If there isn't, that is
    // okay as it means the original code did not reference byte code offset 0 so inserting our
    // instrumentation code at that point will not cause a problem.
    @SuppressWarnings("nullness:assignment") // can't have gotten here if CodeAttribute is null
    @NonNull CodeAttribute ca = mgen.getCodeAttribute();
    for (CodeElement ce : mgen.getInstructionList()) {
      debugInstrument.log("CodeElement: %s%n", ce);
      switch (ce) {
        case LocalVariable lv -> {} // we have already processed these
        case LocalVariableType lvt -> {} // we can discard local variable types
        // debugging code
        // case LocalVariableType lvt -> {
        // @FieldDescriptor String lvFD = lvt.signatureSymbol().signatureString();
        // System.out.printf("  %s : %s%n", lvt, convertDescriptorToFqBinaryName(lvFD)); }
        case LabelTarget l -> {
          if (ca.labelToBci(l.label()) == 0) {
            minfo.oldStartLabel = l.label();
          }
          codeList.add(ce);
        }
        // debug code
        // case BranchInstruction bi -> {
        // System.out.printf("  %s : %s%n", bi, bi.target());
        // codeList.add(ce);
        // }
        default -> codeList.add(ce); // save all other elements
      }
    }

    // Generate and insert our instrumentation code.
    instrumentInstructionList(codeList, mgen, curMethodInfo, minfo);

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
    for (CodeElement ce : codeList) {
      // If there is a new CodeBuilder label associated with this instruction, it needs to be
      // defined.
      Label l = minfo.labelMap.get(ce);
      if (l != null) {
        debugInstrument.log("Label: %s%n", l);
        codeBuilder.labelBinding(l);
        // We've defined the label, remove it from the map.
        minfo.labelMap.remove(ce);
      }
      // If this instruction references a Label, we need to see if it is the oldStartLabel
      // and, if so, replace the target with our new entryLabel.
      ce = retargetStartLabel(ce, minfo);
      debugInstrument.log("CodeElement: %s%n", ce);
      codeBuilder.with(ce);
    }
  }

  /**
   * Insert the our instrumentation code into the instruction list for the given method. This
   * comprises adding instrumentation code at method entry and at each return from the method.
   *
   * @param instructions instruction list for method
   * @param mgen describes the given method
   * @param curMethodInfo provides additional information about the method
   * @param minfo for the given method's code
   */
  private void instrumentInstructionList(
      List<CodeElement> instructions,
      MethodGen24 mgen,
      MethodInfo curMethodInfo,
      MethodGen24.MInfo24 minfo) {

    addInstrumentationAtEntry(instructions, mgen, minfo);
    addInstrumentationAtExits(instructions, mgen, curMethodInfo, minfo);
  }

  /**
   * Inserts instrumentation code at the start of the method. This includes adding a local variable
   * (this_invocation_nonce) that is initialized to Runtime.nonce++. This provides a unique id on
   * each method entry/exit that allows them to be matched up from the dtrace file. Also inserts
   * code to call daikon.chicory.Runtime.enter().
   *
   * @param instructions instruction list for method
   * @param mgen describes the given method
   * @param minfo for the given method's code
   */
  private void addInstrumentationAtEntry(
      List<CodeElement> instructions, MethodGen24 mgen, MethodGen24.MInfo24 minfo) {

    List<CodeElement> newCode = generateIncrementNonce(mgen, minfo);

    callEnterOrExit(newCode, mgen, minfo, "enter", -1);

    // The start of the list of CodeElements looks as follows:
    //   LocalVariable declarations (if any)
    //   Label for start of code (if present)
    //   LineNumber for start of code (if present)
    //   <the actual code for the method>
    //
    // We want to insert our instrumentation code after the LocalVariables (if any) and after the
    // initial label (if present), but before any LineNumber or Instruction.
    CodeElement inst = null;
    try {
      ListIterator<CodeElement> li = instructions.listIterator();
      while (li.hasNext()) {
        inst = li.next();
        if ((inst instanceof LineNumber) || (inst instanceof Instruction)) {
          break;
        }
      }

      // Label for new location of start of original code.
      debugInstrument.log("entryLabel: %s%n", minfo.entryLabel);
      assert inst != null : "@AssumeAssertion(nullness): inst will always be set in loop above";
      minfo.labelMap.put(inst, minfo.entryLabel);

      // Insert code before this LineNumber or Instruction.
      // Back up iterator to point to 'inst'.
      li.previous();
      for (CodeElement ce : newCode) {
        li.add(ce);
      }
    } catch (Exception e) {
      System.err.printf("Unexpected exception encountered: %s", e);
      e.printStackTrace();
    }
  }

  /**
   * Generates code to initialize a new local variable (this_invocation_nonce) to Runtime.nonce++.
   *
   * @param mgen describes the given method
   * @param minfo for the given method's code
   */
  private List<CodeElement> generateIncrementNonce(MethodGen24 mgen, MethodGen24.MInfo24 minfo) {
    String atomic_int_classname = "java.util.concurrent.atomic.AtomicInteger";
    ClassDesc atomic_intClassDesc = ClassDesc.of(atomic_int_classname);

    List<CodeElement> newCode = new ArrayList<>();

    // Create the nonce local variable.
    minfo.nonceLocal = createLocalWithMethodScope(mgen, minfo, "this_invocation_nonce", CD_int);

    // The following implements:
    //     this_invocation_nonce = Runtime.nonce++;

    // getstatic Runtime.nonce (load reference to AtomicInteger daikon.chicory.Runtime.nonce)
    newCode.add(
        FieldInstruction.of(
            Opcode.GETSTATIC,
            mgen.getPoolBuilder().fieldRefEntry(runtimeCD, "nonce", atomic_intClassDesc)));

    // Do an atomic get and increment of nonce value.
    // This is multi-thread safe and leaves int value of nonce on stack.
    MethodRefEntry mre =
        mgen.getPoolBuilder()
            .methodRefEntry(atomic_intClassDesc, "getAndIncrement", MethodTypeDesc.of(CD_int));
    newCode.add(InvokeInstruction.of(Opcode.INVOKEVIRTUAL, mre));

    // store original value of nonce into this_invocation_nonce)
    assert minfo.nonceLocal != null : "@AssumeAssertion(nullness): can't get here if null";
    newCode.add(StoreInstruction.of(TypeKind.INT, minfo.nonceLocal.slot()));

    return newCode;
  }

  /**
   * Inserts instrumentation code at each exit (return) from the method. For each return
   * instruction, generate additional instructions to assign the method's result to a local variable
   * (return__$trace2_val) and then call daikon.chicory.Runtime.exit().
   *
   * @param instructions instruction list for method
   * @param mgen describes the given method
   * @param curMethodInfo provides additional information about the method
   * @param minfo for the given method's code
   */
  private void addInstrumentationAtExits(
      List<CodeElement> instructions,
      MethodGen24 mgen,
      MethodInfo curMethodInfo,
      MethodGen24.MInfo24 minfo) {

    assert curMethodInfo != null : "@AssumeAssertion(nullness): can't get here if null";
    Iterator<Boolean> shouldIncludeIter = curMethodInfo.exit_location_is_included.iterator();
    Iterator<Integer> exitLocationIter = curMethodInfo.exit_locations.iterator();

    // Instrument return instructions.
    ListIterator<CodeElement> li = instructions.listIterator();
    while (li.hasNext()) {

      CodeElement inst = li.next();

      // back up iterator to point to 'inst'
      li.previous();

      // If this is a return instruction, insert method exit instrumentation
      List<CodeElement> new_il =
          generate_return_instrumentation(inst, mgen, minfo, shouldIncludeIter, exitLocationIter);

      // insert instrumentation code prior to 'inst'
      for (CodeElement ce : new_il) {
        li.add(ce);
      }

      // skip over 'inst' we just inserted new_il in front of
      li.next();
    }
  }

  /**
   * If this is a return instruction, generate a new instruction list to assign the result to a
   * local variable (return__$trace2_val) and then call daikon.chicory.Runtime.exit(). This
   * instruction list will be inserted immediately before the return.
   *
   * @param inst the instruction to inspect, which might be a return instruction
   * @param mgen describes the given method
   * @param minfo for the given method's code
   * @param shouldIncludeIter if shouldIncludeIter.next() is true, instrument this return
   * @param exitLocationIter if we should instrument this return, exitLocationIter.next() is its
   *     line number
   * @return instruction list for instrumenting the return, or an empty list if {@code inst} is not
   *     a return or the return should not be instrumented
   */
  @SuppressWarnings("MixedMutabilityReturnType")
  private List<CodeElement> generate_return_instrumentation(
      CodeElement inst,
      MethodGen24 mgen,
      MethodGen24.MInfo24 minfo,
      Iterator<Boolean> shouldIncludeIter,
      Iterator<Integer> exitLocationIter) {

    if (!(inst instanceof ReturnInstruction)) {
      return Collections.emptyList();
    }

    // There is a single boolean element on shouldIncludeIter for every return in the method. Its
    // value was calculated by {@link #shouldIgnore} and indicates whether or not that return should
    // be instrumented. If the value is true the next exitLocationIter element contains the source
    // line number for the return in question.

    if (!shouldIncludeIter.hasNext()) {
      throw new RuntimeException("Not enough entries in shouldIncludeIter");
    }

    boolean shouldInclude = shouldIncludeIter.next();

    if (!shouldInclude) {
      return Collections.emptyList();
    }

    List<CodeElement> newCode = new ArrayList<>(2);
    ClassDesc type = mgen.getReturnType();
    if (!type.equals(CD_void)) {
      TypeKind typeKind = TypeKind.from(type);
      LocalVariable returnLocal = getReturnLocal(mgen, type, minfo);
      newCode.add(StackInstruction.of(typeKind.slotSize() == 1 ? Opcode.DUP : Opcode.DUP2));
      newCode.add(StoreInstruction.of(typeKind, returnLocal.slot()));
    }

    if (!exitLocationIter.hasNext()) {
      throw new RuntimeException("Not enough exit locations in the exitLocationIter");
    }

    callEnterOrExit(newCode, mgen, minfo, "exit", exitLocationIter.next());
    return newCode;
  }

  /**
   * Returns the local variable used to store the return result. If it is not present, creates it
   * with the specified type. If the variable is known to already exist, the type can be null.
   *
   * @param mgen describes a method
   * @param returnType the type of the return; may be null if the variable is known to already exist
   * @param minfo the method's code
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
   * Pushes the object, nonce, parameters, and return value on the stack and calls the specified
   * method (either enter or exit) in daikon.chicory.Runtime. The parameters are passed as an array
   * of objects. Any primitive values are wrapped in the appropriate daikon.chicory.Runtime wrapper
   * (IntWrap, FloatWrap, etc).
   *
   * @param newCode an instruction list to append the enter/exit code to
   * @param mgen describes the method to be instrumented
   * @param minfo for the method to be instrumented
   * @param methodToCall either "enter" or "exit"
   * @param line source line number if this is an exit
   */
  private void callEnterOrExit(
      List<CodeElement> newCode,
      MethodGen24 mgen,
      MethodGen24.MInfo24 minfo,
      String methodToCall,
      int line) {

    ClassDesc[] paramTypes = mgen.getParameterTypes();

    // aload
    // Push the object.
    if (mgen.isStatic() || (methodToCall.equals("enter") && isConstructor(mgen))) {
      // Push null if this is a static method or a constructor.
      newCode.add(ConstantInstruction.ofIntrinsic(Opcode.ACONST_NULL));
    } else {
      // Must be an instance method.
      newCode.add(LoadInstruction.of(TypeKind.REFERENCE, 0));
    }

    // The offset of the first parameter.
    int param_offset = mgen.isStatic() ? 0 : 1;

    // Assumes addInstrumentationAtEntry has been called which creates the nonce local.
    // iload
    // Push the nonce.
    assert minfo.nonceLocal != null : "@AssumeAssertion(nullness): can't get here if null";
    newCode.add(LoadInstruction.of(TypeKind.INT, minfo.nonceLocal.slot()));

    // iconst
    // Push the MethodInfo index.
    newCode.add(loadIntegerConstant(minfo.method_info_index, mgen));

    // iconst
    // anewarray
    // Create an array of objects with elements for each parameter.
    newCode.add(loadIntegerConstant(paramTypes.length, mgen));
    newCode.add(NewReferenceArrayInstruction.of(mgen.getPoolBuilder().classEntry(CD_Object)));

    // Put each parameter into the array.
    int param_index = param_offset;
    for (int ii = 0; ii < paramTypes.length; ii++) {
      newCode.add(StackInstruction.of(Opcode.DUP));
      newCode.add(loadIntegerConstant(ii, mgen));
      ClassDesc at = paramTypes[ii];
      if (at.isPrimitive()) {
        createPrimitiveWrapper(newCode, at, param_index, mgen);
      } else { // it's a reference of some sort
        newCode.add(LoadInstruction.of(TypeKind.REFERENCE, param_index));
      }
      newCode.add(ArrayStoreInstruction.of(Opcode.AASTORE));
      param_index += TypeKind.from(at).slotSize();
    }

    // If this is an exit, push the return value and line number.
    // The return value is stored in the local "return__$trace2_val".
    // If the return value is a primitive, wrap it in the appropriate wrapper.
    if (methodToCall.equals("exit")) {
      ClassDesc ret_type = mgen.getReturnType();
      if (ret_type.equals(CD_void)) {
        newCode.add(ConstantInstruction.ofIntrinsic(Opcode.ACONST_NULL));
      } else {
        LocalVariable return_local = getReturnLocal(mgen, ret_type, minfo);
        if (ret_type.isPrimitive()) {
          createPrimitiveWrapper(newCode, ret_type, return_local.slot(), mgen);
        } else {
          newCode.add(LoadInstruction.of(TypeKind.REFERENCE, return_local.slot()));
        }
      }

      // Push the line number.
      newCode.add(loadIntegerConstant(line, mgen));
    }

    ClassDesc objectArrayCD = CD_Object.arrayType(1);
    MethodTypeDesc methodArgs;
    // Call the specified method.
    if (methodToCall.equals("exit")) {
      methodArgs =
          MethodTypeDesc.of(CD_void, CD_Object, CD_int, CD_int, objectArrayCD, CD_Object, CD_int);
    } else {
      methodArgs = MethodTypeDesc.of(CD_void, CD_Object, CD_int, CD_int, objectArrayCD);
    }
    MethodRefEntry mre = mgen.getPoolBuilder().methodRefEntry(runtimeCD, methodToCall, methodArgs);
    newCode.add(InvokeInstruction.of(Opcode.INVOKESTATIC, mre));
  }

  /** Used for processing a switch instruction. */
  private record ModifiedSwitchInfo(Label modifiedTarget, List<SwitchCase> modifiedCaseList) {}

  /**
   * Checks to see if the instruction targets the method's CodeModel startLabel (held in
   * minfo.oldStartLabel). If so, it replaces the target with the minfo.entryLabel. Unfortunately,
   * the classfile API does not allow us to simply replace the label, we have to replace the entire
   * instruction. Note that oldStartLabel may be null, but that is okay as any comparison to it will
   * fail and we will do nothing.
   *
   * @param inst the instruction to check
   * @param minfo for the given method's code
   * @return the original instruction or its replacement
   */
  private CodeElement retargetStartLabel(CodeElement inst, MethodGen24.MInfo24 minfo) {
    ModifiedSwitchInfo info;
    switch (inst) {
      case BranchInstruction bi -> {
        if (bi.target().equals(minfo.oldStartLabel)) {
          return BranchInstruction.of(bi.opcode(), minfo.entryLabel);
        }
      }
      case ExceptionCatch ec -> {
        if (ec.tryStart().equals(minfo.oldStartLabel)) {
          return ExceptionCatch.of(ec.handler(), minfo.entryLabel, ec.tryEnd(), ec.catchType());
        }
      }
      case LookupSwitchInstruction ls -> {
        info = retargetStartLabel(ls.defaultTarget(), ls.cases(), minfo);
        if (info != null) {
          return LookupSwitchInstruction.of(info.modifiedTarget, info.modifiedCaseList);
        }
      }
      case TableSwitchInstruction ts -> {
        info = retargetStartLabel(ts.defaultTarget(), ts.cases(), minfo);
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
   * Checks to see if a switch instruction's default target or any of the case targets refers to
   * {@code minfo.oldStartLabel}. If so, replace those targets with minfo.entryLabel, and return the
   * result in a ModifiedSwitchInfo. Otherwise, return null.
   *
   * @param defaultTarget the default target for the switch instruction
   * @param caseList the case list for the switch instruction
   * @param minfo for the given method's code
   * @return a ModifiedSwitchInfo with the changed values, or null if no changes
   */
  private @Nullable ModifiedSwitchInfo retargetStartLabel(
      Label defaultTarget, List<SwitchCase> caseList, MethodGen24.MInfo24 minfo) {
    Label modifiedTarget;
    boolean modified = false;

    if (defaultTarget.equals(minfo.oldStartLabel)) {
      modifiedTarget = minfo.entryLabel;
      modified = true;
    } else {
      modifiedTarget = defaultTarget;
    }

    List<SwitchCase> newCaseList = new ArrayList<SwitchCase>();
    for (SwitchCase item : caseList) {
      if (item.target().equals(minfo.oldStartLabel)) {
        newCaseList.add(SwitchCase.of(item.caseValue(), minfo.entryLabel));
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
   * Creates code to put the local var/param at the specified var_index into a wrapper appropriate
   * for prim_type. prim_type must be a primitive type (Type.INT, Type.FLOAT, etc.). The wrappers
   * are those defined in daikon.chicory.Runtime.
   *
   * <p>Adds the created code to {@code newCode}.
   *
   * <p>The stack is left with a pointer to the newly created wrapper at the top.
   *
   * @param newCode an instruction list to append the wrapper code to
   * @param prim_type the primitive type of the local variable or parameter
   * @param var_index the offset into the local stack of the variable or parameter
   */
  private void createPrimitiveWrapper(
      List<CodeElement> newCode, ClassDesc prim_type, int var_index, MethodGen24 mgen) {

    String wrapperClassName;
    TypeKind typeKind;
    switch (prim_type.displayName()) {
      case "boolean":
        typeKind = TypeKind.BOOLEAN;
        wrapperClassName = "BooleanWrap";
        break;
      case "byte":
        typeKind = TypeKind.BYTE;
        wrapperClassName = "ByteWrap";
        break;
      case "char":
        typeKind = TypeKind.CHAR;
        wrapperClassName = "CharWrap";
        break;
      case "double":
        typeKind = TypeKind.DOUBLE;
        wrapperClassName = "DoubleWrap";
        break;
      case "float":
        typeKind = TypeKind.FLOAT;
        wrapperClassName = "FloatWrap";
        break;
      case "int":
        typeKind = TypeKind.INT;
        wrapperClassName = "IntWrap";
        break;
      case "long":
        typeKind = TypeKind.LONG;
        wrapperClassName = "LongWrap";
        break;
      case "short":
        typeKind = TypeKind.SHORT;
        wrapperClassName = "ShortWrap";
        break;
      default:
        throw new Error("unexpected type " + prim_type);
    }

    String classname = runtime_classname + "$" + wrapperClassName;
    ClassDesc wrapperCD = ClassDesc.of(classname);
    newCode.add(NewObjectInstruction.of(mgen.getPoolBuilder().classEntry(wrapperCD)));
    newCode.add(StackInstruction.of(Opcode.DUP));
    newCode.add(LoadInstruction.of(typeKind, var_index));
    MethodRefEntry mre =
        mgen.getPoolBuilder()
            .methodRefEntry(wrapperCD, "<init>", MethodTypeDesc.of(CD_void, prim_type));
    newCode.add(InvokeInstruction.of(Opcode.INVOKESPECIAL, mre));
  }

  /**
   * Returns true iff mgen is a constructor.
   *
   * @param mgen describes the given method
   * @return true iff mgen is a constructor
   */
  @Pure
  private boolean isConstructor(MethodGen24 mgen) {
    if (mgen.getName().equals("<init>")) {
      debugInstrument.log("isConstructor(%s) => true%n", mgen.getName());
      return true;
    } else {
      return false;
    }
  }

  /**
   * Returns the ClassGetName form of a ClassDesc. For a non-array type, the binary name; for an
   * array type, a format like the FieldDescriptor field descriptor, but using "." where the field
   * descriptor uses "/".
   *
   * @param t a type
   * @return the class name in ClassGetName format
   */
  @SuppressWarnings("signature") // conversion method
  public static @ClassGetName String classDescToClassGetName(ClassDesc t) {
    String s = t.descriptorString();
    if (s.startsWith("[")) {
      return s.replace('/', '.');
    } else {
      return convertDescriptorToFqBinaryName(s);
    }
  }

  /**
   * Returns an array of fully qualified names, one for each of mgen's parameter types.
   *
   * @param mgen describes the given method
   * @return an array of strings, each corresponding to mgen's parameter types
   */
  @SuppressWarnings("signature") // conversion method
  private @BinaryName String[] getFqBinaryNameParameterTypes(MethodGen24 mgen) {
    return ArraysPlume.mapArray(
        paramType -> convertDescriptorToFqBinaryName(paramType.descriptorString()),
        mgen.getParameterTypes(),
        String.class);
  }

  /**
   * Creates a MethodInfo corresponding to {@code mgen}.
   *
   * @param classInfo class containing the method
   * @param mgen method to inspect
   * @return a new MethodInfo for the method, or null if the method should not be instrumented
   */
  private @Nullable MethodInfo create_method_info(ClassInfo classInfo, MethodGen24 mgen) {

    // Get the parameter names for this method.
    String[] paramNames = mgen.getParameterNames();
    LocalVariable[] lvs = mgen.getOriginalLocalVariables();
    int param_offset = 1;
    if (mgen.isStatic()) {
      param_offset = 0;
    }

    if (debugInstrument.enabled) {
      debugInstrument.log("create_method_info for: %s%n", classInfo.class_name);
      debugInstrument.log("number of parameters: %s%n", paramNames.length);
      for (String paramName : paramNames) {
        debugInstrument.log("param name: %s%n", paramName);
      }
    }

    int lv_start = 0;
    // If this is an inner class constructor, then its first parameter is
    // the outer class constructor.  I need to detect this and adjust the
    // parameter names appropriately.  This check is ugly.
    if (mgen.getName().equals("<init>") && mgen.getParameterTypes().length > 0) {
      int dollarPos = mgen.getClassName().lastIndexOf('$');
      @SuppressWarnings("signature:assignment") // need JDK annotations
      @FieldDescriptor String arg0Fd = mgen.getParameterType(0).descriptorString();
      String arg0Name = convertDescriptorToFqBinaryName(arg0Fd);
      if (dollarPos >= 0
          &&
          // Type of first parameter is classname up to the "$".
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

    for (int i = lv_start; i < paramNames.length; i++) {
      if ((i + param_offset) < lvs.length) {
        paramNames[i] = lvs[i + param_offset].name().stringValue();
      }
    }

    if (debugInstrument.enabled) {
      debugInstrument.log("create_method_info part 2%n");
      debugInstrument.log("number of parameters: %s%n", paramNames.length);
      for (String paramName : paramNames) {
        debugInstrument.log("param name: %s%n", paramName);
      }
    }

    boolean shouldInclude = false;

    // See if we should track the entry point. Further below are more tests that set shouldInclude.
    if (!shouldIgnore(
        classInfo.class_name,
        mgen.getName(),
        DaikonWriter.methodEntryName(
            classInfo.class_name,
            getFqBinaryNameParameterTypes(mgen),
            // It looks like DaikonWriter.methodEntryName does not use the mgen.toString() argument.
            mgen.toString(),
            mgen.getName()))) {
      shouldInclude = true;
    }
    // Get the parameter types for this method.
    ClassDesc[] paramTypes = mgen.getParameterTypes();
    @ClassGetName String[] arg_type_strings = new @ClassGetName String[paramTypes.length];
    for (int i = 0; i < paramTypes.length; i++) {
      arg_type_strings[i] = classDescToClassGetName(paramTypes[i]);
    }

    if (debugInstrument.enabled) {
      debugInstrument.log("create_method_info part 3%n");
      debugInstrument.log("number of parameters: %s%n", paramNames.length);
      for (int ii = 0; ii < paramTypes.length; ii++) {
        debugInstrument.log("param type: %s%n", arg_type_strings[ii]);
      }
    }

    // Loop through each instruction and find the line number for each return opcode.
    List<Integer> exit_line_numbers = new ArrayList<>();

    // Tells whether each exit loc in the method is included or not (based on filters).
    List<Boolean> isIncluded = new ArrayList<>();

    debugInstrument.log("Looking for exit points in %s%n", mgen.getName());
    List<CodeElement> il = mgen.getInstructionList();
    int line_number = 0;
    int prev_line_number = 0;

    for (CodeElement inst : il) {
      boolean foundLine = false;

      if (inst instanceof LineNumber ln) {
        line_number = ln.line();
        foundLine = true;
      }

      if (inst instanceof ReturnInstruction) {
        debugInstrument.log("Exit at line %d%n", line_number);

        // Only do incremental lines if we haven't seen a line number since the last return.
        if (line_number == prev_line_number && foundLine == false) {
          debugInstrument.log("Could not find line %d%n", line_number);
          line_number++;
        }

        prev_line_number = line_number;

        if (!shouldIgnore(
            classInfo.class_name,
            mgen.getName(),
            DaikonWriter.methodExitName(
                classInfo.class_name,
                getFqBinaryNameParameterTypes(mgen),
                mgen.toString(),
                mgen.getName(),
                line_number))) {
          shouldInclude = true;
          exit_line_numbers.add(line_number);

          isIncluded.add(true);
        } else {
          isIncluded.add(false);
        }
      }
    }

    if (shouldInclude) {
      return new MethodInfo(
          classInfo, mgen.getName(), paramNames, arg_type_strings, exit_line_numbers, isIncluded);
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
  private static boolean isChicoryClass(@InternalForm String classname) {

    if (classname.startsWith("daikon/chicory/")
        && !classname.equals("daikon/chicory/ChicoryTest")) {
      return true;
    }
    if (classname.equals("daikon/PptTopLevel$PptType")) {
      return true;
    }
    if (classname.startsWith("daikon/plumelib/")) {
      return true;
    }
    return false;
  }

  /**
   * Format a field descriptor for output. In addition, this method includes some support for
   * signatures as well. Signatures are a superset of descriptors and may include type variables and
   * parameterized types. However, this method does not support type variables and type arguments
   * with wildcard bounds.
   *
   * <p>The output format is an extension of binary name format that includes primitives and arrays.
   * It is the same as a fully qualified name, but using “$” instead of “.” to separate nested
   * classes from their enclosing classes.
   *
   * @param descriptor the descriptor to format
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

    // Convert primitive types.
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
      case 'L': // Class type, starts with 'L' and ends with ';'
        result.append(convertClassTypeDescriptorToFqBinaryName(descriptor));
        break;
      default:
        throw new IllegalArgumentException("Invalid descriptor: " + descriptor);
    }

    // Append array brackets if applicable.
    for (int i = 0; i < arrayDimensions; i++) {
      result.append("[]");
    }

    return result.toString();
  }

  /**
   * Format a class name that may contain type arguments.
   *
   * @param descriptor the class descriptor to format
   * @return a @FqBinaryName formatted string
   */
  @SuppressWarnings("signature") // conversion method
  private static @FqBinaryName String convertClassTypeDescriptorToFqBinaryName(String descriptor) {
    StringBuilder result = new StringBuilder();
    int genericStart = descriptor.indexOf('<');
    int genericEnd = descriptor.lastIndexOf('>');
    int endOfBaseType = descriptor.indexOf(';');

    if (genericStart > 0 && genericEnd > genericStart) {
      // Base type with generics.
      String baseType = descriptor.substring(1, genericStart).replace('/', '.');
      result.append(baseType).append('<');
      String genericPart = descriptor.substring(genericStart + 1, genericEnd);
      result.append(convertTypeArgumentsToBinaryNames(genericPart));
      result.append('>');
    } else if (endOfBaseType > 0) {
      // Regular object type.
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
  private static String convertTypeArgumentsToBinaryNames(String genericPart) {

    if (genericPart.equals("*")) {
      return "?";
    }

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

  /**
   * Convert the value of a constant static field to a string. {@link #instrumentClass} saves these
   * values for use in the Chicory runtime.
   *
   * @param item the constant to format
   * @return a string containing the constant's value
   */
  private final String formatConstantDesc(ConstantDesc item) {
    try {
      return item.resolveConstantDesc(MethodHandles.lookup()).toString();
    } catch (Exception e) {
      System.err.printf("Unexpected error %s getting constant value for: %s%n", e, item);
      return "";
    }
  }

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
   * Build a load constant instruction for values of type int, short, char, byte.
   *
   * @param value to be pushed
   * @return a push instruction
   */
  protected CodeElement loadIntegerConstant(final int value, MethodGen24 mgen) {
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
                  : buildLDCInstruction(mgen.getPoolBuilder().intEntry(value));
    };
  }
}
