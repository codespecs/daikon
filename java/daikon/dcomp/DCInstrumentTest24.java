package daikon.dcomp;

import static java.lang.classfile.Opcode.INVOKEVIRTUAL;
import static java.lang.constant.ConstantDescs.CD_int;
import static java.lang.constant.ConstantDescs.CD_void;
import static java.nio.charset.StandardCharsets.UTF_8;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import daikon.chicory.ClassInfo;
import daikon.chicory.Runtime;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintStream;
import java.io.UncheckedIOException;
import java.lang.classfile.Annotation;
import java.lang.classfile.Attribute;
import java.lang.classfile.ClassFile;
import java.lang.classfile.ClassHierarchyResolver;
import java.lang.classfile.ClassModel;
import java.lang.classfile.ClassTransform;
import java.lang.classfile.CodeElement;
import java.lang.classfile.CodeModel;
import java.lang.classfile.Label;
import java.lang.classfile.MethodModel;
import java.lang.classfile.MethodTransform;
import java.lang.classfile.attribute.CodeAttribute;
import java.lang.classfile.attribute.RuntimeVisibleAnnotationsAttribute;
import java.lang.classfile.attribute.StackMapFrameInfo;
import java.lang.classfile.attribute.StackMapTableAttribute;
import java.lang.classfile.instruction.InvokeInstruction;
import java.lang.constant.ClassDesc;
import java.lang.constant.MethodTypeDesc;
import java.lang.reflect.AccessFlag;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Supplier;
import java.util.regex.Pattern;
import org.apache.bcel.classfile.ClassParser;
import org.apache.bcel.classfile.JavaClass;
import org.apache.bcel.classfile.Method;
import org.checkerframework.checker.interning.qual.Interned;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.checkerframework.checker.signature.qual.BinaryName;
import org.junit.Test;

/**
 * Unit tests for {@link DCInstrument24}.
 *
 * <p>Note that this test must be located in daikon.dcomp rather than daikon.test.dcomp as it needs
 * to access protected fields of DCRuntime and Premain.
 */
public final class DCInstrumentTest24 {

  /** Create a new DCInstrumentTest24. */
  public DCInstrumentTest24() {}

  /** A class that DCInstrument24 can instrument without error. */
  static class SimpleClass {
    /** A field. */
    int field;

    /** Create a new SimpleClass. */
    SimpleClass() {}

    /**
     * Adds this object's field to its argument.
     *
     * @param x the value to add to the field
     * @return the sum of {@code x} and the field
     */
    int increment(int x) {
      return x + field;
    }
  }

  /**
   * Number of no-op instructions that appear before each label in the class built by {@link
   * #badClassBytes}. This makes the indices in the worklist larger than the number of instructions
   * in any method of {@link SimpleClass}.
   */
  private static final int PADDING = 400;

  /**
   * Returns the bytes of a class whose sole method has two branch targets, one of which is reached
   * with two different operand stacks. Instrumenting the method throws {@code DynCompError} after
   * both branch targets have been added to {@code DCInstrument24.worklist}.
   *
   * <p>Such bytecode does not pass the JVM's verifier, so it cannot be produced by a compiler. This
   * method writes the StackMapTable by hand because {@code java.lang.classfile} creates a label for
   * each StackMapTable entry, and DCInstrument24 only simulates branches whose target is a label.
   *
   * @return the bytes of a class that DCInstrument24 cannot instrument
   */
  private static byte[] badClassBytes() {
    ClassFile classFile = ClassFile.of(ClassFile.StackMapsOption.DROP_STACK_MAPS);
    return classFile.build(
        ClassDesc.of("BadStackMerge"),
        classBuilder -> {
          classBuilder.withFlags(ClassFile.ACC_PUBLIC | ClassFile.ACC_SUPER);
          classBuilder.withMethodBody(
              "bad",
              MethodTypeDesc.of(CD_void),
              ClassFile.ACC_PUBLIC | ClassFile.ACC_STATIC,
              codeBuilder -> {
                Label label1 = codeBuilder.newLabel();
                Label label2 = codeBuilder.newLabel();
                // Reach label1 with an empty operand stack.
                codeBuilder.iconst_0();
                codeBuilder.ifeq(label1);
                // Add label2 to the worklist; it is never simulated.
                codeBuilder.iconst_0();
                codeBuilder.ifeq(label2);
                // Reach label1 with one item on the operand stack; this is the error.
                codeBuilder.iconst_5();
                codeBuilder.goto_(label1);
                for (int i = 0; i < PADDING; i++) {
                  codeBuilder.nop();
                }
                codeBuilder.labelBinding(label1);
                codeBuilder.return_();
                for (int i = 0; i < PADDING; i++) {
                  codeBuilder.nop();
                }
                codeBuilder.labelBinding(label2);
                codeBuilder.return_();
                codeBuilder.with(
                    StackMapTableAttribute.of(
                        List.of(
                            StackMapFrameInfo.of(label1, List.of(), List.of()),
                            StackMapFrameInfo.of(label2, List.of(), List.of()))));
              });
        });
  }

  /**
   * Returns the class loader of this class.
   *
   * @return the class loader of this class
   */
  private static ClassLoader classLoader() {
    ClassLoader loader = DCInstrumentTest24.class.getClassLoader();
    assert loader != null : "@AssumeAssertion(nullness): this class is not in the boot classpath";
    return loader;
  }

  /**
   * Returns the bytes of the given class.
   *
   * @param binaryName the name of the class to read
   * @return the bytes of the given class
   * @throws IOException if the class file cannot be read
   */
  private static byte[] classBytes(@BinaryName String binaryName) throws IOException {
    String resourceName = binaryName.replace('.', '/') + ".class";
    try (InputStream is = classLoader().getResourceAsStream(resourceName)) {
      assert is != null : "@AssumeAssertion(nullness): " + resourceName + " is on the classpath";
      return is.readAllBytes();
    }
  }

  /**
   * Instruments the given class, as {@code Instrument24.transform} does.
   *
   * @param bytes the bytes of the class to instrument
   * @param binaryName the name of the class to instrument
   * @return the instrumented class, or null if the class was not instrumented
   */
  private static byte @Nullable [] instrument(byte[] bytes, @BinaryName String binaryName) {
    ClassLoader loader = classLoader();
    ClassFile classFile =
        ClassFile.of(
            ClassFile.ClassHierarchyResolverOption.of(
                ClassHierarchyResolver.ofResourceParsing(loader)));
    ClassModel classModel = classFile.parse(bytes);
    ClassInfo classInfo = new ClassInfo(binaryName, loader);
    DCInstrument24 dci = new DCInstrument24(classFile, classModel, false);
    return dci.instrument(classInfo);
  }

  /**
   * Tests that a class that cannot be instrumented does not prevent instrumentation of the classes
   * that are processed after it. When instrumentation of a method fails, items remain in {@code
   * DCInstrument24.worklist}; each such item contains an index into that method's instruction list,
   * so it is meaningless for any other method.
   *
   * @throws IOException if a class file cannot be read
   */
  @SuppressWarnings({"nullness:argument", "nullness:unneeded.suppression"}) // TODO: bug in CF?
  @Test
  public void staleWorklistDoesNotAffectLaterClasses() throws IOException {
    boolean savedJdkInstrumented = Premain.jdk_instrumented;
    @BinaryName @Interned String savedInstrumentationInterface = DCRuntime.instrumentation_interface;
    PrintStream savedOut = System.out;
    PrintStream savedErr = System.err;
    try {
      // Instrument as if the JDK were not instrumented, so that the marker and runtime classes
      // are the ones on the classpath.
      Premain.jdk_instrumented = false;
      DCRuntime.instrumentation_interface = "daikon.dcomp.DCompInstrumented";

      @BinaryName String simpleName = "daikon.dcomp.DCInstrumentTest24$SimpleClass";
      byte[] simpleBytes = classBytes(simpleName);
      assertNotNull("cannot instrument " + simpleName, instrument(simpleBytes, simpleName));

      // Instrumenting this class fails, which leaves items in DCInstrument24.worklist.
      // Discard the diagnostics about the expected failure.
      PrintStream discard = new PrintStream(new ByteArrayOutputStream(), false, UTF_8);
      System.setOut(discard);
      System.setErr(discard);
      byte[] badResult;
      try {
        badResult = instrument(badClassBytes(), "BadStackMerge");
      } finally {
        System.setOut(savedOut);
        System.setErr(savedErr);
      }
      assertNull("BadStackMerge should not be instrumented", badResult);

      assertNotNull(
          "cannot instrument " + simpleName + " after a failed instrumentation",
          instrument(simpleBytes, simpleName));
    } finally {
      Premain.jdk_instrumented = savedJdkInstrumented;
      DCRuntime.instrumentation_interface = savedInstrumentationInterface;
    }
  }

  /**
   * A package-private class with a tracked method ({@link #increment}) followed by an untracked
   * method ({@link #notTracked}, excluded via {@link daikon.chicory.Runtime#ppt_omit_pattern} in
   * {@link #trackedMethodPromotionSurvivesLaterUntrackedMethod}). Used to verify that once tracking
   * one method promotes the class to public, processing a later untracked method does not revert
   * that promotion.
   */
  static class MixedTrackingClass {
    /** A field. */
    int field;

    /** Create a new MixedTrackingClass. */
    MixedTrackingClass() {}

    /**
     * A tracked method whose processing should promote the class to public.
     *
     * @param x the value to add to the field
     * @return the sum of {@code x} and the field
     */
    int increment(int x) {
      return x + field;
    }

    /**
     * An untracked method, processed after {@link #increment}.
     *
     * @param x the value to subtract from the field
     * @return the difference of the field and {@code x}
     */
    int notTracked(int x) {
      return field - x;
    }
  }

  /**
   * Tests that once processing a tracked method promotes a package-private class to public,
   * processing a later, untracked method does not revert the class back to package-private. In
   * {@link MixedTrackingClass}, {@link MixedTrackingClass#increment} is tracked and {@link
   * MixedTrackingClass#notTracked} is not (excluded here via {@link
   * daikon.chicory.Runtime#ppt_omit_pattern}); {@code notTracked} is declared last so it is
   * processed last.
   *
   * @throws IOException if the class file for {@link MixedTrackingClass} cannot be read
   */
  @Test
  public void trackedMethodPromotionSurvivesLaterUntrackedMethod() throws IOException {
    List<Pattern> savedOmitPattern = Runtime.ppt_omit_pattern;
    @BinaryName String savedInstrumentationInterface = DCRuntime.instrumentation_interface;
    try {
      Runtime.ppt_omit_pattern = List.of(Pattern.compile("notTracked"));
      DCRuntime.instrumentation_interface = "daikon.dcomp.DCompInstrumented";

      @BinaryName String classname = "daikon.dcomp.DCInstrumentTest24$MixedTrackingClass";
      byte[] original = classBytes(classname);
      ClassModel originalModel = ClassFile.of().parse(original);
      assertFalse(
          classname + " must not already be public for this test to be meaningful",
          originalModel.flags().has(AccessFlag.PUBLIC));

      byte[] instrumented = instrument(original, classname);
      assert instrumented != null : "@AssumeAssertion(nullness)";

      ClassModel instrumentedModel = ClassFile.of().parse(instrumented);
      assertTrue(
          classname + " should be public after instrumentation because it has a tracked method",
          instrumentedModel.flags().has(AccessFlag.PUBLIC));
    } finally {
      Runtime.ppt_omit_pattern = savedOmitPattern;
      DCRuntime.instrumentation_interface = savedInstrumentationInterface;
    }
  }

  /** A small class that the tests instrument. */
  public static class Sample {

    /** An arbitrary value. */
    int value;

    /** Creates a new Sample. */
    public Sample() {
      value = 0;
    }

    /**
     * Adds to {@link #value}.
     *
     * @param x the amount to add
     * @return the new value
     */
    public int add(int x) {
      value += x;
      return value;
    }
  }

  /**
   * A superclass with a parameterized constructor, so that {@link TwoConstructors} can read a field
   * in the argument expression of its {@code super()} call -- that is, before the superclass
   * constructor has run.
   */
  public static class Base {

    /** An arbitrary value. */
    int base;

    /**
     * Creates a new Base.
     *
     * @param base the value to store
     */
    public Base(int base) {
      this.base = base;
    }
  }

  /**
   * A class with two constructors, the second of which reads a field before calling its superclass
   * constructor. Used by {@link #constructorInitializedStateDoesNotLeakBetweenMethods}.
   */
  public static class TwoConstructors extends Base {

    /** Creates a new TwoConstructors. This constructor reads no fields before {@code super()}. */
    public TwoConstructors() {
      super(0);
    }

    /**
     * Creates a new TwoConstructors, reading {@code s.value} before {@code super()} runs.
     *
     * @param s supplies the value to store
     */
    public TwoConstructors(Sample s) {
      super(s.value);
    }
  }

  /**
   * Tests that a class instrumented by {@link DCInstrument24#instrument_jdk_class} calls the shadow
   * runtime class {@code java.lang.DCRuntime} rather than {@code daikon.dcomp.DCRuntime}. A class
   * in a pre-instrumented {@code java.base} module may not refer to anything outside {@code
   * java.base}. This must hold even when the DCInstrument24 constructor chose {@code
   * daikon.dcomp.DCRuntime}, which it does whenever {@code Premain.jdk_instrumented} is false.
   *
   * @throws IOException if the class file for {@link Sample} cannot be read
   */
  @Test
  public void testJdkClassCallsShadowRuntime() throws IOException {
    boolean savedJdkInstrumented = Premain.jdk_instrumented;
    @BinaryName String savedInstrumentationInterface = DCRuntime.instrumentation_interface;
    Premain.jdk_instrumented = false;
    // BuildJDK24 sets this static field before each class it instruments.
    DCRuntime.instrumentation_interface = "daikon.dcomp.DCompInstrumented";
    byte[] instrumented;
    try {
      instrumented = instrumentAsJdkClass();
    } finally {
      Premain.jdk_instrumented = savedJdkInstrumented;
      DCRuntime.instrumentation_interface = savedInstrumentationInterface;
    }
    Set<String> invoked = invokedClasses(instrumented);
    assertTrue(
        "instrumented class does not call java/lang/DCRuntime: " + invoked,
        invoked.contains("java/lang/DCRuntime"));
    assertFalse(
        "instrumented class calls daikon/dcomp/DCRuntime: " + invoked,
        invoked.contains("daikon/dcomp/DCRuntime"));
  }

  /**
   * Instruments {@link Sample} as if it were a JDK class.
   *
   * @return the instrumented bytes of {@link Sample}
   * @throws IOException if the class file for {@link Sample} cannot be read
   */
  private byte[] instrumentAsJdkClass() throws IOException {
    @SuppressWarnings("signature:assignment") // the name of a nested class
    @BinaryName String classname = Sample.class.getName();
    InputStream sampleStream =
        DCInstrumentTest24.class.getResourceAsStream("DCInstrumentTest24$Sample.class");
    if (sampleStream == null) {
      throw new Error("cannot find the class file for " + classname);
    }
    byte[] original;
    try (InputStream is = sampleStream) {
      original = is.readAllBytes();
    }
    return instrumentAsJdkClass(original);
  }

  /**
   * Instruments the given definition of {@link Sample} as if it were a JDK class.
   *
   * @param original the class-file bytes to instrument
   * @return the instrumented bytes
   */
  private byte[] instrumentAsJdkClass(byte[] original) {
    ClassFile classFile = ClassFile.of();
    ClassModel classModel = classFile.parse(original);
    ClassInfo classInfo = new ClassInfo(sampleClassName(), classLoader());
    DCInstrument24 dci = new DCInstrument24(classFile, classModel, true);
    // instrument_jdk_class throws rather than returning null if it cannot instrument the class.
    return dci.instrument_jdk_class(classInfo);
  }

  /** Tests that removing a blacklisted annotation does not remove its permitted siblings. */
  @Test
  public void preservesAnnotationsThatAreNotBlacklisted() throws IOException {
    @BinaryName String savedInstrumentationInterface = DCRuntime.instrumentation_interface;
    try {
      DCRuntime.instrumentation_interface = "daikon.dcomp.DCompInstrumented";
      ClassFile classFile = ClassFile.of();
      ClassModel sample = classFile.parse(classBytes(sampleClassName()));
      byte[] annotated =
          classFile.transformClass(
              sample,
              ClassTransform.transformingMethods(
                  method -> method.methodName().stringValue().equals(SMALL_METHOD),
                  MethodTransform.endHandler(
                      methodBuilder -> {
                        methodBuilder.with(
                            RuntimeVisibleAnnotationsAttribute.of(
                                Annotation.of(ClassDesc.of("java.lang.Deprecated")),
                                Annotation.of(
                                    ClassDesc.of(
                                        "jdk.internal.vm.annotation.IntrinsicCandidate"))));
                      })));

      MethodModel instrumentedMethod =
          instrumentedCopy(classFile.parse(instrumentAsJdkClass(annotated)), SMALL_METHOD);
      Set<String> annotations = new HashSet<>();
      for (Attribute<?> attribute : instrumentedMethod.attributes()) {
        if (attribute instanceof RuntimeVisibleAnnotationsAttribute rvaa) {
          for (Annotation annotation : rvaa.annotations()) {
            annotations.add(annotation.className().stringValue());
          }
        }
      }
      assertEquals(Set.of("Ljava/lang/Deprecated;"), annotations);
    } finally {
      DCRuntime.instrumentation_interface = savedInstrumentationInterface;
    }
  }

  /** Name of the method that {@link #oversizedClassBytes} adds to {@link Sample}. */
  private static final String OVERSIZED_METHOD = "tooBig";

  /** Name of the {@link Sample} method that instruments normally. */
  private static final String SMALL_METHOD = "add";

  /**
   * Number of {@code iload_1; iconst_1; iadd; istore_1} groups in {@link #OVERSIZED_METHOD}. Each
   * group is 4 bytes, so the uninstrumented method is well under the JVM's 64K code-size limit, but
   * instrumentation adds several DCRuntime calls per group, which pushes the instrumented form over
   * it.
   */
  private static final int OVERSIZED_GROUPS = 6000;

  /**
   * Number of {@code iload_1; iconst_1; iadd; istore_1} groups in {@link #OVERSIZED_METHOD} for
   * {@link #testHugeMethodUsesForwardingStub}. The method is 4 * HUGE_GROUPS + 2 bytes long, which
   * is just under the JVM's 64K code-size limit -- so close that even the handful of bytes of
   * tag-stack bookkeeping that an oversized method is given does not fit.
   */
  private static final int HUGE_GROUPS = 16382;

  /**
   * Number of {@code iload_1; iconst_1; iadd; istore_1} groups in {@link #OVERSIZED_METHOD} for
   * {@link #testOversizedJunitFallbackRebuildsStackMap}, which uses the {@code branching} form of
   * {@link #oversizedClassBytes}. That form adds 16 bytes, so the method is 4 *
   * HUGE_BRANCHING_GROUPS + 16 bytes long: just under the JVM's 64K code-size limit, and too close
   * to it for the handful of bytes of tag-stack bookkeeping that an oversized method is given.
   */
  private static final int HUGE_BRANCHING_GROUPS = 16379;

  /**
   * Returns the bytes of {@link Sample} with an added method, {@link #OVERSIZED_METHOD}, whose
   * instrumented form exceeds the JVM's 64K code-size limit. The method is added to a real class
   * rather than a synthetic one because DCInstrument24 resolves the class being instrumented, and
   * its superclasses, from the classpath.
   *
   * @param groups the number of 4-byte instruction groups in the added method
   * @return the bytes of {@link Sample} plus a method that is too large to instrument
   * @throws IOException if the class file for {@link Sample} cannot be read
   */
  private static byte[] oversizedClassBytes(int groups) throws IOException {
    return oversizedClassBytes(groups, false);
  }

  /**
   * Returns the bytes of {@link Sample} with an added method, {@link #OVERSIZED_METHOD}, whose
   * instrumented form exceeds the JVM's 64K code-size limit. The method is added to a real class
   * rather than a synthetic one because DCInstrument24 resolves the class being instrumented, and
   * its superclasses, from the classpath.
   *
   * @param groups the number of 4-byte instruction groups in the added method
   * @param branching if true, the added method uses locals beyond its parameters and contains a
   *     branch, so it has a StackMapTable and its instructions must be widened when a DCompMarker
   *     parameter displaces those locals; if false, the method is straight-line code that uses no
   *     local beyond its parameter
   * @return the bytes of {@link Sample} plus a method that is too large to instrument
   * @throws IOException if the class file for {@link Sample} cannot be read
   */
  private static byte[] oversizedClassBytes(int groups, boolean branching) throws IOException {
    ClassFile classFile = ClassFile.of();
    ClassModel classModel = classFile.parse(classBytes(sampleClassName()));
    return classFile.transformClass(
        classModel,
        ClassTransform.endHandler(
            classBuilder ->
                classBuilder.withMethodBody(
                    OVERSIZED_METHOD,
                    MethodTypeDesc.of(CD_int, CD_int),
                    ClassFile.ACC_PUBLIC,
                    codeBuilder -> {
                      if (branching) {
                        // Locals 2 and 3 are referenced by one-byte instructions; adding a
                        // DCompMarker parameter moves them to 3 and 4, which widens those
                        // instructions and thus shifts every later bytecode offset.  The branch
                        // target needs a stack map frame, whose offset therefore shifts too.
                        //
                        // All of this precedes the groups, and the branch skips only a few bytes,
                        // because BCEL cannot represent a branch offset or a stack map offset
                        // beyond 32767.
                        Label target = codeBuilder.newLabel();
                        codeBuilder.iconst_0();
                        codeBuilder.istore(2);
                        codeBuilder.iconst_0();
                        codeBuilder.istore(3);
                        codeBuilder.iload(3);
                        codeBuilder.ifge(target);
                        codeBuilder.iinc(3, 1);
                        codeBuilder.iinc(2, 1);
                        codeBuilder.labelBinding(target);
                        // Declare every local, including "this" and the parameter.  Without a
                        // complete LocalVariableTable, BCEL's fixLocalVariableTable runs the stack
                        // verifier over the whole method to infer the live range of each
                        // undeclared local, which takes minutes on a method this large.
                        Label start = codeBuilder.startLabel();
                        Label end = codeBuilder.endLabel();
                        codeBuilder.localVariable(
                            0, "this", ClassDesc.of(sampleClassName()), start, end);
                        codeBuilder.localVariable(1, "arg", CD_int, start, end);
                        codeBuilder.localVariable(2, "local2", CD_int, start, end);
                        codeBuilder.localVariable(3, "local3", CD_int, start, end);
                      }
                      for (int i = 0; i < groups; i++) {
                        codeBuilder.iload(1);
                        codeBuilder.iconst_1();
                        codeBuilder.iadd();
                        codeBuilder.istore(1);
                      }
                      codeBuilder.iload(1);
                      codeBuilder.ireturn();
                    })));
  }

  /**
   * Returns the binary name of {@link Sample}.
   *
   * @return the binary name of {@link Sample}
   */
  @SuppressWarnings("signature:return") // the name of a nested class
  private static @BinaryName String sampleClassName() {
    return Sample.class.getName();
  }

  /**
   * Tests that a method whose instrumented form exceeds the JVM's 64K code-size limit is emitted
   * without instrumentation, rather than causing the whole class to be abandoned. {@code
   * java.lang.classfile} does not report the oversized method until it serializes the class, so
   * {@link DCInstrument24#instrument_jdk_class} has to rebuild the class from scratch; this test
   * checks that the rebuild leaves the rest of the class instrumented, and that the oversized
   * method is not reported as skipped.
   *
   * <p>Also checks that the emitted method still maintains the tag stack. It carries the
   * DCompMarker parameter, so its callers use the calling convention for an instrumented method,
   * and the emitted method must honor that convention even though it does no comparability
   * tracking: it discards the tag pushed for the primitive argument and pushes one for the
   * primitive result. The method retains its original body to avoid adding an observable forwarding
   * frame.
   */
  @Test
  public void testOversizedMethodUsesOriginalBody() throws IOException {
    byte[] original = oversizedClassBytes(OVERSIZED_GROUPS);
    ClassFile classFile = ClassFile.of();
    ClassModel originalModel = classFile.parse(original);
    // The premise of this test is that only the *instrumented* method is too large.
    assertTrue(
        "uninstrumented " + OVERSIZED_METHOD + " is already over the code-size limit",
        codeLength(originalModel, OVERSIZED_METHOD) < 65536);

    ClassInfo classInfo = new ClassInfo(sampleClassName(), classLoader());
    DCInstrument24 dci = new DCInstrument24(classFile, classFile.parse(original), true);
    @BinaryName String savedInstrumentationInterface = DCRuntime.instrumentation_interface;
    // BuildJDK24 sets this static field before each class it instruments.
    DCRuntime.instrumentation_interface = "daikon.dcomp.DCompInstrumented";
    byte[] instrumented;
    try {
      // Skipping the oversized method prints a warning; discard it.
      instrumented = withDiagnosticsDiscarded(() -> dci.instrument_jdk_class(classInfo));
    } finally {
      DCRuntime.instrumentation_interface = savedInstrumentationInterface;
    }

    // The method is emitted with the DCompMarker signature, so it is still callable and does not
    // belong on the skipped_methods list, which names methods that are missing from the
    // instrumented class. DCInstrument treats oversized methods the same way.
    assertFalse(
        "oversized method reported as skipped: " + dci.get_skipped_methods(),
        dci.get_skipped_methods().stream().anyMatch(m -> m.contains(OVERSIZED_METHOD)));

    ClassModel instrumentedModel = classFile.parse(instrumented);
    // Exactly the tag-stack bookkeeping, and none of the instrumentation proper: no
    // create_tag_frame, no enter/exit.  OVERSIZED_METHOD takes one primitive argument and returns
    // a primitive, so it owes the caller one discarded argument tag and one pushed result tag.
    assertEquals(
        "oversized method does not maintain the tag stack",
        Set.of("discard_tag", "push_const"),
        runtimeCalls(instrumentedModel, OVERSIZED_METHOD));
    assertFalse(
        "oversized method adds an observable forwarding frame",
        callsOwnMethod(instrumentedModel, OVERSIZED_METHOD, MethodTypeDesc.of(CD_int, CD_int)));
    assertFalse(
        "rest of the class was not instrumented",
        runtimeCalls(instrumentedModel, SMALL_METHOD).isEmpty());
  }

  /**
   * Returns true if the instrumented copy of the named method -- the copy with a DCompMarker
   * parameter -- invokes a method of its own class with the same name and the given descriptor.
   * Used to check that an oversized method does not add a forwarding frame.
   *
   * @param classModel an instrumented class
   * @param methodName the name of the method to examine
   * @param target the descriptor of the method it should invoke
   * @return true if the instrumented copy invokes that method
   */
  private static boolean callsOwnMethod(
      ClassModel classModel, String methodName, MethodTypeDesc target) {
    String ownName = classModel.thisClass().asInternalName();
    for (CodeElement element : instrumentedCopy(classModel, methodName).code().orElseThrow()) {
      if (element instanceof InvokeInstruction invoke
          && invoke.owner().asInternalName().equals(ownName)
          && invoke.name().stringValue().equals(methodName)
          && invoke.typeSymbol().equals(target)) {
        return true;
      }
    }
    return false;
  }

  /**
   * Returns the instrumented copy of the named method, that is, the copy that has a DCompMarker
   * parameter.
   *
   * @param classModel an instrumented class
   * @param methodName the name of the method
   * @return the instrumented copy of the named method
   */
  private static MethodModel instrumentedCopy(ClassModel classModel, String methodName) {
    for (MethodModel method : classModel.methods()) {
      if (!method.methodName().stringValue().equals(methodName)) {
        continue;
      }
      List<ClassDesc> params = method.methodTypeSymbol().parameterList();
      if (!params.isEmpty() && params.get(params.size() - 1).displayName().equals("DCompMarker")) {
        return method;
      }
    }
    throw new Error("no instrumented copy of " + methodName);
  }

  /**
   * Tests that a method that is too large for even the minimal tag-stack bookkeeping is emitted as
   * a forwarding stub, rather than aborting the class. The bookkeeping is only a few bytes long,
   * but the method that needs it is by definition close to the JVM's 64K code-size limit, so the
   * copy that {@link DCInstrument24#copyOversizedMethod} makes can exceed the limit too.
   *
   * <p>The forwarding stub has the DCompMarker signature its callers expect, discards primitive
   * argument tags, invokes the unchanged original body with virtual dispatch, and produces the
   * primitive result tag.
   *
   * @throws IOException if the class file for {@link Sample} cannot be read
   * @throws ReflectiveOperationException if the generated classes cannot be loaded or invoked
   */
  @SuppressWarnings({"nullness:argument", "signedness:argument"}) // TODO
  @Test
  public void testHugeMethodUsesForwardingStub() throws IOException, ReflectiveOperationException {
    byte[] original = oversizedClassBytes(HUGE_GROUPS);
    ClassFile classFile = ClassFile.of();
    ClassModel originalModel = classFile.parse(original);
    // The premise of this test is that the uninstrumented method fits, but only just: adding the
    // tag-stack bookkeeping to it would not.
    int length = codeLength(originalModel, OVERSIZED_METHOD);
    assertTrue("uninstrumented " + OVERSIZED_METHOD + " does not fit: " + length, length <= 65535);
    assertTrue(
        "uninstrumented " + OVERSIZED_METHOD + " has room for the bookkeeping: " + length,
        length > 65535 - 8);

    ClassInfo classInfo = new ClassInfo(sampleClassName(), classLoader());
    boolean savedJdkInstrumented = Premain.jdk_instrumented;
    @BinaryName String savedInstrumentationInterface = DCRuntime.instrumentation_interface;
    // BuildJDK24 sets this static field before each class it instruments.
    Premain.jdk_instrumented = false;
    DCRuntime.instrumentation_interface = "daikon.dcomp.DCompInstrumented";
    DCInstrument24 dci = new DCInstrument24(classFile, classFile.parse(original), true);
    byte[] instrumented;
    try {
      // Skipping the oversized method prints a warning; discard it.
      instrumented = withDiagnosticsDiscarded(() -> dci.instrument_jdk_class(classInfo));
    } finally {
      Premain.jdk_instrumented = savedJdkInstrumented;
      DCRuntime.instrumentation_interface = savedInstrumentationInterface;
    }

    ClassModel instrumentedModel = classFile.parse(instrumented);
    assertEquals(
        "huge method's forwarding stub does not maintain the tag stack",
        Set.of("discard_tag", "push_const"),
        runtimeCalls(instrumentedModel, OVERSIZED_METHOD));
    assertTrue(
        "huge method has no forwarding stub",
        callsOwnMethod(instrumentedModel, OVERSIZED_METHOD, MethodTypeDesc.of(CD_int, CD_int)));
    assertEquals(
        "huge method's forwarding stub does not preserve virtual dispatch",
        INVOKEVIRTUAL,
        ownMethodCallOpcode(
            instrumentedModel, OVERSIZED_METHOD, MethodTypeDesc.of(CD_int, CD_int)));
    // The unchanged original body remains alongside the small DCompMarker forwarding overload.
    assertEquals(
        "huge method's body was changed", length, codeLength(instrumentedModel, OVERSIZED_METHOD));
    assertFalse(
        "huge method reported as skipped: " + dci.get_skipped_methods(),
        dci.get_skipped_methods().stream().anyMatch(m -> m.contains(OVERSIZED_METHOD)));
    assertFalse(
        "rest of the class was not instrumented",
        runtimeCalls(instrumentedModel, SMALL_METHOD).isEmpty());

    // Make the pre-instrumented-JDK class executable in this test JVM by redirecting its shadow
    // runtime calls to the ordinary DynComp runtime.
    ClassDesc runtimeClass = ClassDesc.of("daikon.dcomp.DCRuntime");
    byte[] executable =
        classFile.transformClass(
            instrumentedModel,
            ClassTransform.transformingMethodBodies(
                (codeBuilder, element) -> {
                  if (element instanceof InvokeInstruction invoke
                      && invoke.owner().asInternalName().equals("java/lang/DCRuntime")) {
                    codeBuilder.invoke(
                        invoke.opcode(),
                        runtimeClass,
                        invoke.name().stringValue(),
                        invoke.typeSymbol(),
                        false);
                  } else {
                    codeBuilder.with(element);
                  }
                }));

    @BinaryName String superclassName = sampleClassName();
    @SuppressWarnings("signature:assignment") // Appending a nested-class suffix preserves format.
    @BinaryName String subclassName = superclassName + "$DispatchOverride";
    byte[] subclass =
        classFile.build(
            ClassDesc.of(subclassName),
            classBuilder -> {
              classBuilder.withFlags(ClassFile.ACC_PUBLIC | ClassFile.ACC_SUPER);
              classBuilder.withSuperclass(ClassDesc.of(superclassName));
              classBuilder.withMethodBody(
                  "<init>",
                  MethodTypeDesc.of(CD_void),
                  ClassFile.ACC_PUBLIC,
                  codeBuilder -> {
                    codeBuilder.aload(0);
                    codeBuilder.invokespecial(
                        ClassDesc.of(superclassName), "<init>", MethodTypeDesc.of(CD_void));
                    codeBuilder.return_();
                  });
              classBuilder.withMethodBody(
                  OVERSIZED_METHOD,
                  MethodTypeDesc.of(CD_int, CD_int),
                  ClassFile.ACC_PUBLIC,
                  codeBuilder -> {
                    codeBuilder.bipush(42);
                    codeBuilder.ireturn();
                  });
            });
    ClassLoader loader =
        byteArrayClassLoader(Map.of(superclassName, executable, subclassName, subclass));
    Class<?> superclass = loader.loadClass(superclassName);
    Object receiver = loader.loadClass(subclassName).getConstructor().newInstance();

    Object[] tagFrame = DCRuntime.create_tag_frame("1");
    try {
      DCRuntime.push_const();
      @SuppressWarnings("nullness:argument") // The DCompMarker argument is always null.
      Object result =
          superclass
              .getMethod(OVERSIZED_METHOD, int.class, DCompMarker.class)
              .invoke(receiver, 1, null);
      assertNotNull("forwarding stub returned null", result);
      assertEquals("forwarding stub bypassed the subclass override", 42, result);
      DCRuntime.discard_tag(1);
    } finally {
      DCRuntime.normal_exit(tagFrame);
    }
  }

  /**
   * Tests the final oversized-method fallback for a JUnit class in the legacy BCEL instrumenter. A
   * JUnit method retains its original descriptor, so the fallback must put the bookkeeping in a
   * wrapper with that descriptor and move the unchanged body to a private marker overload.
   *
   * @throws IOException if the generated class cannot be parsed
   * @throws ReflectiveOperationException if the generated class cannot be loaded or invoked
   */
  @SuppressWarnings({"nullness:argument", "signedness:argument"}) // TODO
  @Test
  public void testHugeJunitMethodUsesForwardingStub()
      throws IOException, ReflectiveOperationException {
    ClassFile classFile = ClassFile.of();
    byte[] original = oversizedClassBytes(HUGE_GROUPS);
    byte[] junitClass =
        classFile.transformClass(
            classFile.parse(original),
            ClassTransform.transformingMethods(
                method -> method.methodName().stringValue().equals(OVERSIZED_METHOD),
                MethodTransform.endHandler(
                    methodBuilder ->
                        methodBuilder.with(
                            RuntimeVisibleAnnotationsAttribute.of(
                                Annotation.of(ClassDesc.of("org.junit.Test")))))));

    @BinaryName String className = sampleClassName();
    JavaClass parsed = new ClassParser(new ByteArrayInputStream(junitClass), className).parse();
    boolean wasJunitClass = DCInstrument.junitTestClasses.contains(className);
    boolean savedJdkInstrumented = Premain.jdk_instrumented;
    List<Pattern> savedOmitPattern = Runtime.ppt_omit_pattern;
    JavaClass instrumented;
    try {
      Premain.jdk_instrumented = false;
      Runtime.ppt_omit_pattern = List.of(Pattern.compile(Pattern.quote(className)));
      DCInstrument dci = new DCInstrument(parsed, false, classLoader());
      instrumented = withDiagnosticsDiscarded(dci::instrument);
    } finally {
      Premain.jdk_instrumented = savedJdkInstrumented;
      Runtime.ppt_omit_pattern = savedOmitPattern;
      if (!wasJunitClass) {
        DCInstrument.junitTestClasses.remove(className);
      }
    }

    ClassModel instrumentedModel = classFile.parse(instrumented.getBytes());
    MethodModel wrapper =
        methodWithType(instrumentedModel, OVERSIZED_METHOD, MethodTypeDesc.of(CD_int, CD_int));
    MethodModel body =
        methodWithType(
            instrumentedModel,
            OVERSIZED_METHOD,
            MethodTypeDesc.of(CD_int, CD_int, ClassDesc.of("daikon.dcomp.DCompMarker")));

    assertEquals(
        "JUnit forwarding stub does not maintain the tag stack",
        Set.of("discard_tag", "push_const"),
        runtimeCalls(wrapper));
    assertEquals("unchanged body contains runtime calls", Set.of(), runtimeCalls(body));
    assertEquals(
        "oversized body was changed",
        codeLength(classFile.parse(original), OVERSIZED_METHOD),
        ((CodeAttribute) body.code().orElseThrow()).codeLength());
    assertTrue("oversized body is not private", body.flags().has(AccessFlag.PRIVATE));
    assertTrue("oversized body is not synthetic", body.flags().has(AccessFlag.SYNTHETIC));

    Class<?> generatedClass =
        byteArrayClassLoader(Map.of(className, instrumented.getBytes())).loadClass(className);
    Object receiver = generatedClass.getConstructor().newInstance();
    Object[] tagFrame = DCRuntime.create_tag_frame("1");
    try {
      DCRuntime.push_const(); // primitive argument tag
      DCRuntime.push_const(); // caller-produced primitive result tag
      Object result = generatedClass.getMethod(OVERSIZED_METHOD, int.class).invoke(receiver, 1);
      assertEquals("forwarding stub returned the wrong value", HUGE_GROUPS + 1, result);

      DCRuntime.discard_tag(1);
      boolean rejectedExtraDiscard = false;
      try {
        DCRuntime.discard_tag(1);
      } catch (AssertionError expected) {
        rejectedExtraDiscard = true;
      }
      assertTrue("forwarding stub left a stale tag", rejectedExtraDiscard);
    } finally {
      DCRuntime.normal_exit(tagFrame);
    }
  }

  /**
   * Tests that the JUnit oversized-method fallback rebuilds the stack map of the body it moves to
   * the private marker overload. Adding the DCompMarker parameter renumbers every local that
   * follows the parameters, which widens the instructions that reference them and thereby shifts
   * the bytecode offsets that the stack map records. If the body keeps the stack map it was parsed
   * with, the class fails verification when it is loaded below.
   *
   * <p>This calls {@link DCInstrument#create_oversized_method} directly rather than instrumenting
   * the class. Fully instrumenting a 64K method that has a stack map takes minutes, because BCEL
   * rescans the instruction list for each instruction it rewrites, and the fully instrumented form
   * is discarded as oversized anyway.
   *
   * @throws IOException if the generated class cannot be parsed
   * @throws ReflectiveOperationException if the generated class cannot be loaded or invoked
   */
  @SuppressWarnings({"nullness:argument", "signedness:argument"}) // TODO
  @Test
  public void testOversizedJunitFallbackRebuildsStackMap()
      throws IOException, ReflectiveOperationException {
    ClassFile classFile = ClassFile.of();
    byte[] original = oversizedClassBytes(HUGE_BRANCHING_GROUPS, true);
    // The premise of this test is that the uninstrumented method fits, but only just: adding the
    // tag-stack bookkeeping to it would not, so the fallback is used.
    int originalLength = codeLength(classFile.parse(original), OVERSIZED_METHOD);
    assertTrue(
        "uninstrumented " + OVERSIZED_METHOD + " does not fit: " + originalLength,
        originalLength <= 65535);
    assertTrue(
        "uninstrumented " + OVERSIZED_METHOD + " has room for the bookkeeping: " + originalLength,
        originalLength > 65535 - 7);

    @BinaryName String className = sampleClassName();
    JavaClass parsed = new ClassParser(new ByteArrayInputStream(original), className).parse();
    boolean savedJdkInstrumented = Premain.jdk_instrumented;
    JavaClass instrumented;
    try {
      Premain.jdk_instrumented = false;
      DCInstrument dci = new DCInstrument(parsed, false, classLoader());
      Method huge = dci.classGen.containsMethod(OVERSIZED_METHOD, "(I)I");
      assert huge != null : "@AssumeAssertion(nullness): oversizedClassBytes added this method";
      // A JUnit method keeps its original descriptor, so no DCompMarker is added to it.
      Method wrapper =
          withDiagnosticsDiscarded(
              () -> {
                try {
                  return dci.create_oversized_method(huge, false);
                } catch (IOException e) {
                  throw new UncheckedIOException(e);
                }
              });
      dci.classGen.replaceMethod(huge, wrapper);
      instrumented = dci.classGen.getJavaClass();
    } finally {
      Premain.jdk_instrumented = savedJdkInstrumented;
    }

    ClassModel instrumentedModel = classFile.parse(instrumented.getBytes());
    MethodModel body =
        methodWithType(
            instrumentedModel,
            OVERSIZED_METHOD,
            MethodTypeDesc.of(CD_int, CD_int, ClassDesc.of("daikon.dcomp.DCompMarker")));
    // The body is unchanged except that the DCompMarker parameter displaces locals 2 and 3, which
    // widens the two one-byte instructions that end up referencing slot 4.
    assertEquals(
        "oversized body was changed beyond renumbering its locals",
        originalLength + 2,
        ((CodeAttribute) body.code().orElseThrow()).codeLength());

    // Loading the class verifies it, which is what checks the rebuilt stack map.
    Class<?> generatedClass =
        byteArrayClassLoader(Map.of(className, instrumented.getBytes())).loadClass(className);
    Object receiver = generatedClass.getConstructor().newInstance();
    Object[] tagFrame = DCRuntime.create_tag_frame("1");
    try {
      DCRuntime.push_const(); // primitive argument tag
      DCRuntime.push_const(); // caller-produced primitive result tag
      Object result = generatedClass.getMethod(OVERSIZED_METHOD, int.class).invoke(receiver, 1);
      assertEquals("forwarding stub returned the wrong value", HUGE_BRANCHING_GROUPS + 1, result);
      DCRuntime.discard_tag(1);
    } finally {
      DCRuntime.normal_exit(tagFrame);
    }
  }

  /**
   * Returns a child-first class loader for the given class definitions.
   *
   * @param definitions maps binary class names to class-file bytes
   * @return the class loader
   */
  private static ClassLoader byteArrayClassLoader(Map<@BinaryName String, byte[]> definitions) {
    return new ClassLoader(classLoader()) {
      @Override
      protected Class<?> loadClass(@BinaryName String name, boolean resolve)
          throws ClassNotFoundException {
        if (!definitions.containsKey(name)) {
          return super.loadClass(name, resolve);
        }
        synchronized (getClassLoadingLock(name)) {
          Class<?> result = findLoadedClass(name);
          if (result == null) {
            byte[] bytes = definitions.get(name);
            assert bytes != null : "@AssumeAssertion(definitions.containsKey(name))";
            result = defineClass(name, bytes, 0, bytes.length);
          }
          if (resolve) {
            resolveClass(result);
          }
          return result;
        }
      }
    };
  }

  /**
   * Tests that an instrumentation error other than an oversized method is fatal when building the
   * instrumented JDK. Such an error implies a bug in the instrumentor, and returning the class
   * uninstrumented would write a broken class into the prebuilt JDK, where it would fail much later
   * and far from the cause.
   */
  @Test
  public void testJdkInstrumentationErrorIsFatal() {
    ClassFile classFile = ClassFile.of();
    byte[] bad = badClassBytes();
    ClassInfo classInfo = new ClassInfo("BadStackMerge", classLoader());
    DCInstrument24 dci = new DCInstrument24(classFile, classFile.parse(bad), true);
    try {
      // Discard the diagnostics about the expected failure.
      withDiagnosticsDiscarded(() -> dci.instrument_jdk_class(classInfo));
      throw new AssertionError("instrument_jdk_class silently returned an uninstrumented class");
    } catch (AssertionError e) {
      throw e;
    } catch (Throwable t) {
      // Expected: the error propagates so that BuildJDK24 halts.
    }
  }

  /**
   * A class whose second constructor delegates with {@code this(...)} and then writes a field --
   * the shape of {@code six170.Hanoi.Hanoi(int, boolean)} in the daikon-tests suite. After a {@code
   * this(...)} call the receiver is fully initialized, so the field write must use the field's tag
   * accessor.
   */
  public static class DelegatingConstructor extends Base {

    /** An arbitrary value. */
    int flag;

    /** Creates a new DelegatingConstructor. */
    public DelegatingConstructor() {
      super(0);
    }

    /**
     * Creates a new DelegatingConstructor by delegating to {@link #DelegatingConstructor()}.
     *
     * @param flag the value to store
     */
    public DelegatingConstructor(int flag) {
      this();
      this.flag = flag;
    }
  }

  /**
   * Tests that {@code constructor_is_initialized} does not leak from one method to the next.
   *
   * <p>The flag records whether the superclass constructor call has been seen in the method being
   * instrumented. Until it has, a constructor must not touch tag fields, because {@code this} is
   * not yet initialized; {@link DCInstrument24#tag_fields_ok} enforces that. The flag was set when
   * a constructor reached its {@code super()} call but never cleared, so in a class with more than
   * one constructor every constructor after the first was treated as initialized from its very
   * first instruction. {@link DCInstrument24#instrument_jdk_class} made this worse: it may rebuild
   * a class with the same instance, so a value left over from an abandoned attempt would make the
   * retry emit different code than the first attempt.
   *
   * <p>{@link TwoConstructors} reads {@code Sample.value} in the argument to its {@code super()}
   * call, so the read happens while {@code this} is still uninitialized and must use the {@code
   * push_const} path rather than the field's tag accessor.
   *
   * @throws IOException if the class file for {@link TwoConstructors} cannot be read
   */
  @Test
  public void constructorInitializedStateDoesNotLeakBetweenMethods() throws IOException {
    @SuppressWarnings("signature:assignment") // the name of a nested class
    @BinaryName String binaryName = TwoConstructors.class.getName();
    byte[] instrumented = instrument(classBytes(binaryName), binaryName);
    assert instrumented != null : "@AssumeAssertion(nullness)";

    ClassModel classModel = ClassFile.of().parse(instrumented);
    Set<String> calls = constructorCalls(classModel, ClassDesc.of(sampleClassName()));

    // The uninitialized-this path pushes a constant tag instead of reading the field's tag.
    assertTrue(
        "constructor did not use the uninitialized-this path: " + calls,
        calls.contains("push_const"));
    assertFalse(
        "constructor read a tag field before its super() call: " + calls,
        calls.contains(Premain.tag_method_name(Premain.GET_TAG, sampleClassName(), "value")));
  }

  /**
   * Tests that a constructor which delegates with {@code this(...)} treats the receiver as
   * initialized afterward.
   *
   * <p>A {@code this(...)} call initializes the receiver just as {@code super(...)} does -- the
   * delegated-to constructor runs the superclass constructor itself. A field written after that
   * call must therefore use the field's tag accessor, so that the field and the parameter assigned
   * to it end up in the same comparability set. Emitting {@code discard_tag} instead silently
   * splits them, which is visible in the daikon-tests suite as {@code Hanoi.noOutput} moving out of
   * its parameter's comparability set.
   *
   * @throws IOException if the class file for {@link DelegatingConstructor} cannot be read
   */
  @Test
  public void delegatingConstructorInitializesReceiver() throws IOException {
    @SuppressWarnings("signature:assignment") // the name of a nested class
    @BinaryName String binaryName = DelegatingConstructor.class.getName();
    byte[] instrumented = instrument(classBytes(binaryName), binaryName);
    assert instrumented != null : "@AssumeAssertion(nullness)";

    ClassModel classModel = ClassFile.of().parse(instrumented);
    Set<String> calls = constructorCalls(classModel, CD_int);

    assertTrue(
        "field written after this(...) did not use its tag accessor: " + calls,
        calls.contains(Premain.tag_method_name(Premain.SET_TAG, binaryName, "flag")));
    assertFalse(
        "field written after this(...) was treated as uninitialized: " + calls,
        calls.contains("discard_tag"));
  }

  /**
   * Returns the methods invoked by the instrumented copy of the constructor whose first parameter
   * has the given type. Selecting on the parameter type distinguishes the constructors of a class
   * that has several, since they all have the same name.
   *
   * @param classModel an instrumented class
   * @param firstParam the type of the constructor's first parameter
   * @return the names of the methods that the instrumented constructor invokes
   */
  private static Set<String> constructorCalls(ClassModel classModel, ClassDesc firstParam) {
    Set<String> result = new HashSet<>();
    for (MethodModel method : classModel.methods()) {
      if (!method.methodName().stringValue().equals("<init>")) {
        continue;
      }
      List<ClassDesc> params = method.methodTypeSymbol().parameterList();
      if (params.size() < 2
          || !params.get(0).equals(firstParam)
          || !params.get(params.size() - 1).displayName().equals("DCompMarker")) {
        // Either a different constructor, or the uninstrumented copy of this one.
        continue;
      }
      method
          .code()
          .ifPresent(
              code -> {
                for (CodeElement element : code) {
                  if (element instanceof InvokeInstruction invoke) {
                    result.add(invoke.name().stringValue());
                  }
                }
              });
    }
    return result;
  }

  /**
   * Runs the given action with {@code System.out} and {@code System.err} discarded, restoring them
   * afterward. The tests that deliberately trigger an instrumentation failure use this to keep the
   * diagnostics that the failure prints out of the test output, so that a passing test run stays
   * quiet.
   *
   * @param <T> the type the action returns
   * @param action the code to run
   * @return whatever {@code action} returns
   */
  private static <T> T withDiagnosticsDiscarded(Supplier<T> action) {
    PrintStream savedOut = System.out;
    PrintStream savedErr = System.err;
    PrintStream discard = new PrintStream(new ByteArrayOutputStream(), false, UTF_8);
    System.setOut(discard);
    System.setErr(discard);
    try {
      return action.get();
    } finally {
      System.setOut(savedOut);
      System.setErr(savedErr);
    }
  }

  /**
   * Returns the length, in bytes, of the code of the named method.
   *
   * @param classModel the class containing the method
   * @param methodName the name of the method
   * @return the code length of the named method
   */
  private static int codeLength(ClassModel classModel, String methodName) {
    for (MethodModel method : classModel.methods()) {
      if (method.methodName().stringValue().equals(methodName)) {
        // A CodeModel that was parsed from a class file, as opposed to one being built, is a
        // CodeAttribute, which knows the length of the code array.
        return ((CodeAttribute) method.code().orElseThrow()).codeLength();
      }
    }
    throw new Error("no method named " + methodName);
  }

  /**
   * Returns the DCRuntime methods invoked by the instrumented copy of the named method, that is,
   * the copy that has a DCompMarker parameter. An empty result means the method was emitted without
   * instrumentation.
   *
   * @param classModel an instrumented class
   * @param methodName the name of the method to examine
   * @return the names of the DCRuntime methods that the instrumented copy invokes
   */
  private static Set<String> runtimeCalls(ClassModel classModel, String methodName) {
    return runtimeCalls(instrumentedCopy(classModel, methodName));
  }

  /**
   * Returns the DCRuntime methods invoked by the given method.
   *
   * @param method the method to examine
   * @return the names of the DCRuntime methods that the method invokes
   */
  private static Set<String> runtimeCalls(MethodModel method) {
    Set<String> result = new HashSet<>();
    method
        .code()
        .ifPresent(
            code -> {
              for (CodeElement element : code) {
                if (element instanceof InvokeInstruction invoke
                    && invoke.owner().asInternalName().endsWith("/DCRuntime")) {
                  result.add(invoke.name().stringValue());
                }
              }
            });
    return result;
  }

  /**
   * Returns the method with the given name and type.
   *
   * @param classModel the class containing the method
   * @param methodName the method name
   * @param methodType the method type
   * @return the matching method
   */
  private static MethodModel methodWithType(
      ClassModel classModel, String methodName, MethodTypeDesc methodType) {
    for (MethodModel method : classModel.methods()) {
      if (method.methodName().stringValue().equals(methodName)
          && method.methodTypeSymbol().equals(methodType)) {
        return method;
      }
    }
    throw new AssertionError("no method named " + methodName + " with type " + methodType);
  }

  /**
   * Returns the opcode used by the instrumented copy's call to its own original overload.
   *
   * @param classModel an instrumented class
   * @param methodName the method to examine
   * @param target the original overload's descriptor
   * @return the invocation opcode
   */
  private static java.lang.classfile.Opcode ownMethodCallOpcode(
      ClassModel classModel, String methodName, MethodTypeDesc target) {
    String ownName = classModel.thisClass().asInternalName();
    for (CodeElement element : instrumentedCopy(classModel, methodName).code().orElseThrow()) {
      if (element instanceof InvokeInstruction invoke
          && invoke.owner().asInternalName().equals(ownName)
          && invoke.name().stringValue().equals(methodName)
          && invoke.typeSymbol().equals(target)) {
        return invoke.opcode();
      }
    }
    throw new AssertionError("no call to the original " + methodName + " overload");
  }

  /**
   * Returns the names, in internal form, of the classes that the methods of the given class file
   * invoke a method of. Unlike the constant pool, this contains only classes that the code actually
   * calls.
   *
   * @param classBytes the bytes of a class file
   * @return the internal names of the classes whose methods {@code classBytes} invokes
   */
  private Set<String> invokedClasses(byte[] classBytes) {
    Set<String> result = new HashSet<>();
    ClassModel classModel = ClassFile.of().parse(classBytes);
    for (MethodModel method : classModel.methods()) {
      method.code().ifPresent(code -> addInvokedClasses(code, result));
    }
    return result;
  }

  /**
   * Adds, to {@code result}, the internal name of the owner of each invocation instruction in the
   * given method body.
   *
   * @param code the body of a method
   * @param result the set to add to; is side-effected by this method
   */
  private void addInvokedClasses(CodeModel code, Set<String> result) {
    for (CodeElement element : code) {
      if (element instanceof InvokeInstruction invoke) {
        result.add(invoke.owner().asInternalName());
      }
    }
  }
}
