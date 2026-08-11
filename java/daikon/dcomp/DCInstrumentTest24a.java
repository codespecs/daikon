package daikon.dcomp;

import static java.lang.constant.ConstantDescs.CD_void;
import static java.nio.charset.StandardCharsets.UTF_8;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import daikon.chicory.ClassInfo;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintStream;
import java.lang.classfile.ClassFile;
import java.lang.classfile.ClassHierarchyResolver;
import java.lang.classfile.ClassModel;
import java.lang.classfile.Label;
import java.lang.classfile.attribute.StackMapFrameInfo;
import java.lang.classfile.attribute.StackMapTableAttribute;
import java.lang.constant.ClassDesc;
import java.lang.constant.MethodTypeDesc;
import java.util.List;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.checkerframework.checker.signature.qual.BinaryName;
import org.junit.Test;

/**
 * Unit tests for {@link DCInstrument24}.
 *
 * <p>Note that this test must be located in daikon.dcomp rather than daikon.test.dcomp as it needs
 * to access protected fields of DCRuntime and Premain.
 */
public final class DCInstrumentTest24a {

  /** Create a new DCInstrumentTest24a. */
  public DCInstrumentTest24a() {}

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
    ClassLoader loader = DCInstrumentTest24a.class.getClassLoader();
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
    @BinaryName String savedInstrumentationInterface = DCRuntime.instrumentation_interface;
    PrintStream savedOut = System.out;
    PrintStream savedErr = System.err;
    try {
      // Instrument as if the JDK were not instrumented, so that the marker and runtime classes
      // are the ones on the classpath.
      Premain.jdk_instrumented = false;
      DCRuntime.instrumentation_interface = "daikon.dcomp.DCompInstrumented";

      @BinaryName String simpleName = "daikon.dcomp.DCInstrumentTest24a$SimpleClass";
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
}
