package daikon.test.dcomp;

import static java.nio.charset.StandardCharsets.UTF_8;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import daikon.dcomp.Instrument24;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintStream;
import java.lang.classfile.ClassFile;
import java.lang.classfile.ClassModel;
import java.lang.classfile.ClassTransform;
import java.lang.classfile.CodeBuilder;
import java.lang.classfile.CodeElement;
import java.lang.classfile.CodeTransform;
import java.lang.classfile.MethodModel;
import java.lang.classfile.attribute.CodeAttribute;
import java.lang.classfile.instruction.ConstantInstruction;
import java.lang.classfile.instruction.InvokeInstruction;
import java.lang.constant.ClassDesc;
import java.lang.constant.ConstantDescs;
import java.lang.constant.MethodTypeDesc;
import java.lang.instrument.IllegalClassFormatException;
import java.lang.reflect.Field;
import org.checkerframework.checker.interning.qual.Interned;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.checkerframework.checker.signature.qual.InternalForm;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * Tests the size of the tag frame that {@link daikon.dcomp.DCInstrument24} creates. The tag frame
 * holds one tag per local variable slot, plus one slot in which {@code DCRuntime.create_tag_frame}
 * records the call depth. The tag frame size is encoded as a single character of a string, so a
 * method whose tag frame would be too large cannot be instrumented; an oversized tag frame would
 * therefore make DynComp reject methods that it can instrument.
 *
 * <p>Note that this test must be located in daikon.test.dcomp rather than daikon.dcomp as it calls
 * Instrument24.transform which would ignore the test thinking it was part of the DynComp tool.
 */
@SuppressWarnings("nullness") // testing code
public class TagFrameTest24 {

  /** Creates a TagFrameTest24. */
  public TagFrameTest24() {}

  /** The class that the tests instrument. */
  public static class Sample {

    /** Creates a Sample. */
    public Sample() {}

    /**
     * The method that the tests instrument. Each test rewrites the body to use a given number of
     * local variable slots.
     */
    public static void manyLocals() {}
  }

  /** The name, in internal form, of the class that the tests instrument. */
  @SuppressWarnings("signature:assignment")
  private static final @InternalForm String sampleInternalName =
      Sample.class.getName().replace('.', '/');

  /** The name of the sample method. */
  private static final String sampleMethodName = "manyLocals";

  /**
   * The descriptor of the instrumented sample method: the descriptor of {@link Sample#manyLocals}
   * plus the {@code DCompMarker} parameter that instrumentation adds.
   */
  private static final MethodTypeDesc instrumentedSampleMethodType =
      MethodTypeDesc.of(ConstantDescs.CD_void, ClassDesc.of("daikon.dcomp.DCompMarker"));

  /**
   * The largest tag frame that {@code DCInstrument24.create_tag_frame} accepts. The tag frame size
   * is passed to {@code DCRuntime.create_tag_frame} as a character obtained by adding the size to
   * '0' (decimal 48), and an unsigned byte holds at most 255.
   */
  private static final int maxTagFrameSize = 206;

  /**
   * The largest number of local variable slots that a method may use and still be instrumented.
   * Before the tag frame is created, instrumentation adds two locals to the method: the {@code
   * DCompMarker} parameter and the tag frame itself. The tag frame has one more slot than the
   * method has locals, for the call depth.
   */
  private static final int maxOriginalLocals = maxTagFrameSize - 3;

  /** The value of {@code Premain.jdk_instrumented} before this test class ran. */
  private static boolean savedJdkInstrumented;

  /** The value of {@code DCRuntime.instrumentation_interface} before this test class ran. */
  private static @Nullable @Interned String savedInstrumentationInterface;

  /**
   * Instruments for an uninstrumented JDK, so that the references that DCInstrument24 emits to
   * {@code DCompMarker}, {@code DCRuntime}, and {@code DCompInstrumented} are the {@code
   * daikon.dcomp} versions, which are on this test's classpath. Saves the runtime state that this
   * test class changes, directly or via {@code Instrument24.transform}.
   *
   * @throws ReflectiveOperationException if the fields do not exist
   */
  @SuppressWarnings("interning:cast.unsafe") // field is interned
  @BeforeClass
  public static void useUninstrumentedJdk() throws ReflectiveOperationException {
    Field jdkInstrumented = dcompField("Premain", "jdk_instrumented");
    savedJdkInstrumented = jdkInstrumented.getBoolean(null);
    savedInstrumentationInterface =
        (@Interned String) dcompField("DCRuntime", "instrumentation_interface").get(null);
    jdkInstrumented.setBoolean(null, false);
  }

  /**
   * Restores the runtime state that {@link #useUninstrumentedJdk} saved, so that this test class
   * does not affect tests that run after it in the same JVM.
   *
   * @throws ReflectiveOperationException if the fields do not exist
   */
  @AfterClass
  public static void restoreJdkInstrumented() throws ReflectiveOperationException {
    dcompField("Premain", "jdk_instrumented").setBoolean(null, savedJdkInstrumented);
    dcompField("DCRuntime", "instrumentation_interface").set(null, savedInstrumentationInterface);
  }

  /**
   * Returns the given static field of the given class in package {@code daikon.dcomp}, made
   * accessible. Uses reflection because the fields are not visible in this package.
   *
   * @param className the simple name of a class in package {@code daikon.dcomp}
   * @param fieldName the name of a static field of that class
   * @return the field, made accessible
   * @throws ReflectiveOperationException if the field does not exist
   */
  private static Field dcompField(String className, String fieldName)
      throws ReflectiveOperationException {
    @SuppressWarnings("signature:argument") // string concatenation
    Field field = Class.forName("daikon.dcomp." + className).getDeclaredField(fieldName);
    field.setAccessible(true);
    return field;
  }

  /**
   * Tests that the tag frame has exactly one more slot than the instrumented method has local
   * variable slots. A smaller tag frame would be indexed out of bounds at run time; a larger one
   * both wastes space and needlessly reduces the number of locals that a method may have.
   *
   * <p>The comparison uses the instrumented method's {@code maxLocals}, which is the value that the
   * tag frame is sized from. That is valid because {@link Sample#manyLocals} returns void and
   * accesses no field, so instrumentation creates no local variable after creating the tag frame.
   */
  @Test
  public void tagFrameHasOneSlotPerLocalPlusOne() {
    byte[] instrumented = instrument(withLocals(sampleClassFile(), 10));
    assertNotNull("instrumentation of the sample failed", instrumented);
    CodeAttribute code = instrumentedSampleCode(instrumented);
    assertEquals("tag frame size", code.maxLocals() + 1, tagFrameSize(instrumented));
  }

  /**
   * Tests that a method that uses the largest permitted number of local variable slots is
   * instrumented rather than rejected as too large, and that a method with one more local variable
   * slot is rejected. Rejecting a class makes DCInstrument24 print a warning to standard error.
   */
  @Test
  public void maximumLocalsIsInstrumented() {
    byte[] instrumented = instrument(withLocals(sampleClassFile(), maxOriginalLocals));
    assertNotNull(
        "instrumentation of a method with " + maxOriginalLocals + " locals failed", instrumented);
    assertEquals("tag frame size", maxTagFrameSize, tagFrameSize(instrumented));

    // Instrumentation returns null for many unrelated reasons: the class is filtered out by name,
    // its bytes do not parse, instrumentation throws. So check the diagnostic too, to be sure the
    // method was rejected because its tag frame is too large.
    ByteArrayOutputStream diagnostics = new ByteArrayOutputStream();
    PrintStream savedErr = System.err;
    byte[] tooManyLocals;
    try {
      System.setErr(new PrintStream(diagnostics, true, UTF_8));
      tooManyLocals = instrument(withLocals(sampleClassFile(), maxOriginalLocals + 1));
    } finally {
      System.setErr(savedErr);
    }
    assertNull(
        "instrumentation of a method with "
            + (maxOriginalLocals + 1)
            + " locals should have failed",
        tooManyLocals);
    String message = diagnostics.toString(UTF_8);
    assertTrue(
        "expected a \"too many local variables\" diagnostic, but got: " + message,
        message.contains("too many local variables"));
  }

  /**
   * Returns the class file of {@link Sample}.
   *
   * @return the class file of the sample class
   */
  private static byte[] sampleClassFile() {
    String resource = sampleInternalName + ".class";
    try (InputStream inputStream = Sample.class.getClassLoader().getResourceAsStream(resource)) {
      assertNotNull("cannot find " + resource, inputStream);
      return inputStream.readAllBytes();
    } catch (IOException e) {
      throw new Error(e);
    }
  }

  /**
   * Rewrites {@link Sample#manyLocals} to use the given number of local variable slots, by storing
   * zero into each of them. The stores are dead code, but they force the method's {@code maxLocals}
   * to the given value.
   *
   * @param classFile the class file to rewrite
   * @param slots the number of local variable slots the sample method should use
   * @return the rewritten class file
   */
  private static byte[] withLocals(byte[] classFile, int slots) {
    CodeTransform useLocals =
        new CodeTransform() {
          @Override
          public void atStart(CodeBuilder codeBuilder) {
            for (int slot = 0; slot < slots; slot++) {
              codeBuilder.iconst_0();
              codeBuilder.istore(slot);
            }
          }

          @Override
          public void accept(CodeBuilder codeBuilder, CodeElement codeElement) {
            codeBuilder.with(codeElement);
          }
        };
    ClassFile cf = ClassFile.of();
    return cf.transformClass(
        cf.parse(classFile),
        ClassTransform.transformingMethodBodies(
            methodModel -> methodModel.methodName().equalsString(sampleMethodName), useLocals));
  }

  /**
   * Instruments the given class file, exactly as the DynComp javaagent would.
   *
   * @param classFile the class file to instrument
   * @return the instrumented class file, or null if instrumentation failed
   */
  private static byte @Nullable [] instrument(byte[] classFile) {
    try {
      return new Instrument24()
          .transform(
              Sample.class.getClassLoader(),
              sampleInternalName,
              null,
              Sample.class.getProtectionDomain(),
              classFile);
    } catch (IllegalClassFormatException e) {
      throw new Error(e);
    }
  }

  /**
   * Returns the code of the instrumented version of the sample method.
   *
   * @param classFile an instrumented class file
   * @return the code of the instrumented sample method
   */
  private static CodeAttribute instrumentedSampleCode(byte[] classFile) {
    ClassModel classModel = ClassFile.of().parse(classFile);
    for (MethodModel methodModel : classModel.methods()) {
      if (methodModel.methodName().equalsString(sampleMethodName)
          && methodModel.methodTypeSymbol().equals(instrumentedSampleMethodType)) {
        return (CodeAttribute) methodModel.code().get();
      }
    }
    throw new Error("no instrumented " + sampleMethodName + " in " + classModel.thisClass());
  }

  /**
   * Returns the size of the tag frame that the instrumented sample method creates. That is the
   * first character of the string that the method passes to {@code DCRuntime.create_tag_frame}.
   *
   * @param classFile an instrumented class file
   * @return the size of the sample method's tag frame
   */
  private static int tagFrameSize(byte[] classFile) {
    String constant = null;
    for (CodeElement codeElement : instrumentedSampleCode(classFile)) {
      switch (codeElement) {
        case ConstantInstruction ci -> {
          if (ci.constantValue() instanceof String s) {
            constant = s;
          }
        }
        case InvokeInstruction invoke -> {
          if (invoke.name().equalsString("create_tag_frame")) {
            assertNotNull("no string constant before create_tag_frame", constant);
            return constant.charAt(0) - '0';
          }
        }
        default -> {}
      }
    }
    throw new Error("no call to create_tag_frame in " + sampleMethodName);
  }
}
