package daikon.test.dcomp;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import daikon.dcomp.Instrument24;
import java.io.IOException;
import java.io.InputStream;
import java.lang.classfile.ClassFile;
import java.lang.classfile.ClassModel;
import java.lang.classfile.ClassTransform;
import java.lang.classfile.CodeTransform;
import java.lang.classfile.MethodModel;
import java.lang.classfile.Opcode;
import java.lang.classfile.instruction.LoadInstruction;
import java.lang.classfile.instruction.StoreInstruction;
import java.lang.constant.ClassDesc;
import java.lang.constant.MethodTypeDesc;
import java.lang.instrument.IllegalClassFormatException;
import java.lang.reflect.Field;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.checkerframework.checker.signature.qual.InternalForm;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * Tests {@link daikon.dcomp.DCInstrument24} on bytecodes that javac does not produce, and that the
 * DynComp system tests therefore do not cover.
 */
@SuppressWarnings("nullness") // testing code
public class DCInstrumentTest24 {

  /** Creates a DCInstrumentTest24. */
  public DCInstrumentTest24() {}

  /** The class that the tests instrument. */
  private static final Class<?> sampleClass = WideOpcodeSample24.class;

  /** The name, in internal form, of the class that the tests instrument. */
  @SuppressWarnings("signature:assignment")
  private static final @InternalForm String sampleInternalName =
      sampleClass.getName().replace('.', '/');

  /** The name of the sample method. */
  private static final String sampleMethodName = "localVariableAccesses";

  /** The descriptor of the sample method. */
  private static final MethodTypeDesc sampleMethodType =
      MethodTypeDesc.ofDescriptor("(IJFDLjava/lang/Object;)I");

  /**
   * Instruments for an uninstrumented JDK, so that the references that DCInstrument24 emits to
   * {@code DCompMarker}, {@code DCRuntime}, and {@code DCompInstrumented} are the {@code
   * daikon.dcomp} versions, which are on this test's classpath. Uses reflection because the field
   * is not visible in this package.
   *
   * @throws ReflectiveOperationException if {@code Premain.jdk_instrumented} does not exist
   */
  @BeforeClass
  public static void useUninstrumentedJdk() throws ReflectiveOperationException {
    Field jdkInstrumented =
        Class.forName("daikon.dcomp.Premain").getDeclaredField("jdk_instrumented");
    jdkInstrumented.setAccessible(true);
    jdkInstrumented.setBoolean(null, false);
  }

  /**
   * Tests that the sample class is instrumented. This is a control for {@link
   * #wideLocalVariableAccesses}; the two tests differ only in how the local variable accesses are
   * encoded.
   */
  @Test
  public void narrowLocalVariableAccesses() {
    byte[] instrumented = instrument(sampleClassFile());
    assertNotNull("instrumentation of the narrow sample failed", instrumented);
    assertTrue(containsInstrumentedSampleMethod(instrumented));
  }

  /**
   * Tests that the sample class is instrumented after each of its local variable loads and stores
   * has been rewritten into its wide form. javac emits the wide forms only for slots greater than
   * 255, but they are legal for any slot and other bytecode producers do emit them.
   */
  @Test
  public void wideLocalVariableAccesses() {
    byte[] instrumented = instrument(widenLocalVariableAccesses(sampleClassFile()));
    assertNotNull("instrumentation of the wide sample failed", instrumented);
    assertTrue(containsInstrumentedSampleMethod(instrumented));
  }

  /**
   * Returns the class file of {@link #sampleClass}.
   *
   * @return the class file of the sample class
   */
  private static byte[] sampleClassFile() {
    String resource = sampleInternalName + ".class";
    try (InputStream inputStream = sampleClass.getClassLoader().getResourceAsStream(resource)) {
      assertNotNull("cannot find " + resource, inputStream);
      return inputStream.readAllBytes();
    } catch (IOException e) {
      throw new Error(e);
    }
  }

  /**
   * Rewrites every local variable load and store in the given class file into its wide (two-byte
   * operand) form, leaving the meaning of the class unchanged.
   *
   * @param classFile the class file to rewrite
   * @return the rewritten class file
   */
  private static byte[] widenLocalVariableAccesses(byte[] classFile) {
    CodeTransform widen =
        (codeBuilder, codeElement) -> {
          switch (codeElement) {
            case LoadInstruction load ->
                codeBuilder.with(LoadInstruction.of(wideLoad(load.opcode()), load.slot()));
            case StoreInstruction store ->
                codeBuilder.with(StoreInstruction.of(wideStore(store.opcode()), store.slot()));
            default -> codeBuilder.with(codeElement);
          }
        };
    ClassFile cf = ClassFile.of();
    return cf.transformClass(cf.parse(classFile), ClassTransform.transformingMethodBodies(widen));
  }

  /**
   * Returns the wide form of a local variable load opcode.
   *
   * @param opcode a local variable load opcode
   * @return the wide form of opcode
   */
  private static Opcode wideLoad(Opcode opcode) {
    return switch (opcode) {
      case ALOAD, ALOAD_0, ALOAD_1, ALOAD_2, ALOAD_3, ALOAD_W -> Opcode.ALOAD_W;
      case DLOAD, DLOAD_0, DLOAD_1, DLOAD_2, DLOAD_3, DLOAD_W -> Opcode.DLOAD_W;
      case FLOAD, FLOAD_0, FLOAD_1, FLOAD_2, FLOAD_3, FLOAD_W -> Opcode.FLOAD_W;
      case ILOAD, ILOAD_0, ILOAD_1, ILOAD_2, ILOAD_3, ILOAD_W -> Opcode.ILOAD_W;
      case LLOAD, LLOAD_0, LLOAD_1, LLOAD_2, LLOAD_3, LLOAD_W -> Opcode.LLOAD_W;
      default -> throw new Error("not a load opcode: " + opcode);
    };
  }

  /**
   * Returns the wide form of a local variable store opcode.
   *
   * @param opcode a local variable store opcode
   * @return the wide form of opcode
   */
  private static Opcode wideStore(Opcode opcode) {
    return switch (opcode) {
      case ASTORE, ASTORE_0, ASTORE_1, ASTORE_2, ASTORE_3, ASTORE_W -> Opcode.ASTORE_W;
      case DSTORE, DSTORE_0, DSTORE_1, DSTORE_2, DSTORE_3, DSTORE_W -> Opcode.DSTORE_W;
      case FSTORE, FSTORE_0, FSTORE_1, FSTORE_2, FSTORE_3, FSTORE_W -> Opcode.FSTORE_W;
      case ISTORE, ISTORE_0, ISTORE_1, ISTORE_2, ISTORE_3, ISTORE_W -> Opcode.ISTORE_W;
      case LSTORE, LSTORE_0, LSTORE_1, LSTORE_2, LSTORE_3, LSTORE_W -> Opcode.LSTORE_W;
      default -> throw new Error("not a store opcode: " + opcode);
    };
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
              sampleClass.getClassLoader(),
              sampleInternalName,
              null,
              sampleClass.getProtectionDomain(),
              classFile);
    } catch (IllegalClassFormatException e) {
      throw new Error(e);
    }
  }

  /**
   * Returns true if the given class file contains an instrumented version of the sample method,
   * that is, one with an extra {@code DCompMarker} parameter.
   *
   * @param classFile the class file to examine
   * @return true if classFile contains an instrumented version of the sample method
   */
  private static boolean containsInstrumentedSampleMethod(byte[] classFile) {
    MethodTypeDesc instrumentedType =
        sampleMethodType.insertParameterTypes(
            sampleMethodType.parameterCount(), ClassDesc.of("daikon.dcomp.DCompMarker"));
    ClassModel classModel = ClassFile.of().parse(classFile);
    for (MethodModel methodModel : classModel.methods()) {
      if (methodModel.methodName().equalsString(sampleMethodName)
          && methodModel.methodTypeSymbol().equals(instrumentedType)) {
        return true;
      }
    }
    return false;
  }
}
