package daikon.dcomp;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import daikon.chicory.ClassInfo;
import java.io.IOException;
import java.io.InputStream;
import java.lang.classfile.ClassFile;
import java.lang.classfile.ClassModel;
import java.lang.classfile.constantpool.ClassEntry;
import java.lang.classfile.constantpool.PoolEntry;
import java.util.HashSet;
import java.util.Set;
import org.checkerframework.checker.signature.qual.BinaryName;
import org.junit.Test;

/** Unit tests for {@link DCInstrument24}. */
public final class DCInstrumentTest24 {

  /** Creates a new DCInstrumentTest24. */
  public DCInstrumentTest24() {}

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
    Set<String> referenced = referencedClasses(instrumented);
    assertTrue(
        "instrumented class does not reference java/lang/DCRuntime: " + referenced,
        referenced.contains("java/lang/DCRuntime"));
    assertFalse(
        "instrumented class references daikon/dcomp/DCRuntime: " + referenced,
        referenced.contains("daikon/dcomp/DCRuntime"));
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
    ClassFile classFile = ClassFile.of();
    ClassModel classModel = classFile.parse(original);
    ClassInfo classInfo = new ClassInfo(classname, DCInstrumentTest24.class.getClassLoader());
    DCInstrument24 dci = new DCInstrument24(classFile, classModel, true);
    byte[] instrumented = dci.instrument_jdk_class(classInfo);
    if (instrumented == null) {
      throw new Error("instrumentation of " + classname + " failed");
    }
    return instrumented;
  }

  /**
   * Returns the names, in internal form, of all the classes in the constant pool of the given class
   * file.
   *
   * @param classBytes the bytes of a class file
   * @return the internal names of the classes that {@code classBytes} references
   */
  private Set<String> referencedClasses(byte[] classBytes) {
    Set<String> result = new HashSet<>();
    ClassModel classModel = ClassFile.of().parse(classBytes);
    for (PoolEntry entry : classModel.constantPool()) {
      if (entry instanceof ClassEntry ce) {
        result.add(ce.asInternalName());
      }
    }
    return result;
  }
}
