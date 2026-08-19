package daikon.test.dcomp;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import daikon.dcomp.Instrument24;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.lang.classfile.ClassFile;
import java.lang.classfile.ClassModel;
import java.lang.classfile.CodeElement;
import java.lang.classfile.CodeModel;
import java.lang.classfile.Instruction;
import java.lang.classfile.MethodModel;
import java.lang.instrument.IllegalClassFormatException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.TreeMap;
import java.util.concurrent.CyclicBarrier;
import java.util.concurrent.TimeUnit;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.checkerframework.checker.signature.qual.InternalForm;
import org.junit.Test;

/**
 * Tests that DynComp's Java 24 instrumentation may be run on more than one thread at a time. The
 * JVM calls {@link daikon.dcomp.Instrument24#transform} on whichever thread triggers a class load,
 * so a multithreaded target program instruments classes concurrently.
 */
public final class DCInstrumentConcurrencyTest24 {

  /** Create a new DCInstrumentConcurrencyTest24. */
  public DCInstrumentConcurrencyTest24() {}

  /** The class that this test instruments. */
  private static final @InternalForm String inputClassName =
      "daikon/test/dcomp/DCInstrumentConcurrencyTest24$Input";

  /** The number of threads that instrument at the same time. */
  private static final int threadCount = 4;

  /** The number of times each thread instruments {@link Input}. */
  private static final int iterationCount = 25;

  /**
   * How long, in milliseconds, to wait for all the threads to finish. If instrumentation deadlocks,
   * the test fails after this long rather than hanging forever.
   */
  private static final long joinTimeoutMillis = 120_000;

  /**
   * Instruments {@link Input} on several threads at once, and checks that every thread produces the
   * same instrumented methods as instrumenting {@link Input} by itself does.
   *
   * <p>The instrumented bytes are not identical from one run to the next, because each instrumented
   * method contains its own method identifier, and method identifiers are handed out sequentially.
   * Therefore, this test compares the number of instructions in each method.
   *
   * @throws Exception if the class file cannot be read or a thread is interrupted
   */
  @Test
  public void concurrentTransformsMatchSequentialTransform() throws Exception {

    byte[] originalBytes = readClassFile(inputClassName);
    Instrument24 transformer = new Instrument24();

    Map<String, Integer> expected = instructionCounts(transform(transformer, originalBytes));

    List<Throwable> failures = Collections.synchronizedList(new ArrayList<Throwable>());
    CyclicBarrier barrier = new CyclicBarrier(threadCount);
    List<Thread> threads = new ArrayList<>();
    for (int i = 0; i < threadCount; i++) {
      @SuppressWarnings("AssertionFailureIgnored") // saved in the `failures` variable
      Thread thread =
          new Thread(
              () -> {
                try {
                  // Start instrumenting at the same time on every thread.
                  barrier.await();
                  for (int j = 0; j < iterationCount; j++) {
                    byte[] actualBytes = transform(transformer, originalBytes);
                    assertEquals(
                        "concurrently instrumented class does not verify",
                        List.of(),
                        ClassFile.of().verify(actualBytes));
                    assertEquals(
                        "concurrent instrumentation differs from sequential instrumentation",
                        expected,
                        instructionCounts(actualBytes));
                  }
                } catch (Throwable t) {
                  failures.add(t);
                }
              });
      thread.setDaemon(true);
      threads.add(thread);
      thread.start();
    }
    // Use nanoTime, not currentTimeMillis, because nanoTime is monotonic:  the deadline is not
    // affected by a change to the system clock while the test is running.
    long deadlineNanos = System.nanoTime() + TimeUnit.MILLISECONDS.toNanos(joinTimeoutMillis);
    int unfinishedCount = 0;
    for (Thread thread : threads) {
      long remainingNanos = deadlineNanos - System.nanoTime();
      thread.join(Math.max(1, TimeUnit.NANOSECONDS.toMillis(remainingNanos)));
      if (thread.isAlive()) {
        unfinishedCount++;
      }
    }
    if (unfinishedCount > 0) {
      fail(
          unfinishedCount
              + " of "
              + threadCount
              + " threads did not finish within "
              + joinTimeoutMillis
              + "ms; instrumentation may deadlock");
    }

    if (!failures.isEmpty()) {
      StringBuilder message = new StringBuilder();
      message.append(failures.size() + " of " + threadCount + " threads failed:");
      for (Throwable t : failures) {
        message.append(System.lineSeparator() + t);
      }
      message.append(System.lineSeparator() + "Stack trace of the first failure:");
      StringWriter stackTrace = new StringWriter();
      try (PrintWriter stackTraceWriter = new PrintWriter(stackTrace)) {
        failures.get(0).printStackTrace(stackTraceWriter);
      }
      message.append(System.lineSeparator() + stackTrace);
      fail(message.toString());
    }
  }

  /**
   * Returns the number of bytecode instructions in each method of the given class.
   *
   * @param classBytes a class file
   * @return a map from method name and descriptor to the method's number of instructions
   */
  private static Map<String, Integer> instructionCounts(byte[] classBytes) {
    ClassModel classModel = ClassFile.of().parse(classBytes);
    Map<String, Integer> result = new TreeMap<>();
    for (MethodModel method : classModel.methods()) {
      Optional<CodeModel> code = method.code();
      int count = 0;
      if (code.isPresent()) {
        for (CodeElement element : code.get()) {
          if (element instanceof Instruction) {
            count++;
          }
        }
      }
      String key = method.methodName().stringValue() + method.methodType().stringValue();
      Integer previous = result.put(key, count);
      if (previous != null) {
        throw new AssertionError("duplicate method " + key);
      }
    }
    return result;
  }

  /**
   * Instruments the given class file bytes, in the same way that the JVM does when DynComp is
   * running.
   *
   * @param transformer the instrumenter to use
   * @param classBytes the class file to instrument; is not modified
   * @return the instrumented class file
   * @throws IllegalClassFormatException if the class file is malformed
   */
  private static byte[] transform(Instrument24 transformer, byte[] classBytes)
      throws IllegalClassFormatException {
    byte @Nullable [] result =
        transformer.transform(
            DCInstrumentConcurrencyTest24.class.getClassLoader(),
            inputClassName,
            null,
            DCInstrumentConcurrencyTest24.class.getProtectionDomain(),
            classBytes.clone());
    if (result == null) {
      throw new AssertionError("instrumentation of " + inputClassName + " failed");
    }
    return result;
  }

  /**
   * Returns the contents of the given class file.
   *
   * @param className the class to read, in internal form
   * @return the bytes of the class file
   * @throws IOException if the class file cannot be read
   */
  private static byte[] readClassFile(@InternalForm String className) throws IOException {
    String resource = "/" + className + ".class";
    try (InputStream stream = DCInstrumentConcurrencyTest24.class.getResourceAsStream(resource)) {
      if (stream == null) {
        throw new IOException("Cannot find " + resource);
      }
      return stream.readAllBytes();
    }
  }

  /**
   * The class that {@link #concurrentTransformsMatchSequentialTransform} instruments. It is never
   * executed. Its methods use a variety of control flow, so that instrumenting them exercises the
   * operand stack simulation.
   *
   * <p>This class has no static fields, because DynComp's map from static field to identifier is
   * shared by all instrumenters, and this test is about per-method instrumentation state.
   */
  public static class Input {

    /** Create a new Input. */
    public Input() {}

    /** Some data to compute over. */
    private final int[] data = new int[16];

    /** How many times a method of this class has been called. */
    @SuppressWarnings("UnusedVariable") // the code contains a variety of control flow
    private int counter;

    /**
     * Adds up some of {@link #data}.
     *
     * @param limit how many elements to consider
     * @return a sum of elements of {@link #data}
     */
    public int sum(int limit) {
      int total = 0;
      for (int i = 0; i < limit; i++) {
        if ((i & 1) == 0) {
          total += data[i % data.length];
        } else {
          total -= i;
        }
      }
      counter++;
      return total;
    }

    /**
     * Computes an arbitrary function of its arguments.
     *
     * @param x a double
     * @param y a long
     * @return an arbitrary function of the arguments
     */
    public double compute(double x, long y) {
      double result = x;
      switch ((int) (y % 4)) {
        case 0:
          result = x * 2.0;
          break;
        case 1:
          result = x / (y == 0 ? 1.0 : y);
          break;
        case 2:
          result = Math.sqrt(Math.abs(x)) + y;
          break;
        default:
          result = x - (float) y;
          break;
      }
      long bits = (long) result ^ y;
      return result + (bits >> 3);
    }

    /**
     * Returns a description of its argument.
     *
     * @param o an object
     * @return a description of {@code o}
     */
    public String describe(@Nullable Object o) {
      try {
        if (o == null) {
          return "null";
        }
        if (o instanceof int[] array) {
          return "int[" + array.length + "]";
        }
        Object[] wrapper = new Object[] {o, o.getClass()};
        return wrapper[0] + " of " + wrapper[1];
      } catch (RuntimeException e) {
        return "exception " + e;
      } finally {
        counter++;
      }
    }

    /**
     * Copies part of {@link #data} into a new array.
     *
     * @param start the first index to copy
     * @return a copy of part of {@link #data}
     */
    public byte[] extract(int start) {
      byte[] result = new byte[data.length];
      int index = 0;
      while (start < data.length) {
        result[index] = (byte) data[start];
        index++;
        start++;
      }
      return result;
    }
  }
}
