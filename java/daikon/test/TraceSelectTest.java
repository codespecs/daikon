package daikon.test;

import static java.nio.charset.StandardCharsets.UTF_8;
import static org.junit.Assert.assertEquals;

import daikon.tools.TraceSelect;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Random;
import org.junit.Test;

/** Tests the TraceSelect tool. */
public class TraceSelectTest {

  /** The number of invocations in the trace file created by {@link #writeTrace}. */
  private static final int NUM_INVOCATIONS = 20;

  /**
   * Writes a trace file with {@link #NUM_INVOCATIONS} invocations of one method.
   *
   * @return the trace file
   * @throws IOException if the file cannot be written
   */
  private static Path writeTrace() throws IOException {
    StringBuilder sb = new StringBuilder();
    for (int nonce = 1; nonce <= NUM_INVOCATIONS; nonce++) {
      for (String kind : new String[] {"ENTER", "EXIT1"}) {
        sb.append("foo.Bar.baz():::")
            .append(kind)
            .append("\nthis_invocation_nonce\n")
            .append(nonce)
            .append("\nx\n")
            .append(nonce)
            .append("\n1\n\n");
      }
    }
    Path trace = Files.createTempFile("TraceSelectTest", ".dtrace");
    Files.writeString(trace, sb, UTF_8);
    return trace;
  }

  /** Tests that sampling twice with the same seed selects the same invocations. */
  @Test
  public void testSameSeedSameSample() throws IOException {
    Path trace = writeTrace();
    try {
      int sampleSize = 5;
      List<String> sample1 =
          TraceSelect.selectSample(trace.toString(), sampleSize, new Random(1000), false);
      List<String> sample2 =
          TraceSelect.selectSample(trace.toString(), sampleSize, new Random(1000), false);
      // The sample is a proper subset of the invocations, so the test is not vacuous.
      assertEquals(sampleSize, sample1.size());
      assertEquals(sample1, sample2);
    } finally {
      Files.delete(trace);
    }
  }
}
