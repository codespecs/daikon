package daikon.test;

import static java.nio.charset.StandardCharsets.UTF_8;
import static org.junit.Assert.assertEquals;

import daikon.tools.TraceSelect;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Comparator;
import java.util.List;
import java.util.Random;
import java.util.stream.Stream;
import org.junit.Test;

/** Tests the TraceSelect tool. */
public class TraceSelectTest {

  /** The number of invocations in the trace file created by {@link #writeTrace}. */
  private static final int NUM_INVOCATIONS = 20;

  /** The declarations for the trace file created by {@link #writeTrace}. */
  private static final String DECLS =
      String.join(
          "\n",
          "decl-version 2.0",
          "var-comparability none",
          "",
          "ppt foo.Bar.baz():::ENTER",
          "ppt-type enter",
          "variable x",
          "  var-kind variable",
          "  dec-type int",
          "  rep-type int",
          "",
          "ppt foo.Bar.baz():::EXIT1",
          "ppt-type subexit",
          "variable x",
          "  var-kind variable",
          "  dec-type int",
          "  rep-type int",
          "",
          "");

  /**
   * Writes a trace file with {@link #NUM_INVOCATIONS} invocations of one method. The file ends with
   * an EXIT record, for which TraceSelect's partitioner yields an empty string.
   *
   * @param dir the directory in which to create the trace file
   * @return the trace file
   * @throws IOException if the file cannot be written
   */
  private static Path writeTrace(Path dir) throws IOException {
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
    Path trace = dir.resolve("trace.dtrace");
    Files.writeString(trace, sb, UTF_8);
    return trace;
  }

  /**
   * Deletes a directory and its contents.
   *
   * @param dir the directory to delete
   * @throws IOException if a file cannot be deleted
   */
  private static void deleteRecursively(Path dir) throws IOException {
    try (Stream<Path> paths = Files.walk(dir)) {
      for (Path p : paths.sorted(Comparator.reverseOrder()).toList()) {
        Files.delete(p);
      }
    }
  }

  /** Tests that sampling twice with the same seed selects the same invocations. */
  @Test
  public void testSameSeedSameSample() throws IOException {
    Path dir = Files.createTempDirectory("TraceSelectTest");
    try {
      Path trace = writeTrace(dir);
      int numReps = 3;
      int sampleSize = 5;
      List<List<String>> samples1 =
          TraceSelect.selectSamples(trace.toString(), numReps, sampleSize, new Random(1000), false);
      List<List<String>> samples2 =
          TraceSelect.selectSamples(trace.toString(), numReps, sampleSize, new Random(1000), false);
      assertEquals(numReps, samples1.size());
      // Each sample is a proper subset of the invocations, so the test is not vacuous.
      for (List<String> sample : samples1) {
        assertEquals(sampleSize, sample.size());
      }
      assertEquals(samples1, samples2);
    } finally {
      deleteRecursively(dir);
    }
  }

  /** Tests that running TraceSelect twice with the same -SEED writes the same sample files. */
  @Test
  public void testSeedOption() throws IOException {
    Path dir = Files.createTempDirectory("TraceSelectTest");
    try {
      Path trace = writeTrace(dir);
      Path decls = dir.resolve("trace.decls");
      Files.writeString(decls, DECLS, UTF_8);
      String[] args = {"2", "5", "-SEED", "1000", "-NOCLEAN", trace.toString(), decls.toString()};
      Path sample1 = dir.resolve("trace1.dtrace");
      Path sample2 = dir.resolve("trace2.dtrace");

      TraceSelect.mainHelper(args);
      String firstRunSample1 = Files.readString(sample1, UTF_8);
      String firstRunSample2 = Files.readString(sample2, UTF_8);
      TraceSelect.mainHelper(args);

      assertEquals(firstRunSample1, Files.readString(sample1, UTF_8));
      assertEquals(firstRunSample2, Files.readString(sample2, UTF_8));
    } finally {
      deleteRecursively(dir);
    }
  }
}
