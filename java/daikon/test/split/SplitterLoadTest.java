package daikon.test.split;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assume.assumeTrue;

import daikon.Daikon;
import daikon.FileIO;
import daikon.PptMap;
import daikon.PptTopLevel;
import daikon.split.PptSplitter;
import daikon.split.SpinfoFile;
import daikon.split.Splitter;
import daikon.split.SplitterFactory;
import daikon.split.SplitterList;
import daikon.split.SplitterObject;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.function.Consumer;
import java.util.regex.Pattern;
import org.junit.BeforeClass;
import org.junit.Test;

/** Tests loading of splitters when some of the splitters for a program point are erroneous. */
@SuppressWarnings("nullness") // testing code
public class SplitterLoadTest {

  /** Creates a SplitterLoadTest. */
  public SplitterLoadTest() {}

  /** The directory containing the decls file. */
  private static final String targetDir = "daikon/test/split/targets/";

  /** The name of the program point whose splitters are tested. */
  private static final String pptName = "DataStructures.QueueAr.isEmpty";

  /** A splitter condition that is valid for {@link #pptName}. */
  private static final String goodCondition = "currentSize == 0";

  /**
   * Skips the tests if splitters cannot be compiled, because the compiler is not available or
   * because Daikon's classes are not on the classpath that the compiler uses.
   */
  @BeforeClass
  public static void assumeSplittersCanBeCompiled() {
    assumeTrue("Cannot run the splitter compiler", compilerIsRunnable());
    assumeTrue("Daikon's classes are not on java.class.path", splitterIsOnClassPath());
  }

  /**
   * Returns true if the compiler of {@link SplitterFactory#dkconfig_compiler} can be run.
   *
   * @return true if the compiler can be run
   */
  private static boolean compilerIsRunnable() {
    String compiler = SplitterFactory.dkconfig_compiler.trim().split(" +")[0];
    try {
      @SuppressWarnings({
        "resourceleak:required.method.not.called", // Process is AutoCloseable only in Java 26+
        "resourceleak:unneeded.suppression" // the suppression is needed only in Java 26+
      })
      Process p = new ProcessBuilder(compiler, "-version").redirectErrorStream(true).start();
      try (InputStream in = p.getInputStream()) {
        while (in.read() != -1) {
          // Discard the output, so that the process does not block.
        }
      }
      return p.waitFor() == 0;
    } catch (IOException e) {
      return false;
    } catch (InterruptedException e) {
      Thread.currentThread().interrupt();
      return false;
    }
  }

  /**
   * Returns true if the location of the {@link Splitter} class is on {@code java.class.path}, which
   * {@link SplitterFactory#dkconfig_compiler} passes to the compiler.
   *
   * @return true if the {@link Splitter} class is on {@code java.class.path}
   */
  private static boolean splitterIsOnClassPath() {
    Path splitterLocation;
    try {
      splitterLocation =
          Paths.get(Splitter.class.getProtectionDomain().getCodeSource().getLocation().toURI())
              .toAbsolutePath()
              .normalize();
    } catch (URISyntaxException | RuntimeException e) {
      return false;
    }
    for (String entry : System.getProperty("java.class.path").split(File.pathSeparator)) {
      try {
        if (!entry.isEmpty()
            && Paths.get(entry).toAbsolutePath().normalize().equals(splitterLocation)) {
          return true;
        }
      } catch (InvalidPathException e) {
        // Ignore a malformed classpath entry.
      }
    }
    return false;
  }

  /**
   * Erroneous splitters (a parse error and a lexical error) must not prevent the other splitters
   * from being compiled and loaded.
   */
  @Test
  public void testUnparseableSplitter() throws IOException {
    String parseErrorCondition = "currentSize == == 0";
    // An unterminated comment is a lexical error, which the parser reports as a TokenMgrError.
    String lexicalErrorCondition = "currentSize == 0 /* unterminated";

    SplitterObject[] splitters =
        loadSplitters(List.of(goodCondition, parseErrorCondition, lexicalErrorCondition), s -> {});
    SplitterObject good = splitters[0];
    SplitterObject parseError = splitters[1];
    SplitterObject lexicalError = splitters[2];

    assertTrue(good.getError(), good.splitterExists());
    assertFalse(parseError.splitterExists());
    assertTrue(parseError.getError(), parseError.getError().contains("cannot be parsed"));
    assertFalse(lexicalError.splitterExists());
    assertTrue(lexicalError.getError(), lexicalError.getError().contains("cannot be parsed"));
  }

  /**
   * A splitter whose source file cannot be written must not prevent the other splitters from being
   * compiled and loaded.
   */
  @Test
  public void testUnwritableSplitter() throws IOException {
    // No file can be created in a read-only directory.  Deleting the nonexistent class file from
    // that directory succeeds, so writing the source file is the step that fails.
    Path readOnlyDirectory = Files.createTempDirectory("SplitterLoadTest");
    try {
      assumeTrue(readOnlyDirectory.toFile().setWritable(false, false));
      // A privileged user can write to a read-only directory.
      assumeTrue(!Files.isWritable(readOnlyDirectory));
      SplitterObject[] splitters =
          loadSplitters(
              List.of(goodCondition, "currentSize != 0"),
              s -> s[1].setDirectory(readOnlyDirectory + File.separator));
      SplitterObject good = splitters[0];
      SplitterObject unwritable = splitters[1];

      assertTrue(good.getError(), good.splitterExists());
      assertFalse(unwritable.splitterExists());
      assertTrue(
          unwritable.getError(),
          unwritable.getError().contains("Error while writing splitter file"));
    } finally {
      readOnlyDirectory.toFile().setWritable(true);
      Files.delete(readOnlyDirectory);
    }
  }

  /**
   * Creates splitters with the given conditions for {@link #pptName}, then writes, compiles, and
   * loads them. Leaves no splitters registered in {@link SplitterList}.
   *
   * @param conditions the splitter conditions
   * @param modifier a function that modifies the splitters before they are loaded
   * @return the splitters, in the same order as {@code conditions}
   */
  private static SplitterObject[] loadSplitters(
      List<String> conditions, Consumer<SplitterObject[]> modifier) throws IOException {
    Path spinfo = Files.createTempFile("SplitterLoadTest", ".spinfo");
    try {
      List<String> lines = new ArrayList<>();
      lines.add("PPT_NAME " + pptName);
      lines.addAll(conditions);
      lines.add("");
      Files.writeString(spinfo, String.join(System.lineSeparator(), lines));
      SpinfoFile spfile = SplitterFactory.parse_spinfofile(spinfo.toFile());
      SplitterObject[][] splitterObjects = spfile.getSplitterObjects();
      assertEquals(1, splitterObjects.length);
      SplitterObject[] result = splitterObjects[0];
      assertEquals(conditions.size(), result.length);
      for (int i = 0; i < conditions.size(); i++) {
        assertEquals(conditions.get(i), result[i].condition());
      }
      modifier.accept(result);

      // Reading the decls file sets FileIO.new_decl_format, which writing the splitters uses.
      // Restore it afterward, so that it does not affect other tests.
      Boolean oldNewDeclFormat = FileIO.new_decl_format;
      boolean oldSuppress = PptSplitter.dkconfig_suppressSplitterErrors;
      PptSplitter.dkconfig_suppressSplitterErrors = true;
      try {
        PptTopLevel ppt = readPpt();
        SplitterFactory.load_splitters(ppt, Collections.singletonList(spfile));
      } finally {
        FileIO.new_decl_format = oldNewDeclFormat;
        PptSplitter.dkconfig_suppressSplitterErrors = oldSuppress;
        SplitterList.remove(pptName);
      }
      return result;
    } finally {
      Files.delete(spinfo);
    }
  }

  /**
   * Reads the program point to which the splitters apply.
   *
   * @return the program point to which the splitters apply
   */
  private static PptTopLevel readPpt() throws IOException {
    PptMap allPpts = new PptMap();
    // Other tests may have set program point filters, which would prevent reading the decls.
    Pattern oldPptRegexp = Daikon.ppt_regexp;
    Pattern oldPptOmitRegexp = Daikon.ppt_omit_regexp;
    String oldPptMaxName = Daikon.ppt_max_name;
    Daikon.ppt_regexp = null;
    Daikon.ppt_omit_regexp = null;
    Daikon.ppt_max_name = null;
    try {
      FileIO.resetNewDeclFormat();
      FileIO.read_data_trace_file(targetDir + "QueueAr.decls", allPpts);
    } finally {
      Daikon.ppt_regexp = oldPptRegexp;
      Daikon.ppt_omit_regexp = oldPptOmitRegexp;
      Daikon.ppt_max_name = oldPptMaxName;
    }
    PptTopLevel ppt = allPpts.get(pptName + "():::EXIT47");
    assertNotNull(ppt);
    return ppt;
  }
}
