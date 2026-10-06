package daikon.test.split;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import daikon.Daikon;
import daikon.FileIO;
import daikon.PptMap;
import daikon.PptTopLevel;
import daikon.split.PptSplitter;
import daikon.split.SpinfoFile;
import daikon.split.SplitterFactory;
import daikon.split.SplitterObject;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collections;
import java.util.regex.Pattern;
import org.junit.Test;

/** Tests loading of splitters when some of the splitters for a program point are erroneous. */
@SuppressWarnings("nullness") // testing code
public class SplitterLoadTest {

  /** Creates a SplitterLoadTest. */
  public SplitterLoadTest() {}

  /** The directory containing the decls file. */
  private static final String targetDir = "daikon/test/split/targets/";

  /**
   * An unparseable splitter must not prevent the other splitters from being compiled and loaded.
   */
  @Test
  public void testUnparseableSplitter() throws IOException {
    String goodCondition = "currentSize == 0";
    String badCondition = "currentSize == == 0";

    Path spinfo = Files.createTempFile("SplitterLoadTest", ".spinfo");
    try {
      Files.writeString(
          spinfo,
          String.join(
              System.lineSeparator(),
              "PPT_NAME DataStructures.QueueAr.isEmpty",
              goodCondition,
              badCondition,
              ""));
      SpinfoFile spfile = SplitterFactory.parse_spinfofile(spinfo.toFile());
      SplitterObject[][] splitterObjects = spfile.getSplitterObjects();
      assertEquals(1, splitterObjects.length);
      assertEquals(2, splitterObjects[0].length);
      SplitterObject good = splitterObjects[0][0];
      SplitterObject bad = splitterObjects[0][1];
      assertEquals(goodCondition, good.condition());
      assertEquals(badCondition, bad.condition());

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
      PptTopLevel ppt = allPpts.get("DataStructures.QueueAr.isEmpty():::EXIT47");
      assertNotNull(ppt);

      boolean oldSuppress = PptSplitter.dkconfig_suppressSplitterErrors;
      PptSplitter.dkconfig_suppressSplitterErrors = true;
      try {
        SplitterFactory.load_splitters(ppt, Collections.singletonList(spfile));
      } finally {
        PptSplitter.dkconfig_suppressSplitterErrors = oldSuppress;
      }

      assertTrue(good.getError(), good.splitterExists());
      assertFalse(bad.splitterExists());
      assertTrue(bad.getError(), bad.getError().contains("cannot be parsed"));
    } finally {
      Files.delete(spinfo);
    }
  }
}
