package daikon.test.split;

import static org.junit.Assert.assertTrue;

import daikon.Daikon;
import daikon.FileIO;
import daikon.PptMap;
import daikon.PptTopLevel;
import daikon.ValueTuple;
import daikon.VarInfo;
import daikon.split.SpinfoFile;
import daikon.split.Splitter;
import daikon.split.SplitterFactory;
import daikon.split.SplitterList;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;
import org.checkerframework.checker.nullness.qual.Nullable;

/** Utilities for tests that compile, load, and evaluate the splitters of a spinfo file. */
final class SplitterTestUtils {

  /** Do not instantiate. */
  private SplitterTestUtils() {
    throw new Error("do not instantiate");
  }

  /** The directory that contains the test inputs. */
  static final String targetDir = "daikon/test/split/targets/";

  /** The splitters that were loaded for a program point. */
  static final class LoadedSplitters {

    /** The standard output of {@link SplitterFactory#load_splitters}. */
    final String output;

    /** The program point to which the splitters apply. */
    final PptTopLevel ppt;

    /** The splitters that were loaded, instantiated for {@link #ppt}. */
    final List<Splitter> splitters;

    /**
     * Creates a new LoadedSplitters.
     *
     * @param output the standard output of {@link SplitterFactory#load_splitters}
     * @param ppt the program point to which the splitters apply
     * @param splitters the splitters that were loaded, instantiated for ppt
     */
    LoadedSplitters(String output, PptTopLevel ppt, List<Splitter> splitters) {
      this.output = output;
      this.ppt = ppt;
      this.splitters = splitters;
    }

    /**
     * Evaluates each splitter on a ValueTuple.
     *
     * @param values maps the name of each variable of {@link #ppt} that the conditions use to its
     *     value; the other variables are null
     * @return maps the condition of each splitter to its value
     */
    Map<String, Boolean> test(Map<String, Object> values) {
      ValueTuple vt = makeVt(ppt, values);
      Map<String, Boolean> results = new LinkedHashMap<>();
      for (Splitter splitter : splitters) {
        results.put(splitter.condition(), splitter.test(vt));
      }
      return results;
    }
  }

  /**
   * Reads a decls file and returns one of its program points.
   *
   * @param declsFile the name of the decls file, in {@link #targetDir}
   * @param pptName the full name of the program point to return
   * @return the program point named pptName
   * @throws IOException if the decls file cannot be read
   */
  static PptTopLevel readPpt(String declsFile, String pptName) throws IOException {
    PptMap ppts = new PptMap();
    FileIO.resetNewDeclFormat();
    // Other tests may have set the patterns that filter the program points and variables.
    Pattern oldPptRegexp = Daikon.ppt_regexp;
    Pattern oldPptOmitRegexp = Daikon.ppt_omit_regexp;
    Pattern oldVarRegexp = Daikon.var_regexp;
    Pattern oldVarOmitRegexp = Daikon.var_omit_regexp;
    Daikon.ppt_regexp = null;
    Daikon.ppt_omit_regexp = null;
    Daikon.var_regexp = null;
    Daikon.var_omit_regexp = null;
    try {
      FileIO.read_data_trace_file(targetDir + declsFile, ppts);
    } finally {
      Daikon.ppt_regexp = oldPptRegexp;
      Daikon.ppt_omit_regexp = oldPptOmitRegexp;
      Daikon.var_regexp = oldVarRegexp;
      Daikon.var_omit_regexp = oldVarOmitRegexp;
    }
    PptTopLevel ppt = ppts.get(pptName);
    if (ppt == null) {
      throw new AssertionError("No program point " + pptName + " in " + ppts.nameStringSet());
    }
    return ppt;
  }

  /**
   * Compiles and loads the splitters of a spinfo file for a program point. Checks that each
   * splitter is valid and that no two splitters have the same condition.
   *
   * @param spinfoFile the name of the spinfo file, in {@link #targetDir}
   * @param ppt the program point to which the splitters apply
   * @param splitterPptName the PPT_NAME of the splitters in the spinfo file
   * @return the output of loading the splitters, and the splitters that were loaded
   * @throws IOException if the spinfo file cannot be read
   */
  @SuppressWarnings("nullness:contracts.precondition") // parse_spinfofile sets the temp directory
  static LoadedSplitters load(String spinfoFile, PptTopLevel ppt, String splitterPptName)
      throws IOException {
    SpinfoFile spinfo = SplitterFactory.parse_spinfofile(new File(targetDir + spinfoFile));
    // Splitters that other tests registered for splitterPptName would be returned below.
    SplitterList.remove(splitterPptName);
    try {
      PrintStream oldOut = System.out;
      ByteArrayOutputStream out = new ByteArrayOutputStream();
      System.setOut(new PrintStream(out, true, StandardCharsets.UTF_8));
      try {
        SplitterFactory.load_splitters(ppt, Collections.singletonList(spinfo));
      } finally {
        System.setOut(oldOut);
      }
      String output = new String(out.toByteArray(), StandardCharsets.UTF_8);

      List<Splitter> splitters = new ArrayList<>();
      Set<String> conditions = new HashSet<>();
      Splitter[] factories = SplitterList.get_raw(splitterPptName);
      if (factories != null) {
        for (Splitter factory : factories) {
          String condition = factory.condition();
          Splitter splitter = factory.instantiateSplitter(ppt);
          assertTrue(condition, splitter.valid());
          assertTrue("duplicate condition " + condition, conditions.add(condition));
          splitters.add(splitter);
        }
      }
      return new LoadedSplitters(output, ppt, splitters);
    } finally {
      SplitterList.remove(splitterPptName);
    }
  }

  /**
   * Returns a ValueTuple for ppt that gives the given values to its variables.
   *
   * @param ppt a program point
   * @param values maps the name of each variable of ppt that is used to its value; the other
   *     variables are null
   * @return a ValueTuple for ppt
   */
  private static ValueTuple makeVt(PptTopLevel ppt, Map<String, Object> values) {
    int size = 0;
    for (VarInfo vi : ppt.var_infos) {
      size = Math.max(size, vi.value_index + 1);
    }
    @Nullable Object[] vals = new @Nullable Object[size];
    int[] mods = new int[size];
    Arrays.fill(mods, ValueTuple.MODIFIED);
    for (VarInfo vi : ppt.var_infos) {
      if (vi.value_index >= 0) {
        vals[vi.value_index] = values.get(vi.name());
      }
    }
    return ValueTuple.makeUninterned(vals, mods);
  }
}
