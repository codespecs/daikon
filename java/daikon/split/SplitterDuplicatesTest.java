package daikon.split;

import static org.junit.Assert.assertEquals;

import daikon.PptTopLevel;
import daikon.VarInfo;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.junit.After;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;

/** Tests the removal of splitters with duplicate conditions. */
@RunWith(JUnit4.class)
public class SplitterDuplicatesTest {

  /** The names that a test passed to {@link SplitterList#put}. */
  private final List<String> putNames = new ArrayList<>();

  /** Removes the splitters that a test added, because Daikon uses every splitter by default. */
  @After
  public void tearDown() {
    for (String name : putNames) {
      SplitterList.remove(name);
    }
  }

  /**
   * Calls {@link SplitterList#put}, and records the name so that {@link #tearDown} removes the
   * splitters.
   *
   * @param pptname a name on a PPT_NAME line of a {@code .spinfo} file
   * @param splits the splitters for the name
   */
  private void put(String pptname, Splitter[] splits) {
    putNames.add(pptname);
    SplitterList.put(pptname, splits);
  }

  /**
   * Returns the conditions of the splitters that {@link SplitterList#get_all} returns, among those
   * whose condition is in {@code relevant}. Other code may leave splitters in SplitterList; for
   * example, loading {@link daikon.PptTopLevel} adds a splitter whose PPT_NAME is ".*".
   *
   * @param relevant the conditions of interest
   * @return the conditions of the splitters, among those in {@code relevant}
   */
  private static List<String> allConditions(List<String> relevant) {
    List<String> result = new ArrayList<>();
    for (Splitter splitter : SplitterList.get_all()) {
      if (relevant.contains(splitter.condition())) {
        result.add(splitter.condition());
      }
    }
    return result;
  }

  @Test
  public void testGetAllKeepsDuplicates() {
    put(
        "splitterduplicatestest.C.f():::EXIT",
        new Splitter[] {new ConditionSplitter("c > 0"), new ConditionSplitter("d > 0")});
    put(
        "splitterduplicatestest.D.g():::EXIT",
        new Splitter[] {new ConditionSplitter(" d > 0"), new ConditionSplitter("e > 0")});
    List<String> relevant = Arrays.asList("c > 0", "d > 0", " d > 0", "e > 0");
    // addConditions removes the duplicate, among the splitters that are valid at a program point.
    assertEquals(relevant, allConditions(relevant));
  }

  @Test
  public void testAddConditionsNoDuplicates() {
    PptTopLevel ppt = new PptTopLevel("splitterduplicatestest.E.h():::EXIT", new VarInfo[0]);
    Splitter invalid = new ConditionSplitter("i > 0", false);
    // These two might test different variables, such as "i" and "this.i", but the conditional
    // program points' names would be the same.
    Splitter valid = new ConditionSplitter("i > 0");
    Splitter duplicate = new ConditionSplitter(" i > 0");
    // Conditions that differ as written are not duplicates, even if they mean the same thing.
    Splitter isEmpty = new ConditionSplitter("isEmpty()");
    Splitter sizeIsZero = new ConditionSplitter("size == 0");
    ppt.addConditions(new Splitter[] {invalid, valid, isEmpty, duplicate, sizeIsZero});

    List<PptSplitter> pptSplitters = ppt.splitters;
    if (pptSplitters == null) {
      throw new AssertionError("No splitters");
    }
    List<@Nullable Splitter> added = new ArrayList<>();
    for (PptSplitter pptSplitter : pptSplitters) {
      added.add(pptSplitter.splitter);
    }
    assertEquals(Arrays.asList(valid, isEmpty, sizeIsZero), added);
  }
}
