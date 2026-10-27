package daikon.test.split;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import daikon.split.Splitter;
import daikon.split.SplitterList;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.junit.After;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;

/** Tests for {@link SplitterList}. */
@RunWith(JUnit4.class)
public class SplitterListTest {

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
   * Returns the conditions of the splitters that {@link SplitterList#get} returns.
   *
   * @param pptName the name of a program point
   * @return the conditions of the splitters for the program point
   */
  private static List<String> conditions(String pptName) {
    Splitter[] splitters = SplitterList.get(pptName);
    if (splitters == null) {
      throw new AssertionError("No splitters for " + pptName);
    }
    return Arrays.stream(splitters).map(Splitter::condition).collect(Collectors.toList());
  }

  @Test
  public void testGetSeveralNames() {
    String exit = "splitterlisttest.Foo.bar(int):::EXIT";
    String exit12 = "splitterlisttest.Foo.bar(int):::EXIT12";
    put(exit, new Splitter[] {new ConditionSplitter("x > 0")});
    put(exit12, new Splitter[] {new ConditionSplitter("x > 0"), new ConditionSplitter("y > 0")});
    assertEquals(Arrays.asList("x > 0"), conditions(exit));
    assertEquals(Arrays.asList("x > 0", "x > 0", "y > 0"), conditions(exit12));
  }

  @Test
  public void testGetObjectUsesAll() {
    // A PPT_NAME that contains "OBJECT" makes OBJECT program points use every PPT_NAME's
    // splitters, each once.
    put("splitterlisttest.OBJECTsplit", new Splitter[] {new ConditionSplitter("o > 0")});
    put("splitterlisttest.OBJECTsplit2", new Splitter[] {new ConditionSplitter("p > 0")});
    put("splitterlisttest.Unrelated", new Splitter[] {new ConditionSplitter("u > 0")});
    List<String> relevant = Arrays.asList("o > 0", "p > 0", "u > 0");
    assertEquals(
        relevant, relevantConditions(SplitterList.get("splitterlisttest.F:::OBJECT"), relevant));
    // A program point that is not an OBJECT program point does not, even if its name contains
    // "OBJECT".
    assertTrue(SplitterList.get("splitterlisttest.OBJECTive.run():::EXIT") == null);
  }

  /**
   * Returns the conditions of the given splitters, among those whose condition is in {@code
   * relevant}. Other code may leave splitters in SplitterList; for example, loading {@link
   * daikon.PptTopLevel} adds a splitter whose PPT_NAME is ".*".
   *
   * @param splitters splitters returned by {@link SplitterList}, or null
   * @param relevant the conditions of interest
   * @return the conditions of the splitters, among those in {@code relevant}
   */
  private static List<String> relevantConditions(
      Splitter @Nullable [] splitters, List<String> relevant) {
    if (splitters == null) {
      throw new AssertionError("No splitters");
    }
    List<String> result = new ArrayList<>();
    for (Splitter splitter : splitters) {
      if (relevant.contains(splitter.condition())) {
        result.add(splitter.condition());
      }
    }
    return result;
  }
}
