package daikon.test.split;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
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

  @Test
  public void testMatchesPartialName() {
    assertTrue(SplitterList.matches("Foo.bar", "pkg.Foo.bar(int):::ENTER"));
    assertTrue(SplitterList.matches("Foo.bar", "pkg.Foo.barBaz(int):::EXIT1"));
    assertTrue(SplitterList.matches("Math::BigFloat.bdiv_", "Math::BigFloat.bdiv_s():::EXIT36"));
    assertFalse(SplitterList.matches("Foo.baz", "pkg.Foo.bar(int):::ENTER"));
  }

  @Test
  public void testMatchesCompleteName() {
    String enter = "pkg.Foo.bar(int):::ENTER";
    assertTrue(SplitterList.matches(enter, enter));
    assertFalse(SplitterList.matches(enter, "pkg.Foo.bar(int):::EXIT"));
    assertFalse(SplitterList.matches(enter, "pkg.BigFoo.bar(int):::ENTER"));
    assertFalse(SplitterList.matches(enter, "pkg.Foo.bar(int):::ENTER2"));
    // A complete name may omit a qualifier, such as a package or an enclosing class.
    assertTrue(SplitterList.matches(enter, "a.pkg.Foo.bar(int):::ENTER"));
    assertTrue(SplitterList.matches(enter, "pkg.Outer$pkg.Foo.bar(int):::ENTER"));
    assertTrue(SplitterList.matches("Foo.bar(int):::ENTER", enter));
    assertFalse(SplitterList.matches("Foo.bar(int):::ENTER", "pkg.My_Foo.bar(int):::ENTER"));
    assertFalse(SplitterList.matches("Foo.bar(int):::ENTER", "pkg.Foo.bar(int):::EXIT"));
    assertTrue(SplitterList.matches("Hanoi:::OBJECT", "six170.Hanoi:::OBJECT"));
    assertFalse(SplitterList.matches("Hanoi:::OBJECT", "six170.MyHanoi:::OBJECT"));
    assertTrue(
        SplitterList.matches("BigFloat.bdiv_s():::EXIT36", "Math::BigFloat.bdiv_s():::EXIT36"));

    String exit1 = "pkg.Foo.bar(int):::EXIT1";
    assertTrue(SplitterList.matches(exit1, exit1));
    assertFalse(SplitterList.matches(exit1, "pkg.Foo.bar(int):::EXIT12"));
    assertFalse(SplitterList.matches(exit1, "pkg.Foo.bar(int):::EXIT"));

    String exit = "pkg.Foo.bar(int):::EXIT";
    assertTrue(SplitterList.matches(exit, exit));
    assertTrue(SplitterList.matches(exit, exit1));
    assertTrue(SplitterList.matches(exit, "pkg.Foo.bar(int):::EXIT12"));
    assertFalse(SplitterList.matches(exit, "pkg.Foo.bar(int):::EXITx"));
    assertFalse(SplitterList.matches(exit, "pkg.Foo.bar(int):::ENTER"));
    assertFalse(SplitterList.matches(exit, "pkg.BigFoo.bar(int):::EXIT1"));
    assertTrue(SplitterList.matches("Foo.bar(int):::EXIT", "pkg.Foo.bar(int):::EXIT12"));
    assertTrue(SplitterList.matches("Foo.bar(int):::EXIT", exit));
    assertFalse(SplitterList.matches("Foo.bar(int):::EXIT", "pkg.BigFoo.bar(int):::EXIT12"));

    assertTrue(SplitterList.matches("pkg.Foo:::OBJECT", "pkg.Foo:::OBJECT"));
    assertFalse(SplitterList.matches("pkg.Foo:::OBJECT", "pkg.BigFoo:::OBJECT"));
    assertFalse(SplitterList.matches("aprogram.point:::POINT", "aprogram.point:::POINT2"));
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
  public void testGetObject() {
    String objectA = "splitterlisttest.A:::OBJECT";
    String objectB = "splitterlisttest.B:::OBJECT";
    put(objectA, new Splitter[] {new ConditionSplitter("a > 0")});
    put(objectB, new Splitter[] {new ConditionSplitter("b > 0")});
    List<String> relevant = Arrays.asList("a > 0", "b > 0");
    assertEquals(Arrays.asList("a > 0"), relevantConditions(SplitterList.get(objectA), relevant));
    assertEquals(Arrays.asList("b > 0"), relevantConditions(SplitterList.get(objectB), relevant));
  }

  @Test
  public void testGetCompleteObjectUsesAllIncomplete() {
    // A complete PPT_NAME that contains "OBJECT", as CreateSpinfo writes, makes OBJECT program
    // points use every incomplete PPT_NAME's splitters.
    String object = "splitterlisttest.G:::OBJECT";
    put(object, new Splitter[] {new ConditionSplitter("g > 0")});
    put("splitterlisttest.G.m", new Splitter[] {new ConditionSplitter("m > 0")});
    put("splitterlisttest.H.n():::EXIT", new Splitter[] {new ConditionSplitter("n > 0")});
    List<String> relevant = Arrays.asList("g > 0", "m > 0", "n > 0");
    assertEquals(
        Arrays.asList("g > 0", "m > 0"), relevantConditions(SplitterList.get(object), relevant));
    assertEquals(
        Arrays.asList("m > 0"),
        relevantConditions(SplitterList.get("splitterlisttest.H:::OBJECT"), relevant));
  }

  @Test
  public void testGetObjectUsesAllIncomplete() {
    // An incomplete PPT_NAME that contains "OBJECT" makes OBJECT program points use every
    // incomplete PPT_NAME's splitters.
    put("splitterlisttest.OBJECTsplit", new Splitter[] {new ConditionSplitter("o > 0")});
    put("splitterlisttest.Unrelated", new Splitter[] {new ConditionSplitter("u > 0")});
    List<String> relevant = Arrays.asList("o > 0", "u > 0");
    assertEquals(
        relevant, relevantConditions(SplitterList.get("splitterlisttest.F:::OBJECT"), relevant));
    // A program point that is not an OBJECT program point does not, even if its name contains
    // "OBJECT".
    assertTrue(SplitterList.get("splitterlisttest.OBJECTive.run():::EXIT") == null);
  }

  /**
   * Returns the conditions of the given splitters, among those whose condition is in {@code
   * relevant}. Other code may leave splitters in SplitterList; for example, loading {@link
   * daikon.PptTopLevel} adds a splitter whose PPT_NAME is the incomplete name ".*".
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
