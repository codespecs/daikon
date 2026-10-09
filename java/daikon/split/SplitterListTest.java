package daikon.split;

import static java.nio.charset.StandardCharsets.UTF_8;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import daikon.Ppt;
import daikon.ValueTuple;
import daikon.inv.DummyInvariant;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import org.checkerframework.checker.initialization.qual.UnknownInitialization;
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
   * Calls {@link SplitterList#put(String, Splitter[], StatementReplacer)}, and records the name so
   * that {@link #tearDown} removes the splitters.
   *
   * @param pptname a name on a PPT_NAME line of a {@code .spinfo} file
   * @param splits the splitters for the name
   * @param replacer the REPLACE statements of the {@code .spinfo} file
   */
  private void put(String pptname, Splitter[] splits, StatementReplacer replacer) {
    putNames.add(pptname);
    SplitterList.put(pptname, splits, replacer);
  }

  /**
   * Returns the REPLACE statements of a {@code .spinfo} file that contains the given REPLACE
   * section and no splitters.
   *
   * @param replaceLines the lines of the REPLACE section, after the "REPLACE" line
   * @return the REPLACE statements
   * @throws IOException if there is trouble writing or reading the file
   */
  private static StatementReplacer replacer(String... replaceLines) throws IOException {
    Path spinfo = Files.createTempFile("SplitterListTest", ".spinfo");
    try {
      List<String> lines = new ArrayList<>();
      lines.add("REPLACE");
      lines.addAll(Arrays.asList(replaceLines));
      Files.write(spinfo, lines, UTF_8);
      return SplitterFactory.parse_spinfofile(spinfo.toFile()).getReplacer();
    } finally {
      Files.delete(spinfo);
    }
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
    assertFalse(SplitterList.matches(enter, "a.pkg.Foo.bar(int):::ENTER"));
    assertFalse(SplitterList.matches(enter, "pkg.Outer$pkg.Foo.bar(int):::ENTER"));
    assertFalse(SplitterList.matches("Foo.bar(int):::ENTER", enter));

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

    assertTrue(SplitterList.matches("pkg.Foo:::OBJECT", "pkg.Foo:::OBJECT"));
    assertFalse(SplitterList.matches("pkg.Foo:::OBJECT", "pkg.BigFoo:::OBJECT"));
    assertFalse(SplitterList.matches("aprogram.point:::POINT", "aprogram.point:::POINT2"));
  }

  /** A splitter factory that only has a condition. */
  private static class ConditionSplitter extends Splitter {
    static final long serialVersionUID = 20261006L;

    /** The condition. */
    private final String condition;

    /**
     * Creates a new ConditionSplitter.
     *
     * @param condition the condition
     */
    ConditionSplitter(String condition) {
      this.condition = condition;
    }

    @Override
    public Splitter instantiateSplitter(@UnknownInitialization(Ppt.class) Ppt ppt) {
      throw new UnsupportedOperationException();
    }

    @Override
    public boolean valid() {
      return false;
    }

    @Override
    public boolean test(ValueTuple vt) {
      throw new UnsupportedOperationException();
    }

    @Override
    public String condition() {
      return condition;
    }

    @Override
    public @Nullable DummyInvariant getDummyInvariant() {
      return null;
    }
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
  public void testGetNoDuplicates() {
    String exit = "splitterlisttest.Foo.bar(int):::EXIT";
    String exit12 = "splitterlisttest.Foo.bar(int):::EXIT12";
    put(exit, new Splitter[] {new ConditionSplitter("x > 0")});
    put(exit12, new Splitter[] {new ConditionSplitter("x > 0"), new ConditionSplitter("y > 0")});
    assertEquals(Arrays.asList("x > 0"), conditions(exit));
    assertEquals(Arrays.asList("x > 0", "y > 0"), conditions(exit12));
  }

  @Test
  public void testGetObject() {
    String objectA = "splitterlisttest.A:::OBJECT";
    String objectB = "splitterlisttest.B:::OBJECT";
    put(objectA, new Splitter[] {new ConditionSplitter("a > 0")});
    put(objectB, new Splitter[] {new ConditionSplitter("b > 0")});
    assertEquals(Arrays.asList("a > 0"), conditions(objectA));
    assertEquals(Arrays.asList("b > 0"), conditions(objectB));
  }

  /**
   * Returns the conditions of the splitters that {@link SplitterList#get_all} returns, among those
   * whose condition is in {@code relevant}. Other tests may leave splitters in SplitterList.
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
  public void testGetAllNoDuplicates() {
    put(
        "splitterlisttest.C.f():::EXIT",
        new Splitter[] {new ConditionSplitter("c > 0"), new ConditionSplitter("d > 0")});
    put(
        "splitterlisttest.D.g():::EXIT",
        new Splitter[] {new ConditionSplitter(" d > 0"), new ConditionSplitter("e > 0")});
    List<String> relevant = Arrays.asList("c > 0", "d > 0", " d > 0", "e > 0");
    assertEquals(Arrays.asList("c > 0", "d > 0", "e > 0"), allConditions(relevant));
  }

  @Test
  public void testGetDifferentReplacements() throws IOException {
    String exit = "splitterlisttest.E.h():::EXIT";
    StatementReplacer replacer1 = replacer("isEmpty()", "size == 0");
    StatementReplacer replacer2 = replacer("isEmpty()", "top == -1");
    // The same condition means different things in different .spinfo files.
    put(exit, new Splitter[] {new ConditionSplitter("isEmpty()")}, replacer1);
    put(exit, new Splitter[] {new ConditionSplitter("isEmpty()")}, replacer2);
    assertEquals(Arrays.asList("isEmpty()", "isEmpty()"), conditions(exit));
    // The same condition means the same thing in the same .spinfo file.
    put(exit, new Splitter[] {new ConditionSplitter("isEmpty()")}, replacer1);
    assertEquals(Arrays.asList("isEmpty()", "isEmpty()"), conditions(exit));
  }

  @Test
  public void testGetSameExpansion() throws IOException {
    String exit = "splitterlisttest.F.f():::EXIT";
    StatementReplacer replacer = replacer("isEmpty()", "top == -1");
    // Conditions that differ as written are not duplicates, even if their expansions are the same.
    put(exit, new Splitter[] {new ConditionSplitter("top == -1")}, replacer);
    put(exit, new Splitter[] {new ConditionSplitter("isEmpty()")}, replacer);
    assertEquals(Arrays.asList("top == -1", "isEmpty()"), conditions(exit));
  }
}
