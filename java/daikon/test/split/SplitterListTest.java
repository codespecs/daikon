package daikon.test.split;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import daikon.split.SplitterList;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;

/** Tests for {@link SplitterList}. */
@RunWith(JUnit4.class)
public class SplitterListTest {

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
}
