package daikon.test;

import static org.junit.Assert.assertEquals;

import daikon.chicory.DaikonWriter;
import org.junit.Test;

/** Tests for {@link DaikonWriter}. */
public class DaikonWriterTest {

  /**
   * Asserts that {@link DaikonWriter#escape} converts the given name to the given representation
   * and that {@link DaikonWriter#unescape} converts it back.
   *
   * @param name a name
   * @param escaped the representation of the name in a decl/dtrace file
   */
  private static void assertRoundTrip(String name, String escaped) {
    assertEquals(escaped, DaikonWriter.escape(name));
    assertEquals(name, DaikonWriter.unescape(escaped));
  }

  /** {@link DaikonWriter#escape} and {@link DaikonWriter#unescape} are inverses. */
  @Test
  public void testEscapeRoundTrip() {
    assertRoundTrip("", "");
    assertRoundTrip("C.m():::ENTER", "C.m():::ENTER");
    assertRoundTrip("C.m(int, int):::ENTER", "C.m(int,\\_int):::ENTER");
    assertRoundTrip("a\\b", "a\\\\b");
    assertRoundTrip("\\", "\\\\");
    assertRoundTrip(" ", "\\_");
    assertRoundTrip("\\ ", "\\\\\\_");
    assertRoundTrip("\\_", "\\\\_");
    assertRoundTrip("a \\\\ b", "a\\_\\\\\\\\\\_b");
  }

  /** {@link DaikonWriter#unescape} retains a backslash that begins no escape sequence. */
  @Test
  public void testUnescapeUnpairedBackslash() {
    assertEquals("a\\xb", DaikonWriter.unescape("a\\xb"));
    assertEquals("a\\", DaikonWriter.unescape("a\\"));
  }
}
