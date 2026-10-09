package daikon.split;

import static java.nio.charset.StandardCharsets.UTF_8;
import static org.junit.Assert.assertEquals;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;

/** Tests for {@link SplitterFactory#truncateToUtf8Bytes}. */
@RunWith(JUnit4.class)
public class SplitterFactoryFileNameTest {

  @Test
  public void testTruncateToUtf8BytesAscii() {
    assertEquals("abc", SplitterFactory.truncateToUtf8Bytes("abc", 5));
    assertEquals("abc", SplitterFactory.truncateToUtf8Bytes("abc", 3));
    assertEquals("ab", SplitterFactory.truncateToUtf8Bytes("abc", 2));
    assertEquals("", SplitterFactory.truncateToUtf8Bytes("abc", 0));
  }

  @Test
  public void testTruncateToUtf8BytesNonAscii() {
    // Each of these characters is 3 bytes in UTF-8.
    String cjk = "一丁丂";
    assertEquals("一", SplitterFactory.truncateToUtf8Bytes(cjk, 5));
    assertEquals("一丁", SplitterFactory.truncateToUtf8Bytes(cjk, 6));
    StringBuilder longCjk = new StringBuilder();
    for (int i = 0; i < 100; i++) {
      longCjk.append(cjk);
    }
    assertEquals(
        198, SplitterFactory.truncateToUtf8Bytes(longCjk.toString(), 200).getBytes(UTF_8).length);
  }

  @Test
  public void testTruncateToUtf8BytesSurrogatePair() {
    // U+1D400 is 4 bytes in UTF-8 and 2 chars in UTF-16.
    String s = "a𝐀b";
    assertEquals("a", SplitterFactory.truncateToUtf8Bytes(s, 4));
    assertEquals("a𝐀", SplitterFactory.truncateToUtf8Bytes(s, 5));
  }
}
