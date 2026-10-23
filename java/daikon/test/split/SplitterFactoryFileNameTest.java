package daikon.test.split;

import static org.junit.Assert.assertEquals;

import daikon.split.SplitterFactory;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;

/** Tests for {@link SplitterFactory#fileNamePrefix}. */
@RunWith(JUnit4.class)
public class SplitterFactoryFileNameTest {

  @Test
  public void testFileNamePrefixReplacesInvalidCharacters() {
    assertEquals(
        "pkg_C_m_int__int____ENTER", SplitterFactory.fileNamePrefix("pkg.C.m(int, int):::ENTER"));
    assertEquals("a_b$c", SplitterFactory.fileNamePrefix("a_b$c"));
    // A leading digit becomes "_", but other digits are retained.
    assertEquals("_foo2_m", SplitterFactory.fileNamePrefix("1foo2.m"));
    // Non-ASCII characters, including both halves of a surrogate pair, become "_".
    assertEquals("pkg_Gr__e_m", SplitterFactory.fileNamePrefix("pkg.Gr\u00f6\u00dfe.m"));
    assertEquals("a__b", SplitterFactory.fileNamePrefix("a\uD835\uDC00b"));
    // Control characters that Java identifiers ignore become "_".
    assertEquals("a_b", SplitterFactory.fileNamePrefix("a\u0000b"));
  }

  @Test
  public void testFileNamePrefixTruncates() {
    String longName = new String(new char[1000]).replace('\0', 'x');
    assertEquals(200, SplitterFactory.fileNamePrefix(longName).length());
  }
}
