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
    // Non-ASCII characters, including both halves of a surrogate pair, become "_".
    assertEquals("pkg_Gr__e_m", SplitterFactory.fileNamePrefix("pkg.Größe.m"));
    assertEquals("a__b", SplitterFactory.fileNamePrefix("a𝐀b"));
    // Control characters that Java identifiers ignore become "_".
    assertEquals("a_b", SplitterFactory.fileNamePrefix("a\u0000b"));
  }

  @Test
  public void testFileNamePrefixTruncates() {
    StringBuilder longName = new StringBuilder();
    for (int i = 0; i < 1000; i++) {
      longName.append('x');
    }
    assertEquals(200, SplitterFactory.fileNamePrefix(longName.toString()).length());
  }
}
