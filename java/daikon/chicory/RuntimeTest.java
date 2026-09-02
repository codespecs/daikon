package daikon.chicory;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThrows;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;

/** Tests for {@link daikon.chicory.Runtime}. */
@RunWith(JUnit4.class)
public class RuntimeTest {

  /**
   * Tests {@link Runtime#javaMajorVersion}. The values are real {@code java.version} strings; the
   * bare "9" is the one that matters most, because Java 9 GA reported its version with no separator
   * after the major number.
   */
  @Test
  public void testJavaMajorVersion() {
    // Java 8 and earlier: the major version is the second component.
    assertEquals(8, Runtime.javaMajorVersion("1.8.0_432"));
    assertEquals(7, Runtime.javaMajorVersion("1.7.0_80"));
    // Java 9 and later, with minor components.
    assertEquals(9, Runtime.javaMajorVersion("9.0.4"));
    assertEquals(11, Runtime.javaMajorVersion("11.0.28"));
    assertEquals(24, Runtime.javaMajorVersion("24.0.1"));
    assertEquals(25, Runtime.javaMajorVersion("25.0.2"));
    // Java 9 and later, major version alone.
    assertEquals(9, Runtime.javaMajorVersion("9"));
    assertEquals(24, Runtime.javaMajorVersion("24"));
    // Pre-release and build identifiers.
    assertEquals(9, Runtime.javaMajorVersion("9-ea"));
    assertEquals(26, Runtime.javaMajorVersion("26-ea"));
    assertEquals(26, Runtime.javaMajorVersion("26+11"));

    assertThrows(IllegalArgumentException.class, () -> Runtime.javaMajorVersion("bogus"));
  }

  /** Tests that {@link Runtime#isJava24orLater} agrees with the JVM running the test. */
  @Test
  public void testIsJava24orLater() {
    int major = Runtime.javaMajorVersion(System.getProperty("java.version"));
    assertEquals(major >= 24, Runtime.isJava24orLater());
    assertEquals(major >= 9, Runtime.isJava9orLater());
  }
}
