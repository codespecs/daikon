package daikon.test.split;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assume.assumeTrue;

import daikon.split.FileCompiler;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import org.junit.After;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.plumelib.util.FilesPlume;

/** Tests compilation of a set of files, some of which have compilation errors. */
@SuppressWarnings("nullness") // testing code
public class FileCompilerTest {

  /** Creates a FileCompilerTest. */
  public FileCompilerTest() {}

  /** A file that has a type error. */
  private static final String typeErrorClass = "class A { void m() { int x = \"s\"; } }";

  /** A file that has a syntax error. */
  private static final String syntaxErrorClass = "class B { void m() { int x = 1 } }";

  /** A file that has no errors. */
  private static final String goodClass1 = "class C { }";

  /** A file that has no errors. */
  private static final String goodClass2 = "class Z { void m() { } }";

  /** The directory that contains the files to compile. */
  private Path dir;

  /** Skips the tests if javac cannot be run. */
  @BeforeClass
  public static void assumeJavacIsRunnable() {
    boolean runnable;
    try {
      @SuppressWarnings({
        "resourceleak:required.method.not.called", // Process is AutoCloseable only in Java 26+
        "resourceleak:unneeded.suppression" // the suppression is needed only in Java 26+
      })
      Process p = new ProcessBuilder("javac", "-version").redirectErrorStream(true).start();
      try (InputStream in = p.getInputStream()) {
        while (in.read() != -1) {
          // Discard the output, so that the process does not block.
        }
      }
      runnable = p.waitFor() == 0;
    } catch (IOException e) {
      runnable = false;
    } catch (InterruptedException e) {
      Thread.currentThread().interrupt();
      runnable = false;
    }
    assumeTrue("Cannot run javac", runnable);
  }

  /** Creates the directory that contains the files to compile. */
  @Before
  public void createDirectory() throws IOException {
    dir = Files.createTempDirectory("FileCompilerTest");
  }

  /** Deletes the directory that contains the files to compile. */
  @After
  public void deleteDirectory() {
    FilesPlume.deleteDir(dir.toFile());
  }

  /**
   * A syntax error in one file hides a type error in another file, and javac produces no class
   * files. Nonetheless, every good file is compiled, and every error is reported exactly once.
   */
  @Test
  public void testHiddenErrors() throws IOException {
    List<String> files = writeFiles(typeErrorClass, syntaxErrorClass, goodClass1, goodClass2);
    String errors = new FileCompiler("javac -nowarn", 60000).compileFiles(files);

    assertFalse(classFileExists("A"));
    assertFalse(classFileExists("B"));
    assertTrue(errors, classFileExists("C"));
    assertTrue(errors, classFileExists("Z"));
    assertEquals(errors, 1, count(errors, "A.java:1: error:"));
    assertEquals(errors, 1, count(errors, "B.java:1: error:"));
  }

  /**
   * When the compiler's error messages are not recognized, every good file is still compiled, and
   * every error is still reported exactly once.
   */
  @Test
  public void testUnrecognizedErrorMessages() throws IOException {
    List<String> files = writeFiles(typeErrorClass, syntaxErrorClass, goodClass1, goodClass2);
    // -XDrawDiagnostics changes the format of javac's error messages to one that FileCompiler does
    // not recognize; for example, "A.java:1:30: compiler.err.prob.found.req: ...".
    String errors =
        new FileCompiler("javac -nowarn -XDrawDiagnostics", 60000).compileFiles(files);

    assertFalse(classFileExists("A"));
    assertFalse(classFileExists("B"));
    assertTrue(errors, classFileExists("C"));
    assertTrue(errors, classFileExists("Z"));
    assertEquals(errors, 1, count(errors, "A.java:"));
    assertEquals(errors, 1, count(errors, "B.java:"));
  }

  /** An error that is not about any one file is reported exactly once. */
  @Test
  public void testFileIndependentError() throws IOException {
    List<String> files = writeFiles(goodClass1, goodClass2);
    String errors = new FileCompiler("javac -nowarn -badflag", 60000).compileFiles(files);

    assertFalse(classFileExists("C"));
    assertFalse(classFileExists("Z"));
    assertEquals(errors, 1, count(errors, "-badflag"));
  }

  /** A file that does not exist does not prevent the other files from being compiled. */
  @Test
  public void testMissingFile() throws IOException {
    List<String> files = writeFiles(goodClass1);
    String missingFile = dir.resolve("Missing.java").toString();
    files.add(missingFile);
    String errors = new FileCompiler("javac -nowarn", 60000).compileFiles(files);

    assertTrue(errors, classFileExists("C"));
    assertEquals(errors, 1, count(errors, missingFile));
  }

  /**
   * Writes each class to a file in {@link #dir}.
   *
   * @param classes the source code of classes, each of whose names is one letter long
   * @return the paths of the files
   */
  private List<String> writeFiles(String... classes) throws IOException {
    List<String> result = new ArrayList<>();
    for (String c : classes) {
      Path file = dir.resolve(c.substring("class ".length(), "class ".length() + 1) + ".java");
      Files.writeString(file, c);
      result.add(file.toString());
    }
    return result;
  }

  /**
   * Returns true if the class file for the given class exists in {@link #dir}.
   *
   * @param className the name of a class
   * @return true if the class file for the given class exists
   */
  private boolean classFileExists(String className) {
    return Files.exists(dir.resolve(className + ".class"));
  }

  /**
   * Returns the number of non-overlapping occurrences of {@code target} in {@code s}.
   *
   * @param s a string
   * @param target the string to search for
   * @return the number of occurrences of {@code target} in {@code s}
   */
  private static int count(String s, String target) {
    int result = 0;
    for (int i = s.indexOf(target); i != -1; i = s.indexOf(target, i + target.length())) {
      result++;
    }
    return result;
  }
}
