package daikon.test.split;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assume.assumeTrue;

import daikon.split.FileCompiler;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.junit.After;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

/** Tests {@link FileCompiler}. */
@SuppressWarnings("nullness") // testing code
public class FileCompilerTest {

  /** Creates a FileCompilerTest. */
  public FileCompilerTest() {}

  /** The compile command. {@code -Xlint:rawtypes} makes javac warn about a raw type. */
  private static final String compiler = "javac -Xlint:rawtypes";

  /** The directory that contains the source files. Its name contains a space. */
  private Path dir;

  /** Skips the tests if javac cannot be run. */
  @BeforeClass
  public static void assumeJavacIsRunnable() {
    assumeTrue("Cannot run javac", javacIsRunnable());
  }

  /**
   * Returns true if javac can be run.
   *
   * @return true if javac can be run
   */
  private static boolean javacIsRunnable() {
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
      return p.waitFor() == 0;
    } catch (IOException e) {
      return false;
    } catch (InterruptedException e) {
      Thread.currentThread().interrupt();
      return false;
    }
  }

  /** Creates the directory for the source files. */
  @Before
  public void createDirectory() throws IOException {
    dir = Files.createTempDirectory("FileCompiler Test");
  }

  /** Deletes the directory for the source files. */
  @After
  public void deleteDirectory() throws IOException {
    List<Path> paths;
    try (Stream<Path> walk = Files.walk(dir)) {
      // Delete the files before the directory that contains them.
      paths = walk.sorted(Comparator.reverseOrder()).collect(Collectors.toList());
    }
    for (Path p : paths) {
      Files.delete(p);
    }
  }

  /**
   * Writes a Java source file.
   *
   * @param className the name of the class, which is also the base name of the file
   * @param contents the contents of the file
   * @return the path of the file
   */
  private String writeSource(String className, String contents) throws IOException {
    Path file = dir.resolve(className + ".java");
    Files.writeString(file, contents);
    return file.toString();
  }

  /**
   * Returns true if the class file for the given class exists.
   *
   * @param className the name of a class
   * @return true if the class file for the class exists
   */
  private boolean compiled(String className) {
    return Files.exists(dir.resolve(className + ".class"));
  }

  /**
   * javac reports type errors only if there are no syntax errors, so excluding the files with
   * syntax errors may reveal type errors in other files. The files without errors, including a file
   * with only a warning, must be compiled anyway.
   */
  @Test
  public void testRecompileUntilNoErrors() throws IOException {
    List<String> files = new ArrayList<>();
    files.add(writeSource("Good", "class Good {}\n"));
    files.add(writeSource("SyntaxError", "class SyntaxError { int x = ; }\n"));
    files.add(writeSource("TypeError", "class TypeError { int x = \"string\"; }\n"));
    files.add(writeSource("Warning", "class Warning { java.util.List rawList; }\n"));

    String errors = new FileCompiler(compiler, 60_000).compileFiles(files);

    assertTrue(compiled("Good"));
    assertTrue(compiled("Warning"));
    assertFalse(compiled("SyntaxError"));
    assertFalse(compiled("TypeError"));
    // The result contains the errors of every compilation.
    assertTrue(errors, errors.contains("SyntaxError.java:1: error:"));
    assertTrue(errors, errors.contains("TypeError.java:1: error:"));
  }

  /**
   * After a syntax error, javac reports no warnings for other files, so a warning first appears in
   * a recompilation, even one that succeeds.
   */
  @Test
  public void testWarningFromSuccessfulRecompilation() throws IOException {
    List<String> files = new ArrayList<>();
    files.add(writeSource("SyntaxError", "class SyntaxError { int x = ; }\n"));
    files.add(writeSource("Warning", "class Warning { java.util.List rawList; }\n"));

    String errors = new FileCompiler(compiler, 60_000).compileFiles(files);

    assertTrue(compiled("Warning"));
    assertFalse(compiled("SyntaxError"));
    assertTrue(errors, errors.contains("SyntaxError.java:1: error:"));
    assertTrue(errors, errors.contains("Warning.java:1: warning:"));
  }

  /**
   * With {@code -Werror}, a warning makes the compilation fail without reporting an error in any
   * file. The file with the warning must be excluded so that the other files are compiled.
   */
  @Test
  public void testWerror() throws IOException {
    List<String> files = new ArrayList<>();
    files.add(writeSource("Good", "class Good {}\n"));
    files.add(writeSource("Warning", "class Warning { java.util.List rawList; }\n"));

    String errors = new FileCompiler(compiler + " -Werror", 60_000).compileFiles(files);

    assertTrue(compiled("Good"));
    assertFalse(compiled("Warning"));
    assertTrue(errors, errors.contains("Warning.java:1: warning:"));
  }

  /** If no file has an error, the result contains only warnings. */
  @Test
  public void testNoErrors() throws IOException {
    List<String> files = new ArrayList<>();
    files.add(writeSource("Good", "class Good {}\n"));
    files.add(writeSource("Warning", "class Warning { java.util.List rawList; }\n"));

    String errors = new FileCompiler(compiler, 60_000).compileFiles(files);

    assertTrue(compiled("Good"));
    assertTrue(compiled("Warning"));
    assertFalse(errors, errors.contains("error:"));
    assertTrue(errors, errors.contains("Warning.java:1: warning:"));
  }
}
