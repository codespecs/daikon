package daikon.test.split;

import daikon.*;
import daikon.split.*;
import java.io.BufferedWriter;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import org.plumelib.util.FilesPlume;

/**
 * This class's main method can be used to update both the target files of SplitterFactoryTest and
 * the code of the SplitterFactoryTest itself.
 *
 * <p>To use this program to update SplitterFactoryTest and the target files, run
 *
 * <pre>    \rm -f targets/*.java.goal SplitterFactoryTest.java</pre>
 *
 * Then simply run the main method without any arguments in the daikon/java directory, and then
 * re-compile the SplitterFactoryTest.
 *
 * <p>To add additional tests to this test program, place the .spinfo and decls files into the
 * "targets" directory then add a call to generateSplitters with the new files. generateSplitters is
 * overloaded, and takes either one {@code .spinfo} file and one decls file, or else a list of each.
 */
public class SplitterFactoryTestUpdater {
  public static java.lang.Runtime commander = java.lang.Runtime.getRuntime();
  private static String targetDir = "daikon/test/split/targets/";
  private static String splitDir = "daikon/test/split/";

  private static ArrayList<ArrayList<File>> spinfoFileLists = new ArrayList<>();
  private static ArrayList<ArrayList<File>> declsFileLists = new ArrayList<>();
  private static ArrayList<String> classNames = new ArrayList<>();

  private SplitterFactoryTestUpdater() {} // blocks public constructor

  /**
   * If one has changed the test cases used below, for best results run {@code rm *.java.goal} while
   * in the targets directory before running this method. Creates new splitter java files, moves the
   * new files into target directory, rewrites the code of SplitterFactoryTest to use the new files.
   * One should recompile SplitterFactoryTest after running this method.
   *
   * @param args are ignored
   */
  public static void main(String[] args) {
    // For debugging
    // SplitterFactory.dkconfig_delete_splitters_on_exit = false;

    generateSplitters("StreetNumberSet.spinfo", "StreetNumberSet.decls");
    generateSplitters("Fib.spinfo", "Fib.decls");
    generateSplitters("QueueAr.spinfo", "QueueAr.decls");
    generateSplitters("BigFloat.spinfo", "BigFloat.decls");
    moveFiles();
    writeTestClass();
  }

  /**
   * This is a short-cut method if only one spinfo file and only one decls files is to be used.
   *
   * @see #generateSplitters(List, List)
   */
  private static void generateSplitters(String spinfoFile, String declsFile) {
    List<String> spinfo = new ArrayList<>();
    spinfo.add(spinfoFile);
    List<String> decls = new ArrayList<>();
    decls.add(declsFile);
    generateSplitters(spinfo, decls);
  }

  /**
   * Generates the splitter {@code .java} files.
   *
   * @param spinfos the spinfo files that should be used in generating the splitter java files
   * @param decls the decls files that should be used in generating the splitter java files
   */
  private static void generateSplitters(List<String> spinfos, List<String> decls) {
    HashSet<File> declsFileSet = new HashSet<>();
    HashSet<File> spinfoFiles = new HashSet<>();
    PptMap allPpts = new PptMap();
    for (String spinfoFile : spinfos) {
      spinfoFile = targetDir + spinfoFile;
      spinfoFiles.add(new File(spinfoFile));
    }
    spinfoFileLists.add(new ArrayList<File>(spinfoFiles));
    for (String declsFile : decls) {
      declsFile = targetDir + declsFile;
      declsFileSet.add(new File(declsFile));
    }
    declsFileLists.add(new ArrayList<File>(declsFileSet));
    try {
      PptSplitter.dkconfig_suppressSplitterErrors = true;
      Daikon.create_splitters(spinfoFiles);
      // calling read_data_trace_file in a loop instead of calling
      // read_data_trace_files allows us to mix version 1 and
      // version 2 decls file formats.
      for (String declsFile : decls) {
        // This reset allows current format to differ from previous.
        FileIO.resetNewDeclFormat();
        FileIO.read_data_trace_file(targetDir + declsFile, allPpts);
      }
    } catch (IOException e) {
      throw new RuntimeException(e);
    }
  }

  /** Moves the generated splitter files from the tempDir to the target Dir. */
  private static void moveFiles() {
    File tempDir = new File(SplitterFactory.getTempDir());
    String[] fileNames = tempDir.list();
    if (fileNames == null) {
      throw new Error("tmpdir = " + tempDir + " which is not a directory");
    }
    for (int i = 0; i < fileNames.length; i++) {
      if (fileNames[i].endsWith(".java")) {
        String fileName = fileNames[i];
        String fromName = tempDir.getPath() + File.separator + fileName;
        String toName = targetDir + fileName + ".goal";
        try {
          moveFile(fromName, toName);
        } catch (Error e) {
          System.out.printf("Failed to move %s to %s%n", fromName, toName);
          throw e;
        } catch (RuntimeException e) {
          System.out.printf("Failed to move %s to %s%n", fromName, toName);
          throw e;
        }
        String javaFileName = new File(fileName).getName();
        String className = javaFileName.substring(0, javaFileName.length() - ".java".length());
        classNames.add(className);
      }
    }
  }

  private static void moveFile(String fromName, String toName) {
    File from = new File(fromName);
    File to = new File(toName);
    if (!from.exists()) {
      throw new Error("Does not exist: " + fromName);
    }
    if (!from.canRead()) {
      throw new Error("Cannot read " + fromName);
    }
    // canWrite() requires that the file already exists.  So comment this out.
    // if (! to.canWrite()) {
    //   throw new Error("Cannot write " + toName + " = " + to.getAbsoluteFile()
    //                   + " when copying from " + fromName);
    // }
    if (to.exists()) {
      to.delete();
    }
    // file.renameTo(to) fails if the two files are on different file systems
    // (e.g., /tmp and /scratch may be different).
    // So read and write the file directly rather than using renameTo().
    FilesPlume.writeString(to, FilesPlume.readString(from.toPath()));
  }

  /** Writes the new code for "SplitterFactoryTest.java". */
  private static void writeTestClass() {
    String code = getTestClassText();
    try {
      // Delete the file, in case it is unwriteable (in which case deleting
      // works, but overwriting does not).
      new File(splitDir + "SplitterFactoryTest.java").delete();
      try (BufferedWriter writer =
          FilesPlume.newBufferedFileWriter(splitDir + "SplitterFactoryTest.java")) {
        writer.write(code);
        writer.flush();
      }
    } catch (IOException e) {
      throw new RuntimeException(e);
    }
  }

  /** Returns a String of the new text for the SplitterFactoryTest class. */
  private static String getTestClassText() {
    OutputStream code = new ByteArrayOutputStream();
    PrintStream ps = new PrintStream(code);

    // Use string concatenation to avoid matching the text literally.
    ps.println("// ***** This file is automatically generated by SplitterFactoryTestUpdater.java");
    ps.println();
    ps.println("package daikon.test.split;");
    ps.println();
    ps.println("import daikon.*;");
    ps.println("import daikon.split.*;");
    ps.println("import gnu.getopt.*;");
    ps.println("import java.io.*;");
    ps.println("import java.util.*;");
    ps.println("import junit.framework.*;");
    ps.println("import org.junit.Test;");
    ps.println("import org.plumelib.util.FilesPlume;");
    ps.println("import org.plumelib.util.StringsPlume;");
    ps.println("import org.plumelib.util.UtilPlume;");
    ps.println("import static org.junit.Assert.fail;");

    ps.println();
    ps.println("import org.checkerframework.checker.nullness.qual.*;");
    ps.println();
    ps.println("/**");
    ps.println(
        " * THIS CLASS WAS GENERATED BY SplitterFactoryTestUpdater. Therefore, it is a bad idea");
    ps.println(
        " * to directly edit this class's code for all but temporary reasons. Any permanent");
    ps.println(" * changes should be made through SplitterFactoryUpdater.");
    ps.println(" *");
    ps.println(
        " * <p>This class contains regression tests for the SplitterFactory class. The tests");
    ps.println(
        " * directly test the Java files produced by the load_splitters method by comparing them");
    ps.println(
        " * against goal files. Note that it is normal for some classes not to compile during");
    ps.println(" * this test.");
    ps.println(" *");
    ps.println(" * <p>These tests assume that the goal files are contained in the directory:");
    ps.println(" * \"" + targetDir + "\"");
    ps.println(" *");
    ps.println(" * <p>These tests ignore extra white spaces.");
    ps.println(" */");
    ps.println("public class SplitterFactoryTest {");
    ps.println("  // Because the SplitterFactory sequentially numbers the");
    ps.println("  // java files it produces, changing the order that the setUpTests");
    ps.println("  // commands are run will cause the tests to fail.");
    ps.println();
    ps.println("  private static String targetDir = \"" + targetDir + "\";");
    ps.println();
    ps.println("  private static @Nullable String tempDir = null;");
    ps.println();
    ps.println("  private static boolean saveFiles = false;");
    ps.println();
    ps.println("  private static String usage =");
    ps.println("    StringsPlume.joinLines(");
    ps.println("      \"Usage:  java daikon.tools.CreateSpinfo FILE.java ...\",");
    ps.println(
        "      \"  -s       Save (do not delete) the splitter java files in the temp directory\",");
    ps.println("      \"  -h       Display this usage message\");");
    ps.println();
    ps.println("  public static void main(String[] args) {");
    ps.println(
        "    Getopt g = new Getopt(\"daikon.test.split.SplitterFactoryTest\", args, \"hs\");");
    ps.println("    int c;");
    ps.println("    while ((c = g.getopt()) != -1) {");
    ps.println("      switch (c) {");
    ps.println("        case 's':");
    ps.println("          saveFiles = true;");
    ps.println("          break;");
    ps.println("        case 'h':");
    ps.println("          System.out.println(usage);");
    ps.println("          System.exit(1);");
    ps.println("          break;");
    ps.println("        case '?':");
    ps.println("          break;");
    ps.println("        default:");
    ps.println("          System.out.println(\"getopt() returned \" + c);");
    ps.println("          break;");
    ps.println("      }");
    ps.println("    }");
    ps.println("  }");
    ps.println();

    appendSetUpTest(ps);

    ps.println();
    ps.println("  /** Sets up the test by generating the needed splitter java files. */");
    ps.println("  private static void createSplitterFiles(String spinfo, String decl) {");
    ps.println("    List<String> spinfoFiles = new ArrayList<>();");
    ps.println("    spinfoFiles.add(spinfo);");
    ps.println("    List<String> declsFiles = Collections.singletonList(decl);");
    ps.println("    createSplitterFiles(spinfoFiles, declsFiles);");
    ps.println("  }");
    ps.println();
    ps.println("  /** Sets up the test by generating the needed splitter java files. */");
    ps.println(
        "  private static void createSplitterFiles(List<String> spinfos, List<String> decls) {");
    ps.println("    Set<File> spFiles = new HashSet<>();");
    ps.println("    PptMap allPpts = new PptMap();");
    ps.println("    for (String spinfo : spinfos) {");
    ps.println("      spFiles.add(new File(spinfo));");
    ps.println("    }");
    ps.println("    try {");
    ps.println("      if (saveFiles) {");
    ps.println("        SplitterFactory.dkconfig_delete_splitters_on_exit = false;");
    ps.println("      }");
    ps.println("      PptSplitter.dkconfig_suppressSplitterErrors = true;");
    ps.println("      Daikon.create_splitters(spFiles);");
    ps.println("      for (String declsFile : decls) {");
    ps.println("        FileIO.resetNewDeclFormat();");
    ps.println(
        "        FileIO.read_data_trace_file(declsFile, allPpts);"); // invoked for side effects
    ps.println("      }");
    ps.println("      tempDir = SplitterFactory.getTempDir();");
    ps.println("    } catch (IOException e) {");
    ps.println("      throw new RuntimeException(e);");
    ps.println("    }");
    ps.println("  }");
    ps.println();

    appendTests(ps);

    ps.println("}");

    ps.close();
    return code.toString();
  }

  /**
   * Appends the code to write the static block of code to code. This code is used by the
   * SplitterFactoryTest to set up the needed files to run the tests on.
   */
  public static void appendSetUpTest(PrintStream ps) {
    ps.println("  private static void setUpTests() {");
    ps.println("    List<String> spinfoFiles;");
    ps.println("    List<String> declsFiles;");
    for (int i = 0; i < spinfoFileLists.size(); i++) {
      ps.println("    createSplitterFiles(");
      ps.println(
          "        \""
              + FilesPlume.javaSource(spinfoFileLists.get(i).get(0))
              + "\", \""
              + FilesPlume.javaSource(declsFileLists.get(i).get(0))
              + "\");");
    }
    ps.println("  }");
  }

  /** Appends the code that executes the test in SplitterFactoryTest to code. */
  private static void appendTests(PrintStream ps) {
    ps.println("  /** Returns true iff files are the same (ignoring extra white space). */");
    ps.println("  public static void assertEqualFiles(String f1, String f2) {");
    ps.println("    if (!FilesPlume.equalFiles(f1, f2)) {");
    ps.println("      fail(\"Files \" + f1 + \" and \" + f2 + \" differ.\");");
    ps.println("    }");
    ps.println("  }");
    ps.println();
    ps.println("  public static void assertEqualFiles(String f1) {");
    ps.println("    assertEqualFiles(tempDir + f1, targetDir + f1 + \".goal\");");
    ps.println("  }");
    ps.println();
    for (String className : classNames) {
      ps.println("  @Test");
      ps.println("  public static void test" + className + "() {");
      ps.println("    assertEqualFiles(\"" + className + ".java\");");
      ps.println("  }");
      ps.println();
    }
  }
}
