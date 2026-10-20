package daikon.test.split;

import daikon.Daikon;
import daikon.FileIO;
import daikon.PptMap;
import daikon.split.PptSplitter;
import daikon.split.SplitterFactory;
import java.io.BufferedWriter;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.plumelib.util.FilesPlume;

/**
 * This class's main method writes the code of SplitterFactoryTest. When passed {@code
 * --update-goals}, it also overwrites the goal files of SplitterFactoryTest with the splitter files
 * that the current SplitterFactory produces.
 *
 * <p>To update the goal files, run {@code make update-splitter-goals} in the daikon/java directory,
 * then examine the differences in the goal files (for example, with {@code git diff}) before
 * committing them.
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

  /** The class names of the generated splitters that are expected to compile. */
  private static Set<String> compilingClassNames = new HashSet<>();

  /**
   * The splitters that are expected not to compile, because their condition refers to a variable
   * that is not in the decls file. SplitterFactoryTest checks that every other splitter compiles.
   * Each element is a {@link #splitterKey splitter key}.
   */
  private static final Set<String> nonCompilingSplitters =
      new HashSet<>(
          Arrays.asList(
              "MapQuick1_StreetNumberSet_StreetNumberSet: dashLoc == -1",
              "MapQuick1_StreetNumberSet_StreetNumberSet: items.hasMoreTokens()",
              "MapQuick1_StreetNumberSet_StreetNumberSet: orig(dashLoc == -1)",
              "MapQuick1_StreetNumberSet_StreetNumberSet: orig(items.hasMoreTokens())",
              "MapQuick1_StreetNumberSet_checkRep: i<sets.length",
              "MapQuick1_StreetNumberSet_checkRep: orig(i<sets.length)",
              "MapQuick1_StreetNumberSet_contains: i<sets.length",
              "MapQuick1_StreetNumberSet_contains: orig(i<sets.length)",
              "MapQuick1_StreetNumberSet_contains: orig(sets[i].contains(sn))",
              "MapQuick1_StreetNumberSet_contains: sets[i].contains(sn)",
              "MapQuick1_StreetNumberSet_orderStatistic: i<sets.length",
              "MapQuick1_StreetNumberSet_orderStatistic: orig(i<sets.length)",
              "MapQuick1_StreetNumberSet_size: i<sets.length",
              "MapQuick1_StreetNumberSet_size: orig(i<sets.length)",
              "misc_Fib_main: i < STEPS"));

  /**
   * Matches the condition in a splitter's source code. Group 1 is the condition, as a Java string
   * literal body.
   */
  private static final Pattern conditionPattern =
      Pattern.compile("String condition\\(\\) \\{\\s*return \"(.*)\";");

  private SplitterFactoryTestUpdater() {} // blocks public constructor

  /**
   * Creates new splitter java files and writes the code of SplitterFactoryTest, which compares
   * splitter files against the goal files. One should recompile SplitterFactoryTest after running
   * this method.
   *
   * @param args either empty, or {@code --update-goals} to also replace the goal files in the
   *     target directory by the new splitter java files
   */
  public static void main(String[] args) {
    boolean updateGoals;
    if (args.length == 0) {
      updateGoals = false;
    } else if (args.length == 1 && args[0].equals("--update-goals")) {
      updateGoals = true;
    } else {
      System.err.println(
          "Usage:  java daikon.test.split.SplitterFactoryTestUpdater [--update-goals]");
      System.exit(1);
      return;
    }

    // For debugging
    // SplitterFactory.dkconfig_delete_splitters_on_exit = false;

    generateSplitters("StreetNumberSet.spinfo", "StreetNumberSet.decls");
    generateSplitters("Fib.spinfo", "Fib.decls");
    generateSplitters("QueueAr.spinfo", "QueueAr.decls");
    generateSplitters("BigFloat.spinfo", "BigFloat.decls");
    generateSplitters("CharBuf.spinfo", "CharBuf.decls");
    processFiles(updateGoals);
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

  /**
   * Records the class names of the generated splitter files, which are in the tempDir.
   *
   * @param updateGoals if true, also replace the goal files in the target directory by the
   *     generated splitter files
   */
  private static void processFiles(boolean updateGoals) {
    File tempDir = new File(SplitterFactory.getTempDir());
    String[] fileNames = tempDir.list();
    if (fileNames == null) {
      throw new Error("tmpdir = " + tempDir + " which is not a directory");
    }
    // Sort so that the generated test does not depend on the file system's directory order.
    Arrays.sort(fileNames);
    if (updateGoals) {
      // Remove all goal files, so that none remain for splitters that SplitterFactory does not
      // produce.
      File[] oldGoals = new File(targetDir).listFiles((dir, name) -> name.endsWith(".java.goal"));
      if (oldGoals == null) {
        throw new Error("targetDir = " + targetDir + " which is not a directory");
      }
      for (File oldGoal : oldGoals) {
        if (!oldGoal.delete()) {
          throw new Error("Failed to delete " + oldGoal);
        }
      }
    }
    // The elements of nonCompilingSplitters that have not yet matched a generated splitter.
    Set<String> unmatched = new HashSet<>(nonCompilingSplitters);
    for (int i = 0; i < fileNames.length; i++) {
      if (fileNames[i].endsWith(".java")) {
        String fileName = fileNames[i];
        String fromName = tempDir.getPath() + File.separator + fileName;
        String className = fileName.substring(0, fileName.length() - ".java".length());
        classNames.add(className);
        if (!unmatched.remove(splitterKey(className, fromName))) {
          compilingClassNames.add(className);
        }
        if (updateGoals) {
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
        }
      }
    }
    if (!unmatched.isEmpty()) {
      throw new Error("No splitter matches these nonCompilingSplitters: " + unmatched);
    }
  }

  /**
   * Returns a key that identifies a splitter independently of the sequence number that
   * SplitterFactory gives it: its class name without the sequence number, then ": ", then its
   * condition.
   *
   * @param className the splitter's class name
   * @param javaFileName the splitter's source file
   * @return the key for the splitter
   */
  private static String splitterKey(String className, String javaFileName) {
    String source = FilesPlume.readString(new File(javaFileName).toPath());
    Matcher m = conditionPattern.matcher(source);
    if (!m.find()) {
      throw new Error("No condition found in " + javaFileName);
    }
    return className.replaceFirst("_[0-9]+$", "") + ": " + m.group(1);
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
    ps.println("import org.junit.BeforeClass;");
    ps.println("import org.junit.Test;");
    ps.println("import org.junit.internal.TextListener;");
    ps.println("import org.junit.runner.JUnitCore;");
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
        " * against goal files. They also check that each splitter compiles, except the ones in");
    ps.println(" * SplitterFactoryTestUpdater.nonCompilingSplitters.");
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
    ps.println("      \"Usage:  java daikon.test.split.SplitterFactoryTest [OPTION]...\",");
    ps.println(
        "      \"  -s          Save (do not delete) the splitter java files in the temp"
            + " directory\",");
    ps.println("      \"  -h          Display this usage message\");");
    ps.println();
    ps.println("  /**");
    ps.println("   * Runs the tests.");
    ps.println("   *");
    ps.println("   * @param args command-line arguments");
    ps.println("   */");
    ps.println("  public static void main(String[] args) {");
    ps.println("    try {");
    ps.println("      if (!mainHelper(args)) {");
    ps.println("        System.exit(1);");
    ps.println("      }");
    ps.println("    } catch (Daikon.DaikonTerminationException e) {");
    ps.println("      Daikon.handleDaikonTerminationException(e);");
    ps.println("    }");
    ps.println("  }");
    ps.println();
    ps.println("  /**");
    ps.println(
        "   * This does the work of {@link #main(String[])}, but it never calls System.exit,"
            + " so");
    ps.println("   * it is appropriate to be called programmatically.");
    ps.println("   *");
    ps.println("   * @param args the command-line arguments; see {@link #usage}");
    ps.println("   * @return true if all the tests passed");
    ps.println("   */");
    ps.println("  public static boolean mainHelper(String[] args) {");
    ps.println("    DaikonGetopt g = new DaikonGetopt(args, \"s\", new LongOpt[0], usage);");
    ps.println("    int c;");
    ps.println("    while ((c = g.getopt()) != -1) {");
    ps.println("      switch (c) {");
    ps.println("        case 's':");
    ps.println("          saveFiles = true;");
    ps.println("          break;");
    ps.println("        default:");
    ps.println("          throw new Daikon.BugInDaikon(\"getopt() returned \" + c);");
    ps.println("      }");
    ps.println("    }");
    ps.println("    if (g.getOptind() < args.length) {");
    ps.println("      throw new Daikon.UserError(");
    ps.println(
        "          \"Unexpected argument \" + args[g.getOptind()] + \"; run with -h for"
            + " usage\");");
    ps.println("    }");
    ps.println("    JUnitCore junit = new JUnitCore();");
    ps.println("    junit.addListener(new TextListener(System.out));");
    ps.println("    return junit.run(SplitterFactoryTest.class).wasSuccessful();");
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
   * Writes the {@code @BeforeClass} method setUpTests to {@code ps}. SplitterFactoryTest uses
   * setUpTests to generate the splitter files that its tests check.
   */
  public static void appendSetUpTest(PrintStream ps) {
    ps.println("  @BeforeClass");
    ps.println("  public static void setUpTests() {");
    ps.println("    SplitterFactory.resetGuid();");
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
    ps.println(
        "  /** Fails the test unless the files are the same (ignoring extra white space). */");
    ps.println("  public static void assertEqualFiles(String f1, String f2) {");
    ps.println("    if (!FilesPlume.equalFiles(f1, f2)) {");
    ps.println("      fail(\"Files \" + f1 + \" and \" + f2 + \" differ.\");");
    ps.println("    }");
    ps.println("  }");
    ps.println();
    ps.println("  public static void assertEqualFiles(String f1) {");
    ps.println("    String goalFile = targetDir + f1 + \".goal\";");
    ps.println("    if (!new File(goalFile).exists()) {");
    ps.println(
        "      fail(\"Goal file \" + goalFile"
            + " + \" does not exist; run: make update-splitter-goals\");");
    ps.println("    }");
    ps.println("    assertEqualFiles(tempDir + f1, goalFile);");
    ps.println("  }");
    ps.println();
    ps.println("  /** Fails the test unless the splitter class compiled. */");
    ps.println("  public static void assertCompiled(String className) {");
    ps.println("    if (!new File(tempDir + className + \".class\").exists()) {");
    ps.println("      fail(");
    ps.println("          \"Splitter \"");
    ps.println("              + className");
    ps.println(
        "              + \" did not compile. To see why, run SplitterFactoryTest with -s, then"
            + " compile \"");
    ps.println("              + tempDir");
    ps.println("              + className");
    ps.println("              + \".java\");");
    ps.println("    }");
    ps.println("  }");
    ps.println();
    for (String className : classNames) {
      ps.println("  @Test");
      ps.println("  public void test" + className + "() {");
      ps.println("    assertEqualFiles(\"" + className + ".java\");");
      if (compilingClassNames.contains(className)) {
        ps.println("    assertCompiled(\"" + className + "\");");
      }
      ps.println("  }");
      ps.println();
    }
  }
}
