package daikon.test.split;

import daikon.split.*;
import java.util.*;
import java.io.*;
import utilMDE.*;
import daikon.*;

/**
 * This class's main method can be used to update both the
 * target files of SplitterFactoryTest and the code of the
 * SplitterFactoryTest itself.
 *
 * To use this program to update SplitterFactoryTest
 * and the target files, run "rm *.java.goal" while in the
 * target directory ($inv/java/daikon/test/split/target).
 * Then simply run the main method with out any arguments
 * in the $INV/java directory. After running the
 * main method one should re-compile the SplitterFactoryTest.
 *
 * To add additional tests to this test program, place the .spinfo
 * and decls files into the "targets" directory then add a call to
 * generateSplitters with the new files.  generateSplitters is
 * overloaded; therefore if there are only one .spinfo file and
 * only decls file then only the names of those two files need to be
 * used as arguments to generateSplitters.  However, if there are
 * multiple .spinfo files or multiple decls files, the file names
 * should be placed into Lists then passed to generateSplitters.
 * See generateSplitters for more information.
 */
public class SplitterFactoryTestUpdater {
  public static java.lang.Runtime commander = java.lang.Runtime.getRuntime();
  private static String targetDir = "daikon/test/split/targets/";
  private static String splitDir = "daikon/test/split/";

  private static List<List<String>> spinfoFileLists = new ArrayList();
  private static List<List<String>> declsFileLists = new ArrayList();
  private static List<String> classNames = new ArrayList();

  private static File tempDir = null;

  private SplitterFactoryTestUpdater() {} //blocks public constructor

  /**
   * If one has changed the test cases used below, for best results run
   * "rm *.java.goal" while in the targets directory before running this
   * method. Creates new splitter java files, move the new files into
   * target directory, rewrites the code of SplitterFactoryTest
   * to use the new files.  One should recompile SplitterFactoryTest
   * after running this method.
   * @param args are ignored.
   */
  public static void  main(String[] args) {
    generateSplitters("StreetNumberSet.spinfo", "StreetNumberSet.decls");
    generateSplitters("Fib.spinfo", "Fib.decls");
    generateSplitters("QueueAr.spinfo", "QueueAr.decls");
    generateSplitters("muldiv.spinfo", "BigFloat.decls");
    moveFiles();
    writeTestClass();
    UtilMDE.deleteDir(tempDir); // file's delete requires a dir be empty
  }

  /**
   * This is a short-cut method if only one spinfo file and only
   * one decls files is to be used.  See generateSplitters(List, List).
   */
  private static void generateSplitters(String spinfoFile, String declsFile) {
    List spinfo = new ArrayList();
    spinfo.add(spinfoFile);
    List decls = new ArrayList();
    decls.add(declsFile);
    generateSplitters(spinfo, decls);
  }

  /**
   * Generates the splitter java files in the tempDir.
   * @param spinfos the spinfo files that should be used in generating
   *  the splitter java files.
   * @param decls the decls files that should be used in generating the
   *  splitter java files.
   */
  private static void generateSplitters(List<String> spinfos,
                                        List<String> decls) {
    Set /*File*/ declsFileSet = new HashSet();
    Set /*File*/ spinfoFiles = new HashSet();
    for (int i = 0; i < spinfos.size(); i++) {
      String spinfoFile = (String) spinfos.get(i);
      spinfoFile = targetDir + spinfoFile;
      spinfoFiles.add(new File(spinfoFile));
    }
    spinfoFileLists.add(new ArrayList(spinfoFiles));
    for (int i = 0; i < decls.size(); i++) {
      String declsFile = (String) decls.get(i);
      declsFile = targetDir + declsFile;
      declsFileSet.add(new File(declsFile));
    }
    declsFileLists.add(new ArrayList(declsFileSet));
    try {
      Daikon.dkconfig_suppressSplitterErrors = true;
      Daikon.create_splitters(spinfoFiles);
      PptMap allPpts = FileIO.read_declaration_files(declsFileSet);
    } catch(IOException e) {
      throw new RuntimeException(e);
    }
  }

  /**
   * Moves the generated splitter files from the tempDir to the target Dir.
   */
  private static void moveFiles() {
    // TODO: Should this reuse the field "tempDir" rather than declaring a
    // new local variable?
    File tempDir = new File(SplitterFactory.getTempDir());
    SplitterFactoryTestUpdater.tempDir = tempDir;
    String[] fileNames = tempDir.list();
    for (int i = 0; i < fileNames.length; i++) {
      if (fileNames[i].endsWith(".java")) {
        String fileName  = fileNames[i];
        moveFile(tempDir.getPath() + "/" + fileName,
                 targetDir + fileName + ".goal");
        String className =
          fileName.substring(fileName.lastIndexOf('/') + 1 ,
                             fileName.length()-".java".length());
        classNames.add(className);
      }
    }
  }

  private static void moveFile(String from, String to) {
      (new File(from)).renameTo(new File(to));
  }

  /**
   * Writes the new code for "SplitterFactoryTest.java".
   */
  private static void writeTestClass() {
    String code = getTestClassText();
    try {
      BufferedWriter writer = UtilMDE.bufferedFileWriter(splitDir + "SplitterFactoryTest.java");
      writer.write(code);
      writer.flush();
    } catch (IOException e) {
      throw new RuntimeException(e);
    }
  }

  /**
   * Returns a String of the new text for the SplitterFactoryTest class.
   */
  private static String getTestClassText() {
    OutputStream code = new ByteArrayOutputStream();
    PrintStream ps = new PrintStream(code);

    ps.println("// ***** This file is automatically generated by SplitterFactoryTestUpdater.java");
    ps.println();
    ps.println("package daikon.test.split;");
    ps.println();
    ps.println("import junit.framework.*;");
    ps.println("import daikon.split.*;"  );
    ps.println("import daikon.*;");
    ps.println("import java.util.*;");
    ps.println("import java.io.*;");
    ps.println("import utilMDE.*;");
    ps.println("import gnu.getopt.*;");
    ps.println();
    ps.println("/**");
    ps.println(" * THIS CLASS WAS GENERATED BY SplitterFactoryTestUpdater.");
    ps.println(" * Therefore, it is a bad idea to directly edit this class's");
    ps.println(" * code for all but temporary reasons.  Any permanent changes");
    ps.println(" * should be made through SplitterFactoryUpdater.");
    ps.println(" *");
    ps.println(" * This class contains regression tests for the SplitterFactory class.");
    ps.println(" * The tests directly test the java files produced by the");
    ps.println(" * load_splitters method by comparing them against goal files.");
    ps.println(" * Note that it is normal for some classes not to compile during this test.");
    ps.println(" *");
    ps.println(" * These tests assume that the goal files are contained in the directory:");
    ps.println(" * \"daikon/test/split/targets\"");
    ps.println(" * These tests ignore extra white spaces.");
    ps.println(" */");
    ps.println(" public class SplitterFactoryTest extends TestCase {");
    ps.println("  // Because the SplitterFactory sequentially numbers the");
    ps.println("  // java files it produces, changing the order that the setUpTests");
    ps.println("  // commands are run will cause the tests to fail.");
    ps.println("  //");
    ps.println("  // javaFiles are the names of the files created by SplitterFactory");
    ps.println("  // targetFiles are the names of the files that the created files");
    ps.println("  // are to by checked for equality.");
    ps.println();
    ps.println("  private static String targetDir = \"daikon/test/split/targets/\";");
    ps.println();
    ps.println("  private static String tempDir = null;");
    ps.println();
    ps.println("  private static boolean saveFiles = false;");
    ps.println();
    ps.println("    private static String usage =");
    ps.println("      UtilMDE.joinLines(");
    ps.println("        \"Usage:  java daikon.tools.CreateSpinfo FILE.java ...\",");
    ps.println("        \"  -s save  Do not delete the splitter java files from the temp directory\",");
    ps.println("        \"  -h       Display this usage message\"");
    ps.println("      );");
    ps.println();
    ps.println("  public static void main(String[] args) {");
    ps.println("      Getopt g =");
    ps.println("        new Getopt(\"daikon.test.split.SplitterFactoryTest\", args, \"hs\");");
    ps.println("      int c;");
    ps.println("      while ((c = g.getopt()) != -1) {");
    ps.println("        switch(c) {");
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
    ps.println("        }");
    ps.println("      }");
    ps.println("    junit.textui.TestRunner.run(suite());");
    ps.println("  }");
    ps.println();

    appendSetUpTest(ps);

    ps.println();
    ps.println("  public SplitterFactoryTest(String name) {");
    ps.println("    super(name);");
    ps.println("  }");
    ps.println();
    ps.println("  /**");
    ps.println("   * Sets up the test by generating the needed splitter java files.");
    ps.println("   */");
    ps.println("  private static void createSplitterFiles(List<String> spinfos, List<String> decls) {");
    ps.println("    List declsFiles = new ArrayList();");
    ps.println("    for (int i = 0; i < decls.size(); i++) {");
    ps.println("      declsFiles.add(new File((String) decls.get(i)));");
    ps.println("    }");
    ps.println("    Set spFiles = new HashSet();");
    ps.println("    for (int i = 0; i < spinfos.size(); i++) {");
    ps.println("      spFiles.add(new File((String) spinfos.get(i)));");
    ps.println("    }");
    ps.println("    try {");
    ps.println("      if (saveFiles) {");
    ps.println("        SplitterFactory.dkconfig_delete_splitters_on_exit = false;");
    ps.println("      }");
    ps.println("      Daikon.dkconfig_suppressSplitterErrors = true;");
    ps.println("      Daikon.create_splitters(spFiles);");
    ps.println("      FileIO.read_declaration_files(declsFiles); // invoked for side effect");
    ps.println("      tempDir = SplitterFactory.getTempDir();");
    ps.println("    } catch(IOException e) {");
    ps.println("        throw new RuntimeException(e);");
    ps.println("    }");
    ps.println("   }");
    ps.println();
    ps.println("  /**");
    ps.println("   * Returns true iff files are the same. (ignoring extra white space)");
    ps.println("   */");
    ps.println();

    appendTests(ps);

    ps.println("  public static Test suite() {");
    ps.println("    setUpTests();");
    ps.println("    TestSuite suite = new TestSuite();");

    appendSuite(ps);

    ps.println("    return suite;");
    ps.println("  }");
    ps.println();
    ps.println("}");

    ps.close();
    return code.toString();
  }

  /**
   * Appends the code to write the static block of code to code.
   * This code is used by the SplitterFactoryTest to set up the
   * needed files to run the tests on.
   */
  public static void appendSetUpTest(PrintStream ps) {
    ps.println("  private static void setUpTests() {");
    ps.println("    List<String> spinfoFiles;");
    ps.println("    List<String> declsFiles;");
    for (int i = 0; i < spinfoFileLists.size(); i++) {
      List spinfoFiles = (List) spinfoFileLists.get(i);
      ps.println("    spinfoFiles = new ArrayList();");
      for (int j = 0; j < spinfoFiles.size(); j++) {
        ps.println("    spinfoFiles.add(\"" + spinfoFiles.get(j) + "\");");
      }
      List declsFiles = (List) declsFileLists.get(i);
      ps.println("    declsFiles = new ArrayList();");
      for (int j = 0; j < declsFiles.size(); j++) {
        ps.println("    declsFiles.add(\"" + declsFiles.get(j) + "\");");
      }
      ps.println("    createSplitterFiles(spinfoFiles, declsFiles);");
    }
    ps.println("  }");
  }

  /**
   * Appends the code that executes the test in SplitterFactoryTest
   * to code.
   */
  private static void appendTests(PrintStream ps) {
    for (int i = 0; i < classNames.size(); i++) {
      String className = (String) classNames.get(i);
      ps.println("  public static void test" + className + "() {");
      ps.println("    assertTrue(UtilMDE.equalFiles(");
      ps.println("      tempDir +\"" + className + ".java\", ");
      ps.println("      \"" + targetDir + className + ".java.goal\"));");
      ps.println("  }");
      ps.println();
    }
  }

  /**
   * Appends the code that generates the test suite in SplitterFactoryTest
   * to code.
   */
  private static void appendSuite(PrintStream ps) {
    for (int i = 0; i < classNames.size(); i++) {
      String className = (String) classNames.get(i);
      ps.println("    suite.addTest(new SplitterFactoryTest(\"test" + className + "\"));");
      ps.println();
    }
  }

}
