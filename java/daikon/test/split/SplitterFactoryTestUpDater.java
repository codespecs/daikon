package daikon.test.split;

import daikon.split.*;
import java.util.*;
import java.io.*;
import utilMDE.*;
import daikon.*;

/**
 * This class's main method can be used to update both the
 * target files of SplitterFactoryTest and the code of the
 * SplitterFactoryTest it self.  Therefore, after running the
 * main method one should re-compile the SplitterFactoryTest.
 * Also to avoid build up of no longer needed goal files one
 * should run "rm *.java.goal" while in the target directory.
 * This class should be ran from the java directory.
 */
public class SplitterFactoryTestUpDater {
  public static java.lang.Runtime commander = java.lang.Runtime.getRuntime();
  private static String lineSep = System.getProperty("line.separator");
  private static String targetDir = "daikon/test/split/targets/";
  private static String splitDir = "daikon/test/split/";

  private static List /*List of Strings*/ spinfoFileLists = new ArrayList();
  private static List /*List of Strings*/ declsFileLists = new ArrayList();
  private static List /*String*/ classNames = new ArrayList();

  private static File tempDir = null;

  private SplitterFactoryTestUpDater() {} //blocks public constructor

  /**
   * For best results run "rm *.java.goal while in the targets
   * directory before running this method.
   * Creates new splitter java files, move the new files into
   * target directory, rewrites the code of SplitterFactoryTest
   * to use the new files.  One should recompile SplitterFactoryTest
   * after running this method.
   * @param args are ignored.
   */
  public static void  main(String[] args) {
    generateSplitters("StreetNumberSet.spinfo", "StreetNumberSet.decls");
    generateSplitters("Fib.spinfo", "Fib.decls");
    generateSplitters("QueueAr.spinfo", "QueueAr.decls");
    moveFiles();
    writeTestClass();
    deleteDir(tempDir); // file's delete requires a dir be empty
  }

  /**
   * This is a short-cut method if only one spinfo file and only
   * one decls files is to be used.  See update(List, List).
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
  private static void generateSplitters(List /*String*/ spinfos,
                                        List /*String*/ decls) {
    Set /*String*/ declsFileSet = new HashSet();
    List /*String*/ spinfoFiles = new ArrayList();
    PptMap allPpts = new PptMap();
    for (int i = 0; i < spinfos.size(); i++) {
      String spinfoFile = (String) spinfos.get(i);
      spinfoFile = targetDir + spinfoFile;
      spinfoFiles.add(spinfoFile);
    }
    spinfoFileLists.add(spinfoFiles);
    for (int i = 0; i < decls.size(); i++) {
      String declsFile = (String) decls.get(i);
      declsFile = targetDir + declsFile;
      declsFileSet.add(declsFile);
    }
    declsFileLists.add(new ArrayList(declsFileSet));
    /* JHP V2/V3 merge hack 7/24/03, these calls are different in V3
    FileIO.read_declaration_files(declsFileSet, allPpts);
    Daikon.add_combined_exits(allPpts);
    end JHP merge hack */
    //ensure the files are not deleted before they are copied
    SplitterFactory.dkconfig_delete_splitters_on_exit = false;
    for (int i = 0; i < spinfoFiles.size(); i++) {
      try {
        //create the java files
        String spinfoFile = (String) spinfoFiles.get(i);
        SplitterObject[][] splitters =
          SplitterFactory.read_spinfofile( new File (spinfoFile), allPpts);
      } catch(IOException e) {
        throw new RuntimeException(e);
      }
    }
  }

  /**
   * Moves the generated splitter files from the tempDir to the target Dir.
   */
  private static void moveFiles() {
    File tempDir = new File(SplitterFactory.getTempDir());
    SplitterFactoryTestUpDater.tempDir = tempDir;
    String[] fileNames = tempDir.list();
    for (int i = 0; i < fileNames.length; i++) {
      if (fileNames[i].endsWith(".java")) {
        StringBuffer command = new StringBuffer();
        String fileName  = fileNames[i];
        moveFile(fileName);
        classNames.add(fileName.substring(fileName.lastIndexOf('/') + 1 , fileName.length()-".java".length()));
      }
    }
  }

  /**
   * moves the file named fileName from the tempDir to the target dir and adds
   * ".goal" to the end of its name.
   */
  private static void moveFile(String fileName) {
    String newFileName = targetDir + fileName + ".goal";
    fileName = tempDir.getPath() + "/" + fileName;
    String command = "mv " + fileName + " " + newFileName;
    try {
      Process process = commander.exec(command);
      process.waitFor();
      BufferedReader error =
        new BufferedReader(new InputStreamReader(process.getErrorStream()));
      String s;
      StringBuffer errorMessage = new StringBuffer();
      try {
        while ((s = error.readLine()) != null) {
          errorMessage.append(s);
        }
      } catch (IOException ioe) {
        System.out.println("TimedProcess: " + ioe.toString());
      }
      if (! errorMessage.toString().trim().equals("")) {
        System.out.println(errorMessage.toString());
      }
    } catch (InterruptedException e) {
      throw new RuntimeException(e);
    }
    catch (IOException e) {
      throw new RuntimeException(e);
    }
  }

  /**
   * Writes the new code for "SplitterFactoryTest.java".
   */
  private static void writeTestClass() {
    String code = getTestClassText();
    try {
      BufferedWriter writer = UtilMDE.BufferedFileWriter(splitDir + "SplitterFactoryTest.java");
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
    StringBuffer code = new StringBuffer();

    code.append("package daikon.test.split;" + lineSep);
    code.append(lineSep);
    code.append("import junit.framework.*; " + lineSep);
    code.append("import daikon.split.*;"   + lineSep);
    code.append("import daikon.*; " + lineSep);
    code.append("import java.util.*; " + lineSep);
    code.append("import java.io.*;" + lineSep);
    code.append("import utilMDE.*;" + lineSep);
    code.append(lineSep);
    code.append("/**" + lineSep);
    code.append(" * THIS CLASS WAS GENERATED BY SplitterFactoryTestUpDater" + lineSep);
    code.append(" * Therefore, it is a bad idea to directly edit this class's" +lineSep);
    code.append(" * code for all but temporary reasons.  Any permanent changes " + lineSep);
    code.append(" * should be made through SplitterFactoryUpDater." + lineSep);
    code.append(" * " + lineSep);
    code.append(" * This class contains regression tests for the SplitterFactory class." + lineSep);
    code.append(" * The tests directly test the java files produced by the" + lineSep);
    code.append(" * read_spinfofile method by comparing them against goal files." + lineSep);
    code.append(" * Note that it is normal for some classes not to compile during this test." + lineSep);
    code.append(" * " + lineSep);
    code.append(" * These tests assume that the goal files are contained in the directory: " + lineSep);
    code.append(" * \"daikon/test/split/targets\"" + lineSep);
    code.append(" * These tests ignore extra white spaces." + lineSep);
    code.append(" */" + lineSep);
    code.append(" public class SplitterFactoryTest extends TestCase { " + lineSep);
    code.append(lineSep);
    code.append("  private static String targetDir = \"daikon/test/split/targets/\";" + lineSep);
    code.append(lineSep);
    code.append("  private static String tempDir = null;" + lineSep);
    code.append(lineSep);
    code.append("  // Because the SplitterFactory sequentially numbers the " + lineSep);
    code.append("  // java files it produces, changing the order that the setUpTests " + lineSep);
    code.append("  // commands are run will cause the tests to fail." + lineSep);
    code.append("  //" + lineSep);
    code.append("  // javaFiles are the names of the files created by SplitterFactory" + lineSep);
    code.append("  // targetFiles are the names of the files that the created files" + lineSep);
    code.append("  // are to by checked for equality." + lineSep);
    code.append(lineSep);
    code.append("  public static void main(String[] args) {" + lineSep);
    code.append("    junit.textui.TestRunner.run(suite());" + lineSep);
    code.append("  }" + lineSep);
    code.append(lineSep);

    appendSetUps(code);

    code.append(lineSep);
    code.append("  public SplitterFactoryTest(String name) {" + lineSep);
    code.append("    super(name);" + lineSep);
    code.append("  }" + lineSep);
    code.append(lineSep);
    code.append("  /**" + lineSep);
    code.append("   * Sets up the test by generating the needed splitter java files. " + lineSep);
    code.append("   */" + lineSep);
    code.append("  private static void setUpTest(List /*String*/ spinfos, List /*String*/ decls) {" + lineSep);
    code.append("    Set /*String*/ declsFileSet = new HashSet();" + lineSep);
    code.append("    List /*String*/ spinfoFiles = new ArrayList();" + lineSep);
    code.append("    PptMap allPpts = new PptMap();" + lineSep);
    code.append("    for (int i = 0; i < spinfos.size(); i++) {" + lineSep);
    code.append("      String spinfoFile = (String) spinfos.get(i);" + lineSep);
    code.append("      spinfoFiles.add(spinfoFile);" + lineSep);
    code.append("    }" + lineSep);
    code.append("    for (int i = 0; i < decls.size(); i++) {" + lineSep);
    code.append("      String declsFile = (String) decls.get(i);" + lineSep);
    code.append("      declsFileSet.add(declsFile);" + lineSep);
    code.append("    }" + lineSep);
    code.append("    FileIO.read_declaration_files(declsFileSet, allPpts);" + lineSep);
    code.append("    Daikon.add_combined_exits(allPpts);" + lineSep);
    code.append("    //ensure the files are not deleted before they are copied" + lineSep);
    code.append("    SplitterFactory.dkconfig_delete_splitters_on_exit = false;" + lineSep);
    code.append("    for (int i = 0; i < spinfoFiles.size(); i++) {" + lineSep);
    code.append("      try {" + lineSep);
    code.append("        //create the java files" + lineSep);
    code.append("        String spinfoFile = (String) spinfoFiles.get(i);" + lineSep);
    code.append("        SplitterObject[][] splitters =" + lineSep);
    code.append("          SplitterFactory.read_spinfofile(spinfoFile, allPpts);" + lineSep);
    code.append("        tempDir = SplitterFactory.getTempDir();" + lineSep);
    code.append("      } catch(IOException e) {" + lineSep);
    code.append("        throw new RuntimeException(e);" + lineSep);
    code.append("      }" + lineSep);
    code.append("    }" + lineSep);
    code.append("   }" + lineSep);
    code.append(lineSep);
    code.append("  /**" + lineSep);
    code.append("   * Returns true iff files are the same. (ignoring extra white space)" + lineSep);
    code.append("   */ " + lineSep);
    code.append("  private static boolean equalFiles(String file1, String file2) { " + lineSep);
    code.append("    try {" + lineSep);
    code.append("      LineNumberReader reader1 = UtilMDE.LineNumberFileReader(file1);" + lineSep);
    code.append("      LineNumberReader reader2 = UtilMDE.LineNumberFileReader(file2);" + lineSep);
    code.append("      String line1 = reader1.readLine().trim();" + lineSep);
    code.append("      String line2 = reader2.readLine().trim();" + lineSep);
    code.append("      while(line1 != null && line2 != null) { " + lineSep);
    code.append("        if (! (line1.trim().equals(line2.trim()))) {" + lineSep);
    code.append("          return false;" + lineSep);
    code.append("        }" + lineSep);
    code.append("        line1 = reader1.readLine();" + lineSep);
    code.append("        line2 = reader2.readLine();" + lineSep);
    code.append("      }" + lineSep);
    code.append("      if (line1 == null && line2 == null) {" + lineSep);
    code.append("        return true;" + lineSep);
    code.append("      }" + lineSep);
    code.append("        return false;" + lineSep);
    code.append("      } catch (IOException e) { " + lineSep);
    code.append("        throw new RuntimeException(e); " + lineSep);
    code.append("      }" + lineSep);
    code.append("  }" + lineSep);
    code.append(lineSep);
    code.append("  /** " + lineSep);
    code.append("   * Deletes the directory at dirName and all its files; there may be no" + lineSep);
    code.append("   * directory is dirName." + lineSep);
    code.append("   */" + lineSep);
    code.append("  private static void deleteDir(String dirName) {" + lineSep);
    code.append("    File dir = new File(dirName);" + lineSep);
    code.append("    File[] files = dir.listFiles();" + lineSep);
    code.append("    for (int i = 0; i < files.length; i++) {" + lineSep);
    code.append("      files[i].delete();" + lineSep);
    code.append("    }" + lineSep);
    code.append("    dir.delete();" + lineSep);
    code.append("  }" + lineSep);
    code.append(lineSep);

    appendTests(code);

    code.append("  public static Test suite() {" + lineSep);
    code.append("    TestSuite suite = new TestSuite();" + lineSep);

    appendSuite(code);

    code.append("    return suite;" + lineSep);
    code.append( "  } " + lineSep);
    code.append(lineSep);
    code.append( "}");

    return code.toString();
  }

  /**
   * Appends the code to write the static block of code to code.
   * This code is used by the SplitterFactoryTest to set up the
   * needed files to run the tests on.
   */
  public static void appendSetUps(StringBuffer code) {
    code.append("  static {" + lineSep);
    code.append("    List spinfoFiles;" + lineSep);
    code.append("    List declsFiles;" + lineSep);
    for (int i = 0; i < spinfoFileLists.size(); i++) {
      List spinfoFiles = (List) spinfoFileLists.get(i);
      code.append("    spinfoFiles = new ArrayList();" + lineSep);
      for (int j = 0; j < spinfoFiles.size(); j++) {
        code.append("    spinfoFiles.add(\"");
        code.append(spinfoFiles.get(j));
        code.append("\");" + lineSep);
      }
      List declsFiles = (List) declsFileLists.get(i);
      code.append("    declsFiles = new ArrayList();" + lineSep);
      for (int j = 0; j < declsFiles.size(); j++) {
        code.append("    declsFiles.add(\"");
        code.append(declsFiles.get(j));
        code.append("\");" + lineSep);
      }
      code.append("    setUpTest(spinfoFiles, declsFiles);" + lineSep);
    }
    code.append("  }" + lineSep);
  }

  /**
   * Appends the code that executes the test in SplitterFactoryTest
   * to code.
   */
  private static void appendTests(StringBuffer code) {
    for (int i = 0; i < classNames.size(); i++) {
      String className = (String) classNames.get(i);
      code.append("  public static void ");
      code.append("test" + className);
      code.append("() {");
      code.append(lineSep);
      code.append("    assertTrue(equalFiles(" + lineSep);
      code.append("      tempDir +\"");
      code.append(className);
      code.append(".java\", ");
      code.append(lineSep);
      code.append("      \"");
      code.append(targetDir);
      code.append(className);
      code.append(".java.goal\"));");
      code.append(lineSep);
      code.append("}" + lineSep);
      code.append(lineSep);
    }
  }

  /**
   * Appends the code that generates the test suite in SplitterFactoryTest
   * to code.
   */
  private static void appendSuite(StringBuffer code) {
    for (int i = 0; i < classNames.size(); i++) {
      String className = (String) classNames.get(i);
      code.append("    suite.addTest(new SplitterFactoryTest(\"");
      code.append("test" + className);
      code.append("\"));");
      code.append(lineSep);
    }
  }

  /**
   *@requires there are no directory in the directory given by dirName
   *@modifies the directory are dirName and all its files
   *@effects deletes the directory at dirName and all its files
   */
  private static void deleteDir(File dir) {
    File[] files = dir.listFiles();
    for (int i = 0; i < files.length; i++) {
      files[i].delete();
    }
    dir.delete();
  }

}
