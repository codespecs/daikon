package daikon.test.split;

import junit.framework.*;
import daikon.split.*;
import daikon.*;
import java.util.*;
import java.io.*;
import utilMDE.*;

/**
 * This class contains tests for the SplitterFactory class.
 * The test directly tests the java files produced by the
 * read_spinfofile method.  These tests assume that the goal
 * files are contained in the directory: "daikon/test/split/targets"
 * These tests ignore extra white spaces.
 */
public class SplitterFactoryTest extends TestCase {

  private static String targetDir = "daikon/test/split/targets/";

  // Because the SplitterFactory sequentially numbers the
  // java files it produces, changing the order that this tests
  // are run will cause the tests to fail.
  //
  // javaFiles are the names of the files created by SplitterFactory
  // targetFiles are the names of the files that the created files
  // are to by checked for equality.





  public static void main(String[] args) {
    junit.textui.TestRunner.run(suite());
  }

  public SplitterFactoryTest(String name) {
    super(name);
  }

  public static void queueArTest() {
    String[] javaFiles = new String[]{ "DataStructures_QueueAr_isFull_1.java",
                                       "DataStructures_QueueAr_increment_2.java",
                                       "DataStructures_QueueAr_isEmpty_3.java",
                                       "DataStructures_QueueAr_main_4.java",
                                       "DataStructures_QueueAr_main_5.java",
                                       "DataStructures_QueueAr_dequeue_6.java"};
    String[] targetFiles = new String[]{ "QueueAr_isFull_1.goal",
                                         "QueueAr_increment_2.goal",
                                         "QueueAr_isEmpty_3.goal",
                                         "QueueAr_main_4.goal",
                                         "QueueAr_main_5.goal",
                                         "QueueAr_dequeue_6.goal"};
    runTest("QueueAr.spinfo",
            "QueueAr.decls",
            javaFiles,
            targetFiles);
  }



  public static void fibTest() {
    String[] javaFiles = new String[] {"misc_Fib_main_8.java" };
    String[] targetFiles = new String[] {"misc_Fib_main_8.goal" };
    runTest("Fib.spinfo",
            "Fib.decls",
            javaFiles,
            targetFiles);
  }

  public static void fuzzyTest() {
    String[] javaFiles = new String[] {"misc_fuzzy_main_10.java"};
    String[] targetFiles = new String[] {"misc_fuzzy_main_10.goal"};
    runTest("fuzzy.spinfo",
            "fuzzy.decls",
            javaFiles,
            targetFiles);
  }

  public static void streetNumberSetTest() {
    String[] javaFiles = new String[]{ "MapQuick__StreetNumberSet_checkRep_21.java",
                                       "MapQuick__StreetNumberSet_checkRep_22.java",
                                       "MapQuick__StreetNumberSet_checkRep_23.java",
                                       "MapQuick__StreetNumberSet_contains_42.java",
                                       "MapQuick__StreetNumberSet_contains_43.java",
                                       "MapQuick__StreetNumberSet_daikonPrintArray_32.java",
                                       "MapQuick__StreetNumberSet_daikonPrintArray_33.java",
                                       "MapQuick__StreetNumberSet_daikonPrintArray_34.java",
                                       "MapQuick__StreetNumberSet_daikonPrintArray_35.java",
                                       "MapQuick__StreetNumberSet_daikonPrintArray_36.java",
                                       "MapQuick__StreetNumberSet_daikonPrintArray_37.java",
                                       "MapQuick__StreetNumberSet_daikonPrintArray_38.java",
                                       "MapQuick__StreetNumberSet_daikonPrintArray_39.java",
                                       "MapQuick__StreetNumberSet_daikonPrintArray_40.java",
                                       "MapQuick__StreetNumberSet_daikonPrintArray_41.java",
                                       "MapQuick__StreetNumberSet_daikonPrintFieldsNonsensical_20.java",
                                       "MapQuick__StreetNumberSet_equals_25.java",
                                       "MapQuick__StreetNumberSet_equals_26.java",
                                       "MapQuick__StreetNumberSet_equals_27.java",
                                       "MapQuick__StreetNumberSet_equals_28.java",
                                       "MapQuick__StreetNumberSet_equals_29.java",
                                       "MapQuick__StreetNumberSet_equals_30.java",
                                       "MapQuick__StreetNumberSet_equals_31.java",
                                       "MapQuick__StreetNumberSet_intersects_44.java",
                                       "MapQuick__StreetNumberSet_intersects_45.java",
                                       "MapQuick__StreetNumberSet_intersects_46.java",
                                       "MapQuick__StreetNumberSet_intersects_47.java",
                                       "MapQuick__StreetNumberSet_max_18.java",
                                       "MapQuick__StreetNumberSet_max_19.java",
                                       "MapQuick__StreetNumberSet_min_24.java",
                                       "MapQuick__StreetNumberSet_orderStatistic_51.java",
                                       "MapQuick__StreetNumberSet_orderStatistic_52.java",
                                       "MapQuick__StreetNumberSet_StreetNumberSet_48.java",
                                       "MapQuick__StreetNumberSet_StreetNumberSet_49.java",
                                       "MapQuick__StreetNumberSet_StreetNumberSet_50.java"};
    String[] targetFiles = new String[]{ "MapQuick__StreetNumberSet_checkRep_21.goal",
                                         "MapQuick__StreetNumberSet_checkRep_22.goal",
                                         "MapQuick__StreetNumberSet_checkRep_23.goal",
                                         "MapQuick__StreetNumberSet_contains_42.goal",
                                         "MapQuick__StreetNumberSet_contains_43.goal",
                                         "MapQuick__StreetNumberSet_daikonPrintArray_32.goal",
                                         "MapQuick__StreetNumberSet_daikonPrintArray_33.goal",
                                         "MapQuick__StreetNumberSet_daikonPrintArray_34.goal",
                                         "MapQuick__StreetNumberSet_daikonPrintArray_35.goal",
                                         "MapQuick__StreetNumberSet_daikonPrintArray_36.goal",
                                         "MapQuick__StreetNumberSet_daikonPrintArray_37.goal",
                                         "MapQuick__StreetNumberSet_daikonPrintArray_38.goal",
                                         "MapQuick__StreetNumberSet_daikonPrintArray_39.goal",
                                         "MapQuick__StreetNumberSet_daikonPrintArray_40.goal",
                                         "MapQuick__StreetNumberSet_daikonPrintArray_41.goal",
                                         "MapQuick__StreetNumberSet_daikonPrintFieldsNonsensical_20.goal",
                                         "MapQuick__StreetNumberSet_equals_25.goal",
                                         "MapQuick__StreetNumberSet_equals_26.goal",
                                         "MapQuick__StreetNumberSet_equals_27.goal",
                                         "MapQuick__StreetNumberSet_equals_28.goal",
                                         "MapQuick__StreetNumberSet_equals_29.goal",
                                         "MapQuick__StreetNumberSet_equals_30.goal",
                                         "MapQuick__StreetNumberSet_equals_31.goal",
                                         "MapQuick__StreetNumberSet_intersects_44.goal",
                                         "MapQuick__StreetNumberSet_intersects_45.goal",
                                         "MapQuick__StreetNumberSet_intersects_46.goal",
                                         "MapQuick__StreetNumberSet_intersects_47.goal",
                                         "MapQuick__StreetNumberSet_max_18.goal",
                                         "MapQuick__StreetNumberSet_max_19.goal",
                                         "MapQuick__StreetNumberSet_min_24.goal",
                                         "MapQuick__StreetNumberSet_orderStatistic_51.goal",
                                         "MapQuick__StreetNumberSet_orderStatistic_52.goal",
                                         "MapQuick__StreetNumberSet_StreetNumberSet_48.goal",
                                         "MapQuick__StreetNumberSet_StreetNumberSet_49.goal",
                                         "MapQuick__StreetNumberSet_StreetNumberSet_50.goal"};
    runTest("StreetNumberSet.spinfo",
            "StreetNumberSet.decls",
            javaFiles,
            targetFiles);
  }




  private static void runTest(String spinfoIn, String decl, String[] javaFiles, String[] targetFiles) {
    Set declFiles = new HashSet();
    Set spinfoFiles = new HashSet();
    spinfoIn = targetDir + spinfoIn;
    decl = targetDir + decl;
    spinfoFiles.add(spinfoIn);
    declFiles.add(decl);
    PptMap allPpts;
    try {
      allPpts = FileIO.read_declaration_files(declFiles);
    } catch (IOException ioe) {
      ioe.printStackTrace();
      throw new Error(ioe.toString());
    }
    Dataflow.create_combined_exits(allPpts);

    //ensure the files are not deleted before they are exaimed
    SplitterFactory.dkconfig_delete_splitters_on_exit = false;
    try {
      //create the java files
      File spinfoInFile = new File(spinfoIn);
      SplitterObject[][] splitters =
        SplitterFactory.read_spinfofile(spinfoInFile, allPpts);
      String tempDir = SplitterFactory.getTempDir();
      for (int i = 0; i < javaFiles.length; i++) {
        String javafile = tempDir + javaFiles[i];
        String targetfile = targetDir + targetFiles[i];
        assertTrue(equalFiles(javafile, targetfile));
      }
      deleteDir(tempDir); //file's delete requires a dir be empty
    } catch(IOException e) {
      throw new RuntimeException(e);
    }
  }

  /**
   *@require no null args
   *@return true iff files are the same. (ignoring extra white space)
   */
  private static boolean equalFiles(String file1, String file2)
    throws IOException {
    LineNumberReader reader1 = UtilMDE.LineNumberFileReader(file1);
    LineNumberReader reader2 = UtilMDE.LineNumberFileReader(file2);
    String line1 = reader1.readLine().trim();
    String line2 = reader2.readLine().trim();
    while(line1 != null && line2 != null) {
      if (! (line1.trim().equals(line2.trim()))) {
        return false;
      }
      line1 = reader1.readLine();
      line2 = reader2.readLine();
    }
    if (line1 == null && line2 == null) {
      return true;
    }
      return false;
  }

  /**
   *@requires there are no directory in the directory given by dirName
   *@modifies the directory are dirName and all its files
   *@effects deletes the directory at dirName and all its files
   */
  private static void deleteDir(String dirName) {
    File dir = new File(dirName);
    File[] files = dir.listFiles();
    for (int i = 0; i < files.length; i++) {
      files[i].delete();
    }
    dir.delete();
  }

  public static Test suite() {
    TestSuite suite = new TestSuite();
    suite.addTest(new SplitterFactoryTest("queueArTest"));
    suite.addTest(new SplitterFactoryTest("fibTest"));
    suite.addTest(new SplitterFactoryTest("fuzzyTest"));
    suite.addTest(new SplitterFactoryTest("streetNumberSetTest"));
    return suite;
  }
}
