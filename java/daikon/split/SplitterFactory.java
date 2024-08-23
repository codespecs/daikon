package daikon.split;

import daikon.Global;
import daikon.PptTopLevel;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import jtb.ParseException;
import org.checkerframework.checker.nullness.qual.MonotonicNonNull;
import org.checkerframework.checker.nullness.qual.NonNull;
import org.checkerframework.checker.nullness.qual.RequiresNonNull;
import org.checkerframework.checker.regex.qual.Regex;
import org.checkerframework.checker.signature.qual.BinaryName;
import org.plumelib.util.FilesPlume;

/**
 * This class contains static methods {@link #parse_spinfofile(File)} which creates Splitterss from
 * a {@code .spinfo} file, and {@link #load_splitters} which loads the splitters for a given Ppt.
 */
public class SplitterFactory {
  private SplitterFactory() {
    throw new Error("do not instantiate");
  }

  public static final Logger debug = Logger.getLogger("daikon.split.SplitterFactory");

  /** The directory in which the Java files for the splitter will be made. */
  // This must *not* be set in a static block, which happens before the
  // Configuration object has had a chance to possibly set
  // dkconfig_delete_splitters_on_exit.
  private static @MonotonicNonNull String tempdir;

  /**
   * Boolean. If true, the temporary Splitter files are deleted on exit. Set it to "false" if you
   * are debugging splitters.
   */
  public static boolean dkconfig_delete_splitters_on_exit = true;

  /**
   * String. Specifies which Java compiler is used to compile Splitters. This can be the full path
   * name or whatever is used on the command line. Uses the current classpath.
   */
  public static String dkconfig_compiler
      // "-source 8 -target 8" is a hack for when using a Java 9+ compiler but
      // a Java 8 runtime.  A better solution would be to add
      // these command-line arguments only when running
      // SplitterFactoryTestUpdater, but that program does not support that.
      = "javac -nowarn -source 8 -target 8 -classpath " + System.getProperty("java.class.path");

  /**
   * Positive integer. Specifies the Splitter compilation timeout, in seconds, after which the
   * compilation process is terminated and retried, on the assumption that it has hung.
   */
  public static int dkconfig_compile_timeout = 20;

  private static @MonotonicNonNull FileCompiler fileCompiler; // lazily initialized

  /**
   * guid is a counter that increments every time a file is written. It is used to ensure that every
   * file written has a unique name.
   */
  private static int guid = 0;

  // Methods

  /**
   * Parses the Splitter info.
   *
   * @param infofile filename.spinfo
   * @return a SpinfoFile encapsulating the parsed splitter info file
   */
  public static SpinfoFile parse_spinfofile(File infofile)
      throws IOException, FileNotFoundException {
    if (tempdir == null) {
      tempdir = createTempDir();
    }
    if (!dkconfig_delete_splitters_on_exit) {
      System.out.println("\rSplitters for this run created in " + tempdir);
    }
    return new SpinfoFile(infofile, tempdir);
  }

  /**
   * Finds the splitters that apply to a given Ppt and loads them (that is, it populates
   * SplitterList).
   *
   * @param ppt the Ppt
   * @param spfiles a list of SpinfoFiles
   */
  @RequiresNonNull("tempdir")
  public static void load_splitters(PptTopLevel ppt, List<SpinfoFile> spfiles) {
    Global.debugSplit.fine("<<enter>> load_splitters");

    for (SpinfoFile spfile : spfiles) {
      SplitterObject[][] splitterObjects = spfile.getSplitterObjects();
      StatementReplacer statementReplacer = spfile.getReplacer();
      for (int i = 0; i < splitterObjects.length; i++) {
        int numsplitters = splitterObjects[i].length;
        if (numsplitters != 0) {
          String ppt_name = splitterObjects[i][0].getPptName();
          Global.debugSplit.fine(
              "          load_splitters: "
                  + ppt_name
                  + ", "
                  + ppt
                  + "; match="
                  + matchPpt(ppt_name, ppt));
          if (matchPpt(ppt_name, ppt)) {
            int numGood = 0;
            // Writes, compiles, and loads the splitter .java files.
            loadSplitters(splitterObjects[i], ppt, statementReplacer);
            List<Splitter> sp = new ArrayList<>();
            for (int k = 0; k < numsplitters; k++) {
              if (splitterObjects[i][k].splitterExists()) {
                @SuppressWarnings("nullness") // dependent: because splitterExists() = true
                @NonNull Splitter splitter = splitterObjects[i][k].getSplitter();
                sp.add(splitter);
                numGood++;
              } else {
                // UNDONE: We should only output the load error if the
                // compile was successful.
                System.out.println(splitterObjects[i][k].getError());
              }
            }
            System.out.printf(
                "%s: %d of %d splitters successful%n", ppt_name, numGood, numsplitters);
            if (sp.size() >= 1) {
              SplitterList.put(ppt_name, sp.toArray(new Splitter[0]));
            }
            // delete this entry in the splitter array to prevent it from
            // matching any other Ppts, since the documented behavior is that
            // it only matches one.
            splitterObjects[i] = new SplitterObject[0];
          }
        }
      }
    }
    Global.debugSplit.fine("<<exit>>  load_splitters");
  }

  // Accessible for the purpose of testing.
  public static String getTempDir() {
    if (tempdir == null) {
      tempdir = createTempDir();
    }
    return tempdir;
  }

  /**
   * Writes, compiles, and loads the splitter {@code .java} files for each splitterObject in
   * splitterObjects.
   *
   * @param splitterObjects are the splitterObjects for ppt
   * @param ppt the Ppt for these splitterObjects
   * @param statementReplacer a StatementReplacer for the replace statements to be used in these
   *     splitterObjects
   */
  @RequiresNonNull("tempdir")
  private static void loadSplitters(
      SplitterObject[] splitterObjects, PptTopLevel ppt, StatementReplacer statementReplacer) {
    Global.debugSplit.fine("<<enter>> loadSplitters - count: " + splitterObjects.length);

    // System.out.println("loadSplitters for " + ppt.name);
    if (splitterObjects.length == 0) {
      return;
    }
    for (int i = 0; i < splitterObjects.length; i++) {
      SplitterObject splitObj = splitterObjects[i];
      String fileName = getFileName(splitObj.getPptName());
      StringBuilder fileContents;
      try {
        SplitterJavaSource splitterWriter =
            new SplitterJavaSource(
                splitObj, splitObj.getPptName(), fileName, ppt.var_infos, statementReplacer);
        fileContents = splitterWriter.getFileText();
      } catch (ParseException e) {
        System.out.println("Error in SplitterFactory while writing splitter java file for: ");
        System.out.println(splitObj.condition() + " cannot be parsed.");
        continue;
      }
      String fileAddress = tempdir + fileName;
      @SuppressWarnings("signature") // safe, has been quoted
      @BinaryName String fileName_bn = fileName;
      splitObj.setClassName(fileName_bn);
      try (BufferedWriter writer = FilesPlume.newBufferedFileWriter(fileAddress + ".java")) {
        if (dkconfig_delete_splitters_on_exit) {
          new File(fileAddress + ".java").deleteOnExit();
          new File(fileAddress + ".class").deleteOnExit();
        }
        writer.write(fileContents.toString());
        writer.flush();
      } catch (IOException ioe) {
        System.out.println("Error while writing Splitter file: " + fileAddress);
        debug.fine(ioe.toString());
      }
    }
    List<String> fileNames = new ArrayList<>();
    for (int i = 0; i < splitterObjects.length; i++) {
      fileNames.add(splitterObjects[i].getFullSourcePath());
    }
    String errorOutput = null;
    try {
      errorOutput = compileFiles(fileNames);
    } catch (IOException ioe) {
      System.out.println("Error while compiling Splitter files (Daikon will continue):");
      debug.fine(ioe.toString());
    }
    boolean errorOutputExists = errorOutput != null && !errorOutput.equals("");
    if (errorOutputExists && !PptSplitter.dkconfig_suppressSplitterErrors) {
      System.out.println();
      System.out.println(
          "Errors while compiling Splitter files (Daikon will use non-erroneous splitters):");
      System.out.println(errorOutput);
    }
    for (int i = 0; i < splitterObjects.length; i++) {
      splitterObjects[i].load();
    }

    Global.debugSplit.fine("<<exit>>  loadSplitters");
  }

  /**
   * Compiles the files given by fileNames. Return the error output.
   *
   * @return the error output from compiling the files
   * @param fileNames paths to the files to be compiled as Strings
   * @throws IOException if there is a problem reading a file
   */
  private static String compileFiles(List<String> fileNames) throws IOException {
    // We delay setting fileCompiler until now because we want to permit
    // the user to set the dkconfig_compiler variable.  Note that our
    // timeout is specified in seconds, but the parameter to FileCompiler
    // is specified in milliseconds.
    if (fileCompiler == null) {
      fileCompiler = new FileCompiler(dkconfig_compiler, 1000 * (long) dkconfig_compile_timeout);
    }
    return fileCompiler.compileFiles(fileNames);
  }

  /** Determine whether a Ppt's name matches the given pattern. */
  private static boolean matchPpt(String ppt_name, PptTopLevel ppt) {
    if (ppt.name.equals(ppt_name)) {
      return true;
    }
    if (ppt_name.endsWith(":::EXIT")) {
      String regex = Pattern.quote(ppt_name) + "[0-9]+";
      if (matchPptRegex(regex, ppt)) {
        return true;
      }
    }

    // Look for corresponding EXIT ppt. This is because the exit ppt usually has
    // more relevant variables in scope (eg. return, hashcodes) than the enter.
    String regex;
    int index = ppt_name.indexOf("OBJECT");
    if (index == -1) {
      // Didn't find "OBJECT" suffix; add ".*EXIT".
      regex = Pattern.quote(ppt_name) + ".*EXIT";
    } else {
      // Found "OBJECT" suffix.
      if (ppt_name.length() > 6) {
        regex = Pattern.quote(ppt_name.substring(0, index - 1)) + ":::OBJECT";
      } else {
        regex = Pattern.quote(ppt_name);
      }
    }
    return matchPptRegex(regex, ppt);
  }

  private static boolean matchPptRegex(@Regex String ppt_regex, PptTopLevel ppt) {
    // System.out.println("matchPptRegex: " + ppt_regex);
    Pattern pattern = Pattern.compile(ppt_regex);
    String name = ppt.name;
    Matcher matcher = pattern.matcher(name);
    // System.out.println("  considering " + name);
    return matcher.find();
  }

  /**
   * Returns a file name for a splitter file to be used with a Ppt with the name, ppt_name. The file
   * name is ppt_name with all characters which are invalid for use in a java file name (such as
   * ".") replaced with "_". Then "_guid" is append to the end. For example if ppt_name is
   * "myPackage.myClass.someMethod" and guid = 12, then the following would be returned:
   * "myPackage_myClass_someMethod_12".
   *
   * @param ppt_name the name of the Ppt that the splitter Java file wil be used with
   */
  private static String getFileName(String ppt_name) {
    String splitterName = clean(ppt_name);
    splitterName = splitterName + "_" + guid;
    guid++;
    return splitterName;
  }

  /**
   * Cleans str by replacing all characters that are not valid java indentifier parts with "_".
   *
   * @param str the string to be cleaned
   * @return str with all non-Java-indentifier parts replaced with "_"
   */
  private static String clean(String str) {
    char[] cleaned = str.toCharArray();
    for (int i = 0; i < cleaned.length; i++) {
      char c = cleaned[i];
      if (!Character.isJavaIdentifierPart(c)) {
        cleaned[i] = '_';
      }
    }
    return new String(cleaned);
  }

  /**
   * Creates the temporary directory in which splitter files will be stored. The return value
   * includes a trailing file separtor (e.g., "/"), unless the return value is "".
   *
   * @return the name of the temporary directory. This is where the Splitters are created.
   */
  private static String createTempDir() {
    try {
      File tmpDir = FilesPlume.createTempDir("daikon", "split");
      if (dkconfig_delete_splitters_on_exit) {
        tmpDir.deleteOnExit();
      }
      return tmpDir.getPath() + File.separator;
    } catch (IOException e) {
      debug.fine(e.toString());
    }
    return ""; // Use current directory
  }
}
