package daikon.tools.jtb;

import static java.nio.charset.StandardCharsets.UTF_8;
import static java.util.logging.Level.FINE;
import static java.util.logging.Level.INFO;

import daikon.*;
import gnu.getopt.*;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.Reader;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;
import jtb.JavaParser;
import jtb.ParseException;
import jtb.syntaxtree.*;
import org.checkerframework.checker.nullness.qual.KeyFor;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.plumelib.util.CollectionsPlume;
import org.plumelib.util.StringsPlume;

/**
 * Create a splitter info file from Java source.
 *
 * <p>The argument is a list of {@code .java} files. The original {@code .java} files are left
 * unmodified. A {@code .spinfo} file is written for every {@code .java} file, or in the single file
 * indicated as the {@code -o} command-line argument..
 */
public class CreateSpinfo {

  // The expressions in the Java source are extracted as follows:
  // For each method:
  //  * extracts all expressions in conditional statements
  //    ie. if, for, which, etc.
  //  * if the method body is a one-line return statement, it
  //    extracts it for later substitution into expressions which
  //    call this function. These statements are referred to as
  //    replace statements
  // For each field declaration
  //  * if the field is a boolean, it stores the expression
  //    "<fieldname> == true" as a splitting condition.
  //
  //  The method printSpinfoFile prints out these expressions and
  //  replace statements in splitter info file format.

  /** Debug logger. */
  public static final Logger debug = Logger.getLogger("daikon.tools.jtb.CreateSpinfo");

  /** The usage message for this program. */
  private static String usage =
      StringsPlume.joinLines(
          "Usage:  java daikon.tools.CreateSpinfo FILE.java ...",
          "  -o outputfile   Put all output in specified file",
          "  -h              Display this usage message");

  public static void main(String[] args) throws IOException {
    try {
      mainHelper(args);
    } catch (Daikon.DaikonTerminationException e) {
      Daikon.handleDaikonTerminationException(e);
    }
  }

  /**
   * This does the work of {@link #main(String[])}, but it never calls System.exit, so it is
   * appropriate to be called progrmmatically.
   */
  public static void mainHelper(final String[] args) throws IOException {

    // If not set, put output in files named after the input (source) files.
    String outputfilename = null;

    daikon.LogHelper.setupLogs(INFO);
    LongOpt[] longopts =
        new LongOpt[] {
          new LongOpt(Daikon.help_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
          new LongOpt(Daikon.debugAll_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
          new LongOpt(Daikon.debug_SWITCH, LongOpt.REQUIRED_ARGUMENT, null, 0),
        };

    Getopt g = new Getopt("daikon.tools.jtb.CreateSpinfo", args, "ho:", longopts);
    int c;
    while ((c = g.getopt()) != -1) {
      switch (c) {
        case 0:
          // got a long option
          String option_name = longopts[g.getLongind()].getName();
          if (Daikon.help_SWITCH.equals(option_name)) {
            System.out.println(usage);
            throw new Daikon.NormalTermination();
          } else if (Daikon.debugAll_SWITCH.equals(option_name)) {
            Global.debugAll = true;
          } else if (Daikon.debug_SWITCH.equals(option_name)) {
            daikon.LogHelper.setLevel(Daikon.getOptarg(g), FINE);
          } else {
            throw new RuntimeException("Unknown long option received: " + option_name);
          }
          break;
        case 'o':
          outputfilename = Daikon.getOptarg(g);
          break;
        case 'h':
          System.out.println(usage);
          throw new Daikon.NormalTermination();
        case '?':
          break; // getopt() already printed an error
        default:
          System.out.println("getopt() returned " + c);
          break;
      }
    }

    // The index of the first non-option argument -- the name of the file
    int argindex = g.getOptind();
    if (argindex >= args.length) {
      throw new Daikon.UserError(
          "Error: No .java file arguments supplied." + Global.lineSep + usage);
    }
    if (outputfilename != null) {
      try (PrintWriter output =
          new PrintWriter(Files.newBufferedWriter(Paths.get(outputfilename), UTF_8))) {
        for (; argindex < args.length; argindex++) {
          String javaFileName = args[argindex];
          writeSplitters(javaFileName, output);
        }
        output.flush();
      }
    } else {
      for (; argindex < args.length; argindex++) {
        String javaFileName = args[argindex];
        String spinfoFileName = spinfoFileName(javaFileName);
        try (PrintWriter output =
            new PrintWriter(Files.newBufferedWriter(Paths.get(spinfoFileName), UTF_8))) {
          writeSplitters(javaFileName, output);
          output.flush();
        }
      }
    }
  }

  /**
   * Returns the default name for a spinfo file created from a java file named javaFileName.
   *
   * @param javaFileName the name of the java file from which this spinfo file is being created
   */
  private static String spinfoFileName(String javaFileName) {
    if (javaFileName.endsWith(".java")) {
      return javaFileName.substring(0, javaFileName.length() - 5) + ".spinfo";
    }

    // The file does not end with ".java".  Proceed, but issue a warning.
    System.err.println(
        "Warning: CreateSpinfo input file " + javaFileName + "does not end in .java.");

    // change the file extension to .spinfo
    int dotPos = javaFileName.indexOf(".");
    if (dotPos == -1) {
      return javaFileName + ".spinfo";
    } else {
      return javaFileName.substring(0, dotPos) + ".spinfo";
    }
  }

  /**
   * Write splitters for the Java file to the PrintWriter as a spinfo file.
   *
   * @param javaFileName the name of the java file from which this spinfo file is being made
   * @param output the PrintWriter to which this spinfo file is being wrote
   * @throws IOException if there is a problem reading or writing files
   */
  private static void writeSplitters(String javaFileName, PrintWriter output) throws IOException {
    Node root;
    try (Reader input = Files.newBufferedReader(Paths.get(javaFileName), UTF_8)) {
      JavaParser parser = new JavaParser(input);
      root = parser.CompilationUnit();
    } catch (ParseException e) {
      e.printStackTrace();
      throw new Daikon.UserError("ParseException");
    }
    debug.fine("CreateSpinfo: processing file " + javaFileName);
    ConditionExtractor extractor = new ConditionExtractor();
    root.accept(extractor);
    // conditions: method name (String) to conditional expressions (String)
    Map<String, List<String>> conditions = extractor.getConditionMap();
    // replaceStatements: method declaration (String) to method body (String)
    Map<String, String> replaceStatements = extractor.getReplaceStatements();
    String packageName = extractor.getPackageName();
    filterConditions(conditions);
    addOrigConditions(conditions);
    printSpinfoFile(output, conditions, replaceStatements, packageName);
  }

  /**
   * Remove redundant and trivial conditions from conditionMap. Side-effects conditionMap.
   *
   * @param conditionMap the map from which to remove redundant and trivial conditions
   */
  private static void filterConditions(Map<String, List<String>> conditionMap) {
    for (Map.Entry<String, List<String>> entry : conditionMap.entrySet()) {
      List<String> conditions = entry.getValue();
      conditions = CollectionsPlume.withoutDuplicates(conditions);
      conditions.remove("true");
      conditions.remove("false");
      entry.setValue(conditions);
    }
  }

  /**
   * For each condition in conditionMap, an additional condition is added which is identical to the
   * initial condition with the exception that it is prefixed with "orig(" and suffixed with ")".
   */
  private static void addOrigConditions(Map<String, List<String>> conditionMap) {
    for (List<String> conditions : conditionMap.values()) {
      int size = conditions.size();
      for (int i = 0; i < size; i++) {
        conditions.add(addOrig(conditions.get(i)));
      }
    }
  }

  /** Returns condition prefixed with "orig(" and suffixed with ")". */
  private static String addOrig(String condition) {
    return "orig(" + condition + ")";
  }

  /**
   * Writes the spinfo file specified by conditions, replaceStatements, and package name to output.
   *
   * @param output the PrintWriter to which the spinfo file is to be written
   * @param conditions the conditions to be included in the spinfo file. conditions should be a map
   *     from method names to the conditional expressions for that method to split upon.
   * @param replaceStatements the replace statements to be included in the spinfo file.
   *     replaceStatements should be a map from method declarations to method bodies.
   * @param packageName the package name of the java file for which this spinfo file is being
   *     written
   */
  private static void printSpinfoFile(
      PrintWriter output,
      Map<String, List<String>> conditions,
      Map<String, String> replaceStatements,
      @Nullable String packageName)
      throws IOException {
    if (!replaceStatements.values().isEmpty()) {
      output.println("REPLACE");
      for (
      @KeyFor("replaceStatements") String declaration : CollectionsPlume.sortedKeySet(replaceStatements)) {
        output.println(declaration);
        String replacement = replaceStatements.get(declaration);
        output.println(removeNewlines(replacement));
      }
      output.println();
    }
    for (@KeyFor("conditions") String method : CollectionsPlume.sortedKeySet(conditions)) {
      List<String> method_conds = conditions.get(method);
      Collections.sort(method_conds);
      if (method_conds.size() > 0) {
        String qualifiedMethod = (packageName == null) ? method : packageName + "." + method;
        output.println("PPT_NAME " + qualifiedMethod);
        for (int i = 0; i < method_conds.size(); i++) {
          String cond = removeNewlines(method_conds.get(i));
          if (!(cond.equals("true") || cond.equals("false"))) {
            output.println(cond);
          }
        }
        output.println();
      }
    }
  }

  /**
   * Returns target with line separators and the whitespace around a line separator replaced by a
   * single space.
   */
  private static String removeNewlines(String target) {
    String[] lines = StringsPlume.splitLines(target);
    for (int i = 0; i < lines.length; i++) {
      lines[i] = lines[i].trim();
    }
    return String.join(" ", lines);
  }
}
