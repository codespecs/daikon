package daikon.tools;

import daikon.Daikon;
import daikon.chicory.DeclReader;
import daikon.chicory.DeclReader.DeclPpt;
import daikon.chicory.DeclReader.DeclVarInfo;
import gnu.getopt.Getopt;
import gnu.getopt.LongOpt;
import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.plumelib.util.FilesPlume;
import org.plumelib.util.StringsPlume;

/**
 * MergeComparability merges the comparability information in multiple declaration files, such as
 * the {@code .decls-DynComp} files produced by multiple runs of DynComp, into a single declaration
 * file.
 *
 * <p>Two variables at a program point are comparable in the output if they are comparable in any of
 * the input files, or if they are related by a chain of such comparabilities. That is, the
 * comparability sets of the output are the finest partition that is coarser than the partition in
 * every input file. A variable whose comparability is negative (meaning that it is comparable to
 * every other variable) in any input file has a negative comparability in the output.
 *
 * <p>A program point that appears in only some of the input files appears in the output. (For
 * example, DynComp produces no declaration for a program point that was never executed.) A program
 * point that appears in multiple input files must declare the same variables, in the same order, in
 * each of them. Each program point in the output is a copy of its first declaration in the input
 * files, except that the comparability records are changed.
 */
public final class MergeComparability {

  /** Do not instantiate. */
  private MergeComparability() {
    throw new UnsupportedOperationException("Do not instantiate");
  }

  /** The system-specific line separator. */
  private static final String lineSep = System.lineSeparator();

  /** The usage message for this program. */
  private static String usage =
      StringsPlume.joinLines(
          "Usage: java daikon.tools.MergeComparability -o OUTFILE DECLFILE [DECLFILE ...]",
          "Merges the comparability information in the declaration files (such as",
          "the .decls-DynComp files output by multiple runs of DynComp) and writes",
          "the result to OUTFILE.",
          "  -h, --" + Daikon.help_SWITCH,
          "      Display this usage message",
          "  -o, --output FILE",
          "      Write the merged declarations to FILE (required)");

  /**
   * Entry point for the MergeComparability program.
   *
   * @param args command-line arguments
   */
  public static void main(String[] args) {
    try {
      mainHelper(args);
    } catch (Daikon.DaikonTerminationException e) {
      Daikon.handleDaikonTerminationException(e);
    }
  }

  /**
   * This does the work of {@link #main(String[])}, but it never calls System.exit (as
   * Daikon.handleDaikonTerminationException does), so it is appropriate to be called
   * programmatically.
   *
   * @param args command-line arguments, like those of {@link #main}
   */
  public static void mainHelper(String[] args) {
    String outputFilename = null;

    LongOpt[] longopts =
        new LongOpt[] {
          new LongOpt(Daikon.help_SWITCH, LongOpt.NO_ARGUMENT, null, 'h'),
          new LongOpt("output", LongOpt.REQUIRED_ARGUMENT, null, 'o'),
        };
    Getopt g = new Getopt("daikon.tools.MergeComparability", args, "ho:", longopts);
    int c;
    while ((c = g.getopt()) != -1) {
      switch (c) {
        case 'h':
          System.out.println(usage);
          throw new Daikon.NormalTermination();
        case 'o':
          if (outputFilename != null) {
            throw new Daikon.UserError("Multiple output files supplied on command line");
          }
          outputFilename = Daikon.getOptarg(g);
          break;
        case '?':
          // getopt() already printed an error
          throw new Daikon.UserError(usage);
        default:
          throw new Daikon.UserError("getopt() returned " + c);
      }
    }

    int fileIndex = g.getOptind();
    if (outputFilename == null) {
      throw new Daikon.UserError("No output file specified (use -o)" + lineSep + usage);
    }
    if (fileIndex == args.length) {
      throw new Daikon.UserError("No input files specified" + lineSep + usage);
    }

    Map<String, DeclReader> declFiles = new LinkedHashMap<>();
    for (int i = fileIndex; i < args.length; i++) {
      declFiles.put(args[i], readDeclFile(args[i]));
    }

    // Merge fully before opening the output file, so that an inconsistency in the input files does
    // not leave a truncated output file or destroy an existing one.
    StringWriter merged = new StringWriter();
    try (PrintWriter pw = new PrintWriter(merged)) {
      merge(declFiles, pw);
    }

    // Use a BufferedWriter rather than a PrintWriter, which would discard write errors.
    try (BufferedWriter writer = FilesPlume.newBufferedFileWriter(outputFilename)) {
      writer.write(merged.toString());
    } catch (IOException e) {
      throw new Daikon.UserError(e, "Problem writing " + outputFilename);
    }
  }

  /**
   * Reads a declaration file.
   *
   * @param filename the file to read
   * @return the contents of the file
   */
  public static DeclReader readDeclFile(String filename) {
    DeclReader result = new DeclReader();
    try {
      result.read(new File(filename));
    } catch (IOException e) {
      throw new Daikon.UserError(e, "Problem reading " + filename);
    }
    return result;
  }

  /**
   * Parses an implicit comparability, such as "3" or "3[4]".
   *
   * @param ppt the program point declaration in which the comparability appears, used in error
   *     messages
   * @param var the variable whose comparability to parse; must have a comparability
   * @return the base comparability followed by the comparabilities of the indices
   */
  static int[] parseComparability(DeclPpt ppt, DeclVarInfo var) {
    String rep = var.comparability;
    if (rep == null) {
      throw new IllegalArgumentException("Variable " + var.name + " has no comparability");
    }
    List<String> parts = new ArrayList<>();
    String rest = rep;
    while (rest.endsWith("]")) {
      int openpos = rest.lastIndexOf('[');
      if (openpos == -1) {
        break;
      }
      parts.add(0, rest.substring(openpos + 1, rest.length() - 1));
      rest = rest.substring(0, openpos);
    }
    parts.add(0, rest);
    int[] result = new int[parts.size()];
    for (int i = 0; i < result.length; i++) {
      try {
        result[i] = Integer.parseInt(parts.get(i));
      } catch (NumberFormatException e) {
        throw new Daikon.UserError(
            e,
            String.format(
                "%s: program point %s: variable %s: malformed comparability \"%s\"",
                ppt.filename, ppt.name, var.name, rep));
      }
    }
    return result;
  }

  /**
   * Formats a comparability as it is written in a declaration file.
   *
   * @param comparability the base comparability followed by the comparabilities of the indices
   * @return the comparability, as written in a declaration file, such as "3" or "3[4]"
   */
  static String formatComparability(int[] comparability) {
    StringBuilder sb = new StringBuilder();
    sb.append(comparability[0]);
    for (int i = 1; i < comparability.length; i++) {
      sb.append('[').append(comparability[i]).append(']');
    }
    return sb.toString();
  }

  // Merging

  /**
   * Merges the declaration files and writes the result.
   *
   * @param declFiles map from file name to the contents of that declaration file; must be non-empty
   * @param pw where to write the merged declarations
   */
  public static void merge(Map<String, DeclReader> declFiles, PrintWriter pw) {
    Map.Entry<String, DeclReader> first = declFiles.entrySet().iterator().next();
    List<String> header = first.getValue().header;
    for (Map.Entry<String, DeclReader> entry : declFiles.entrySet()) {
      String varComparability = findRecord(entry.getValue().header, "var-comparability");
      if (varComparability != null && !varComparability.equals("var-comparability implicit")) {
        throw new Daikon.UserError(
            String.format(
                "%s: \"%s\": only implicit comparability can be merged",
                entry.getKey(), varComparability));
      }
      for (String prefix : new String[] {"decl-version", "var-comparability", "input-language"}) {
        String expected = findRecord(header, prefix);
        String actual = findRecord(entry.getValue().header, prefix);
        if (!Objects.equals(expected, actual)) {
          throw new Daikon.UserError(
              String.format(
                  "Inconsistent headers: %s has \"%s\" but %s has \"%s\"",
                  first.getKey(), expected, entry.getKey(), actual));
        }
      }
    }

    // Map from program point name to all its declarations, in order.
    Map<String, List<DeclPpt>> pptDecls = new LinkedHashMap<>();
    for (DeclReader declFile : declFiles.values()) {
      for (DeclPpt ppt : declFile.ppts.values()) {
        pptDecls.computeIfAbsent(ppt.name, k -> new ArrayList<>()).add(ppt);
      }
    }

    pw.println("// Declarations written by daikon.tools.MergeComparability, merging:");
    for (String filename : declFiles.keySet()) {
      pw.println("//   " + filename);
    }
    pw.println();
    for (String record : header) {
      pw.println(record);
    }
    pw.println();

    for (List<DeclPpt> decls : pptDecls.values()) {
      DeclPpt template = decls.get(0);
      int[][] merged = mergePpt(decls);
      for (String line : template.declHeaderLines) {
        pw.println(line);
      }
      int v = 0;
      for (DeclVarInfo var : template.vars.values()) {
        int[] comparability = merged[v++];
        // A variable with no comparability record is comparable to everything, so its merged
        // comparability is negative and there is no need to add a record.
        for (int j = 0; j < var.lines.size(); j++) {
          if (j == var.comparabilityLine) {
            String line = var.lines.get(j);
            String indent = line.substring(0, line.indexOf("comparability"));
            pw.println(indent + "comparability " + formatComparability(comparability));
          } else {
            pw.println(var.lines.get(j));
          }
        }
      }
      pw.println();
    }
  }

  /**
   * Returns the header record that starts with the given prefix, or null if there is none.
   *
   * @param header the header records of a declaration file
   * @param prefix the start of a record, such as "decl-version"
   * @return the header record that starts with the given prefix, or null
   */
  private static @Nullable String findRecord(List<String> header, String prefix) {
    for (String record : header) {
      if (record.startsWith(prefix)) {
        return record;
      }
    }
    return null;
  }

  /**
   * Throws an exception if a record of a variable differs between two declarations of the same
   * program point.
   *
   * @param keyword the record's keyword, such as "rep-type"
   * @param expected the value of the record in {@code template}, or null if there is none
   * @param actual the value of the record in {@code ppt}, or null if there is none
   * @param varName the variable name
   * @param template the first declaration of the program point
   * @param ppt another declaration of the program point
   */
  private static void checkSameRecord(
      String keyword,
      @Nullable String expected,
      @Nullable String actual,
      String varName,
      DeclPpt template,
      DeclPpt ppt) {
    if (!Objects.equals(expected, actual)) {
      throw new Daikon.UserError(
          String.format(
              "Program point %s: variable %s has %s in %s but %s in %s",
              ppt.name,
              varName,
              describeRecord(keyword, expected),
              template.filename,
              describeRecord(keyword, actual),
              ppt.filename));
    }
  }

  /**
   * Returns a description of a record, for use in error messages.
   *
   * @param keyword the record's keyword, such as "rep-type"
   * @param value the value of the record, or null if there is no such record
   * @return a description of the record
   */
  private static String describeRecord(String keyword, @Nullable String value) {
    return (value == null) ? ("no " + keyword + " record") : ("\"" + keyword + " " + value + "\"");
  }

  /**
   * Merges the comparabilities in multiple declarations of the same program point.
   *
   * @param decls the declarations of one program point; must be non-empty
   * @return for each variable, its merged comparability, in the representation returned by {@link
   *     #parseComparability}; an empty array if no declaration has a comparability for the
   *     variable; and {@code [-1]} (comparable to everything) if some but not all declarations have
   *     a comparability for the variable
   */
  static int[][] mergePpt(List<DeclPpt> decls) {
    DeclPpt template = decls.get(0);
    List<DeclVarInfo> templateVars = new ArrayList<>(template.vars.values());
    int numVars = templateVars.size();

    // comparabilities.get(d)[v] is the parsed comparability of variable v in declaration d, or
    // null if that variable has no comparability record.
    List<int[] @Nullable []> comparabilities = new ArrayList<>(decls.size());
    // Check consistency and determine the number of parts of each variable's comparability.
    int[] numParts = new int[numVars];
    // missing[v] is true if some declaration has no comparability for variable v.
    boolean[] missing = new boolean[numVars];
    for (DeclPpt ppt : decls) {
      List<DeclVarInfo> vars = new ArrayList<>(ppt.vars.values());
      if (vars.size() != numVars) {
        throw new Daikon.UserError(
            String.format(
                "Program point %s has %d variables in %s but %d variables in %s",
                ppt.name, numVars, template.filename, vars.size(), ppt.filename));
      }
      int[] @Nullable [] pptComparabilities = new int[numVars][];
      for (int v = 0; v < numVars; v++) {
        DeclVarInfo var = vars.get(v);
        DeclVarInfo expected = templateVars.get(v);
        if (!var.name.equals(expected.name)) {
          throw new Daikon.UserError(
              String.format(
                  "Program point %s: variable %d is %s in %s but %s in %s",
                  ppt.name, v + 1, expected.name, template.filename, var.name, ppt.filename));
        }
        checkSameRecord("var-kind", expected.var_kind, var.var_kind, var.name, template, ppt);
        checkSameRecord("dec-type", expected.type, var.type, var.name, template, ppt);
        checkSameRecord("rep-type", expected.rep_type, var.rep_type, var.name, template, ppt);
        if (var.comparability == null) {
          missing[v] = true;
        } else {
          int[] comparability = parseComparability(ppt, var);
          pptComparabilities[v] = comparability;
          if (numParts[v] == 0) {
            numParts[v] = comparability.length;
          } else if (numParts[v] != comparability.length) {
            throw new Daikon.UserError(
                String.format(
                    "Program point %s: variable %s has comparabilities with different numbers of"
                        + " array dimensions, such as in %s",
                    ppt.name, var.name, ppt.filename));
          }
        }
      }
      comparabilities.add(pptComparabilities);
    }

    // Each part of each variable's comparability is a "slot".  slotStart[v] is the index of
    // variable v's first slot.
    int[] slotStart = new int[numVars + 1];
    for (int v = 0; v < numVars; v++) {
      slotStart[v + 1] = slotStart[v] + numParts[v];
    }
    int numSlots = slotStart[numVars];

    // A slot is universal if it is comparable to everything (negative) in some declaration, or if
    // some declaration provides no comparability for it.
    boolean[] universal = new boolean[numSlots];
    for (int[] @Nullable [] pptComparabilities : comparabilities) {
      for (int v = 0; v < numVars; v++) {
        int[] comparability = pptComparabilities[v];
        for (int p = 0; p < numParts[v]; p++) {
          if (comparability == null || comparability[p] < 0) {
            universal[slotStart[v] + p] = true;
          }
        }
      }
    }

    // Union the non-universal slots that have the same comparability within some declaration.
    UnionFind uf = new UnionFind(numSlots);
    for (int[] @Nullable [] pptComparabilities : comparabilities) {
      // Map from comparability value to the first slot with that value.
      Map<Integer, Integer> representative = new HashMap<>();
      for (int v = 0; v < numVars; v++) {
        int[] comparability = pptComparabilities[v];
        if (comparability == null) {
          continue;
        }
        for (int p = 0; p < numParts[v]; p++) {
          int slot = slotStart[v] + p;
          if (universal[slot]) {
            continue;
          }
          Integer rep = representative.putIfAbsent(comparability[p], slot);
          if (rep != null) {
            uf.union(rep, slot);
          }
        }
      }
    }

    // Number the equivalence classes in order of first appearance.
    // classNumber[root] is the output comparability of the class whose root is root, or 0 if not
    // yet assigned.
    int[] classNumber = new int[numSlots];
    int nextNumber = 1;
    int[][] result = new int[numVars][];
    for (int v = 0; v < numVars; v++) {
      if (missing[v] && numParts[v] != 0) {
        // A variable with no comparability is comparable to everything, including scalars.  A
        // negative comparability such as "-1[-1]" would not be comparable to scalars.
        result[v] = new int[] {-1};
        continue;
      }
      result[v] = new int[numParts[v]];
      for (int p = 0; p < numParts[v]; p++) {
        int slot = slotStart[v] + p;
        if (universal[slot]) {
          result[v][p] = -1;
        } else {
          int root = uf.find(slot);
          if (classNumber[root] == 0) {
            classNumber[root] = nextNumber++;
          }
          result[v][p] = classNumber[root];
        }
      }
    }
    return result;
  }
}
