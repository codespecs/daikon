package daikon.tools;

import daikon.Daikon;
import gnu.getopt.Getopt;
import gnu.getopt.LongOpt;
import java.io.BufferedReader;
import java.io.BufferedWriter;
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
 * file. The result can be passed to Chicory's {@code --comparability-file} command-line option or
 * to Daikon.
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
 *
 * <p>Comparability values are local to a program point: the same value at two different program
 * points does not indicate that the variables are comparable.
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
   * This does the work of {@link #main(String[])}, but it never calls System.exit, so it is
   * appropriate to be called programmatically.
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

    List<DeclFile> declFiles = new ArrayList<>();
    for (int i = fileIndex; i < args.length; i++) {
      declFiles.add(readDeclFile(args[i]));
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

  // Representation of a declaration file

  /** The contents of a declaration file. */
  public static class DeclFile {
    /** The file name, used in error messages. */
    final String filename;

    /** The header records, such as "decl-version 2.0", without comments or blank lines. */
    final List<String> header;

    /** The program points declared in the file, in the order they appear. */
    final List<PptDecl> ppts;

    /**
     * Creates a new DeclFile.
     *
     * @param filename the file name
     * @param header the header records
     * @param ppts the program points declared in the file
     */
    DeclFile(String filename, List<String> header, List<PptDecl> ppts) {
      this.filename = filename;
      this.header = header;
      this.ppts = ppts;
    }
  }

  /** The declaration of a program point. */
  static class PptDecl {
    /** The program point name. */
    final String name;

    /** The file in which this declaration appears, used in error messages. */
    final String filename;

    /**
     * The lines of the declaration that precede the first variable, starting with the "ppt" line.
     */
    final List<String> pptLines;

    /** The variables, in order. */
    final List<VarDecl> vars;

    /**
     * Creates a new PptDecl.
     *
     * @param name the program point name
     * @param filename the file in which this declaration appears
     * @param pptLines the lines that precede the first variable
     * @param vars the variables
     */
    PptDecl(String name, String filename, List<String> pptLines, List<VarDecl> vars) {
      this.name = name;
      this.filename = filename;
      this.pptLines = pptLines;
      this.vars = vars;
    }
  }

  /** The declaration of a variable. */
  static class VarDecl {
    /** The variable name. */
    final String name;

    /** The lines of the declaration, starting with the "variable" line. */
    final List<String> lines;

    /** The index in {@link #lines} of the comparability record, or -1 if there is none. */
    final int comparabilityLine;

    /**
     * The parsed comparability: element 0 is the base, and the remaining elements are the
     * comparabilities of the indices. For example, "3[4]" is represented as [3, 4]. Null if the
     * variable has no comparability record.
     */
    final int @Nullable [] comparability;

    /**
     * Creates a new VarDecl.
     *
     * @param name the variable name
     * @param lines the lines of the declaration
     * @param comparabilityLine the index in {@code lines} of the comparability record, or -1
     * @param comparability the parsed comparability, or null
     */
    VarDecl(
        String name, List<String> lines, int comparabilityLine, int @Nullable [] comparability) {
      this.name = name;
      this.lines = lines;
      this.comparabilityLine = comparabilityLine;
      this.comparability = comparability;
    }
  }

  // Reading

  /**
   * Returns true if the line is a comment.
   *
   * @param line a line of a declaration file
   * @return true if the line is a comment
   */
  private static boolean isComment(String line) {
    return line.startsWith("//") || line.startsWith("#");
  }

  /**
   * Reads a declaration file.
   *
   * @param filename the file to read
   * @return the contents of the file
   */
  public static DeclFile readDeclFile(String filename) {
    try (BufferedReader reader = FilesPlume.newBufferedFileReader(filename)) {
      List<String> lines = new ArrayList<>();
      for (String line = reader.readLine(); line != null; line = reader.readLine()) {
        lines.add(line);
      }
      return parseDeclFile(filename, lines);
    } catch (IOException e) {
      throw new Daikon.UserError(e, "Problem reading " + filename);
    }
  }

  /**
   * Parses the contents of a declaration file.
   *
   * @param filename the file name, used in error messages
   * @param lines the lines of the file
   * @return the contents of the file
   */
  public static DeclFile parseDeclFile(String filename, List<String> lines) {
    List<String> header = new ArrayList<>();
    List<PptDecl> ppts = new ArrayList<>();
    boolean seenVersion2 = false;
    int i = 0;
    while (i < lines.size()) {
      String line = lines.get(i).trim();
      if (line.isEmpty() || isComment(line)) {
        i++;
        continue;
      }
      if (line.startsWith("ppt ")) {
        if (!seenVersion2) {
          throw new Daikon.UserError(
              String.format(
                  "%s line %d: program point declaration precedes \"decl-version 2.0\"",
                  filename, i + 1));
        }
        int end = i;
        while (end < lines.size() && !lines.get(end).trim().isEmpty()) {
          end++;
        }
        ppts.add(parsePpt(filename, i, lines.subList(i, end)));
        i = end;
        continue;
      }
      if (line.equals("DECLARE")) {
        throw new Daikon.UserError(
            filename + ": MergeComparability supports only version 2.0 declaration files");
      }
      if (line.startsWith("decl-version")) {
        if (!line.equals("decl-version 2.0")) {
          throw new Daikon.UserError(
              String.format("%s line %d: unsupported \"%s\"", filename, i + 1, line));
        }
        seenVersion2 = true;
      } else if (line.startsWith("var-comparability")) {
        if (!line.equals("var-comparability implicit")) {
          throw new Daikon.UserError(
              String.format(
                  "%s line %d: \"%s\": only implicit comparability can be merged",
                  filename, i + 1, line));
        }
      } else if (!seenVersion2) {
        throw new Daikon.UserError(
            String.format(
                "%s line %d: expected \"decl-version 2.0\", found \"%s\"", filename, i + 1, line));
      } else if (!line.startsWith("input-language")) {
        // For example, a sample record in a .dtrace file.
        throw new Daikon.UserError(
            String.format(
                "%s line %d: expected a declaration, found \"%s\"", filename, i + 1, line));
      }
      header.add(line);
      i++;
    }
    return new DeclFile(filename, header, ppts);
  }

  /**
   * Parses a single program point declaration.
   *
   * @param filename the file name, used in error messages
   * @param startLine the index of the first line of the declaration within the file, used in error
   *     messages
   * @param lines the lines of the declaration, starting with the "ppt" line and not including the
   *     terminating blank line
   * @return the program point declaration
   */
  static PptDecl parsePpt(String filename, int startLine, List<String> lines) {
    String pptName = lines.get(0).trim().substring("ppt ".length());
    int firstVar = 1;
    while (firstVar < lines.size() && !lines.get(firstVar).trim().startsWith("variable ")) {
      firstVar++;
    }
    List<String> pptLines = new ArrayList<>(lines.subList(0, firstVar));

    List<VarDecl> vars = new ArrayList<>();
    int i = firstVar;
    while (i < lines.size()) {
      String varName = lines.get(i).trim().substring("variable ".length()).trim();
      int end = i + 1;
      while (end < lines.size() && !lines.get(end).trim().startsWith("variable ")) {
        end++;
      }
      List<String> varLines = new ArrayList<>(lines.subList(i, end));
      int comparabilityLine = -1;
      int[] comparability = null;
      for (int j = 1; j < varLines.size(); j++) {
        String[] tokens = varLines.get(j).trim().split("\\s+");
        if (tokens[0].equals("comparability")) {
          if (comparabilityLine != -1) {
            throw new Daikon.UserError(
                String.format(
                    "%s line %d: multiple comparability records for variable %s",
                    filename, startLine + i + j + 1, varName));
          }
          if (tokens.length != 2) {
            throw new Daikon.UserError(
                String.format(
                    "%s line %d: malformed comparability record \"%s\"",
                    filename, startLine + i + j + 1, varLines.get(j).trim()));
          }
          comparabilityLine = j;
          comparability = parseComparability(tokens[1], filename, startLine + i + j + 1);
        }
      }
      for (VarDecl other : vars) {
        if (other.name.equals(varName)) {
          throw new Daikon.UserError(
              String.format(
                  "%s line %d: variable %s declared twice in program point %s",
                  filename, startLine + i + 1, varName, pptName));
        }
      }
      vars.add(new VarDecl(varName, varLines, comparabilityLine, comparability));
      i = end;
    }
    return new PptDecl(pptName, filename, pptLines, vars);
  }

  /**
   * Parses an implicit comparability, such as "3" or "3[4]".
   *
   * @param rep the comparability, as written in a declaration file
   * @param filename the file name, used in error messages
   * @param lineNumber the line number, used in error messages
   * @return the base comparability followed by the comparabilities of the indices
   */
  static int[] parseComparability(String rep, String filename, int lineNumber) {
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
            String.format("%s line %d: malformed comparability \"%s\"", filename, lineNumber, rep));
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
   * @param declFiles the declaration files to merge
   * @param pw where to write the merged declarations
   */
  public static void merge(List<DeclFile> declFiles, PrintWriter pw) {
    List<String> header = declFiles.get(0).header;
    for (DeclFile df : declFiles) {
      for (String prefix : new String[] {"decl-version", "var-comparability", "input-language"}) {
        String expected = findRecord(header, prefix);
        String actual = findRecord(df.header, prefix);
        if (!Objects.equals(expected, actual)) {
          throw new Daikon.UserError(
              String.format(
                  "Inconsistent headers: %s has \"%s\" but %s has \"%s\"",
                  declFiles.get(0).filename, expected, df.filename, actual));
        }
      }
    }

    // Map from program point name to all its declarations, in order.
    Map<String, List<PptDecl>> pptDecls = new LinkedHashMap<>();
    for (DeclFile df : declFiles) {
      for (PptDecl ppt : df.ppts) {
        pptDecls.computeIfAbsent(ppt.name, k -> new ArrayList<>()).add(ppt);
      }
    }

    pw.println("// Declarations written by daikon.tools.MergeComparability, merging:");
    for (DeclFile df : declFiles) {
      pw.println("//   " + df.filename);
    }
    pw.println();
    for (String record : header) {
      pw.println(record);
    }
    pw.println();

    for (List<PptDecl> decls : pptDecls.values()) {
      PptDecl template = decls.get(0);
      int[][] merged = mergePpt(decls);
      for (String line : template.pptLines) {
        pw.println(line);
      }
      for (int v = 0; v < template.vars.size(); v++) {
        VarDecl var = template.vars.get(v);
        int[] comparability = merged[v];
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
   * Merges the comparabilities in multiple declarations of the same program point.
   *
   * @param decls the declarations of one program point; must be non-empty
   * @return for each variable, its merged comparability, in the representation of {@link
   *     VarDecl#comparability}; an empty array if no declaration has a comparability for the
   *     variable; and {@code [-1]} (comparable to everything) if some but not all declarations have
   *     a comparability for the variable
   */
  static int[][] mergePpt(List<PptDecl> decls) {
    PptDecl template = decls.get(0);
    int numVars = template.vars.size();

    // Check consistency and determine the number of parts of each variable's comparability.
    int[] numParts = new int[numVars];
    // missing[v] is true if some declaration has no comparability for variable v.
    boolean[] missing = new boolean[numVars];
    for (PptDecl ppt : decls) {
      if (ppt.vars.size() != numVars) {
        throw new Daikon.UserError(
            String.format(
                "Program point %s has %d variables in %s but %d variables in %s",
                ppt.name, numVars, template.filename, ppt.vars.size(), ppt.filename));
      }
      for (int v = 0; v < numVars; v++) {
        VarDecl var = ppt.vars.get(v);
        String expectedName = template.vars.get(v).name;
        if (!var.name.equals(expectedName)) {
          throw new Daikon.UserError(
              String.format(
                  "Program point %s: variable %d is %s in %s but %s in %s",
                  ppt.name, v + 1, expectedName, template.filename, var.name, ppt.filename));
        }
        if (var.comparability == null) {
          missing[v] = true;
        } else {
          if (numParts[v] == 0) {
            numParts[v] = var.comparability.length;
          } else if (numParts[v] != var.comparability.length) {
            throw new Daikon.UserError(
                String.format(
                    "Program point %s: variable %s has comparabilities with different numbers of"
                        + " array dimensions, such as in %s",
                    ppt.name, var.name, ppt.filename));
          }
        }
      }
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
    for (PptDecl ppt : decls) {
      for (int v = 0; v < numVars; v++) {
        int[] comparability = ppt.vars.get(v).comparability;
        for (int p = 0; p < numParts[v]; p++) {
          if (comparability == null || comparability[p] < 0) {
            universal[slotStart[v] + p] = true;
          }
        }
      }
    }

    // Union the non-universal slots that have the same comparability within some declaration.
    UnionFind uf = new UnionFind(numSlots);
    for (PptDecl ppt : decls) {
      // Map from comparability value to the first slot with that value.
      Map<Integer, Integer> representative = new HashMap<>();
      for (int v = 0; v < numVars; v++) {
        int[] comparability = ppt.vars.get(v).comparability;
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

  /** A union-find (disjoint-set) data structure over the integers 0..n-1. */
  static class UnionFind {
    /** The parent of each element; an element is a root if it is its own parent. */
    private final int[] parent;

    /**
     * Creates a new UnionFind in which each element is in its own set.
     *
     * @param n the number of elements
     */
    UnionFind(int n) {
      parent = new int[n];
      for (int i = 0; i < n; i++) {
        parent[i] = i;
      }
    }

    /**
     * Returns the representative of the set containing the element.
     *
     * @param x an element
     * @return the representative of the set containing x
     */
    int find(int x) {
      while (parent[x] != x) {
        parent[x] = parent[parent[x]];
        x = parent[x];
      }
      return x;
    }

    /**
     * Merges the sets containing the two elements.
     *
     * @param x an element
     * @param y an element
     */
    void union(int x, int y) {
      int rx = find(x);
      int ry = find(y);
      if (rx != ry) {
        parent[ry] = rx;
      }
    }
  }
}
