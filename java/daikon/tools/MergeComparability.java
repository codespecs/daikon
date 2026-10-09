package daikon.tools;

import daikon.Daikon;
import daikon.VarComparabilityImplicit;
import daikon.chicory.DeclReader;
import daikon.chicory.DeclReader.DeclPpt;
import daikon.chicory.DeclReader.DeclVarInfo;
import gnu.getopt.Getopt;
import gnu.getopt.LongOpt;
import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.nio.file.attribute.PosixFilePermissions;
import java.util.ArrayList;
import java.util.Collections;
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
 * every input file.
 *
 * <p>A variable that is comparable to everything in any input file, because it has a scalar
 * negative comparability such as "-1" or has no comparability record, is comparable to everything
 * in the output. Likewise, a component of an array comparability (the element or an index) that is
 * negative in any input file is negative in the output. A chain of comparabilities does not pass
 * through a variable or component that is comparable to everything: if x and y are comparable in
 * one input file and y is comparable to everything in another, then x does not thereby become
 * comparable to other variables.
 *
 * <p>A program point that appears in only some of the input files appears in the output. (For
 * example, DynComp produces no declaration for a program point that was never executed.) A program
 * point that appears in multiple input files must declare the same variables, in the same order, in
 * each of them, and its declarations must contain the same records, other than comparability
 * records. Each program point in the output is a copy of its first declaration in the input files,
 * except that the comparability records are changed.
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

    // Write to a temporary file and then rename it, so that an inconsistency in the input files
    // does not leave a truncated output file or destroy an existing one.  The temporary file is in
    // the same directory as the output file, so that renaming it does not copy it.  Its name ends
    // with the output file name, so that it is compressed if the output file name ends in ".gz".
    // Its name is unique, so that it does not clobber another file, such as that of a concurrent
    // run.  If the output file is a symbolic link, the file it refers to is replaced, so that the
    // link is preserved.  The permissions of an existing output file are preserved, but its owner
    // and group are not.
    Path outputPath = resolveSymbolicLinks(Paths.get(outputFilename).toAbsolutePath());
    Path outputDir = outputPath.getParent();
    Path outputName = outputPath.getFileName();
    if (outputDir == null || outputName == null) {
      throw new Daikon.UserError("Invalid output file " + outputFilename);
    }
    Path tempPath;
    try {
      if (FileSystems.getDefault().supportedFileAttributeViews().contains("posix")) {
        // Files.createTempFile otherwise creates a file that only its owner can read.  These
        // permissions are restricted by the umask, as for any newly-created file.
        tempPath =
            Files.createTempFile(
                outputDir,
                ".tmp.",
                "." + outputName,
                PosixFilePermissions.asFileAttribute(PosixFilePermissions.fromString("rw-rw-rw-")));
      } else {
        tempPath = Files.createTempFile(outputDir, ".tmp.", "." + outputName);
      }
    } catch (IOException e) {
      throw new Daikon.UserError(e, "Problem creating a temporary file in " + outputDir);
    }
    boolean moved = false;
    try {
      PrintWriter pw = new PrintWriter(FilesPlume.newBufferedFileWriter(tempPath.toString()));
      try {
        merge(declFiles, pw);
      } finally {
        pw.close();
      }
      // A PrintWriter does not throw exceptions, but records whether any write failed.
      if (pw.checkError()) {
        throw new Daikon.UserError("Problem writing " + tempPath);
      }
      if (Files.exists(outputPath)
          && FileSystems.getDefault().supportedFileAttributeViews().contains("posix")) {
        Files.setPosixFilePermissions(tempPath, Files.getPosixFilePermissions(outputPath));
      }
      Files.move(tempPath, outputPath, StandardCopyOption.REPLACE_EXISTING);
      moved = true;
    } catch (IOException e) {
      throw new Daikon.UserError(e, "Problem writing " + outputFilename);
    } finally {
      if (!moved) {
        try {
          Files.deleteIfExists(tempPath);
        } catch (IOException e) {
          System.err.println("Could not delete temporary file " + tempPath + ": " + e);
        }
      }
    }
  }

  /**
   * Returns the file that a path refers to, following symbolic links. Unlike {@link
   * Path#toRealPath}, this does not require the file to exist, and it does not follow symbolic
   * links in the directories that contain the file.
   *
   * @param path an absolute path
   * @return the file that {@code path} refers to, which is {@code path} itself if it is not a
   *     symbolic link
   */
  private static Path resolveSymbolicLinks(Path path) {
    for (int i = 0; Files.isSymbolicLink(path); i++) {
      // Limit the number of links that are followed, in case of a cycle.
      if (i == 40) {
        throw new Daikon.UserError("Too many levels of symbolic links: " + path);
      }
      Path parent = path.getParent();
      if (parent == null) {
        throw new Daikon.UserError("Cannot resolve symbolic link " + path);
      }
      try {
        path = parent.resolve(Files.readSymbolicLink(path));
      } catch (IOException e) {
        throw new Daikon.UserError(e, "Problem reading symbolic link " + path);
      }
    }
    return path;
  }

  /**
   * Reads a declaration file.
   *
   * @param filename the file to read
   * @return the contents of the file
   */
  public static DeclReader readDeclFile(String filename) {
    DeclReader result = new DeclReader(true);
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
    try {
      return VarComparabilityImplicit.parseComponents(rep);
    } catch (IllegalArgumentException e) {
      throw new Daikon.UserError(
          e,
          String.format(
              "%s: program point %s: variable %s: malformed comparability \"%s\"",
              ppt.filename, ppt.name, var.name, rep));
    }
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
      // A file without a var-comparability record uses implicit comparability.  Because every file
      // uses implicit comparability, the var-comparability records need no consistency check.
      String varComparability = findRecord(entry.getValue().header, "var-comparability");
      if (varComparability != null && !varComparability.equals("var-comparability implicit")) {
        throw new Daikon.UserError(
            String.format(
                "%s: \"%s\": only implicit comparability can be merged",
                entry.getKey(), varComparability));
      }
      for (String keyword : new String[] {"decl-version", "input-language"}) {
        String expected = findRecord(header, keyword);
        String actual = findRecord(entry.getValue().header, keyword);
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
        // If the template has no comparability record for the variable, then the variable is
        // comparable to everything, so its merged comparability is negative and there is no need to
        // add a record.
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
   * Returns the header record with the given keyword, or null if there is none.
   *
   * @param header the header records of a declaration file, as stored in {@link DeclReader#header}
   * @param keyword the first token of a record, such as "decl-version"
   * @return the header record with the given keyword, or null
   */
  private static @Nullable String findRecord(List<String> header, String keyword) {
    for (String record : header) {
      if (record.equals(keyword) || record.startsWith(keyword + " ")) {
        return record;
      }
    }
    return null;
  }

  /**
   * Returns the records of a declaration other than its first line and its comparability record,
   * with whitespace normalized outside of string literals, in sorted order. The flags in a "flags"
   * record are also sorted, because their order is not significant.
   *
   * @param lines the lines of a declaration
   * @param comparabilityLine the index in {@code lines} of the comparability record, or -1
   * @return the records other than the first line and the comparability record
   */
  private static List<String> otherRecords(List<String> lines, int comparabilityLine) {
    List<String> result = new ArrayList<>(lines.size());
    for (int j = 1; j < lines.size(); j++) {
      if (j != comparabilityLine) {
        List<String> tokens = tokenizeRespectingQuotes(lines.get(j));
        if (!tokens.isEmpty() && tokens.get(0).equals("flags")) {
          Collections.sort(tokens.subList(1, tokens.size()));
        }
        result.add(String.join(" ", tokens));
      }
    }
    Collections.sort(result);
    return result;
  }

  /**
   * Splits a record into whitespace-separated tokens. A string literal, which starts and ends with
   * a double quote, is part of a token even if it contains whitespace. Within a string literal, a
   * backslash escapes the next character.
   *
   * @param record a record, possibly with leading and trailing whitespace
   * @return the tokens of the record; empty if the record is blank
   */
  public static List<String> tokenizeRespectingQuotes(String record) {
    List<String> result = new ArrayList<>();
    StringBuilder token = new StringBuilder();
    boolean inString = false;
    for (int i = 0; i < record.length(); i++) {
      char ch = record.charAt(i);
      if (inString) {
        token.append(ch);
        if (ch == '\\' && i + 1 < record.length()) {
          token.append(record.charAt(++i));
        } else if (ch == '"') {
          inString = false;
        }
      } else if (Character.isWhitespace(ch)) {
        if (token.length() > 0) {
          result.add(token.toString());
          token.setLength(0);
        }
      } else {
        token.append(ch);
        if (ch == '"') {
          inString = true;
        }
      }
    }
    if (token.length() > 0) {
      result.add(token.toString());
    }
    return result;
  }

  /**
   * Throws an exception if two declarations of the same program point or variable have different
   * records.
   *
   * @param what the program point or variable, used in the error message, such as "Program point
   *     C.m():::ENTER"
   * @param expected the records in {@code template}, as returned by {@link #otherRecords}
   * @param actual the records in {@code ppt}, as returned by {@link #otherRecords}
   * @param template the first declaration of the program point
   * @param ppt another declaration of the program point
   */
  private static void checkSameRecords(
      String what, List<String> expected, List<String> actual, DeclPpt template, DeclPpt ppt) {
    if (actual.equals(expected)) {
      return;
    }
    List<String> onlyExpected = new ArrayList<>(expected);
    List<String> onlyActual = new ArrayList<>(actual);
    for (String record : actual) {
      onlyExpected.remove(record);
    }
    for (String record : expected) {
      onlyActual.remove(record);
    }
    throw new Daikon.UserError(
        String.format(
            "%s has records %s in %s but %s in %s",
            what, onlyExpected, template.filename, onlyActual, ppt.filename));
  }

  /**
   * Merges the comparabilities in multiple declarations of the same program point.
   *
   * @param decls the declarations of one program point; must be non-empty
   * @return for each variable, its merged comparability, in the representation returned by {@link
   *     #parseComparability}; {@code [-1]} (comparable to everything) if some declaration has no
   *     comparability or a scalar negative comparability for the variable
   */
  static int[][] mergePpt(List<DeclPpt> decls) {
    DeclPpt template = decls.get(0);
    List<DeclVarInfo> templateVars = new ArrayList<>(template.vars.values());
    int numVars = templateVars.size();

    // comparabilities.get(d)[v] is the parsed comparability of variable v in declaration d, or
    // null if that variable is comparable to everything in declaration d.
    List<int[] @Nullable []> comparabilities = new ArrayList<>(decls.size());
    // Check consistency and determine the number of parts of each variable's comparability.
    int[] numParts = new int[numVars];
    // universalVar[v] is true if variable v is comparable to everything in some declaration.
    boolean[] universalVar = new boolean[numVars];
    List<String> templateHeaderRecords = otherRecords(template.declHeaderLines, -1);
    List<List<String>> templateVarRecords = new ArrayList<>(numVars);
    for (DeclVarInfo var : templateVars) {
      templateVarRecords.add(otherRecords(var.lines, var.comparabilityLine));
    }
    for (DeclPpt ppt : decls) {
      boolean isTemplate = (ppt == template);
      if (!isTemplate) {
        checkSameRecords(
            "Program point " + ppt.name,
            templateHeaderRecords,
            otherRecords(ppt.declHeaderLines, -1),
            template,
            ppt);
      }
      List<DeclVarInfo> vars = isTemplate ? templateVars : new ArrayList<>(ppt.vars.values());
      if (vars.size() != numVars) {
        throw new Daikon.UserError(
            String.format(
                "Program point %s has %d variables in %s but %d variables in %s",
                ppt.name, numVars, template.filename, vars.size(), ppt.filename));
      }
      int[] @Nullable [] pptComparabilities = new int[numVars][];
      for (int v = 0; v < numVars; v++) {
        DeclVarInfo var = vars.get(v);
        if (!isTemplate) {
          DeclVarInfo expected = templateVars.get(v);
          if (!var.name.equals(expected.name)) {
            throw new Daikon.UserError(
                String.format(
                    "Program point %s: variable %d is %s in %s but %s in %s",
                    ppt.name, v + 1, expected.name, template.filename, var.name, ppt.filename));
          }
          checkSameRecords(
              "Program point " + ppt.name + ": variable " + var.name,
              templateVarRecords.get(v),
              otherRecords(var.lines, var.comparabilityLine),
              template,
              ppt);
        }
        int @Nullable [] comparability =
            (var.comparability == null) ? null : parseComparability(ppt, var);
        // A scalar negative comparability, like the absence of a comparability, makes the variable
        // comparable to everything, including variables with any number of array dimensions.
        if (comparability == null || (comparability.length == 1 && comparability[0] < 0)) {
          universalVar[v] = true;
          continue;
        }
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
    // its variable is comparable to everything in some declaration.  A universal slot is not
    // unioned with any other slot, so a chain of comparabilities does not pass through it.
    boolean[] universal = new boolean[numSlots];
    for (int[] @Nullable [] pptComparabilities : comparabilities) {
      for (int v = 0; v < numVars; v++) {
        int[] comparability = pptComparabilities[v];
        for (int p = 0; p < numParts[v]; p++) {
          if (universalVar[v] || (comparability != null && comparability[p] < 0)) {
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
      if (universalVar[v]) {
        // A negative comparability such as "-1[-1]" would not be comparable to scalars.
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
