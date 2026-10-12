package daikon.tools.jtb;

import static java.nio.charset.StandardCharsets.UTF_8;

import daikon.Daikon;
import daikon.DaikonGetopt;
import java.io.File;
import java.io.IOException;
import java.io.StringReader;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.util.StringJoiner;
import jtb.cparser.*;
import jtb.cparser.customvisitor.*;
import jtb.cparser.syntaxtree.*;

public class CreateSpinfoC {

  /** Do not instantiate. */
  private CreateSpinfoC() {
    throw new UnsupportedOperationException("Do not instantiate");
  }

  /** The usage message for this program. */
  private static final String usage =
      "C Parser Version 0.1Alpha:  Usage:  java daikon.tools.jtb.CreateSpinfoC inputfile ...";

  // CParser is a static parser, so it can be constructed only once.  Each parse reinitializes it.
  static {
    @SuppressWarnings("UnusedVariable") // sets static variables for CParser.TranslationUnit()
    CParser parser = new CParser(new StringReader(""));
  }

  /**
   * The entry point for CreateSpinfoC.
   *
   * @param args the names of the C files
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
    String[] files = DaikonGetopt.nonOptionArgs(args, usage);
    if (files.length == 0) {
      throw new Daikon.UserError(usage);
    }
    // Process every file, even if an earlier one fails.
    StringJoiner errors = new StringJoiner(Daikon.lineSep);
    for (String inputFile : files) {
      try {
        processFile(inputFile);
      } catch (Daikon.UserError e) {
        errors.add(String.valueOf(e.getMessage()));
      }
    }
    if (errors.length() != 0) {
      throw new Daikon.UserError(errors.toString());
    }
  }

  /**
   * Creates a .spinfo file from the given C file. For input file {@code foo.c}, the output file is
   * {@code foo.spinfo}.
   *
   * @param inputFile the name of the C file
   */
  private static void processFile(String inputFile) {
    int dotPos = inputFile.lastIndexOf('.');
    // On Windows, both '/' and '\\' are separators.
    int separatorPos =
        Math.max(inputFile.lastIndexOf('/'), inputFile.lastIndexOf(File.separatorChar));
    if (dotPos <= separatorPos) {
      throw new Daikon.UserError(
          "File name has no extension: " + inputFile + Daikon.lineSep + usage);
    }
    String fileName = inputFile.substring(0, dotPos);
    System.out.println("Create spinfo file from file " + inputFile + " . . .");
    String contents;
    try {
      contents = new String(Files.readAllBytes(Paths.get(inputFile)), UTF_8);
    } catch (IOException e) {
      throw new Daikon.UserError(e, "Problem reading " + inputFile + ": " + e);
    }
    // The parser does not accept '\f' characters.
    contents = contents.replace("\f", "");

    TranslationUnit root;
    try {
      CParser.ReInit(new StringReader(contents));
      root = CParser.TranslationUnit();
    } catch (ParseException | TokenMgrError e) {
      throw new Daikon.UserError(e, "CreateSpinfoC encountered errors while parsing " + inputFile);
    }
    StringFinder finder = new StringFinder();
    root.accept(finder);

    // Write to a temporary file and rename it on success, so that a failure does not leave a
    // partial .spinfo file.
    Path spinfoFile = Paths.get(fileName + ".spinfo");
    Path tempFile = null;
    try {
      Path dir = spinfoFile.toAbsolutePath().getParent();
      assert dir != null : "@AssumeAssertion(nullness): an absolute file path has a parent";
      tempFile = Files.createTempFile(dir, "CreateSpinfoC", ".spinfo");
      try (ConditionPrinter printer = new ConditionPrinter(tempFile.toString())) {
        printer.setActualStrings(finder.functionStringMapping);
        printer.setStringArrays(finder.stringMatrices);
        root.accept(printer);
      }
      Files.move(tempFile, spinfoFile, StandardCopyOption.REPLACE_EXISTING);
    } catch (IOException e) {
      throw new Daikon.UserError(e, "Problem writing " + spinfoFile + ": " + e);
    } finally {
      // After a successful move, the temporary file no longer exists.
      if (tempFile != null) {
        tempFile.toFile().delete();
      }
    }
    System.out.println("CreateSpinfoC:  C program parsed successfully.");
  }
}
