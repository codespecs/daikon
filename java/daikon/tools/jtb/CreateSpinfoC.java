package daikon.tools.jtb;

import static java.nio.charset.StandardCharsets.UTF_8;

import daikon.Daikon;
import daikon.DaikonGetopt;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.Reader;
import java.io.UncheckedIOException;
import java.io.Writer;
import java.nio.file.Files;
import java.nio.file.Paths;
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
      "C Parser Version 0.1Alpha:  Usage:  java daikon.tools.jtb.CreateSpinfoC inputfile";

  /**
   * The entry point for CreateSpinfoC.
   *
   * @param args one argument, the name of the C file
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
    String[] files = DaikonGetopt.nonOptionArgs("daikon.tools.jtb.CreateSpinfoC", args, usage);
    if (files.length != 1) {
      throw new Daikon.UserError(usage);
    }
    String inputFile = files[0];
    System.out.println("Create spinfo file from file " + inputFile + " . . .");
    try {
      String fileName = inputFile.substring(0, inputFile.lastIndexOf('.'));
      File temp = new File(fileName + ".temp");
      // filter out the '\f' characters in the file
      try (Reader reader = Files.newBufferedReader(Paths.get(inputFile), UTF_8);
          Writer writer = Files.newBufferedWriter(temp.toPath(), UTF_8)) {
        int c;
        while ((c = reader.read()) != -1) {
          if (c != '\f') {
            writer.write(c);
          }
        }
      } catch (IOException e) {
        temp.delete();
        throw new Daikon.UserError(e, "Problem copying " + inputFile + " to " + temp);
      }
      try (FileInputStream fis = new FileInputStream(temp)) {
        @SuppressWarnings("UnusedVariable") // sets static variables for TranslationUnit()
        CParser parser = new CParser(fis);
      } catch (IOException e) {
        throw new UncheckedIOException("problem reading " + temp, e);
      }
      TranslationUnit root = CParser.TranslationUnit();
      StringFinder finder = new StringFinder();
      temp.delete();
      root.accept(finder);

      ConditionPrinter printer;
      try {
        printer = new ConditionPrinter(fileName + ".spinfo");
        printer.setActualStrings(finder.functionStringMapping);
        printer.setStringArrays(finder.stringMatrices);
        root.accept(printer);
        printer.close();
      } catch (IOException e) {
        System.out.println("File IO Error");
        System.out.println(e.getMessage());
      }
      System.out.println("CreateSpinfoC:  C program parsed successfully.");
    } catch (ParseException e) {
      System.out.println("CreateSpinfoC encountered errors during parse.");
      System.out.println(e.getMessage());
    }
  }
}
