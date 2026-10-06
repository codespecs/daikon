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
   * Returns the input file named on the command line. Exits if the command line is bad or requests
   * the usage message.
   *
   * @param args the command-line arguments
   * @return the input file named on the command line
   */
  private static String inputFile(String[] args) {
    try {
      String[] files = DaikonGetopt.nonOptionArgs("daikon.tools.jtb.CreateSpinfoC", args, usage);
      if (files.length != 1) {
        throw new Daikon.UserError(usage);
      }
      return files[0];
    } catch (Daikon.DaikonTerminationException e) {
      Daikon.handleDaikonTerminationException(e);
      throw new Error("unreachable");
    }
  }

  public static void main(String[] args) {
    String inputFile = inputFile(args);
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
        System.out.println(e.getMessage());
        if (temp != null) {
          temp.delete();
        }
        System.exit(1);
        throw new Error("unreachable");
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
