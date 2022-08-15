package daikon.tools.jtb;

import static java.nio.charset.StandardCharsets.UTF_8;

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

  public static void main(String[] args) {
    if (args.length == 1) {
      System.out.println("Create spinfo file from file " + args[0] + " . . .");
    } else {
      System.out.println("C Parser Version 0.1Alpha:  Usage is one of:");
      System.out.println("         java CreateSpinfoC < inputfile");
      System.out.println("OR");
      System.out.println("         java CreateSpinfoC inputfile");
      return;
    }
    try {
      String fileName = args[0].substring(0, args[0].lastIndexOf("."));
      File temp = new File(fileName + ".temp");
      // filter out the '\f' characters in the file
      try (Reader reader = Files.newBufferedReader(Paths.get(args[0]), UTF_8);
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
