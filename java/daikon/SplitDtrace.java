package daikon;

import static java.nio.charset.StandardCharsets.UTF_8;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.List;
import java.util.zip.GZIPInputStream;
import java.util.zip.GZIPOutputStream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import org.checkerframework.checker.mustcall.qual.Owning;
import org.checkerframework.dataflow.qual.Pure;

/**
 * Takes one argument: a .dtrace or dtrace.gz file. Splits it into 100 files: the first file
 * contains the first 1% of the original file, the second contains 1-2%, ... until the last one
 * contains 99-100%.
 */
public final class SplitDtrace {
  /**
   * Entry point for SplitDtrace, which splits a trace file into 100 parts.
   *
   * @param args one argument, the name of the .dtrace or .dtrace.gz file
   */
  public static void main(String[] args) throws IOException {
    if (args.length != 1) {
      throw new RuntimeException(
          "You must supply one argument which is the filename of the dtrace file");
    }
    String filename = args[0].trim();
    boolean isGz = filename.endsWith(".dtrace.gz");
    if (!filename.endsWith(".dtrace") && !isGz) {
      throw new RuntimeException(
          "Filename must end with .dtrace or .dtrace.gz: filename=" + filename);
    }
    int declNum = 1;
    int recNum = 0;
    try (BufferedReader reader = getStream(filename)) {
      ArrayList<String> rec = new ArrayList<>();
      while (true) {
        readRec(reader, rec);
        if (isDeclare(rec)) {
          break;
        }
      }
      while (true) {
        readRec(reader, rec);
        if (rec.size() == 0) {
          break;
        }
        if (isDeclare(rec)) {
          declNum++;
        } else {
          recNum++;
        }
      }
    }

    System.out.println(
        "Number of DECLARE statements: " + declNum + " and number of records is: " + recNum);

    // DecimalFormat formatter = new DecimalFormat("000");
    // for (int i = 1; i<=100; i++) writeDtrace(filename, formatter.format(i), 0, 2+recNum*i/200);
    writeDtrace(filename, "second-half", recNum / 2, 2 + recNum);
  }

  private static void writeDtrace(String filename, String out_name, int fromRec, int toRec)
      throws IOException {
    String out = filename.replace(".dtrace", "." + out_name + ".dtrace");
    System.out.println("Writing file " + out);
    try (FileOutputStream fos = new FileOutputStream(out);
        OutputStream output = filename.endsWith(".dtrace.gz") ? new GZIPOutputStream(fos) : fos;
        BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(output, UTF_8));
        BufferedReader reader = getStream(filename)) {

      int currRecCount = 0;
      HashSet<Integer> nonceSet = new HashSet<>();
      ArrayList<String> rec = new ArrayList<>();
      while (true) {
        readRec(reader, rec);
        if (isDeclare(rec)) {
          writer.newLine();
        }
        writeRec(writer, rec);
        if (isDeclare(rec)) {
          break;
        }
      }
      while (true) {
        readRec(reader, rec);
        if (rec.size() == 0) {
          break;
        }
        boolean isDecl = isDeclare(rec);
        if ((currRecCount >= fromRec || isDecl) && currRecCount <= toRec) {
          boolean shouldWrite = true;
          if (!isDecl) {
            int nonce = getNonce(rec);
            if (isEnter(rec)) {
              nonceSet.add(nonce);
            } else {
              if (!isExit(rec)) {
                throw new RuntimeException("Must be either ENTER or EXIT:" + rec);
              }
              if (!nonceSet.contains(nonce)) {
                shouldWrite = false;
              }
              nonceSet.remove(nonce);
            }
          }
          if (shouldWrite) {
            writeRec(writer, rec);
          }
        }
        if (!isDecl) {
          currRecCount++;
        }
      }
    }
  }

  /**
   * Returns the value of the nonce variable in the ppt.
   *
   * @param res a list of ppt records
   * @return the value of the nonce variable
   */
  static int getNonce(List<String> res) {
    for (int i = 0; i < res.size(); i++) {
      if (res.get(i).equals("this_invocation_nonce")) {
        return Integer.parseInt(res.get(i + 1));
      }
    }
    throw new RuntimeException("no nonce: " + res);
  }

  /**
   * Returns true if the given string is an entry program point name.
   *
   * @param res a program point name
   * @return true if the argument is an entry program point name
   */
  @Pure
  static boolean isEnter(List<String> res) {
    return res.get(0).contains(":::ENTER");
  }

  /**
   * Returns true if the given string is an exit program point name.
   *
   * @param res a program point name
   * @return true if the argument is an exit program point name
   */
  @Pure
  static boolean isExit(List<String> res) {
    return res.get(0).contains(":::EXIT");
  }

  /**
   * Returns true if the given line starts a program point declaration
   *
   * @param res a line from a .decls or .dtrace file
   * @return true if the given line starts a program point declaration
   */
  @Pure
  static boolean isDeclare(List<String> res) {
    return res.get(0).equals("DECLARE");
  }

  /**
   * Writes a .dtrace record to a file.
   *
   * @param writer the BufferedWriter to use for output
   * @param res the lines of a .dtrace record
   * @throws IOException if there is a problem writing
   */
  static void writeRec(BufferedWriter writer, List<String> res) throws IOException {
    for (String s : res) {
      writer.write(s);
      writer.newLine();
    }
    writer.newLine();
  }

  @SuppressWarnings(
      "all:purity") // non-deterministic call to trim is used only for equals(), does not affect
  // result
  @Pure
  static boolean isEmpty(String l) {
    return l.trim().equals("") || l.startsWith("#");
  }

  /**
   * Reads a record from a .dtrace file.
   *
   * <p>This method modifies a list argument rather than returning a new list, for efficiency.
   *
   * @param reader the BufferedReader to use for input
   * @param res a list that will be filled with the lines of a .dtrace record
   * @throws IOException if there is a problem reading
   */
  static void readRec(BufferedReader reader, List<String> res) throws IOException {
    res.clear();
    String line;
    while ((line = reader.readLine()) != null) {
      if (!isEmpty(line)) {
        break;
      }
    } // eat white space
    while (line != null) {
      line = line.trim();
      if (isEmpty(line)) {
        break;
      }
      res.add(line.trim());
      line = reader.readLine();
    }
  }

  @SuppressWarnings({
    "JdkObsolete", // ZipFile uses Enumeration
    "builder:required.method.not.called" // @MustCall flows through an enumeration
  })
  static @Owning BufferedReader getStream(String filename) throws IOException {
    InputStream stream = null; // dummy initialization for compiler's definite assignment check
    ZipFile zipfile = null; // declare outside try so that it can be closed if an exception occurs
    try {
      if (filename.endsWith(".dtrace.zip")) {
        zipfile = new ZipFile(filename);
        Enumeration<? extends ZipEntry> e = zipfile.entries();
        if (!e.hasMoreElements()) {
          throw new RuntimeException("No entries in the gz");
        }
        ZipEntry entry = e.nextElement();
        if (e.hasMoreElements()) {
          throw new RuntimeException("More than one entry in the gz");
        }
        stream = zipfile.getInputStream(entry);
        assert stream != null : "@AssumeAssertion(nullness): just tested that one entry exists";
      } else {
        stream = new FileInputStream(filename);
        if (filename.endsWith(".dtrace.gz")) {
          stream = new GZIPInputStream(stream);
        }
      }
    } catch (IOException e) {
      if (zipfile != null) {
        try {
          zipfile.close();
        } catch (IOException e2) {
          // do nothing
        }
      }
      if (stream != null) {
        try {
          stream.close();
        } catch (IOException e2) {
          // do nothing
        }
      }
      throw e;
    }
    return new BufferedReader(new InputStreamReader(stream, "ISO-8859-1"));
  }
}
