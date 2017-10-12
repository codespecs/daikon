import static java.nio.charset.StandardCharsets.UTF_8;

import java.io.*;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.zip.GZIPInputStream;
import java.util.zip.GZIPOutputStream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

/*>>>
import org.checkerframework.dataflow.qual.*;
*/

/** Date: 29/12/2006 */
public final class SplitDtrace {
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
    BufferedReader reader = getStream(filename);
    String line;
    int declNum = 1;
    int recNum = 0;
    ArrayList<String> rec = new ArrayList<String>();
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
    reader.close();

    System.out.println(
        "Number of DECLARE statements: " + declNum + " and number of records is: " + recNum);

    DecimalFormat formatter = new DecimalFormat("000");
    //for (int i = 1; i<=100; i++) writeDtrace(filename, formatter.format(i), 0, 2+recNum*i/200);
    writeDtrace(filename, "second-half", recNum / 2, 2 + recNum);
  }

  private static void writeDtrace(String filename, String out_name, int fromRec, int toRec)
      throws IOException {
    String out = filename.replace(".dtrace", "." + out_name + ".dtrace");
    System.out.println("Writing file " + out);
    OutputStream output = new FileOutputStream(out);
    boolean isGz = filename.endsWith(".dtrace.gz");
    if (isGz) output = new GZIPOutputStream(output);
    BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(output, UTF_8));
    BufferedReader reader = getStream(filename);

    int currRecCount = 0;
    HashSet<Integer> nonceSet = new HashSet<Integer>();
    ArrayList<String> rec = new ArrayList<String>();
    while (true) {
      readRec(reader, rec);
      if (isDeclare(rec)) writer.newLine();
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
            if (!isExit(rec)) throw new RuntimeException("Must be either ENTER or EXIT:" + rec);
            if (!nonceSet.contains(nonce)) shouldWrite = false;
            nonceSet.remove(nonce);
          }
        }
        if (shouldWrite) writeRec(writer, rec);
      }
      if (!isDecl) currRecCount++;
    }
    reader.close();
    writer.close();
  }

  static int getNonce(ArrayList<String> res) {
    for (int i = 0; i < res.size(); i++) {
      if (res.get(i).equals("this_invocation_nonce")) {
        return Integer.parseInt(res.get(i + 1));
      }
    }
    throw new RuntimeException("no nonce: " + res);
  }
  /*@Pure*/
  static boolean isEnter(ArrayList<String> res) {
    return res.get(0).contains(":::ENTER");
  }
  /*@Pure*/
  static boolean isExit(ArrayList<String> res) {
    return res.get(0).contains(":::EXIT");
  }
  /*@Pure*/
  static boolean isDeclare(ArrayList<String> res) {
    return res.get(0).equals("DECLARE");
  }

  static void writeRec(BufferedWriter writer, ArrayList<String> res) throws IOException {
    for (String s : res) {
      writer.write(s);
      writer.newLine();
    }
    writer.newLine();
  }

  @SuppressWarnings(
      "purity") // non-deterministic call to trim is used only for equals() and does not affect result
  /*@Pure*/
  static boolean isEmpty(String l) {
    return l.trim().equals("") || l.startsWith("#");
  }

  static void readRec(BufferedReader reader, ArrayList<String> res) throws IOException {
    res.clear();
    String line;
    while ((line = reader.readLine()) != null) {
      if (!isEmpty(line)) {
        break;
      }
    } //eat white space
    while (line != null) {
      line = line.trim();
      if (isEmpty(line)) {
        break;
      }
      res.add(line.trim());
      line = reader.readLine();
    }
  }

  static BufferedReader getStream(String filename) throws IOException {
    InputStream stream;
    if (filename.endsWith(".dtrace.zip")) {
      ZipFile zipfile = new ZipFile(filename);
      Enumeration<? extends ZipEntry> e = zipfile.entries();
      if (!e.hasMoreElements()) throw new RuntimeException("No entries in the gz");
      ZipEntry entry = e.nextElement();
      if (e.hasMoreElements()) throw new RuntimeException("More than one entry in the gz");
      stream = zipfile.getInputStream(entry);
    } else if (filename.endsWith(".dtrace.gz")) {
      stream = new GZIPInputStream(new FileInputStream(filename));
    } else {
      stream = new FileInputStream(filename);
    }
    return new BufferedReader(new InputStreamReader(stream, "ISO-8859-1"));
  }
}
