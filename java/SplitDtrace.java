import java.io.*;
import java.util.zip.ZipFile;
import java.util.zip.ZipEntry;
import java.util.zip.GZIPInputStream;
import java.util.zip.GZIPOutputStream;
import java.util.Enumeration;

/**
 * Date: 29/12/2006
 */
public final class SplitDtrace {
  public static void main(String[] args) throws IOException {
    if (args.length!=1)
      throw new RuntimeException("You must supply one argument which is the filename of the dtrace file");
    String filename = args[0].trim();
    boolean isGz = filename.endsWith(".dtrace.gz");
    if (!filename.endsWith(".dtrace") && !isGz) throw new RuntimeException("Filename must end with .dtrace or .dtrace.gz: filename="+filename);
    BufferedReader reader = getStream(filename);
    String line;
    int declNum = 1;
    int dataNum = 0;
    while ((line=reader.readLine())!=null) {
      if (line.trim().equals("DECLARE")) break;
    }
    while ((line=reader.readLine())!=null) {
      if (line.trim().equals("")) dataNum++;
      if (line.trim().equals("DECLARE")) declNum++;
    }
    reader.close();

    int recNum = dataNum-declNum;
    System.out.println("Number of DECLARE statements: "+declNum+" and number of empty lines: "+dataNum+" thus number of records is: "+(recNum));

    for (int i=1; i<=100; i++) {
      String out = filename.replace(".dtrace","."+i+".dtrace");
      System.out.println("Writing file "+out);
      OutputStream output = new FileOutputStream(out);
      if (isGz)
        output = new GZIPOutputStream(output);
      BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(output));
      reader = getStream(filename);
      while ((line=reader.readLine())!=null) {
        writer.write(line); writer.newLine();
        if (line.trim().equals("DECLARE")) break;
      }
      int currRecCount = 2+recNum*i/100;
      while ((line=reader.readLine())!=null) {
        if (currRecCount>=0) { writer.write(line);  writer.newLine();}
        if (line.trim().equals("")) currRecCount--;
        if (line.trim().equals("DECLARE")) currRecCount++;
      }
      reader.close();
      writer.close();
    }
  }
  public static BufferedReader getStream(String filename) throws IOException {
    InputStream stream;
    if (filename.endsWith(".dtrace.zip")) {
      ZipFile zipfile = new ZipFile(filename);
      Enumeration e = zipfile.entries();
      if (!e.hasMoreElements()) throw new RuntimeException("No entries in the gz");
      ZipEntry entry = (ZipEntry) e.nextElement();
      if (e.hasMoreElements()) throw new RuntimeException("More than one entry in the gz");
      stream = zipfile.getInputStream(entry);
    } else if (filename.endsWith(".dtrace.gz")) {
      stream = new GZIPInputStream(new FileInputStream(filename));
    } else
      stream = new FileInputStream(filename);
    return new BufferedReader(new InputStreamReader(stream, "ISO-8859-1"));
  }
}
