import java.io.*;
import java.util.zip.ZipFile;
import java.util.zip.ZipEntry;
import java.util.zip.GZIPInputStream;
import java.util.zip.GZIPOutputStream;
import java.util.Enumeration;
import java.text.DecimalFormat;

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

    DecimalFormat formatter = new DecimalFormat("000");
    for (int i=1; i<=100; i++) {
	  writeDtrace(filename, formatter.format(i), 0, 2+recNum*i/200);
    }
    writeDtrace(filename, "second-half", recNum/2, 2+recNum);
  }
  private static void writeDtrace(String filename, String out_name, int fromRec, int toRec) throws IOException {
	  String out = filename.replace(".dtrace","."+out_name+".dtrace");
	  System.out.println("Writing file "+out);
	  OutputStream output = new FileOutputStream(out);
      boolean isGz = filename.endsWith(".dtrace.gz");
	  if (isGz)
		output = new GZIPOutputStream(output);
	  BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(output));
	  BufferedReader reader = getStream(filename);
      String line;
      while ((line=reader.readLine())!=null) {
		writer.write(line); writer.newLine();
		if (line.trim().equals("DECLARE")) break;
	  }
	  int currRecCount = 0;
      boolean isInDecl = true;
      line=reader.readLine();
      while (true) {
		if ((currRecCount>=fromRec || isInDecl) && currRecCount<=toRec) { writer.write(line);  writer.newLine();}
        boolean wasEmpty = false;
        if (line.trim().equals("")) wasEmpty = true;
        line=reader.readLine();
        if (line==null) break;
        if (wasEmpty) {
            isInDecl = line.trim().equals("DECLARE");
            if (!isInDecl) currRecCount++;
        }
      }
	  reader.close();
	  writer.close();
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
