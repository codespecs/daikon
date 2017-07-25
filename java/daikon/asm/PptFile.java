package daikon.asm;

import static java.nio.charset.StandardCharsets.UTF_8;

import java.io.IOException;
import java.io.LineNumberReader;
import java.io.Reader;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * Stores the instructions associated with a generic "ppt file." Such a file consists of records,
 * each record containing information about one program point. Each record begins with a record
 * separator: a line of 20 or more "=" characters. The next line is the name of the ppt, and the
 * remaining lines (until the next record separator) are lines containing the information about the
 * ppt.
 */
public class PptFile {

  public Map<String, List<String>> records = new LinkedHashMap<String, List<String>>();

  public static PptFile getPptFile(String fileName) {
    try {
      Reader fileReader = Files.newBufferedReader(Paths.get(fileName), UTF_8);
      LineNumberReader reader = new LineNumberReader(fileReader);
      return new PptFile(reader);
    } catch (IOException e) {
      System.out.println("I/O error while reading file.");
      throw new RuntimeException(e);
    }
  }

  public PptFile(LineNumberReader reader) throws IOException {

    String line = reader.readLine();
    assert line != null && line.startsWith("====================") : line;
    line = reader.readLine();
    boolean readingPptName = true;
    String name = null;
    List<String> lines = new ArrayList<String>();

    while (line != null) {
      if (readingPptName) {
        // Read ppt name.
        assert line != null;
        assert line.trim().length() != 0;
        assert !line.startsWith("====================") : "line " + reader.getLineNumber();
        name = line;
        //System.out.println("NAME " + name);
        readingPptName = false;
      } else {
        //System.out.println("LINE " + line);
        if (line.startsWith("====================")) {
          // End of record.
          assert name != null : "@AssumeAssertion(nullness): was set on previous loop iteration";
          records.put(name, lines);
          name = null;
          lines = new ArrayList<String>();
          readingPptName = true;
        } else {
          lines.add(line);
        }
      }
      line = reader.readLine();
    }
  }
}
