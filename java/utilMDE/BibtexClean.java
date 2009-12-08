package utilMDE;

import java.io.*;
import java.util.*;
import java.util.regex.*;
import java.nio.CharBuffer;

/**
 * Clean a BibTeX file.
 * Remove each non-empty line that is not in a BibTeX entry, except retain
 * any line that starts with "%".

 * I can't use EntryReader because the @ line does not necessarily follow a
 * blank line.

 Problem:  comment lines before the @.
 Problem:  @string, which can be multiline.


Operate by paragraphs.  A paragraph starts at "\n\n" or "\n@" or after
  ^[ \t]*[\)\}]
If the paragraph starts with @, retain it.
Otherwise, retain each line that starts with %.


Operate line-by-line.  Suppress non-blank lines, but retain thoste  that do not star


for each line:
  if ^@, read to end of entry, outputting.  If you find a blank line first, issue a warning.
  if ^%, output
  if blank, output
  skip
This is a hack, but it works pretty well.

*/

/**
 * Run in the location where you want the cleaned files.  Arguments are the
 * names of the original files, which should be in another directory to
 * avoid being overwritten.
 */
public class BibtexClean {

  private static Pattern entry_end = Pattern.compile("^[ \t]*(?i)(year[ \t]*=[ \t]*[12][0-9][0-9][0-9][ \t]*)?[)}]");
  private static Pattern string = Pattern.compile("^@(?i)string(\\{.*\\}|\\(.*\\))$");

  public static void main(String[] args) {
    for (String filename : args) {
      File in = new File(filename);
      PrintWriter out;
      try {
        out = new PrintWriter(UtilMDE.bufferedFileWriter(in.getName())); // in current directory
      } catch (IOException e) {
        System.err.println("Unable to write " + in.getName());
        System.exit(2);
        throw new Error("This can't happen"); // for definite assignment check
      }
      EntryReader er;
      try {
        er = new EntryReader(filename);
      } catch (IOException e) {
        System.err.println("Unable to read " + in);
        System.exit(2);
        throw new Error("This can't happen"); // for definite assignment check
      }
      for (String line : er) {
        if (line.equals("") || line.startsWith("%")) {
          out.println(line);
        } else if (line.startsWith("@")) {
          if (string.matcher(line).matches()) {
            out.println(line);
          } else {
            out.println(line);
            while (er.hasNext() && ((line = er.next()) != null)) {
              out.println(line);
              if (entry_end.matcher(line).lookingAt()) {
                break;
              } else if (line.equals("")) {
                System.err.printf("%s:%d: unterminated entry%n",
                                  er.getFileName(), er.getLineNumber());
                break;
              }
            }
          }
        }
      }
      out.close();
    }
  }

}




// This uses regular expressions rather than a BibTeX parser, because
// BibTeX parsers generally do not preserve formatting, such as
// indentation, delimiter characters, and order of fields.
