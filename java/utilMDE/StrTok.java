package utilMDE;

import java.util.*;
import java.io.*;

/**
 * Provides a somewhat simpler interface for tokenizing strings than
 * does StreamTokenizer.  All tokenizing is done by StreamTokenizer
 */
public class StrTok {

  Reader reader;
  public StreamTokenizer stok;
  Error err = new Error();

  public StrTok (String s) {

    reader = new StringReader (s);
    stok = new StreamTokenizer (reader);
    stok.wordChars ('_', '_');
  }

  /**
   * Class for error handling.  Users can provide a different implementation
   * to have errors handled differently.
   */
  public static class Error {

    public void tok_error (String s) {
      throw new RuntimeException ("StrTok error: " + s);
    }
  }

  /**
   * Returns the next token as a string.  EOF returns a null, EOL
   * returns an empty string.  Delimiters are returned as one character
   * strings.  Quoted strings and words are returns as strings.
   */
  public String nextToken() {

    // Get the next token.  Turn IO exceptions into runtime exceptions
    // so that callers don't have to catch them.
    int ttype;
    try {
      ttype = stok.nextToken();
    } catch (Exception e) {
      throw new RuntimeException ("StreamTokenizer exception: " + e);
    }

    return (token());
  }

  public String token() {

    int ttype = stok.ttype;

    // Null indicates eof
    if (ttype == StreamTokenizer.TT_EOF)
      return (null);

    // Return end of line as an empty string
    if (ttype == StreamTokenizer.TT_EOL)
      return ("");

    // Return identifiers (words) and quoted strings.  Quoted strings
    // include their quote characters (for recognition)
    if (stok.sval != null) {
      if (ttype > 0) {
        String s = ((char) ttype) + stok.sval + ((char) ttype);
        return (s.intern());
      }
      return (stok.sval.intern());
    }

    // Other tokens are delimiters
    if (ttype > 0) {
      String s = "" + (char)ttype;
      return (s.intern());
    }

    throw new RuntimeException ("Unexpected return " + ttype +
                                " from StreamTokenizer");
  }

  public void commentChar (int ch) {
    stok.commentChar (ch);
  }

  public void quoteChar (int ch) {
    stok.quoteChar (ch);
  }

  public int ttype() {
    return stok.ttype;
  }

  public boolean word() {
    return (stok.ttype == StreamTokenizer.TT_WORD);
  }

  public boolean qstring() {
    return ((stok.sval != null) && (stok.ttype > 0));
  }

  public void set_error_handler (Error err) {
    this.err = err;
  }

  public void need (String tok) {

    String t = nextToken();
    if (tok.equals(t))
      return;

    err.tok_error (Fmt.spf ("Token %s found where %s expected", t, tok));
  }

}
