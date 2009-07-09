package utilMDE;

import java.io.*;
import java.util.*;
import java.util.regex.*;

/**
 * Class that reads "entries" from a file.  In the simplest case, entries
 * can be lines.  It supports:
 *   include files,
 *   comments, and
 *   multi-line entries (paragraphs).
 * The syntax of each of these is customizable.
 * @see #get_entry() and @see #set_entry_start_stop(String,String)
 *
 * @deprecated use EntryReader instead
 * @see EntryReader
 */
@Deprecated
public class MultiReader extends EntryReader {

  ///
  /// Constructors
  ///

  /**
   * Create a MultiReader
   *
   *    @param reader Initial source
   *    @param comment_re_string Regular expression that matches comments.
   *                      Any text that matches comment_re is removed.
   *                      A line that is entirely a comment is ignored
   *    @param include_re_string Regular expression that matches include directives.
   *                      The expression should define one group that contains
   *                      the include file name
   */
  public MultiReader (BufferedReader reader, String filename, /*@Nullable*/ String comment_re_string,
                      /*@Nullable*/ String include_re_string) {
    super(reader, filename, comment_re_string, include_re_string);
  }

  /** Create a MultiReader that does not support comments or include directives.
   * @see #MultiReader(BufferedReader,String,String,String) **/
  public MultiReader (BufferedReader reader) {
    super(reader);
  }

  /**
   * Create a MultiReader
   *
   *    @param file       Initial file to read.
   *    @param comment_re Regular expression that matches comments.
   *                      Any text that matches comment_re is removed.
   *                      A line that is entirely a comment is ignored.
   *    @param include_re Regular expression that matches include directives.
   *                      The expression should define one group that contains
   *                      the include file name.
   */
  public MultiReader (File file, /*@Nullable*/ String comment_re,
                      /*@Nullable*/ String include_re) throws IOException {
    super(file, comment_re, include_re);
  }

  /** Create a MultiReader that does not support comments or include directives.
   * @see #MultiReader(File,String,String) **/
  public MultiReader (File file) throws IOException {
    super (file);
  }

  /**
   * Create a new MultiReader starting with the specified file.
   * @see #MultiReader(File,String,String)
   */
  public MultiReader (String filename, /*@Nullable*/ String comment_re,
                      /*@Nullable*/ String include_re) throws IOException {
    super(filename, comment_re, include_re);
  }

  /** Create a MultiReader that does not support comments or include directives.
   * @see #MultiReader(String,String,String) **/
  public MultiReader (String filename) throws IOException {
    super(filename);
  }


}
