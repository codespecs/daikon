// Global variables

package daikon;

import java.util.*;
import com.oroinc.text.regex.*;

public class Global {

  // Don't permit this class to be instantiated
  private Global() { }

  /// Constants

  // Regular expressions
  public final static PatternCompiler regexp_compiler;
  public final static PatternMatcher regexp_matcher;
  public final static Pattern ws_regexp;

  static {
    regexp_compiler = new Perl5Compiler();
    regexp_matcher = new Perl5Matcher();
    try {
      ws_regexp = regexp_compiler.compile("[ \\t]+");
    } catch (Exception e) {
      throw new Error(e.toString());
    }
  }

  public final static Random random = new Random();


  /// Variables

  // Perhaps I shouldn't have anything in this category (ie, no global
  // variables)?


  /// Debugging

  public final static boolean debugRead = false;
  public final static boolean debugPptTopLevel = false;
  public final static boolean debugDerive = false;
  public final static boolean debugInfer = false;
  public final static boolean debugPptSliceGeneric = false;
  public final static boolean debugPptSplit = false;

  // public final static boolean debugRead = true;
  // public final static boolean debugPptTopLevel = true;
  // public final static boolean debugDerive = true;
  // public final static boolean debugInfer = true;
  // public final static boolean debugPptSliceGeneric = true;
  // public final static boolean debugPptSplit = true;
}
