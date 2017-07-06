package daikon.config;

import java.util.regex.Matcher;
import java.util.regex.Pattern;
import plume.*;

/*>>>
import org.checkerframework.checker.regex.qual.*;
*/

/** Supplies a static method htmlToTexinfo that converts HTML to Texinfo format. */
public class HtmlToTexinfo {

  private static final String lineSep = System.getProperty("line.separator");

  public static /*@Regex(1)*/ Pattern javadocAtCode;

  static {
    // Javadoc actually permits matched braces.  Expand this in the future when needed.
    javadocAtCode = Pattern.compile("\\{@code ([^{}]*?)\\}");
  }

  /**
   * Converts Javadoc-flavored HTML to Texinfo.
   *
   * <p>In particular, handles extra tags that may occur in Javadoc code.
   */
  public static String javadocHtmlToTexinfo(String s) {

    StringBuilder result = new StringBuilder();
    int pos = 0;
    Matcher m = javadocAtCode.matcher(s);
    while (m.find(pos)) {
      result.append(htmlToTexinfo(s.substring(pos, m.start())));
      result.append("@code{");
      result.append(s.substring(m.start(1), m.end(1)));
      result.append("}");
      pos = m.end();
    }
    result.append(htmlToTexinfo(s.substring(pos, s.length())));
    return result.toString();
  }

  /** Converts HTML to Texinfo. */
  public static String htmlToTexinfo(String s) {

    // Remove leading spaces, which throw off Info.
    s = UtilMDE.replaceString(s, lineSep + " ", lineSep);

    s = UtilMDE.replaceString(s, "{", "@{");
    s = UtilMDE.replaceString(s, "}", "@}");
    s = s.replaceAll("(@p?x?ref)@\\{(.*)@\\}", "$1{$2}");
    s = UtilMDE.replaceString(s, "<br>", "@*");
    s = UtilMDE.replaceString(s, lineSep + lineSep + "<p>", lineSep + lineSep);
    s = UtilMDE.replaceString(s, "<p>", "@*@*");
    // Sadly, Javadoc prohibits the <samp> tag.  Use <tt> instead.
    s = UtilMDE.replaceString(s, "<samp>", "@samp{");
    s = UtilMDE.replaceString(s, "</samp>", "}");
    s = UtilMDE.replaceString(s, "<code>", "@code{");
    s = UtilMDE.replaceString(s, "</code>", "}");
    s = UtilMDE.replaceString(s, lineSep + "<pre>" + lineSep, lineSep + "@example" + lineSep);
    s = UtilMDE.replaceString(s, "<pre>" + lineSep, lineSep + "@example" + lineSep);
    s = UtilMDE.replaceString(s, lineSep + "<pre>", lineSep + "@example" + lineSep);
    s = UtilMDE.replaceString(s, "<pre>", lineSep + "@example" + lineSep);
    s = UtilMDE.replaceString(s, lineSep + "</pre>" + lineSep, lineSep + "@end example" + lineSep);
    s = UtilMDE.replaceString(s, "</pre>" + lineSep, lineSep + "@end example" + lineSep);
    s = UtilMDE.replaceString(s, lineSep + "</pre>", lineSep + "@end example" + lineSep);
    s = UtilMDE.replaceString(s, "</pre>", lineSep + "@end example" + lineSep);
    // Catch-all for parameters, filenames, etc. for which there
    // is no specific HTML formatting.
    s = UtilMDE.replaceString(s, "<tt>", "@code{");
    s = UtilMDE.replaceString(s, "</tt>", "}");

    // Other HTML formatting to handle in the future
    // BLOCKQUOTE, "\n\n",        ""
    // LI,         "\n@item ",    ""
    // UL,         "\n\n@itemize @bullet\n", "\n@end itemize\n"
    // OL,         "\n\n@itemize @bullet\n", "\n@end itemize\n"
    // MENU,       "\n\n@itemize @bullet\n", "\n@end itemize\n"
    // DIR,        "\n\n@itemize @bullet\n", "\n@end itemize\n"
    // H1,         "\n\n@section ",  "\n"
    // H2,         "\n\n@section ",  "\n"
    // H3,         "\n\n@section ",  "\n"
    // H4,         "\n\n@section ",  "\n"
    // A,          "", ""
    // SUP,        "^", ""

    s = UtilMDE.replaceString(s, "&gt;", ">");
    s = UtilMDE.replaceString(s, "&ge;", ">=");
    s = UtilMDE.replaceString(s, "&lt;", "<");
    s = UtilMDE.replaceString(s, "&le;", "<=");
    s = UtilMDE.replaceString(s, "&rArr;", "->");

    // &amp must come last
    s = UtilMDE.replaceString(s, "&amp;", "&");

    return s;
  }
}
