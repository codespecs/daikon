package daikon.config;

import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.checkerframework.checker.regex.qual.Regex;
import org.plumelib.util.UtilPlume;

/** Supplies a static method htmlToTexinfo that converts HTML to Texinfo format. */
public class HtmlToTexinfo {

  private static final String lineSep = System.getProperty("line.separator");

  public static @Regex(1) Pattern javadocAtCode;

  static {
    // Javadoc actually permits matched braces.  Expand this in the future when needed.
    // javadocAtCode = Pattern.compile("\\{@code ([^{}]*?)\\}");
    javadocAtCode = Pattern.compile("\\{@code[ \n]+([^{}]*?(\\{[^{}]*?\\}[^{}]*?)?)\\}");
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
      String codeText = s.substring(m.start(1), m.end(1));
      String codeTextQuoted = codeText.replace("{", "@{").replace("}", "@}");
      result.append(codeTextQuoted);
      result.append("}");
      pos = m.end();
    }
    result.append(htmlToTexinfo(s.substring(pos, s.length())));
    return result.toString();
  }

  /** Converts HTML to Texinfo. */
  public static String htmlToTexinfo(String s) {

    // Remove leading spaces, which throw off Info.
    s = UtilPlume.replaceString(s, lineSep + " ", lineSep);

    s = UtilPlume.replaceString(s, "{", "@{");
    s = UtilPlume.replaceString(s, "}", "@}");
    s = s.replaceAll("(@p?x?ref)@\\{(.*)@\\}", "$1{$2}");
    s = UtilPlume.replaceString(s, "<br>", "@*");
    s = UtilPlume.replaceString(s, lineSep + lineSep + "<p>", lineSep + lineSep);
    s = UtilPlume.replaceString(s, "<p>", "@*@*");
    // Sadly, Javadoc prohibits the <samp> tag.  Use <tt> instead.
    s = UtilPlume.replaceString(s, "<samp>", "@samp{");
    s = UtilPlume.replaceString(s, "</samp>", "}");
    s = UtilPlume.replaceString(s, "<code>", "@code{");
    s = UtilPlume.replaceString(s, "</code>", "}");
    s = UtilPlume.replaceString(s, lineSep + "<pre>" + lineSep, lineSep + "@example" + lineSep);
    s = UtilPlume.replaceString(s, "<pre>" + lineSep, lineSep + "@example" + lineSep);
    s = UtilPlume.replaceString(s, lineSep + "<pre>", lineSep + "@example" + lineSep);
    s = UtilPlume.replaceString(s, "<pre>", lineSep + "@example" + lineSep);
    s =
        UtilPlume.replaceString(
            s, lineSep + "</pre>" + lineSep, lineSep + "@end example" + lineSep);
    s = UtilPlume.replaceString(s, "</pre>" + lineSep, lineSep + "@end example" + lineSep);
    s = UtilPlume.replaceString(s, lineSep + "</pre>", lineSep + "@end example" + lineSep);
    s = UtilPlume.replaceString(s, "</pre>", lineSep + "@end example" + lineSep);
    // Catch-all for parameters, filenames, etc. for which there
    // is no specific HTML formatting.
    s = UtilPlume.replaceString(s, "<tt>", "@code{");
    s = UtilPlume.replaceString(s, "</tt>", "}");

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

    s = UtilPlume.replaceString(s, "&gt;", ">");
    s = UtilPlume.replaceString(s, "&ge;", ">=");
    s = UtilPlume.replaceString(s, "&lt;", "<");
    s = UtilPlume.replaceString(s, "&le;", "<=");
    s = UtilPlume.replaceString(s, "&rArr;", "->");

    // &amp must come last
    s = UtilPlume.replaceString(s, "&amp;", "&");

    return s;
  }
}
