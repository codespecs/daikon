package daikon.config;

import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.checkerframework.checker.regex.qual.Regex;

/** Supplies a static method htmlToTexinfo that converts HTML to Texinfo format. */
public class HtmlToTexinfo {

  private static final String lineSep = System.lineSeparator();

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
    s = s.replace(lineSep + " ", lineSep);

    s = s.replace("{", "@{");
    s = s.replace("}", "@}");
    s = s.replaceAll("(@p?x?ref)@\\{(.*)@\\}", "$1{$2}");
    s = s.replace("<br>", "@*");
    s = s.replace(lineSep + lineSep + "<p>", lineSep + lineSep);
    s = s.replace("<p>", "@*@*");
    // Sadly, Javadoc prohibits the <samp> tag.  Use <code> instead.
    s = s.replace("<samp>", "@samp{");
    s = s.replace("</samp>", "}");
    s = s.replace("<code>", "@code{");
    s = s.replace("</code>", "}");
    s = s.replace(lineSep + "<pre>" + lineSep, lineSep + "@example" + lineSep);
    s = s.replace("<pre>" + lineSep, lineSep + "@example" + lineSep);
    s = s.replace(lineSep + "<pre>", lineSep + "@example" + lineSep);
    s = s.replace("<pre>", lineSep + "@example" + lineSep);
    s = s.replace(lineSep + "</pre>" + lineSep, lineSep + "@end example" + lineSep);
    s = s.replace("</pre>" + lineSep, lineSep + "@end example" + lineSep);
    s = s.replace(lineSep + "</pre>", lineSep + "@end example" + lineSep);
    s = s.replace("</pre>", lineSep + "@end example" + lineSep);
    // Catch-all for parameters, filenames, etc. for which there is no specific HTML formatting.
    // But Javadoc should use <code>...</code> rather than <tt>.
    s = s.replace("<tt>", "@code{");
    s = s.replace("</tt>", "}");

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

    s = s.replace("&gt;", ">");
    s = s.replace("&ge;", ">=");
    s = s.replace("&lt;", "<");
    s = s.replace("&le;", "<=");
    s = s.replace("&rArr;", "->");

    // &amp must come last
    s = s.replace("&amp;", "&");

    return s;
  }
}
