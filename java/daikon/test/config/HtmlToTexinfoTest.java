package daikon.test.config;

import static org.junit.Assert.assertEquals;

import daikon.config.*;
import junit.framework.*;
import org.junit.Test;

public class HtmlToTexinfoTest {

  @Test
  public void testHtmlToTexinfo() {
    testHtmlToTexinfoHelper("some &lt;text&gt; &amp; I like {it}", "some <text> & I like @{it@}");
    testHtmlToTexinfoHelper(
        "The <tt>--conf_limit</tt> command-line option to Daikon",
        "The @code{--conf_limit} command-line option to Daikon");
    testHtmlToTexinfoHelper(
        "The <code>--conf_limit</code> command-line option to Daikon",
        "The @code{--conf_limit} command-line option to Daikon");
  }

  private void testHtmlToTexinfoHelper(String in, String expected) {
    String actual = HtmlToTexinfo.htmlToTexinfo(in);
    assertEquals(expected, actual);
  }

  @Test
  public void testJavadocHtmlToTexinfo() {
    // Same as for testHtmlToTexinfo
    testJavadocHtmlToTexinfoHelper(
        "some &lt;text&gt; &amp; I like {it}", "some <text> & I like @{it@}");
    testJavadocHtmlToTexinfoHelper(
        "The <tt>--conf_limit</tt> command-line option to Daikon",
        "The @code{--conf_limit} command-line option to Daikon");
    testJavadocHtmlToTexinfoHelper(
        "The <code>--conf_limit</code> command-line option to Daikon",
        "The @code{--conf_limit} command-line option to Daikon");

    // Additional test casess
    testJavadocHtmlToTexinfoHelper(
        "Split bi-implications {@code a <==> b} into two separate implications {@code a ==> b} and"
            + " {@code b ==> a})",
        "Split bi-implications @code{a <==> b} into two separate implications @code{a ==> b} and"
            + " @code{b ==> a})");
    testJavadocHtmlToTexinfoHelper(
        "     * {@code {e1, e2, e3, ...} subset of x[]}.",
        "     * @code{@{e1, e2, e3, ...@} subset of x[]}.");
  }

  private void testJavadocHtmlToTexinfoHelper(String in, String expected) {
    String actual = HtmlToTexinfo.javadocHtmlToTexinfo(in);
    assertEquals(expected, actual);
  }
}
