package daikon.test.config;

import daikon.config.*;
import junit.framework.*;

public class HtmlToTexinfoTest extends TestCase {

  public static void main(String[] args) {
    daikon.LogHelper.setupLogs(daikon.LogHelper.INFO);
    junit.textui.TestRunner.run(new TestSuite(HtmlToTexinfoTest.class));
  }

  public HtmlToTexinfoTest(String name) {
    super(name);
  }

  public void testHtmlToTexinfo() {
    testHtmlToTexinfo("some &lt;text&gt; &amp; I like {it}", "some <text> & I like @{it@}");
    testHtmlToTexinfo(
        "The <tt>--conf_limit</tt> command-line option to Daikon",
        "The @code{--conf_limit} command-line option to Daikon");
    testHtmlToTexinfo(
        "The <code>--conf_limit</code> command-line option to Daikon",
        "The @code{--conf_limit} command-line option to Daikon");
  }

  private void testHtmlToTexinfo(String in, String expected) {
    String actual = HtmlToTexinfo.htmlToTexinfo(in);
    assertEquals(expected, actual);
  }

  public void testJavadocHtmlToTexinfo() {
    // Same as for testHtmlToTexinfo
    testJavadocHtmlToTexinfo("some &lt;text&gt; &amp; I like {it}", "some <text> & I like @{it@}");
    testJavadocHtmlToTexinfo(
        "The <tt>--conf_limit</tt> command-line option to Daikon",
        "The @code{--conf_limit} command-line option to Daikon");
    testJavadocHtmlToTexinfo(
        "The <code>--conf_limit</code> command-line option to Daikon",
        "The @code{--conf_limit} command-line option to Daikon");

    // Additional test casess
    testJavadocHtmlToTexinfo(
        "Split bi-implications {@code a <==> b} into two separate implications {@code a ==> b} and {@code b ==> a})",
        "Split bi-implications @code{a <==> b} into two separate implications @code{a ==> b} and @code{b ==> a})");
    testJavadocHtmlToTexinfo(
        "     * {@code {e1, e2, e3, ...} subset of x[]}.",
        "     * @code{@{e1, e2, e3, ...@} subset of x[]}.");
  }

  private void testJavadocHtmlToTexinfo(String in, String expected) {
    String actual = HtmlToTexinfo.javadocHtmlToTexinfo(in);
    assertEquals(expected, actual);
  }
}
