package daikon.simplify;

import utilMDE.Assert;

/**
 * Utility functions for the simplify package.
 **/
public class SimpUtil
{

  public static void assert_well_formed(String s) {
    if (!Assert.enabled) {
      return;
    }

    // Unfortunately, most of the tests here aren't sensible if the
    // string can contain quoted strings (like |))|). To do this
    // right, the paren counter would also have to be smarter about
    // details like the rules for | delimiting strings, and how it can
    // be escaped.

    Assert.assertTrue(s != null);
    // XXX not with strings
//     if (s.indexOf("((") != -1)
//       Assert.assertTrue(false, "'((' may not appear, '" + s + "'");
    if (s.charAt(0) != '(')
      Assert.assertTrue(false, "starts with lparen, '" + s + "'");
    if (s.charAt(s.length()-1) != ')')
      Assert.assertTrue(false, "ends with rparen, '" + s + "'");

    int paren = 0;
    char[] cs = s.toCharArray();
    for (int i=0; i < cs.length; i++) {
      char c = cs[i];
      if (c == '(') {
        paren++;
      } else if (c == ')') {
        // XXX not with strings
//         if (paren <= 0)
//           Assert.assertTrue(paren > 0,
//                             "too deep at char " + i + " in '" + s + "'");
        paren--;
        // This check is only sensible for some callers; it needs a flag.
//         if (paren == 0 && i < cs.length -1)
//           Assert.assertTrue(false, "multiple SEXPs in " + s);
      }
    }
    // XXX not with strings
//     if (paren != 0)
//       Assert.assertTrue(paren == 0, "unbalanced parens in '" + s + "'");
  }

}
