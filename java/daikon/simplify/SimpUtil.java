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

    Assert.assertTrue(s != null);
    if (s.indexOf("((") != -1)
      Assert.assertTrue(false, "'((' may not appear, '" + s + "'");
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
        if (paren <= 0)
          Assert.assertTrue(paren > 0,
                            "too deep at char " + i + " in '" + s + "'");
        paren--;
        // This check is only sensible for some callers; it needs a flag.
//         if (paren == 0 && i < cs.length -1)
//           Assert.assertTrue(false, "multiple SEXPs in " + s);
      }
    }
    if (paren != 0)
      Assert.assertTrue(paren == 0, "unbalanced parens in '" + s + "'");
  }

}
