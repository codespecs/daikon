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
    Assert.assertTrue(s.indexOf("((") == -1, "'((' may not appear, '" + s + "'");
    Assert.assertTrue(s.charAt(0) == '(', "starts with lparen, '" + s + "'");
    Assert.assertTrue(s.charAt(s.length()-1) == ')', "ends with rparen, '" + s + "'");

    int paren = 0;
    char[] cs = s.toCharArray();
    for (int i=0; i < cs.length; i++) {
      char c = cs[i];
      if (c == '(') {
        paren++;
      } else if (c == ')') {
        Assert.assertTrue(paren > 0, "too deep at char " + i + " in '" + s + "'");
        paren--;
      }
    }
    Assert.assertTrue(paren == 0, "unbalanced parens in '" + s + "'");
  }

}
