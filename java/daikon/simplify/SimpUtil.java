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
    
    Assert.assert(s != null);
    Assert.assert(s.indexOf("((") == -1, "'((' may not appear");
    Assert.assert(s.charAt(0) == '(', "starts with lparen");
    Assert.assert(s.charAt(s.length()-1) == ')', "ends with rparen");

    int paren = 0;
    char[] cs = s.toCharArray();
    for (int i=0; i < cs.length; i++) {
      char c = cs[i];
      if (c == '(') {
	paren++;
      } else if (c == ')') {
	Assert.assert(paren > 0, "too deep at char " + i + " in '" + s + "'");
	paren--;
      }
    }
    Assert.assert(paren == 0, "unbalanced parens in '" + s + "'");
  }

}
