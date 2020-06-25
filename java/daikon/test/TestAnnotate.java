package daikon.test;

import static org.junit.Assert.assertEquals;

import daikon.tools.jtb.*;
import junit.framework.*;
import org.junit.Test;

/** Tests that Annotate respects tabs. */
public final class TestAnnotate {

  @Test
  public void testGetTabbedIndex() {

    String[] tabbed =
        new String[] {
          "private int[]\telems;",
          "\tprivate\tint\tnumberOfElements\t;",
          "\t\tprivate\t\tint\t\tmax;",
          "   \tpublic   \tuniqueBoundedStack()   \t{",
          "   \t   \tnumberOfElements\t= 0;",
          "\tmax = 2;\t\t",
          "\telems\t=\tnew int[max]\t;",
          "\tpublic void push(int k) {",
          "\t       int index;",
          "\t       for (index = 0; index<numberOfElements; index++) {",
          "                        if (k==elems[index]) {"
        };

    String[] untabbed =
        new String[] {
          "private int[]   elems;",
          "        private int     numberOfElements        ;",
          "                private         int             max;",
          "        public          uniqueBoundedStack()    {",
          "                numberOfElements        = 0;",
          "        max = 2;",
          "        elems   =       new int[max]    ;",
          "        public void push(int k) {",
          "               int index;",
          "               for (index = 0; index<numberOfElements; index++) {",
          "                        if (k==elems[index]) {"
        };

    for (int i = 0; i < tabbed.length; i++) {

      String tabbedString = tabbed[i];
      String untabbedString = untabbed[i];
      for (int j = 0; j < untabbedString.length(); j++) {
        char untabbedChar = untabbedString.charAt(j);
        if (untabbedChar != ' ') {
          char tabbedChar = tabbedString.charAt(AnnotateVisitor.getTabbedIndex(j, tabbedString));
          assertEquals(untabbedChar, tabbedChar);
        }
      }
    }
  }
}
