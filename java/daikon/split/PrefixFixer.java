package daikon.split;

import java.util.*;
import java.io.*;
import junit.framework.*;
import jtb.syntaxtree.*;
import jtb.visitor.*;
import daikon.*;
import daikon.tools.jtb.*;
import jtb.JavaParser;
import jtb.ParseException;

/**
 * PrefixFixer is a visitor for a jtb syntax tree that converts
 * prefixes of variable name to part of the variable name.
 * For example "prefix.x" would go to "prefix_x", "y.prefix.x"
 * would go to "y_prefix_x", and "y.x.methodName()" would be go to
 * y_x.methodName(). Prefixes that are java reserved words are not
 * effected. For example "this.x" yields "this.x". Finally, if the suffix
 * is "length", then it is not effected.  For example "x.y.length" yields
 * "x_y.length".
 */
class PrefixFixer extends DepthFirstVisitor {

  /** The string holding the statement with all instances of prefix removed. */
  private String newStatement;

  /** The last token visited by this. */
  private NodeToken lastToken;

  /** The token visited before lastToken. */
  private NodeToken twoTokensAgo;

  /** Whether presently visiting the statement to be changed. */
  private boolean withInStatement = false;

  /** jtb's value for dot.*/
  private static int DOT = 82;

  /** jtb's value for an identifier. */
  private static int IDENTIFIER = 71;

  /**
   * Creates a new instance of PrefixRemover to remove prefix.
   */
  private PrefixFixer() {
    super();
  }

  /**
   * Fixes prefixes located in statement (see class description).
   * @param statement valid segment of java code from which prefix
   *  should be fixed.
   */
  public static String fixPrefix(String statement)
    throws ParseException {
    String statementClass = "class c { bool b = " + statement + "; }";
    Reader input = new StringReader(statementClass);
    JavaParser parser = new JavaParser(input);
    Node root = parser.CompilationUnit();
    PrefixFixer fixer = new PrefixFixer();
    root.accept(fixer);
    return fixer.newStatement;
  }


  /**
   * This method should not be directly used by users of this class.
   * Replaces the token image with "" if it is a prefix  or a "."
   * following a prefix and appends to the tokenImage_ to the  next token.
   */
  public void visit(NodeToken n) {
    if (withInStatement && isMatch(n)) {
      lastToken.tokenImage = "";
      n.tokenImage = twoTokensAgo.tokenImage + "_" + n.tokenImage;
      twoTokensAgo.tokenImage = "";
    }
    n.beginColumn = -1;
    n.endColumn = -1;
    twoTokensAgo = lastToken;
    lastToken = n;
    super.visit(n);
  }

  /**
   * Returns if twoTokensAgo is a prefix to n and n != "length"
   */
  private boolean isMatch(NodeToken n) {
    return (n.kind == IDENTIFIER &&
            lastToken != null &&
            lastToken.kind == DOT &&
            twoTokensAgo.kind == IDENTIFIER &&
            (! n.tokenImage.equals("length")));
  }

  /**
   * This method should not be directly used by users of this class.
   * Sets the newStatement representing the new statement.
   */
  public void visit(VariableInitializer n) {
    withInStatement = true;
    super.visit(n);
    withInStatement = false;
    newStatement = Ast.print(n);
  }

}
