package daikon.split;

import jtb.JavaParser;
import jtb.ParseException;
import java.io.*;
import jtb.syntaxtree.*;
import jtb.visitor.*;

/**
 * This class consists solely of static method which are useful when
 * working with jtb syntax tree visitors.
 */
class Visitors {

  /** The jtb value for an identifier. */
  private static final int IDENTIFIER = 71;

 /** The jtb value for a left bracket. */
  private static final int LBRACKET = 78;

 /** The jtb value for a dot. */
  private static final int DOT = 82;

  /** The jtb syntax tree value for "this". */
  private static final int THIS = 54;

  /** The jtb syntax tree value for left paren.*/
  private static final int LPAREN = 74;

  /** jtb value for null */
  private static final int NULL = 43;

  /** jtb value for a string */
  public static final int STRING_LITERAL = 70;

  /**
   * Returns the root of the JBT syntax tree for expression.
   * @param expression a valid java expression.
   * @throws ParseException if expression is not a valid java expression.
   */
  public static Node getJtbTree(String expression)
    throws ParseException {
    class ExpressionExtractor extends DepthFirstVisitor {
      private Node expressionNode;
      public void visit(VariableInitializer n) {
        expressionNode = n.f0;
      }
    }
    String expressionClass = "class c { bool b = " + expression + "; }";
    Reader input = new StringReader(expressionClass);
    JavaParser parser = new JavaParser(input);
    Node root = parser.CompilationUnit();
    ExpressionExtractor expressionExtractor = new ExpressionExtractor();
    root.accept(expressionExtractor);
    return expressionExtractor.expressionNode;
  }

  /**
   * Returns whether n represents the java reserved word "this".
   */
  public static boolean isThis(NodeToken n) {
    return n.kind == THIS;
  }

  /**
   * Returns whether n represents a left bracket, "[".
   */
  public static boolean isLBracket(NodeToken n) {
    return n.kind == LBRACKET;
  }

  /**
   * Returns whether n represents a dot, ".".
   */
  public static boolean isDot(NodeToken n) {
    return n.kind == DOT;
  }

  /**
   * Returns whether n represents a java identifier.
   */
  public static boolean isIdentifier(NodeToken n) {
    return n.kind == IDENTIFIER;
  }

  /**
   * Returns whether n represents a left parenthesis, "(".
   */
  public static boolean isLParen(NodeToken n) {
    return n.kind == LPAREN;
  }

  /**
   * Returns whether n represents the java reserved word "null".
   */
  public static boolean isNull(NodeToken n) {
    return n.kind == NULL;
  }

}
