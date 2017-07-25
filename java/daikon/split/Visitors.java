package daikon.split;

import java.io.*;
import jtb.JavaParser;
import jtb.JavaParserConstants;
import jtb.ParseException;
import jtb.syntaxtree.*;
import jtb.visitor.*;

/*>>>
import org.checkerframework.checker.nullness.qual.*;
import org.checkerframework.dataflow.qual.*;
*/

/**
 * This class consists solely of static methods that are useful when working with jtb syntax tree
 * visitors.
 */
class Visitors implements JavaParserConstants {
  private Visitors() {
    throw new Error("do not instantiate");
  }

  /**
   * Returns the root of the JBT syntax tree for expression.
   *
   * @param expression a valid java expression
   * @throws ParseException if expression is not a valid java expression
   */
  public static Node getJtbTree(String expression) throws ParseException {
    class ExpressionExtractor extends DepthFirstVisitor {
      private /*@Nullable*/ Node expressionNode;

      @Override
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
    assert expressionExtractor.expressionNode != null
        : "@AssumeAssertion(nullness): control flow: visitor pattern";
    return expressionExtractor.expressionNode;
  }

  /** Returns whether n represents the java reserved word "this". */
  /*@Pure*/
  public static boolean isThis(NodeToken n) {
    return n.kind == THIS;
  }

  /** Returns whether n represents a left bracket, "[". */
  /*@Pure*/
  public static boolean isLBracket(NodeToken n) {
    return n.kind == LBRACKET;
  }

  /** Returns whether n represents a dot, ".". */
  /*@Pure*/
  public static boolean isDot(NodeToken n) {
    return n.kind == DOT;
  }

  /** Returns whether n represents a java identifier. */
  /*@Pure*/
  public static boolean isIdentifier(NodeToken n) {
    return n.kind == IDENTIFIER;
  }

  /** Returns whether n represents a left parenthesis, "(". */
  /*@Pure*/
  public static boolean isLParen(NodeToken n) {
    return n.kind == LPAREN;
  }

  /** Returns whether n represents the java reserved word "null". */
  /*@Pure*/
  public static boolean isNull(NodeToken n) {
    return n.kind == NULL;
  }
}
