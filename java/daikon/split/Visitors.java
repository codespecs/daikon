package daikon.split;

import java.io.Reader;
import java.io.StringReader;
import java.util.Set;
import jtb.JavaParser;
import jtb.JavaParserConstants;
import jtb.ParseException;
import jtb.syntaxtree.Arguments;
import jtb.syntaxtree.Name;
import jtb.syntaxtree.Node;
import jtb.syntaxtree.NodeSequence;
import jtb.syntaxtree.NodeToken;
import jtb.syntaxtree.PrimaryExpression;
import jtb.syntaxtree.PrimaryPrefix;
import jtb.syntaxtree.PrimarySuffix;
import jtb.syntaxtree.VariableInitializer;
import jtb.visitor.DepthFirstVisitor;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.checkerframework.dataflow.qual.Pure;

/**
 * This class consists solely of static methods that are useful when working with jtb syntax tree
 * visitors.
 */
class Visitors implements JavaParserConstants {
  private Visitors() {
    throw new UnsupportedOperationException("do not instantiate");
  }

  /**
   * Returns the root of the JBT syntax tree for expression.
   *
   * @param expression a valid java expression
   * @throws ParseException if expression is not a valid java expression
   */
  public static Node getJtbTree(String expression) throws ParseException {
    class ExpressionExtractor extends DepthFirstVisitor {
      private @Nullable Node expressionNode;

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

  /** Returns true if n represents the java reserved word "this". */
  @Pure
  public static boolean isThis(NodeToken n) {
    return n.kind == THIS;
  }

  /** Returns true if n represents a left bracket, "[". */
  @Pure
  public static boolean isLBracket(NodeToken n) {
    return n.kind == LBRACKET;
  }

  /** Returns true if n represents a dot, ".". */
  @Pure
  public static boolean isDot(NodeToken n) {
    return n.kind == DOT;
  }

  /** Returns true if n represents a java identifier. */
  @Pure
  public static boolean isIdentifier(NodeToken n) {
    return n.kind == IDENTIFIER;
  }

  /** Returns true if n represents a left parenthesis, "(". */
  @Pure
  public static boolean isLParen(NodeToken n) {
    return n.kind == LPAREN;
  }

  /** Returns true if n represents the java reserved word "null". */
  @Pure
  public static boolean isNull(NodeToken n) {
    return n.kind == NULL;
  }

  /**
   * Returns the i-th identifier of n.
   *
   * @param n a name, such as "a.b.c"
   * @param i the index of an identifier of n; 0 for the first one
   * @return the i-th identifier of n
   */
  public static NodeToken identifier(Name n, int i) {
    return i == 0 ? n.f0 : (NodeToken) ((NodeSequence) n.f1.elementAt(i - 1)).elementAt(1);
  }

  /**
   * Returns true if n is the name of a called method, such as "a.b.m" in "a.b.m(x)".
   *
   * @param n a name
   * @return true if n is the name of a called method
   */
  @Pure
  public static boolean isMethodName(Name n) {
    Node prefix = n.getParent() == null ? null : n.getParent().getParent();
    if (!(prefix instanceof PrimaryPrefix)) {
      return false;
    }
    PrimaryExpression expression = (PrimaryExpression) prefix.getParent();
    return expression.f1.present()
        && ((PrimarySuffix) expression.f1.elementAt(0)).f0.choice instanceof Arguments;
  }

  /**
   * Returns the number of leading identifiers of n that form a variable name. That is the largest k
   * such that, for some element of prefixes, the prefix followed by the first k identifiers of n
   * separated by "_" is in baseNames. The name of a called method is not part of a variable name.
   *
   * @param n a name, such as "a.b.c"
   * @param baseNames the base names of the variables that may appear in the expression
   * @param prefixes the prefixes of base names to try, such as "orig_" or ""
   * @return the number of leading identifiers of n that form a variable name, or 0 if n does not
   *     start with a variable name
   */
  public static int variableNameLength(Name n, Set<String> baseNames, String... prefixes) {
    int numIdentifiers = n.f1.size() + 1;
    int maxLength = isMethodName(n) ? numIdentifiers - 1 : numIdentifiers;
    int result = 0;
    StringBuilder name = new StringBuilder();
    for (int k = 1; k <= maxLength; k++) {
      if (k > 1) {
        name.append('_');
      }
      name.append(identifier(n, k - 1).tokenImage);
      for (String prefix : prefixes) {
        if (baseNames.contains(prefix + name)) {
          result = k;
        }
      }
    }
    return result;
  }
}
