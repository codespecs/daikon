package daikon.split;

import daikon.tools.jtb.Ast;
import java.util.ArrayList;
import java.util.Collections;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Set;
import jtb.ParseException;
import jtb.syntaxtree.EqualityExpression;
import jtb.syntaxtree.Node;
import jtb.syntaxtree.NodeSequence;
import jtb.syntaxtree.NodeToken;
import jtb.visitor.DepthFirstVisitor;

/**
 * NullReplacer is a JTB syntax tree visitor that replaces all instances of "null" with "0" in a
 * given expression. Note: "null" is only referring to the java reserved word "null" not to any
 * instances of the string "null". A "null" that is compared with a call to a daikon.Quant method
 * that returns a reference, such as daikon.Quant.getElement_String, is not replaced.
 */
class NullReplacer extends DepthFirstVisitor {

  private int columnshift = 0;
  private int columnshiftline = -1;

  // column shifting only applies to a single line, then is turned off again.
  // States for the variables:
  // columnshift == 0, columnshiftline == -1:
  //    no column shifting needed
  // columnshift != 0, columnshiftline != -1:
  //    column shifting being needed, applies only to specified line

  /** The null literals that are not replaced, because they are compared with a reference. */
  private final Set<NodeToken> referenceNulls = Collections.newSetFromMap(new IdentityHashMap<>());

  /** Blocks public constructor. */
  private NullReplacer() {
    super();
  }

  /**
   * Replaces all instance of "null" with "0".
   *
   * @param expression a valid java expression
   * @return expression with all instances of null replaced by instances of "0"
   * @throws ParseException if expression is not a valid java expression
   */
  public static String replaceNull(String expression) throws ParseException {
    Node root = Visitors.getJtbTree(expression);
    NullReplacer replacer = new NullReplacer();
    root.accept(replacer);
    return Ast.format(root);
  }

  /**
   * Replaces all instance of "null" with "0" in the JTB syntax tree rooted at root..
   *
   * @param root a JTB syntax tree
   */
  public static void replaceNull(Node root) {
    NullReplacer replacer = new NullReplacer();
    root.accept(replacer);
  }

  /**
   * This method should not be directly used by user of this class; however it must be public to
   * fulfill the visitor interface. Records the null literals in n that are compared with a call to
   * a daikon.Quant method that returns a reference.
   */
  @Override
  public void visit(EqualityExpression n) {
    List<NodeToken[]> operands = new ArrayList<>();
    operands.add(TokenExtractor.extractTokens(n.f0));
    for (Node operatorAndOperand : n.f1.nodes) {
      operands.add(TokenExtractor.extractTokens(((NodeSequence) operatorAndOperand).elementAt(1)));
    }
    for (int i = 0; i + 1 < operands.size(); i++) {
      NodeToken[] left = operands.get(i);
      NodeToken[] right = operands.get(i + 1);
      if (isNullLiteral(left) && isQuantReferenceCall(right)) {
        referenceNulls.add(left[0]);
      } else if (isNullLiteral(right) && isQuantReferenceCall(left)) {
        referenceNulls.add(right[0]);
      }
    }
    super.visit(n);
  }

  /**
   * Returns true if tokens is the null literal.
   *
   * @param tokens the tokens of an expression
   * @return true if tokens is the null literal
   */
  private static boolean isNullLiteral(NodeToken[] tokens) {
    return tokens.length == 1 && Visitors.isNull(tokens[0]);
  }

  /**
   * Returns true if tokens is a call to a daikon.Quant method that returns a reference, such as
   * daikon.Quant.getElement_String(a, i).
   *
   * @param tokens the tokens of an expression
   * @return true if tokens is a call to a daikon.Quant method that returns a reference
   */
  private static boolean isQuantReferenceCall(NodeToken[] tokens) {
    if (tokens.length < 7
        || !tokens[0].tokenImage.equals("daikon")
        || !tokens[1].tokenImage.equals(".")
        || !tokens[2].tokenImage.equals("Quant")
        || !tokens[3].tokenImage.equals(".")
        || !(tokens[4].tokenImage.equals("getElement_String")
            || tokens[4].tokenImage.equals("getElement_Object"))
        || !tokens[5].tokenImage.equals("(")) {
      return false;
    }
    // Check that the parenthesis after the method name matches the last token.
    int depth = 0;
    for (int i = 5; i < tokens.length; i++) {
      if (tokens[i].tokenImage.equals("(")) {
        depth++;
      } else if (tokens[i].tokenImage.equals(")")) {
        depth--;
        if (depth == 0) {
          return i == tokens.length - 1;
        }
      }
    }
    return false;
  }

  /**
   * This method should not be directly used by user of this class; however it must be public to
   * fulfill the visitor interface. If n represents null then it is replaced by "0", unless n is
   * compared with a reference.
   */
  @Override
  public void visit(NodeToken n) {
    if (n.beginLine == columnshiftline) {
      n.beginColumn = n.beginColumn - columnshift;
    } else {
      columnshift = 0;
      columnshiftline = -1;
    }
    if (Visitors.isNull(n) && !referenceNulls.contains(n)) {
      columnshift = columnshift + 3;
      n.tokenImage = "0";
      columnshiftline = n.beginLine;
      n.kind = Visitors.STRING_LITERAL;
    }
    n.endColumn = n.endColumn + columnshift;
  }
}
