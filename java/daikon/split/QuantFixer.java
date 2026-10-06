package daikon.split;

import daikon.tools.jtb.Ast;
import java.util.ArrayList;
import java.util.List;
import jtb.ParseException;
import jtb.syntaxtree.ArgumentList;
import jtb.syntaxtree.Arguments;
import jtb.syntaxtree.Expression;
import jtb.syntaxtree.Name;
import jtb.syntaxtree.Node;
import jtb.syntaxtree.NodeSequence;
import jtb.syntaxtree.NodeToken;
import jtb.syntaxtree.PrimaryExpression;
import jtb.syntaxtree.PrimarySuffix;
import jtb.visitor.DepthFirstVisitor;
import org.checkerframework.checker.nullness.qual.Nullable;

/**
 * QuantFixer is a visitor for a jtb syntax tree that replaces calls to the daikon.Quant methods
 * that access arrays, which appear in Daikon's Java output format, by the Java syntax that
 * splitters use for arrays. For example, {@code daikon.Quant.getElement_int(a, i)} yields {@code
 * a[i]}, and {@code daikon.Quant.size(a)} yields {@code a.length}.
 */
class QuantFixer extends DepthFirstVisitor {

  /** Creates a new QuantFixer. */
  private QuantFixer() {
    super();
  }

  /**
   * Replaces calls to daikon.Quant methods that access arrays (see class description).
   *
   * @param expression a valid segment of Java code
   * @return expression, with calls to daikon.Quant array methods replaced
   * @throws ParseException if expression is not valid Java code
   */
  public static String fixQuant(String expression) throws ParseException {
    Node root = Visitors.getJtbTree(expression);
    root.accept(new QuantFixer());
    return Ast.format(root);
  }

  /**
   * This method should not be directly used by users of this class. If n is a call to a
   * daikon.Quant method that accesses an array, it is replaced.
   */
  @Override
  public void visit(PrimaryExpression n) {
    String methodName = quantMethodName(n);
    if (methodName != null) {
      Arguments args = (Arguments) ((PrimarySuffix) n.f1.elementAt(0)).f0.choice;
      List<Expression> argExprs = new ArrayList<>();
      List<NodeToken> commas = new ArrayList<>();
      if (args.f1.present()) {
        ArgumentList argList = (ArgumentList) args.f1.node;
        argExprs.add(argList.f0);
        for (Node commaAndArg : argList.f1.nodes) {
          NodeSequence seq = (NodeSequence) commaAndArg;
          commas.add((NodeToken) seq.elementAt(0));
          argExprs.add((Expression) seq.elementAt(1));
        }
      }
      boolean isGetElement = methodName.startsWith("getElement_") && argExprs.size() == 2;
      boolean isSize = methodName.equals("size") && argExprs.size() == 1;
      if (isGetElement || isSize) {
        for (NodeToken token : TokenExtractor.extractTokens(n.f0)) {
          token.tokenImage = "";
        }
        // Retain the parentheses around the array argument unless it is a primary expression,
        // which binds as tightly as the array access or field access that follows it.
        boolean parenthesize = !isPrimaryExpression(argExprs.get(0));
        String closeArray = parenthesize ? ")" : "";
        if (!parenthesize) {
          args.f0.tokenImage = "";
        }
        if (isGetElement) {
          commas.get(0).tokenImage = closeArray + "[";
          args.f2.tokenImage = "]";
        } else {
          args.f2.tokenImage = closeArray + ".length";
        }
      }
    }
    super.visit(n);
  }

  /**
   * This method should not be directly used by users of this class. Clears the position of n,
   * because changes to token images invalidate the positions.
   */
  @Override
  public void visit(NodeToken n) {
    n.beginColumn = -1;
    n.endColumn = -1;
  }

  /**
   * If n is a call to a method of daikon.Quant, returns the method name. Otherwise, returns null.
   *
   * @param n an expression
   * @return the name of the daikon.Quant method that n calls, or null
   */
  private static @Nullable String quantMethodName(PrimaryExpression n) {
    if (!(n.f0.f0.choice instanceof Name)
        || n.f1.size() == 0
        || !(((PrimarySuffix) n.f1.elementAt(0)).f0.choice instanceof Arguments)) {
      return null;
    }
    Name name = (Name) n.f0.f0.choice;
    if (!name.f0.tokenImage.equals("daikon") || name.f1.size() != 2) {
      return null;
    }
    NodeToken classToken = (NodeToken) ((NodeSequence) name.f1.elementAt(0)).elementAt(1);
    if (!classToken.tokenImage.equals("Quant")) {
      return null;
    }
    return ((NodeToken) ((NodeSequence) name.f1.elementAt(1)).elementAt(1)).tokenImage;
  }

  /**
   * Returns true if the given expression is a primary expression, such as a variable name, a field
   * access, or an array access.
   *
   * @param e an expression
   * @return true iff e is a primary expression
   */
  private static boolean isPrimaryExpression(Expression e) {
    // The first primary expression in e is all of e, if e is a primary expression.
    @Nullable PrimaryExpression[] first = new @Nullable PrimaryExpression[1];
    e.accept(
        new DepthFirstVisitor() {
          @Override
          public void visit(PrimaryExpression n) {
            if (first[0] == null) {
              first[0] = n;
            }
          }
        });
    return first[0] != null
        && TokenExtractor.extractTokens(first[0]).length == TokenExtractor.extractTokens(e).length;
  }
}
