package daikon.split;

import daikon.tools.jtb.Ast;
import jtb.ParseException;
import jtb.syntaxtree.Arguments;
import jtb.syntaxtree.Name;
import jtb.syntaxtree.Node;
import jtb.syntaxtree.NodeToken;
import jtb.syntaxtree.PrimaryExpression;
import jtb.syntaxtree.PrimarySuffix;
import jtb.visitor.DepthFirstVisitor;
import org.checkerframework.checker.nullness.qual.EnsuresNonNullIf;
import org.checkerframework.checker.nullness.qual.MonotonicNonNull;
import org.checkerframework.dataflow.qual.Pure;

/**
 * OrigFixer is a visitor for a jtb syntax tree that replaces instances of of "orig()" with "orig_".
 * For example, {@code orig(x) < y} would yield {@code orig_x < y}.
 */
class OrigFixer extends DepthFirstVisitor {

  /** True iff presently visiting the args of "orig()". */
  private boolean withinArgList = false;

  private boolean foundOrig = false;

  /** The last NodeToken visited. */
  private @MonotonicNonNull NodeToken lastToken;

  /** The token visited before lastToken. */
  private @MonotonicNonNull NodeToken twoTokensAgo;

  /** Blocks public constructor. */
  private OrigFixer() {
    super();
  }

  /**
   * Replaces all instance of "orig(variableName) with "orig_variableName" in expression. In the
   * case of multiple variable names appearing within the argument of "orig()" all variable names
   * are prefixed with "orig_". For example, "orig(x + y &gt; z - 3)" would yield, "orig_x + orig_y
   * &gt; orig_z - 3".
   *
   * @param expression a valid segment of java code in which "orig()" is being replaced
   * @return condition with all instances of "orig()" replaced
   * @throws ParseException if expression is not valid java code
   */
  public static String fixOrig(String expression) throws ParseException {
    Node root = Visitors.getJtbTree(expression);
    OrigFixer fixer = new OrigFixer();
    root.accept(fixer);
    return Ast.format(root);
  }

  /**
   * This method should not be directly used by users of this class. If n is an instance of "orig()"
   * it is replaced.
   */
  @Override
  public void visit(PrimaryExpression n) {
    if (isOrig(n)) {
      NodeToken origToken = ((Name) n.f0.f0.choice).f0;
      origToken.tokenImage = "";
      NodeToken openParen = ((Arguments) ((PrimarySuffix) n.f1.elementAt(0)).f0.choice).f0;
      openParen.tokenImage = "";
      foundOrig = true;
      super.visit(n);

      // handle lastToken
      if (lastToken != null
          && Visitors.isIdentifier(lastToken)
          && (twoTokensAgo == null || !Visitors.isDot(twoTokensAgo))) {
        lastToken.tokenImage = "orig_" + lastToken.tokenImage;
      }
      foundOrig = false;
      NodeToken closeParen = ((Arguments) ((PrimarySuffix) n.f1.elementAt(0)).f0.choice).f2;
      closeParen.tokenImage = "";
    } else {
      super.visit(n);
    }
  }

  /**
   * This method should not be directly used by users of this class. Marks whether this is presently
   * visiting the arguments to an instance of "orig()".
   */
  @Override
  public void visit(Arguments n) {
    if (foundOrig) {
      withinArgList = true;
    } else {
      withinArgList = false;
    }
    super.visit(n);
    withinArgList = false;
  }

  /**
   * Returns true if n is an instance of the method "orig".
   *
   * @param n an expression that might or might not be a call to "orig"
   * @return true iff n is an instance of the method "orig"
   */
  @Pure
  private boolean isOrig(PrimaryExpression n) {
    return ((n.f0.f0.choice instanceof Name)
        && ((Name) n.f0.f0.choice).f0.tokenImage.equals("orig")
        && (n.f1.size() > 0)
        && (n.f1.elementAt(0) instanceof PrimarySuffix)
        && (((PrimarySuffix) n.f1.elementAt(0)).f0.choice instanceof Arguments));
  }

  /**
   * This method should not be directly used by users of this class. Updates n to be prefixed with
   * "orig_" if needed.
   */
  @Override
  public void visit(NodeToken n) {
    n.beginColumn = -1;
    n.endColumn = -1;
    if (withinArgList && isLastTokenVar(n)) {
      lastToken.tokenImage = "orig_" + lastToken.tokenImage;
    }
    if (lastToken != null) { // test is to quiet the Nullness Checker
      twoTokensAgo = lastToken;
    }
    lastToken = n;
  }

  /** Returns if the last token represents a variable name. */
  @EnsuresNonNullIf(result = true, expression = "lastToken")
  @Pure
  private boolean isLastTokenVar(NodeToken n) {
    return (lastToken != null
        && Visitors.isIdentifier(lastToken)
        && (twoTokensAgo == null || !Visitors.isDot(twoTokensAgo))
        && !Visitors.isLParen(n));
  }
}
