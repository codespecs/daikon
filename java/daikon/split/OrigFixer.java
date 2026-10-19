package daikon.split;

import daikon.tools.jtb.Ast;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
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
 * OrigFixer is a visitor for a jtb syntax tree that replaces instances of "orig()" with "orig_".
 * For example, {@code orig(x) < y} would yield {@code orig_x < y}. Names that do not start with a
 * variable name are not affected. For example, {@code orig(java.lang.Math.abs(x))} would yield
 * {@code java.lang.Math.abs(orig_x)}.
 */
class OrigFixer extends DepthFirstVisitor {

  /** True iff presently visiting the args of "orig()". */
  private boolean withinArgList = false;

  private boolean foundOrig = false;

  /** The last NodeToken visited. */
  private @MonotonicNonNull NodeToken lastToken;

  /** The token visited before lastToken. */
  private @MonotonicNonNull NodeToken twoTokensAgo;

  /** The base names of the variables that may appear in the expression. */
  private final Set<String> baseNames;

  /** The tokens visited by this, in order. */
  private final List<NodeToken> tokens = new ArrayList<>();

  /** The indices in {@link #tokens} of the names within "orig()" that may be variable names. */
  private final List<Integer> origNames = new ArrayList<>();

  /**
   * Creates a new instance of OrigFixer.
   *
   * @param baseNames the base names of the variables that may appear in the expression
   */
  private OrigFixer(String[] baseNames) {
    super();
    this.baseNames = new HashSet<>(Arrays.asList(baseNames));
  }

  /**
   * Replaces all instance of "orig(variableName) with "orig_variableName" in expression. In the
   * case of multiple variable names appearing within the argument of "orig()" all variable names
   * are prefixed with "orig_". For example, "orig(x + y &gt; z - 3)" would yield, "orig_x + orig_y
   * &gt; orig_z - 3".
   *
   * @param expression a valid segment of java code in which "orig()" is being replaced
   * @param baseNames the base names of the variables that may appear in the expression
   * @return condition with all instances of "orig()" replaced
   * @throws ParseException if expression is not valid java code
   */
  public static String fixOrig(String expression, String[] baseNames) throws ParseException {
    Node root = Visitors.getJtbTree(expression);
    OrigFixer fixer = new OrigFixer(baseNames);
    root.accept(fixer);
    fixer.fixNames();
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
        origNames.add(tokens.size() - 1);
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
      origNames.add(tokens.size() - 1);
    }
    tokens.add(n);
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

  /** Prefixes "orig_" to each name in {@link #origNames} that starts with a variable name. */
  private void fixNames() {
    for (int i : origNames) {
      if (PrefixFixer.variableNameEnd(tokens, i, "orig_", baseNames) != -1) {
        NodeToken token = tokens.get(i);
        token.tokenImage = "orig_" + token.tokenImage;
      }
    }
  }
}
