package daikon.split;

import daikon.tools.jtb.Ast;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;
import jtb.ParseException;
import jtb.syntaxtree.Name;
import jtb.syntaxtree.Node;
import jtb.syntaxtree.NodeSequence;
import jtb.syntaxtree.NodeToken;
import jtb.visitor.DepthFirstVisitor;

/**
 * PrefixFixer is a visitor for a jtb syntax tree that converts a dotted name that starts with a
 * variable name to the base name of that variable. For example, if "prefix_x" is a base name, then
 * "prefix.x" goes to "prefix_x"; if "y_prefix_x" is a base name, then "y.prefix.x" goes to
 * "y_prefix_x". The longest prefix of the dotted name that is a base name is converted, and the
 * rest is not affected. For example, if "y_x" is a base name but "y_x_methodName" is not, then
 * "y.x.methodName()" goes to "y_x.methodName()", and "y.x.length" goes to "y_x.length". Names that
 * do not start with a variable name are not affected. For example, "java.lang.Math.abs(x.y)" goes
 * to "java.lang.Math.abs(x_y)".
 */
class PrefixFixer extends DepthFirstVisitor {

  /** The base names of the variables that may appear in the expression. */
  private final Set<String> baseNames;

  /**
   * Creates a new instance of PrefixFixer to fix "." prefixes.
   *
   * @param baseNames the base names of the variables that may appear in the expression
   */
  private PrefixFixer(String[] baseNames) {
    super();
    this.baseNames = new HashSet<>(Arrays.asList(baseNames));
  }

  /**
   * Fixes prefixes located in statement (see class description).
   *
   * @param expression valid segment of java code from which prefix should be fixed
   * @param baseNames the base names of the variables that may appear in the expression
   */
  public static String fixPrefix(String expression, String[] baseNames) throws ParseException {
    Node root = Visitors.getJtbTree(expression);
    PrefixFixer fixer = new PrefixFixer(baseNames);
    root.accept(fixer);
    return Ast.format(root);
  }

  /**
   * This method should not be directly used by users of this class; however, must be public to
   * fulfill the visitor interface. If n starts with a variable name, converts that part of n to the
   * variable's base name: the first identifier gets the base name as its image, and the other
   * converted tokens get "".
   */
  @Override
  public void visit(Name n) {
    int length = Visitors.variableNameLength(n, baseNames, "");
    for (int k = 1; k < length; k++) {
      NodeToken dot = (NodeToken) ((NodeSequence) n.f1.elementAt(k - 1)).elementAt(0);
      NodeToken identifier = Visitors.identifier(n, k);
      n.f0.tokenImage = n.f0.tokenImage + "_" + identifier.tokenImage;
      dot.tokenImage = "";
      identifier.tokenImage = "";
    }
    super.visit(n);
  }

  /**
   * This method should not be directly used by users of this class; however, must be public to
   * fulfill the visitor interface. Resets the columns of n, so that {@link Ast#format} does not use
   * the original layout.
   */
  @Override
  public void visit(NodeToken n) {
    n.beginColumn = -1;
    n.endColumn = -1;
    super.visit(n);
  }
}
