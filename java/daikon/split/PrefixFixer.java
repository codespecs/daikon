package daikon.split;

import daikon.tools.jtb.Ast;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import jtb.ParseException;
import jtb.syntaxtree.Node;
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

  /** The tokens visited by this, in order. */
  private final List<NodeToken> tokens = new ArrayList<>();

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
    fixer.fixNames();
    return Ast.format(root);
  }

  /**
   * This method should not be directly used by users of this class; however, must be public to
   * fulfill the visitor interface. Records n.
   */
  @Override
  public void visit(NodeToken n) {
    n.beginColumn = -1;
    n.endColumn = -1;
    tokens.add(n);
    super.visit(n);
  }

  /**
   * Converts each dotted name in {@link #tokens} that starts with a variable name. The first token
   * of the dotted name gets the base name as its image, and the other converted tokens get "".
   */
  private void fixNames() {
    int i = 0;
    while (i < tokens.size()) {
      // A dotted name that is preceded by "." is a member of some other expression.
      if (!Visitors.isIdentifier(tokens.get(i)) || (i > 0 && Visitors.isDot(tokens.get(i - 1)))) {
        i++;
        continue;
      }
      // Find the longest prefix of the dotted name that is a base name.
      StringBuilder name = new StringBuilder(tokens.get(i).tokenImage);
      String baseName = null;
      int end = i; // index of the last identifier of baseName
      int j = i;
      // A method name is not part of a variable name.
      while (j + 2 < tokens.size()
          && Visitors.isDot(tokens.get(j + 1))
          && Visitors.isIdentifier(tokens.get(j + 2))
          && !(j + 3 < tokens.size() && Visitors.isLParen(tokens.get(j + 3)))) {
        j += 2;
        name.append('_').append(tokens.get(j).tokenImage);
        if (baseNames.contains(name.toString())) {
          baseName = name.toString();
          end = j;
        }
      }
      if (baseName != null) {
        tokens.get(i).tokenImage = baseName;
        for (int k = i + 1; k <= end; k++) {
          tokens.get(k).tokenImage = "";
        }
      }
      i = j + 1;
    }
  }
}
