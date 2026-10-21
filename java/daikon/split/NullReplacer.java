package daikon.split;

import daikon.ProglangType;
import daikon.VarInfo;
import daikon.tools.jtb.Ast;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Set;
import jtb.ParseException;
import jtb.syntaxtree.ConditionalExpression;
import jtb.syntaxtree.EqualityExpression;
import jtb.syntaxtree.Node;
import jtb.syntaxtree.NodeSequence;
import jtb.syntaxtree.NodeToken;
import jtb.visitor.DepthFirstVisitor;

/**
 * NullReplacer is a JTB syntax tree visitor that replaces instances of "null" with "0" in a given
 * expression. Note: "null" is only referring to the java reserved word "null" not to any instances
 * of the string "null".
 *
 * <p>A splitter represents some references by their hashcode, which is 0 for null. A "null" is
 * replaced only if it stands for a hashcode: that is, if it is compared, by "==" or "!=", with an
 * expression that the splitter represents as a hashcode, or if it is a branch of a conditional
 * expression ("?:") whose other branch is such an expression. An expression that the splitter
 * represents as a hashcode is a hashcode variable, an element of an array of hashcodes, a call
 * {@code getElement_Object(a, i)} on an array of hashcodes, or a conditional expression whose
 * branches are each such an expression or a replaced "null". Every other "null" is left unchanged:
 * for example, one that is compared with a String, or one that is passed as an argument. For
 * example, in {@code this_next == null && this_name != null}, where {@code this_next} is a hashcode
 * and {@code this_name} is a String, the first "null" is replaced and the second "null" is not.
 *
 * <p>NullReplacer runs after the other fixers, because it recognizes the names that they produce.
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

  /**
   * The expressions that the splitter represents as a hashcode. Each is the compilable name of a
   * hashcode variable, or the compilable name of a hashcode array followed by "[]", which stands
   * for an element of that array.
   */
  private final Set<String> hashcodeExpressions = new HashSet<>();

  /** The null literals that are replaced, because they stand for a hashcode. */
  private final Set<NodeToken> hashcodeNulls = Collections.newSetFromMap(new IdentityHashMap<>());

  /**
   * The conditional expressions that the splitter represents as a hashcode. Each maps the first
   * token of the conditional expression to its last token.
   */
  private final IdentityHashMap<NodeToken, NodeToken> hashcodeConditionals =
      new IdentityHashMap<>();

  /**
   * Creates a new NullReplacer.
   *
   * @param varInfos the VarInfos of the variables that may appear in the expression
   */
  private NullReplacer(VarInfo[] varInfos) {
    super();
    for (VarInfo varInfo : varInfos) {
      String name = SplitterJavaSource.compilableName(varInfo);
      if (varInfo.file_rep_type == ProglangType.HASHCODE) {
        hashcodeExpressions.add(name);
      } else if (varInfo.file_rep_type == ProglangType.HASHCODE_ARRAY) {
        hashcodeExpressions.add(name + "[]");
      }
    }
  }

  /**
   * Replaces instances of "null" with "0" (see class description).
   *
   * @param expression a valid java expression, whose variables have their compilable names
   * @param varInfos the VarInfos of the variables that may appear in the expression
   * @return expression with instances of null replaced by instances of "0"
   * @throws ParseException if expression is not a valid java expression
   */
  public static String replaceNull(String expression, VarInfo[] varInfos) throws ParseException {
    Node root = Visitors.getJtbTree(expression);
    NullReplacer replacer = new NullReplacer(varInfos);
    root.accept(replacer.new HashcodeNullFinder());
    root.accept(replacer);
    return Ast.format(root);
  }

  /**
   * Records the null literals that stand for a hashcode, and the conditional expressions that the
   * splitter represents as a hashcode. It visits the subexpressions of an expression before the
   * expression itself, so that it classifies each conditional expression before the expressions
   * that contain it.
   */
  private class HashcodeNullFinder extends DepthFirstVisitor {

    /**
     * This method should not be directly used by user of this class; however it must be public to
     * fulfill the visitor interface. Records the null literals in n that are compared with an
     * expression that the splitter represents as a hashcode.
     */
    @Override
    public void visit(EqualityExpression n) {
      super.visit(n);
      List<NodeToken[]> operands = new ArrayList<>();
      operands.add(TokenExtractor.extractTokens(n.f0));
      for (Node operatorAndOperand : n.f1.nodes) {
        operands.add(
            TokenExtractor.extractTokens(((NodeSequence) operatorAndOperand).elementAt(1)));
      }
      for (int i = 0; i + 1 < operands.size(); i++) {
        NodeToken[] left = stripParens(operands.get(i));
        NodeToken[] right = stripParens(operands.get(i + 1));
        if (isNullLiteral(left) && isHashcode(right)) {
          hashcodeNulls.add(left[0]);
        } else if (isNullLiteral(right) && isHashcode(left)) {
          hashcodeNulls.add(right[0]);
        }
      }
    }

    /**
     * This method should not be directly used by user of this class; however it must be public to
     * fulfill the visitor interface. If n is a conditional expression with a branch that the
     * splitter represents as a hashcode, and the other branch is too or is null, then records n and
     * its null branch.
     */
    @Override
    public void visit(ConditionalExpression n) {
      super.visit(n);
      NodeSequence branches = (NodeSequence) n.f1.node;
      if (branches == null) {
        return;
      }
      NodeToken[] thenBranch = stripParens(TokenExtractor.extractTokens(branches.elementAt(1)));
      NodeToken[] elseBranch = stripParens(TokenExtractor.extractTokens(branches.elementAt(3)));
      boolean thenIsHashcode = isHashcode(thenBranch);
      boolean elseIsHashcode = isHashcode(elseBranch);
      if (thenIsHashcode && isNullLiteral(elseBranch)) {
        hashcodeNulls.add(elseBranch[0]);
      } else if (elseIsHashcode && isNullLiteral(thenBranch)) {
        hashcodeNulls.add(thenBranch[0]);
      } else if (!(thenIsHashcode && elseIsHashcode)) {
        return;
      }
      NodeToken[] tokens = TokenExtractor.extractTokens(n);
      hashcodeConditionals.put(tokens[0], tokens[tokens.length - 1]);
    }
  }

  /**
   * Returns the tokens of an expression, without the parentheses that enclose the whole expression.
   *
   * @param tokens the tokens of an expression
   * @return tokens without enclosing parentheses
   */
  @SuppressWarnings("nullness:return") // end + 1 <= tokens.length, so the copy is not padded
  private static NodeToken[] stripParens(NodeToken[] tokens) {
    int start = 0;
    int end = tokens.length - 1;
    while (end > start && Visitors.isLParen(tokens[start]) && matchingIndex(tokens, start) == end) {
      start++;
      end--;
    }
    return Arrays.copyOfRange(tokens, start, end + 1);
  }

  /**
   * Returns true if tokens is the null literal.
   *
   * @param tokens the tokens of an expression, without enclosing parentheses
   * @return true if tokens is the null literal
   */
  private static boolean isNullLiteral(NodeToken[] tokens) {
    return tokens.length == 1 && Visitors.isNull(tokens[0]);
  }

  /**
   * Returns true if the splitter represents the expression whose tokens are given as a hashcode:
   * that is, if the expression is a hashcode variable, an element of a hashcode array such as
   * {@code a[i]}, a call {@code getElement_Object(a, i)} on an array of hashcodes, or a conditional
   * expression that HashcodeNullFinder has recorded. No other expression is a hashcode, because a
   * hashcode is a number, whose fields and methods cannot be used.
   *
   * @param tokens the tokens of an expression, without enclosing parentheses
   * @return true if the splitter represents the expression as a hashcode
   */
  @SuppressWarnings("interning:not.interned") // NodeTokens are compared by identity
  private boolean isHashcode(NodeToken[] tokens) {
    int last = tokens.length - 1;
    if (last == 0) {
      return hashcodeExpressions.contains(tokens[0].tokenImage);
    }
    if (last > 0 && hashcodeConditionals.get(tokens[0]) == tokens[last]) {
      return true;
    }
    if (last >= 5
        && tokens[0].tokenImage.equals("getElement_Object")
        && Visitors.isLParen(tokens[1])
        && matchingIndex(tokens, 1) == last
        && tokens[3].tokenImage.equals(",")
        && hashcodeExpressions.contains(tokens[2].tokenImage + "[]")) {
      return true;
    }
    return last >= 3
        && Visitors.isLBracket(tokens[1])
        && matchingIndex(tokens, 1) == last
        && hashcodeExpressions.contains(tokens[0].tokenImage + "[]");
  }

  /**
   * Returns the index of the token that closes the parenthesis or bracket at index open.
   *
   * @param tokens the tokens of an expression
   * @param open the index of a "(" or "[" token in tokens
   * @return the index of the matching ")" or "]" token, or -1 if there is none
   */
  private static int matchingIndex(NodeToken[] tokens, int open) {
    int depth = 0;
    for (int i = open; i < tokens.length; i++) {
      NodeToken token = tokens[i];
      if (Visitors.isLParen(token) || Visitors.isLBracket(token)) {
        depth++;
      } else if (Visitors.isRParen(token) || Visitors.isRBracket(token)) {
        depth--;
        if (depth == 0) {
          return i;
        }
      }
    }
    return -1;
  }

  /**
   * This method should not be directly used by user of this class; however it must be public to
   * fulfill the visitor interface. If n represents a null that stands for a hashcode, then it is
   * replaced by "0".
   */
  @Override
  public void visit(NodeToken n) {
    if (n.beginLine == columnshiftline) {
      n.beginColumn = n.beginColumn - columnshift;
    } else {
      columnshift = 0;
      columnshiftline = -1;
    }
    if (hashcodeNulls.contains(n)) {
      columnshift = columnshift + 3;
      n.tokenImage = "0";
      columnshiftline = n.beginLine;
      n.kind = Visitors.STRING_LITERAL;
    }
    n.endColumn = n.endColumn + columnshift;
  }
}
