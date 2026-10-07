package daikon.split;

import daikon.tools.jtb.Ast;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.IdentityHashMap;
import java.util.Set;
import jtb.ParseException;
import jtb.syntaxtree.Name;
import jtb.syntaxtree.Node;
import jtb.syntaxtree.NodeToken;
import jtb.visitor.DepthFirstVisitor;
import org.checkerframework.checker.nullness.qual.EnsuresNonNullIf;
import org.checkerframework.checker.nullness.qual.MonotonicNonNull;
import org.checkerframework.dataflow.qual.Pure;

/**
 * PrefixFixer is a visitor for a jtb syntax tree that converts prefixes of variable name to part of
 * the variable name. For example "prefix.x" would go to "prefix_x", "y.prefix.x" would go to
 * "y_prefix_x", and "y.x.methodName()" would be go to y_x.methodName(). Prefixes that are java
 * reserved words are not affected. For example "this.x" yields "this.x". Finally, if the suffix is
 * "length", then it is not affected. For example "x.y.length" yields "x_y.length". Names that do
 * not start with a variable name are not affected. For example, "java.lang.Math.abs(x.y)" yields
 * "java.lang.Math.abs(x_y)".
 */
class PrefixFixer extends DepthFirstVisitor {

  /** The last token visited by this. */
  private @MonotonicNonNull NodeToken lastToken;

  /** The token visited before lastToken. */
  private @MonotonicNonNull NodeToken twoTokensAgo;

  /** The token visited before twoTokensAgo. */
  private @MonotonicNonNull NodeToken threeTokensAgo;

  /**
   * The identifiers that may start a variable name: each base name of a variable that may appear in
   * the expression, and each prefix of such a base name that is followed by "_".
   */
  private final Set<String> variableNamePrefixes = new HashSet<>();

  /** The tokens of names that do not start with a variable name, such as "java.lang.Math.abs". */
  private final Set<NodeToken> nonVariableNameTokens =
      Collections.newSetFromMap(new IdentityHashMap<>());

  /**
   * Creates a new instance of PrefixFixer to fix "." prefixes.
   *
   * @param baseNames the base names of the variables that may appear in the expression
   */
  private PrefixFixer(String[] baseNames) {
    super();
    for (String baseName : baseNames) {
      variableNamePrefixes.add(baseName);
      for (int i = baseName.indexOf('_'); i != -1; i = baseName.indexOf('_', i + 1)) {
        variableNamePrefixes.add(baseName.substring(0, i));
      }
    }
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
    fixer.fixLastToken();
    return Ast.format(root);
  }

  /**
   * This method should not be directly used by users of this class; however, must be public to
   * fulfill the visitor interface. Replaces the token image with "" if it is a prefix or a "."
   * following a prefix. Appends to the tokenImage and "_" to the next token's image.
   */
  @Override
  public void visit(NodeToken n) {
    if (isMatch(n)) {
      twoTokensAgo.tokenImage = "";
      lastToken.tokenImage = threeTokensAgo.tokenImage + "_" + lastToken.tokenImage;
      threeTokensAgo.tokenImage = "";
    }
    n.beginColumn = -1;
    n.endColumn = -1;
    if (twoTokensAgo != null) { // test is to quiet the Nullness Checker
      threeTokensAgo = twoTokensAgo;
    }
    if (lastToken != null) { // test is to quiet the Nullness Checker
      twoTokensAgo = lastToken;
    }
    lastToken = n;
    super.visit(n);
  }

  /**
   * This method should not be directly used by users of this class; however, must be public to
   * fulfill the visitor interface. Records the tokens of n if n does not start with a variable
   * name.
   */
  @Override
  public void visit(Name n) {
    if (!variableNamePrefixes.contains(n.f0.tokenImage)) {
      nonVariableNameTokens.addAll(Arrays.asList(TokenExtractor.extractTokens(n)));
    }
    super.visit(n);
  }

  /** Fixes the last token if needed. */
  private void fixLastToken() {
    if (threeTokensAgo != null
        && twoTokensAgo != null
        && // redundant, but for Nullness Checker
        lastToken != null
        && // redundant, but for Nullness Checker
        Visitors.isIdentifier(lastToken)
        && Visitors.isDot(twoTokensAgo)
        && Visitors.isIdentifier(threeTokensAgo)
        && !lastToken.tokenImage.equals("length")
        && !nonVariableNameTokens.contains(lastToken)) {
      twoTokensAgo.tokenImage = "";
      lastToken.tokenImage = threeTokensAgo.tokenImage + "_" + lastToken.tokenImage;
      threeTokensAgo.tokenImage = "";
    }
  }

  /**
   * Returns true if n is at the end of a set of node tokens that form a prefixed name needing
   * fixing.
   *
   * @param n a node token
   * @return true iff n is at the end of a set of node tokens that form a prefixed name needing
   *     fixing
   */
  @EnsuresNonNullIf(
      result = true,
      expression = {"lastToken", "twoTokensAgo", "threeTokensAgo"})
  @Pure
  private boolean isMatch(NodeToken n) {
    return (!Visitors.isLParen(n)
        && lastToken != null
        && Visitors.isIdentifier(lastToken)
        && twoTokensAgo != null
        && Visitors.isDot(twoTokensAgo)
        && threeTokensAgo != null
        && Visitors.isIdentifier(threeTokensAgo)
        && !lastToken.tokenImage.equals("length")
        && !nonVariableNameTokens.contains(lastToken));
  }
}
