package daikon.split;

import daikon.VarInfo;
import daikon.tools.jtb.Ast;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import jtb.ParseException;
import jtb.syntaxtree.ArgumentList;
import jtb.syntaxtree.Arguments;
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
 * that access arrays, which appear in Daikon's Java output format, by Java code that uses the
 * variables of a splitter. For example, if {@code this_a} is the base name of an int array, then
 * {@code daikon.Quant.getElement_int(this_a, i)} yields
 *
 * <pre>{@code (this_a_array==null?Integer.MAX_VALUE:(int)this_a_array[(int)(i)])}</pre>
 *
 * and {@code daikon.Quant.size(this_a)} yields
 *
 * <pre>{@code (this_a_array==null?Integer.MAX_VALUE:this_a_array.length)}</pre>
 *
 * Like the daikon.Quant methods, the replacements yield a default value if the array is null, and
 * they accept a long index.
 *
 * <p>QuantFixer runs after the fixers that convert variable names to base names, such as {@link
 * PrefixFixer}. A call is left alone if its array argument is not the base name of an array
 * variable, or if the splitter does not represent the array's elements, as for {@code
 * getElement_Object}.
 */
class QuantFixer extends DepthFirstVisitor {

  /** The default value that each daikon.Quant getElement_* method returns for a null array. */
  private static final Map<String, String> defaultValues = new HashMap<>();

  static {
    defaultValues.put("boolean", "false");
    defaultValues.put("byte", "Byte.MAX_VALUE");
    defaultValues.put("char", "Character.MAX_VALUE");
    defaultValues.put("double", "Double.NaN");
    defaultValues.put("float", "Float.NaN");
    defaultValues.put("int", "Integer.MAX_VALUE");
    defaultValues.put("long", "Long.MAX_VALUE");
    defaultValues.put("short", "Short.MAX_VALUE");
    defaultValues.put("String", "null");
  }

  /** Maps the base name of each array variable to its VarInfo. */
  private final Map<String, VarInfo> arrays;

  /**
   * Creates a new QuantFixer.
   *
   * @param arrays maps the base name of each array variable to its VarInfo
   */
  private QuantFixer(Map<String, VarInfo> arrays) {
    super();
    this.arrays = arrays;
  }

  /**
   * Replaces calls to daikon.Quant methods that access arrays (see class description).
   *
   * @param expression a valid segment of Java code
   * @param arrays maps the base name of each array variable to its VarInfo
   * @return expression, with calls to daikon.Quant array methods replaced
   * @throws ParseException if expression is not valid Java code
   */
  public static String fixQuant(String expression, Map<String, VarInfo> arrays)
      throws ParseException {
    if (!expression.contains("daikon.Quant")) {
      return expression;
    }
    Node root = Visitors.getJtbTree(expression);
    root.accept(new QuantFixer(arrays));
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
      fixQuantCall(n, methodName);
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
   * Replaces n, a call to a daikon.Quant method, if the method accesses an array that the splitter
   * represents.
   *
   * @param n a call to a daikon.Quant method
   * @param methodName the name of the daikon.Quant method that n calls
   */
  private void fixQuantCall(PrimaryExpression n, String methodName) {
    Arguments args = (Arguments) ((PrimarySuffix) n.f1.elementAt(0)).f0.choice;
    if (!args.f1.present()) {
      return;
    }
    ArgumentList argList = (ArgumentList) args.f1.node;
    List<NodeToken> commas = new ArrayList<>();
    for (Node commaAndArg : argList.f1.nodes) {
      commas.add((NodeToken) ((NodeSequence) commaAndArg).elementAt(0));
    }
    NodeToken[] arrayTokens = TokenExtractor.extractTokens(argList.f0);
    if (arrayTokens.length != 1) {
      return;
    }
    VarInfo array = arrays.get(arrayTokens[0].tokenImage);
    if (array == null) {
      return;
    }
    String arrayName = SplitterJavaSource.compilableName(array);
    // A splitter represents a char array as a String.
    boolean isString = SplitterJavaSource.getVarType(array).equals("char[]");

    // The replacement is prefix, then the index argument if any, then suffix.
    String prefix;
    String suffix;
    if (methodName.equals("size") && commas.isEmpty()) {
      prefix = "Integer.MAX_VALUE:" + arrayName + (isString ? ".length()" : ".length");
      suffix = ")";
    } else if (methodName.startsWith("getElement_") && commas.size() == 1) {
      String elementType = methodName.substring("getElement_".length());
      String defaultValue = defaultValues.get(elementType);
      if (defaultValue == null) {
        return;
      }
      if (isString) {
        prefix = defaultValue + ":" + arrayName + ".charAt((int)(";
        suffix = ")))";
      } else {
        // A splitter represents the elements of an integral or boolean array as longs, and those
        // of a float array as doubles.
        String cast =
            (elementType.equals("byte")
                    || elementType.equals("short")
                    || elementType.equals("int")
                    || elementType.equals("float"))
                ? "(" + elementType + ")"
                : "";
        prefix = defaultValue + ":" + cast + arrayName + "[(int)(";
        suffix = ")]" + (elementType.equals("boolean") ? ">0" : "") + ")";
      }
    } else {
      return;
    }

    NodeToken[] methodTokens = TokenExtractor.extractTokens(n.f0);
    for (NodeToken token : methodTokens) {
      token.tokenImage = "";
    }
    methodTokens[0].tokenImage = "(" + arrayName + "==null?" + prefix;
    args.f0.tokenImage = "";
    arrayTokens[0].tokenImage = "";
    for (NodeToken comma : commas) {
      comma.tokenImage = "";
    }
    args.f2.tokenImage = suffix;
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
}
