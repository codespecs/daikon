package daikon;

import utilMDE.*;

import java.lang.ref.WeakReference;
import java.io.Serializable;
import java.util.*;

/**
 * VarInfoName is an type which represents the "name" of a variable.
 * Calling it a "name", however, is somewhat misleading.  It can be
 * some expression which includes more than one variable, term, etc.
 * We separate this from the VarInfo itself because clients wish to
 * manipulate names into new expressions independent of the VarInfo
 * which they might be associated with.
 **/
public abstract class VarInfoName
  implements Serializable, Comparable
{

  /**
   * Given the standard String representation of a variable name (from
   * a decls file), return the corresponding VarInfoName.  This cannot
   * handle generalized expressions, so name.equals(parse(e.name()))
   * is not certain to be true.
   **/
  public static VarInfoName parse(String name) {
    // x.class
    if (name.endsWith(".class")) {
      return parse(name.substring(0, name.length()-6)).applyTypeOf();
    }

    // stringvar.hashCode
    if (name.endsWith(".hashCode")) {
      // This is a temporary term in the AST, so that we can
      // instrument String hashcodes.  We will change dfej to do that
      // in another more general way when time allows.
      final VarInfoName term = parse(name.substring(0, name.length()-9));
      return (new VarInfoName() {
	  protected String repr_impl() {
	    return "HashCode[" + term.repr() + "]";
	  }
	  protected String name_impl() {
	    return term.name();
	  }
	  protected String esc_name_impl() {
	    return term.esc_name();
	  }
	  protected String simplify_name_impl(boolean prestate) {
	    return term.simplify_name(prestate);
	  }
	  public Object accept(Visitor v) {
	    return term.accept(v);
	  }
	}).intern();
    }

    // x or this.x
    if ((name.indexOf('[') == -1) && (name.indexOf('(') == -1)) {
      // checking for only legal characters would be more robust
      int dot = name.lastIndexOf('.');
      if (dot >= 0) {
	String first = name.substring(0, dot);
	String field = name.substring(dot+1);
	return parse(first).applyField(field);
      } else {
	return (new Simple(name)).intern();
      }
    }

    // a[]
    if (name.endsWith("[]")) {
      return parse(name.substring(0, name.length()-2)).applyElements();
    }

    // a[].foo or a[].foo.bar
    if (name.indexOf("[]") >= 0) {
      int brackets = name.lastIndexOf("[]");
      int dot = name.lastIndexOf('.');
      if (dot >= brackets) {
	String first = name.substring(0, dot);
	String field = name.substring(dot+1);
	return parse(first).applyField(field);
      }
    }

    // ??
    throw new UnsupportedOperationException("parse error: '" + name + "'");
  }

  /**
   * @return the string representation (interned) of this name, in the
   * default output format
   **/
  public String name() {
    if (name_cached == null) {
      try {
	name_cached = name_impl().intern();
      } catch (RuntimeException e) {
	System.err.println("repr = " + repr());
	throw e;
      }
    }
    return name_cached;
  }
  private String name_cached = null;
  protected abstract String name_impl();

  /**
   * @return the string representation (interned) of this name, in the
   * esc style output format
   **/
  public String esc_name() {
    if (esc_name_cached == null) {
      try {
	esc_name_cached = esc_name_impl().intern();
      } catch (RuntimeException e) {
	System.err.println("repr = " + repr());
	throw e;
      }
    }
    // System.out.println("esc_name = " + esc_name_cached + " for " + name() + " of class " + this.getClass().getName());
    return esc_name_cached;
  }
  private String esc_name_cached = null;
  protected abstract String esc_name_impl();

  /**
   * @return the string representation (interned) of this name, in the
   * Simplify tool output format
   **/
  public String simplify_name() {
    return simplify_name(false);
  }
  /**
   * @return the string representation (interned) of this name, in the
   * Simplify tool output format, in the given pre/post-state context.
   **/
  protected String simplify_name(boolean prestate) {
    int which = prestate ? 0 : 1;
    if (simplify_name_cached[which] == null) {
      try {
	simplify_name_cached[which] = simplify_name_impl(prestate).intern();
      } catch (RuntimeException e) {
	System.err.println("repr = " + repr());
	throw e;
      }
    }
    return simplify_name_cached[which];
  }
  private String simplify_name_cached[] = new String[2];
  protected abstract String simplify_name_impl(boolean prestate);

  /**
   * @return the string reprenentation (interned) of this name, in a
   * debugging format
   **/
  public String repr() {
    if (repr_cached == null) {
      repr_cached = repr_impl().intern();
    }
    return repr_cached;
  }
  private String repr_cached = null;
  protected abstract String repr_impl();

  // It would be nice if a generalized form of the mechanics of
  // interning were abstracted out somewhere.
  private static final WeakHashMap internTable = new WeakHashMap();
  public VarInfoName intern() {
    Object lookup = internTable.get(this);
    if (lookup != null) {
      WeakReference ref = (WeakReference)lookup;
      return (VarInfoName)ref.get();
    } else {
      internTable.put(this, new WeakReference(this));
      return this;
    }
  }

  // ============================================================
  // Helpful constants

  public static final VarInfoName ZERO = parse("0");

  // ============================================================
  // Interesting observers

  /**
   * @return true when this is "0", "-1", "1", etc.
   **/
  public boolean isLiteralConstant() {
    return false;
  }

  /**
   * @return the nodes of this, as given by an inorder traversal.
   **/
  public Collection inOrderTraversal() {
    return Collections.unmodifiableCollection(new InorderFlattener(this).nodes());
  }

  /**
   * @return true iff the given node can be found in this.  If the
   * node has children, the whole subtree must match.
   **/
  public boolean hasNode(VarInfoName node) {
    return inOrderTraversal().contains(node);
  }

  /**
   * @return true iff a node of the given type exists in this
   **/
  public boolean hasNodeOfType(Class type) {
    Iterator nodes = inOrderTraversal().iterator();
    while (nodes.hasNext()) {
      if (type.equals(nodes.next().getClass())) {
	return true;
      }
    }
    return false;
  }

  /**
   * @return true if the given node is in a prestate context within
   * this tree; the node must be a member of this tree.
   **/
  public boolean inPrestateContext(VarInfoName node) {
    return (new NodeFinder(this, node)).inPre();
  }

  // ============================================================
  // Special producers, or other helpers

  public VarInfoName replaceAll(VarInfoName node, VarInfoName replacement) {
    VarInfoName result = this;
    Replacer r = new Replacer(node, replacement);
    while (result.hasNode(node)) {
      result = r.replace(result).intern();
    }
    return result;
  }

  // ============================================================
  // The usual Object methods

  public boolean equals(Object o) {
    return (o instanceof VarInfoName) && equals((VarInfoName) o);
  }

  public boolean equals(VarInfoName other) {
    return (other == this) || ((other != null) && (this.repr().equals(other.repr())));
  }

  public int hashCode() {
    return repr().hashCode();
  }

  public int compareTo(Object o) {
    int nameCmp = name().compareTo(((VarInfoName) o).name());
    if (nameCmp != 0) return nameCmp;
    int reprCmp = repr().compareTo(((VarInfoName) o).repr());
    return reprCmp;
  }

  public String toString() {
    // Too much code uses the implicit toString when it really wants
    // name().  Eventually change this to repr() and hunt down diffs
    // in the regression tests and fix the other code.
    return name();
    // return repr();
  }

  // ============================================================
  // Static inner classes which form the expression langugage

  /**
   * A simple identifier like "a", "this.foo", etc.
   **/
  public static class Simple extends VarInfoName {
    public final String name;
    public Simple(String name) {
      Assert.assert(name != null);
      this.name = name;
    }
    public boolean isLiteralConstant() {
      try {
	Integer.parseInt(name);
	return true;
      } catch (NumberFormatException e) {
	return false;
      }
    }
    protected String repr_impl() {
      return name;
    }
    protected String name_impl() {
      return name;
    }
    protected String esc_name_impl() {
      return "return".equals(name) ? "\\result" : name;
    }
    protected String simplify_name_impl(boolean prestate) {
      if (isLiteralConstant()) {
	return name;
      } else {
	return simplify_name_impl(name, prestate);
      }
    }
    protected static String simplify_name_impl(String s, boolean prestate) {
      if (s.startsWith("~") && s.endsWith("~")) {
	s = s.substring(1, s.length()-2) + ":closure";
      }
      if (prestate) {
	s = "__orig__" + s;
      }
      return "|" + s + "|";
    }
    public Object accept(Visitor v) {
      return v.visitSimple(this);
    }
  }

  /**
   * @return true iff applySize will not throw an exception
   * @see #applySize
   **/
  public boolean isApplySizeSafe() {
    return (new ElementsFinder(this)).elems() != null;
  }

  /**
   * Returns a name for the size of this (this object should be a
   * sequence).  Form is like "size(a[])" or "a.length".
   **/
  public VarInfoName applySize() {
    // The simple approach is wrong because this might be "orig(a[])"
    // return (new SizeOf((Elements) this)).intern();
    Elements elems = (new ElementsFinder(this)).elems();
    Assert.assert(elems != null,
                  "applySize should have elements to use in " + this + ";\n"
                  + "that is, " + this + " does not appear to be a sequence/collection.\n"
                  + "Perhaps its name should be suffixed by \"[]\"?");
    Replacer r = new Replacer(elems, (new SizeOf(elems)).intern());
    return r.replace(this).intern();
  }

  /**
   * The size of a contained sequence; form is like "size(sequence)"
   * or "sequence.length".
   **/
  public static class SizeOf extends VarInfoName {
    public final Elements sequence;
    public SizeOf(Elements sequence) {
      Assert.assert(sequence != null);
      this.sequence = sequence;
    }
    protected String repr_impl() {
      return "SizeOf[" + sequence.repr() + "]";
    }
    protected String name_impl() {
      return "size(" + sequence.name() + ")";
    }
    protected String esc_name_impl() {
      return sequence.term.esc_name() + ".length";
    }
    protected String simplify_name_impl(boolean prestate) {
      return "(arrayLength " + sequence.term.simplify_name(prestate) + ")";
    }
    public Object accept(Visitor v) {
      return v.visitSizeOf(this);
    }
  }

  /**
   * Returns a name for a function over this object; form is like
   * "sum(this)".
   **/
  public VarInfoName applyFunction(String function) {
    return (new FunctionOf(function, this)).intern();
  }

  /**
   * A function over a term, like "sum(argument)"
   **/
  public static class FunctionOf extends VarInfoName {
    public final String function;
    public final VarInfoName argument;
    public FunctionOf(String function, VarInfoName argument) {
      Assert.assert(function != null);
      Assert.assert(argument != null);
      this.function = function;
      this.argument = argument;
    }
    protected String repr_impl() {
      return "FunctionOf{" + function + "}[" + argument.repr() + "]";
    }
    protected String name_impl() {
      return function + "(" + argument.name() + ")";
    }
    protected String esc_name_impl() {
      return "(format_esc needs to be changed: " +
	function + " on " + argument.repr() + ")";
    }
    protected String simplify_name_impl(boolean prestate) {
      return "(format_simplify needs to be changed: " +
	function + " on " + argument.repr() + ")";
    }
    public Object accept(Visitor v) {
      return v.visitFunctionOf(this);
    }
  }

  /**
   * Returns a 'getter' operation for some field of this name, like
   * a[i].foo if this is a[i].
   **/
  public VarInfoName applyField(String field) {
    return (new Field(this, field)).intern();
  }

  /**
   * A 'getter' operation for some field, like a[i].foo
   **/
  public static class Field extends VarInfoName {
    public final VarInfoName term;
    public final String field;
    public Field(VarInfoName term, String field) {
      Assert.assert(term != null);
      Assert.assert(field != null);
      this.term = term;
      this.field = field;
    }
    protected String repr_impl() {
      return "Field{" + field + "}[" + term.repr() + "]";
    }
    protected String name_impl() {
      return term.name() + "." + field;
    }
    protected String esc_name_impl() {
      return term.esc_name() + "." + field;
    }
    protected String simplify_name_impl(boolean prestate) {
      return "(select " + Simple.simplify_name_impl(field, prestate) + " " + term.simplify_name(prestate) + ")";
    }
    public Object accept(Visitor v) {
      return v.visitField(this);
    }
  }

  /**
   * Returns a name for a the type of this object; form is like
   * "this.class" or "\typeof(this)".
   **/
  public VarInfoName applyTypeOf() {
    return (new TypeOf(this)).intern();
  }

  /**
   * The type of the term, like "term.class"
   **/
  public static class TypeOf extends VarInfoName {
    public final VarInfoName term;
    public TypeOf(VarInfoName term) {
      Assert.assert(term != null);
      this.term = term;
    }
    protected String repr_impl() {
      return "TypeOf[" + term.repr() + "]";
    }
    protected String name_impl() {
      return term.name() + ".class";
    }
    protected String esc_name_impl() {
      return "\\typeof(" + term.esc_name() + ")";
    }
    protected String simplify_name_impl(boolean prestate) {
      return "(typeof " + term.simplify_name(prestate) + ")";
    }
    public Object accept(Visitor v) {
      return v.visitTypeOf(this);
    }
  }

  /**
   * Returns a name for a the prestate value of this object; form is
   * like "orig(this)" or "\old(this)".
   **/
  public VarInfoName applyPrestate() {
    return (new Prestate(this)).intern();
  }

  /**
   * The prestate value of a term, like "orig(term)"
   **/
  public static class Prestate extends VarInfoName {
    public final VarInfoName term;
    public Prestate(VarInfoName term) {
      Assert.assert(term != null);
      this.term = term;
    }
    protected String repr_impl() {
      return "Prestate[" + term.repr() + "]";
    }
    protected String name_impl() {
      return "orig(" + term.name() + ")";
    }
    protected String esc_name_impl() {
      return "\\old(" + term.esc_name() + ")";
    }
    protected String simplify_name_impl(boolean prestate) {
      return term.simplify_name(true);
    }
    public Object accept(Visitor v) {
      return v.visitPrestate(this);
    }
  }

  // sansOrig()
  //      int origpos = s.indexOf("orig(");
  //      Assert.assert(origpos != -1);
  //      int rparenpos = s.lastIndexOf(")");
  //      return s.substring(0, origpos)
  //        + s.substring(origpos+5, rparenpos)
  //        + s.substring(rparenpos+1);

  //      int origpos = s.indexOf("\\old(");
  //      Assert.assert(origpos != -1);
  //      int rparenpos = s.lastIndexOf(")");
  //      return s.substring(0, origpos)
  //        + s.substring(origpos+5, rparenpos)
  //        + s.substring(rparenpos+1);

  /**
   * Returns a name for a the poststate value of this object; form is
   * like "new(this)" or "\new(this)".
   **/
  public VarInfoName applyPoststate() {
    return (new Poststate(this)).intern();
  }

  /**
   * The poststate value of a term, like "new(term)".  Only used
   * within prestate, so like "orig(this.myArray[new(index)]".
   **/
  public static class Poststate extends VarInfoName {
    public final VarInfoName term;
    public Poststate(VarInfoName term) {
      Assert.assert(term != null);
      this.term = term;
    }
    protected String repr_impl() {
      return "Poststate[" + term.repr() + "]";
    }
    protected String name_impl() {
      return "post(" + term.name() + ")";
    }
    protected String esc_name_impl() {
      return "\\new(" + term.esc_name() + ")";
    }
    protected String simplify_name_impl(boolean prestate) {
      return term.simplify_name(false);
    }
    public Object accept(Visitor v) {
      return v.visitPoststate(this);
    }
  }

  /**
   * Returns a name for the this term plus a constant, like "this-1"
   * or "this+1".
   **/
  public VarInfoName applyAdd(int amount) {
    return (new Add(this, amount)).intern();
  }

  /**
   * An integer amount more or less than some other value
   **/
  public static class Add extends VarInfoName {
    public final VarInfoName term;
    public final int amount;
    public Add(VarInfoName term, int amount) {
      Assert.assert(term != null);
      this.term = term;
      this.amount = amount;
    }
    private String amount() {
      return (amount < 0) ? String.valueOf(amount) : "+" + amount;
    }
    protected String repr_impl() {
      return "Add{" + amount() + "}[" + term.repr() + "]";
    }
    protected String name_impl() {
      return term.name() + amount();
    }
    protected String esc_name_impl() {
      return term.esc_name() + amount();
    }
    protected String simplify_name_impl(boolean prestate) {
      return (amount < 0) ?
	"(- " + term.simplify_name(prestate) + " " + (-amount) + ")" :
	"(+ " + term.simplify_name(prestate) + " " + amount + ")";
    }
    public Object accept(Visitor v) {
      return v.visitAdd(this);
    }
    // override for cleanliness
    public VarInfoName applyAdd(int _amount) {
      int amt = _amount + this.amount;
      return (amt == 0) ? term : term.applyAdd(amt);
    }
  }

  /**
   * Returns a name for the decrement of this term, like "this-1".
   **/
  public VarInfoName applyDecrement() {
    return applyAdd(-1);
  }

  /**
   * Returns a name for the increment of this term, like "this+1".
   **/
  public VarInfoName applyIncrement() {
    return applyAdd(+1);
  }

  /**
   * Returns a name for the elements of a container (as opposed to the
   * identity of the container) like "this[]" or "(elements this)".
   **/
  public VarInfoName applyElements() {
    return (new Elements(this)).intern();
  }

  /**
   * The elements of a container, like "term[]"
   **/
  public static class Elements extends VarInfoName {
    public final VarInfoName term;
    public Elements(VarInfoName term) {
      Assert.assert(term != null);
      this.term = term;
    }
    protected String repr_impl() {
      return "Elements[" + term.repr() + "]";
    }
    protected String name_impl() {
      return name_impl("");
    }
    protected String name_impl(String index) {
      return term.name() + "[" + index + "]";
    }
    protected String esc_name_impl() {
      throw new UnsupportedOperationException("ESC cannot format an unquantified sequence of elements" +
					      " [repr=" + repr() + "]");
    }
    protected String esc_name_impl(String index) {
      return term.esc_name() + "[" + index + "]";
    }
    protected String simplify_name_impl(boolean prestate) {
      return "(select elems " + term.simplify_name(prestate) + ")";
    }
    public Object accept(Visitor v) {
      return v.visitElements(this);
    }
  }

  /**
   * Caller is subscripting an orig(a[]) array.  Take the requested
   * index and make it useful in that context.
   **/
  static VarInfoName indexToPrestate(VarInfoName index) {
    // 1 orig(a[]) . orig(index) -> orig(a[index])
    // 2 orig(a[]) . index       -> orig(a[post(index)])
    if (index instanceof Prestate) {
      index = ((Prestate) index).term; // #1
    } else if (index instanceof Add) {
      Add add = (Add) index;
      if (add.term instanceof Prestate) {
	index = ((Prestate) add.term).term.applyAdd(add.amount); // #1
      } else {
	index = index.applyPoststate();  // #2
      }
    } else if (index.isLiteralConstant()) {
      // we don't want orig(a[post(0)]), so leave index alone
    } else {
      index = index.applyPoststate();  // #2
    }
    return index;
  }

  /**
   * Returns a name for an element selected from a sequence, like
   * "this[i]"
   **/
  public VarInfoName applySubscript(VarInfoName index) {
    Assert.assert(index != null);
    ElementsFinder finder = new ElementsFinder(this);
    Elements elems = finder.elems();
    Assert.assert(elems != null, "applySubscript should have elements to use in " + this);
    if (finder.inPre()) {
      index = indexToPrestate(index);
    }
    Replacer r = new Replacer(elems, (new Subscript(elems, index)).intern());
    return r.replace(this).intern();
  }

  // Given a sequence and subscript index, convert the index to an
  // explicit form if necessary (e.g. a[-1] becomes a[a.length-1])
  static VarInfoName indexExplicit(Elements sequence, VarInfoName index) {
    if (!index.isLiteralConstant()) {
      return index;
    }

    int i = Integer.parseInt(index.name());
    if (i >= 0) {
      return index;
    }

    return sequence.applySize().applyAdd(i);
  }

  /**
   * An element from a sequence, like "sequence[index]"
   **/
  public static class Subscript extends VarInfoName {
    public final Elements sequence;
    public final VarInfoName index;
    public Subscript(Elements sequence, VarInfoName index) {
      Assert.assert(sequence != null);
      Assert.assert(index != null);
      this.sequence = sequence;
      this.index = index;
    }
    protected String repr_impl() {
      return "Subscript{" + index.repr() + "}[" + sequence.repr() + "]";
    }
    protected String name_impl() {
      return sequence.name_impl(index.name());
    }
    protected String esc_name_impl() {
      return sequence.esc_name_impl(indexExplicit(sequence, index).esc_name());
    }
    protected String simplify_name_impl(boolean prestate) {
      return "(select " + sequence.simplify_name(prestate) + " " +
	indexExplicit(sequence, index).simplify_name(prestate) + ")";
    }
    public Object accept(Visitor v) {
      return v.visitSubscript(this);
    }
  }

  /**
   * Returns a name for a slice of element selected from a sequence,
   * like "this[i..j]".  If an endpoint is null, it means "from the
   * start" or "to the end".
   **/
  public VarInfoName applySlice(VarInfoName i, VarInfoName j) {
    // a[] -> a[index..]
    // orig(a[]) -> orig(a[post(index)..])
    ElementsFinder finder = new ElementsFinder(this);
    Elements elems = finder.elems();
    Assert.assert(elems != null);
    if (finder.inPre()) {
      if (i != null) {
	i = indexToPrestate(i);
      }
      if (j != null) {
	j = indexToPrestate(j);
      }
    }
    Replacer r = new Replacer(finder.elems(), (new Slice(elems, i, j)).intern());
    return r.replace(this).intern();
  }

  /**
   * An slice of elements from a sequence, like "sequence[i..j]"
   **/
  public static class Slice extends VarInfoName {
    public final Elements sequence;
    public final VarInfoName i, j;
    public Slice(Elements sequence, VarInfoName i, VarInfoName j) {
      Assert.assert(sequence != null);
      Assert.assert((i != null) || (j != null));
      this.sequence = sequence;
      this.i = i;
      this.j = j;
    }
    protected String repr_impl() {
      return "Slice{" +
	((i == null) ? "" : i.repr()) + "," +
	((j == null) ? "" : j.repr()) + "}[" +
	sequence.repr() + "]";
    }
    protected String name_impl() {
      return sequence.name_impl("" +
				((i == null) ? "0" : i.name()) +
				".." +
				((j == null) ? ""  : j.name())
				);
    }
    protected String esc_name_impl() {
      throw new UnsupportedOperationException("ESC cannot format an unquantified slice of elements");
    }
    protected String simplify_name_impl(boolean prestate) {
      throw new UnsupportedOperationException("Simplify cannot format an unquantified slice of elements");
    }
    public Object accept(Visitor v) {
      return v.visitSlice(this);
    }
  }

  // ============================================================
  // Visitor framework for easier processing

  public abstract Object accept(Visitor v);

  public static interface Visitor {
    public Object visitSimple(Simple o);
    public Object visitSizeOf(SizeOf o);
    public Object visitFunctionOf(FunctionOf o);
    public Object visitField(Field o);
    public Object visitTypeOf(TypeOf o);
    public Object visitPrestate(Prestate o);
    public Object visitPoststate(Poststate o);
    public Object visitAdd(Add o);
    public Object visitElements(Elements o);
    public Object visitSubscript(Subscript o);
    public Object visitSlice(Slice o);
  }

  /**
   * Traverse the tree elements which have exactly one branch (so the
   * traversal order doesn't matter).
   **/
  public static abstract class AbstractVisitor
    implements Visitor
  {
    public Object visitSimple(Simple o) {
      // nothing to do; leaf node
      return null;
    }
    public Object visitSizeOf(SizeOf o) {
      return o.sequence.accept(this);
    }
    public Object visitFunctionOf(FunctionOf o) {
      return o.argument.accept(this);
    }
    public Object visitField(Field o) {
      return o.term.accept(this);
    }
    public Object visitTypeOf(TypeOf o) {
      return o.term.accept(this);
    }
    public Object visitPrestate(Prestate o) {
      return o.term.accept(this);
    }
    public Object visitPoststate(Poststate o) {
      return o.term.accept(this);
    }
    public Object visitAdd(Add o) {
      return o.term.accept(this);
    }
    public Object visitElements(Elements o) {
      return o.term.accept(this);
    }
    // leave abstract; traversal order and return values matter
    public abstract Object visitSubscript(Subscript o);
    // leave abstract; traversal order and return values matter
    public abstract Object visitSlice(Slice o);
  }

  /**
   * Use to report whether a node is in a pre- or post-state context.
   **/
  public static class NodeFinder
    extends AbstractVisitor
  {
    public NodeFinder(VarInfoName root, VarInfoName goal) {
      this.goal = goal;
      Object o = root.accept(this);
      Assert.assert(o != null);
    }
    // state and accessors
    private final VarInfoName goal;
    private boolean pre;
    public boolean inPre() {
      return pre;
    }
    // visitor methods which get the job done
    public Object visitSimple(Simple o) {
      return (o == goal) ? goal : null;
    }
    public Object visitSizeOf(SizeOf o) {
      return (o == goal) ? goal : super.visitSizeOf(o);
    }
    public Object visitFunctionOf(FunctionOf o) {
      return (o == goal) ? goal : super.visitFunctionOf(o);
    }
    public Object visitField(Field o) {
      return (o == goal) ? goal : super.visitField(o);
    }
    public Object visitTypeOf(TypeOf o) {
      return (o == goal) ? goal : super.visitTypeOf(o);
    }
    public Object visitPrestate(Prestate o) {
      pre = true;
      return super.visitPrestate(o);
    }
    public Object visitPoststate(Poststate o) {
      pre = false;
      return super.visitPoststate(o);
    }
    public Object visitAdd(Add o) {
      return (o == goal) ? goal : super.visitAdd(o);
    }
    public Object visitElements(Elements o) {
      return (o == goal) ? goal : super.visitElements(o);
    }
    public Object visitSubscript(Subscript o) {
      if (o == goal) return goal;
      if (o.sequence.accept(this) != null) return goal;
      if (o.index.accept(this) != null) return goal;
      return null;
    }
    public Object visitSlice(Slice o) {
      if (o == goal) return goal;
      if (o.sequence.accept(this) != null) return goal;
      if ((o.i != null) & (o.i.accept(this) != null)) return goal;
      if ((o.j != null) & (o.j.accept(this) != null)) return goal;
      return null;
    }
  }

  /**
   * Use to traverse a tree, find the first (elements ...) node, and
   * report whether it's in pre or post-state.
   **/
  public static class ElementsFinder
    extends AbstractVisitor
  {
    public ElementsFinder (VarInfoName name) {
      elems = (Elements) name.accept(this);
    }

    // state and accessors
    private boolean pre = false;
    private Elements elems = null;

    public boolean inPre() {
      return pre;
    }
    public Elements elems() {
      return elems;
    }

    // visitor methods which get the job done
    public Object visitPrestate(Prestate o) {
      pre = true;
      return super.visitPrestate(o);
    }
    public Object visitPoststate(Poststate o) {
      pre = false;
      return super.visitPoststate(o);
    }
    public Object visitElements(Elements o) {
      return o;
    }
    public Object visitSubscript(Subscript o) {
      // skip the subscripted sequence
      Object tmp = o.sequence.term.accept(this);
      if (tmp == null) { tmp = o.index.accept(this); }
      return tmp;
    }
    public Object visitSlice(Slice o) {
      // skip the sliced sequence
      Object tmp = o.sequence.term.accept(this);
      if (tmp == null && o.i != null) { tmp = o.i.accept(this); }
      if (tmp == null && o.j != null) { tmp = o.j.accept(this); }
      return tmp;
    }
  }

  /**
   * A Replacer is a Visitor which makes a copy of a tree, but
   * replaces some node (and it's children) with another.
   **/
  public static class Replacer
    extends AbstractVisitor
  {
    private final VarInfoName old;
    private final VarInfoName _new;
    public Replacer(VarInfoName old, VarInfoName _new) {
      this.old = old;
      this._new = _new;
    }

    public VarInfoName replace(VarInfoName root) {
      return (VarInfoName) root.accept(this);
    }

    public Object visitSimple(Simple o) {
      return (o == old) ? _new : o;
    }
    public Object visitSizeOf(SizeOf o) {
      return (o == old) ? _new :
	((VarInfoName) super.visitSizeOf(o)).applySize();
    }
    public Object visitFunctionOf(FunctionOf o) {
      return (o == old) ? _new :
	((VarInfoName) super.visitFunctionOf(o)).applyFunction(o.function);
    }
    public Object visitField(Field o) {
      return (o == old) ? _new :
	((VarInfoName) super.visitField(o)).applyField(o.field);
    }
    public Object visitTypeOf(TypeOf o) {
      return (o == old) ? _new :
	((VarInfoName) super.visitTypeOf(o)).applyTypeOf();
    }
    public Object visitPrestate(Prestate o) {
      return (o == old) ? _new :
	((VarInfoName) super.visitPrestate(o)).applyPrestate();
    }
    public Object visitPoststate(Poststate o) {
      return (o == old) ? _new :
	((VarInfoName) super.visitPoststate(o)).applyPoststate();
    }
    public Object visitAdd(Add o) {
      return (o == old) ? _new :
	((VarInfoName) super.visitAdd(o)).applyAdd(o.amount);
    }
    public Object visitElements(Elements o) {
      return (o == old) ? _new :
	((VarInfoName) super.visitElements(o)).applyElements();
    }
    public Object visitSubscript(Subscript o) {
      return (o == old) ? _new :
	((VarInfoName) o.sequence.accept(this)).
	applySubscript((VarInfoName) o.index.accept(this));
    }
    public Object visitSlice(Slice o) {
      return (o == old) ? _new :
	((VarInfoName) o.sequence.accept(this)).
	applySlice((o.i == null) ? null : ((VarInfoName) o.i.accept(this)),
		   (o.j == null) ? null : ((VarInfoName) o.j.accept(this)));
    }
  }

  /**
   * Use to collect all elements in a tree into an inorder-traversal
   * list.  Result includes the root element.
   **/
  public static class InorderFlattener
    extends AbstractVisitor
  {
    public InorderFlattener(VarInfoName root) {
      root.accept(this);
    }

    // state and accessors
    private final List result = new ArrayList();

    public List nodes() {
      return Collections.unmodifiableList(result);
    }

    // visitor methods which get the job done
    public Object visitSimple(Simple o) {
      result.add(o);
      return super.visitSimple(o);
    }
    public Object visitSizeOf(SizeOf o) {
      result.add(o);
      return super.visitSizeOf(o);
    }
    public Object visitFunctionOf(FunctionOf o) {
      result.add(o);
      return super.visitFunctionOf(o);
    }
    public Object visitField(Field o) {
      result.add(o);
      return super.visitField(o);
    }
    public Object visitTypeOf(TypeOf o) {
      result.add(o);
      return super.visitTypeOf(o);
    }
    public Object visitPrestate(Prestate o) {
      result.add(o);
      return super.visitPrestate(o);
    }
    public Object visitPoststate(Poststate o) {
      result.add(o);
      return super.visitPoststate(o);
    }
    public Object visitAdd(Add o) {
      result.add(o);
      return super.visitAdd(o);
    }
    public Object visitElements(Elements o) {
      result.add(o);
      return super.visitElements(o);
    }
    public Object visitSubscript(Subscript o) {
      result.add(o);
      o.sequence.accept(this);
      o.index.accept(this);
      return null;
    }
    public Object visitSlice(Slice o) {
      result.add(o);
      o.sequence.accept(this);
      if (o.i != null) o.i.accept(this);
      if (o.j != null) o.j.accept(this);
      return null;
    }
  }

  // ============================================================
  // Quantification for formatting in ESC or Simplify

  private static final boolean debug_quantify = false;

  /**
   * A quantifier visitor can be used to search a tree and return all
   * unquantified sequences (e.g. a[] or a[i..j], and also all Simple
   * nodes (variable names).  This is useful for restating the name in
   * terms of a quantification.
   **/
  public static class QuantifierVisitor
    extends AbstractVisitor
  {
    public QuantifierVisitor(VarInfoName root) {
      Assert.assert(root != null);
      simples = new HashSet();
      unquant = new HashSet();
      root.accept(this);
    }

    // state and accessors
    private Set simples; // [Simple]
    private Set unquant; // [Elements || Slice]

    /**
     * @return Collection of simple identifiers used in this
     * expression (so that they can be checked for conflict with the
     * quantifier variable name).
     **/
    public Set simples() {
      return Collections.unmodifiableSet(simples);
    }
    /**
     * @return Collection of the nodes under the root which need
     * quantification.  (The values are either of type Elements or
     * Slice).
     **/
    public Set unquants() {
      if (debug_quantify) {
	System.out.println("unquants: " + unquant);
      }
      return Collections.unmodifiableSet(unquant);
    }

    // visitor methods which get the job done
    public Object visitSimple(Simple o) {
      simples.add(o);
      return super.visitSimple(o);
    }
    public Object visitElements(Elements o) {
      unquant.add(o);
      return super.visitElements(o);
    }
    public Object visitSizeOf(SizeOf o) {
      // don't visit the sequence; we aren't using the elements of it,
      // just the length, so we don't want to include it in the results
      return o.sequence.term.accept(this);
    }
    public Object visitSubscript(Subscript o) {
      o.index.accept(this);
      // don't visit the sequence; it is fixed with an exact
      // subscript, so we don't want to include it in the results
      return o.sequence.term.accept(this);
    }
    public Object visitSlice(Slice o) {
      unquant.add(o);
      if (o.i != null) { o.i.accept(this); }
      if (o.j != null) { o.j.accept(this); }
      // don't visit the sequence; we will replace the slice with the
      // subscript, so we want to leave the elements alone
      return o.sequence.term.accept(this);
    }
  }

  public static class QuantHelper {

    /**
     * A FreeVar is very much like a Simple, except that it doesn't
     * care if it's in prestate or poststate for simplify formatting.
     **/
    public static class FreeVar
      extends Simple
    {
      public FreeVar(String name) {
	super(name);
      }
      protected String repr_impl() {
	return "Free[" + super.repr_impl() + "]";
      }
      protected String simplify_name_impl(boolean prestate) {
	return super.simplify_name_impl(false);
      }
    }
    
    // <root, needy, index> -> <root', lower, upper>
    /**
     * Replaces a needy (unquantified term) with its subscripted
     * equivalent, using the given index variable.
     *
     * @param root the root of the expression to be modified
     * @param needy the term to be subscripted (must be of type Elements or Slice)
     * @param index the variable to place in the subscript
     *
     * @return a 3-element array consisting of the new root, the lower
     * bound for the index (inclusive), and the upper bound for the
     * index (inclusive), in that order.
     **/
    public static VarInfoName[] replace(VarInfoName root, VarInfoName needy, VarInfoName index) {
      Assert.assert(root != null);
      Assert.assert(needy != null);
      Assert.assert(index != null);
      Assert.assert((needy instanceof Elements) || (needy instanceof Slice));

      // Figure out what to replace needy with, and the appropriate
      // bounds to use
      VarInfoName replace_with;
      VarInfoName lower, upper;
      if (needy instanceof Elements) {
	Elements sequence = (Elements) needy;
	replace_with = sequence.applySubscript(index);
	lower = ZERO;
	upper = sequence.applySize().applyDecrement();
      } else if (needy instanceof Slice) {
	Slice slice = (Slice) needy;
	replace_with = slice.sequence.applySubscript(index);
	lower = (slice.i != null) ? slice.i : ZERO;
	upper = (slice.j != null) ? slice.j :
	  slice.sequence.applySize().applyDecrement();
      } else {
	// unreachable; placate javac
	throw new IllegalStateException();
      }
      Assert.assert(replace_with != null);

      // If needy was in prestate, adjust bounds appropriately
      if (root.inPrestateContext(needy)) {
	if (!lower.isLiteralConstant()) {
	  if (lower instanceof Poststate) {
	    lower = ((Poststate) lower).term;
	  } else {
	    lower = lower.applyPrestate();
	  }
	}
	if (!upper.isLiteralConstant()) {
	  if (upper instanceof Poststate) {
	    upper = ((Poststate) upper).term;
	  } else {
	    upper = upper.applyPrestate();
	  }
	}
      }

      // replace needy
      VarInfoName root_prime = (new Replacer(needy, replace_with)).replace(root).intern();

      Assert.assert(root_prime != null);
      Assert.assert(lower != null);
      Assert.assert(upper != null);

      return new VarInfoName[] { root_prime, lower, upper };
    }

    /**
     * Record type for return value of the quantify method below
     **/
    public static class QuantifyReturn {
      public VarInfoName[] root_primes;
      public Vector bound_vars; // of VarInfoName[3] = <variable, lower, upper>
    }

    // <root*> -> <root'*, <index, lower, upper>*>
    /**
     * Given a list of roots, changes all Elements or Slice terms to
     * Subscripts by inserting a new free variable; also return bounds
     * for the new variables.
     **/
    public static QuantifyReturn quantify(VarInfoName[] roots) {
      Assert.assert(roots != null);

      if (debug_quantify) {
	System.out.println("roots: " + Arrays.asList(roots));
      }

      // create empty result
      QuantifyReturn result = new QuantifyReturn();
      result.root_primes = new VarInfoName[roots.length];
      result.bound_vars = new Vector();

      // all of the simple identifiers used by these roots
      Set simples = new HashSet(); // [Simple]

      // build helper for each roots; collect identifiers
      QuantifierVisitor[] helper = new QuantifierVisitor[roots.length];
      for (int i=0; i < roots.length; i++) {
	helper[i] = new QuantifierVisitor(roots[i]);
	simples.addAll(helper[i].simples());
      }

      // choose names for the indicies which don't conflict, and then
      // replace the right stuff in the term
      char tmp = 'i';
      for (int i=0; i < roots.length; i++) {
	List uq = new ArrayList(helper[i].unquants());
	if (uq.size() == 0) {
	  // nothing needs quantification
	  result.root_primes[i] = roots[i];
	} else {
	  Assert.assert(uq.size() == 1, "We can only handle 1D arrays for now");
	  VarInfoName uq_elt = (VarInfoName) uq.get(0);

	  VarInfoName idx = (new FreeVar(String.valueOf(tmp++))).intern();
	  Assert.assert(!simples.contains(idx), "Index variable unexpectedly used");

	  if (debug_quantify) {
	    System.out.println("root: " + roots[i]);
	    System.out.println("uq_elt: " + uq_elt);
	    System.out.println("idx: " + idx);
	  }

	  // call replace and unpack results
	  VarInfoName[] replace_result = replace(roots[i], uq_elt, idx);
	  VarInfoName root_prime = replace_result[0];
	  VarInfoName lower = replace_result[1];
	  VarInfoName upper = replace_result[2];

	  result.root_primes[i] = root_prime;
	  result.bound_vars.add(new VarInfoName[] { idx, lower, upper });
	}
      }

      return result;
    }

    // <root*> -> <string string*>
    /**
     * Given a list of roots, return a String array where the first
     * element is a ESC-style quantification over newly-introduced
     * bound variables, the last element is a closer, and the other
     * elements are esc-named strings for the provided roots (with
     * sequenced subscripted by one of the new bound variables).
     **/
    public static String[] format_esc(VarInfoName[] roots) {
      return format_esc(roots, false);
    }
    public static String[] format_esc(VarInfoName[] roots, boolean elementwise) {
      Assert.assert(roots != null);

      QuantifyReturn qret = quantify(roots);

      // build the "\forall ..." predicate
      String[] result = new String[roots.length+2];
      StringBuffer int_list, conditions;
      {
	// "i, j, ..."
	int_list = new StringBuffer();
	// "ai <= i && i <= bi && aj <= j && j <= bj && ..."
	// if elementwise, also do "(i-ai) == (b-bi) && ..."
	conditions = new StringBuffer();
	for (int i=0; i < qret.bound_vars.size(); i++) {
	  VarInfoName[] boundv = (VarInfoName[]) qret.bound_vars.get(i);
	  VarInfoName idx = boundv[0], low = boundv[1], high = boundv[2];
	  if (i != 0) {
	    int_list.append(", ");
	    conditions.append(" && ");
	  }
	  int_list.append(idx.esc_name());
	  conditions.append(low.esc_name());
	  conditions.append(" <= ");
	  conditions.append(idx.esc_name());
	  conditions.append(" && ");
	  conditions.append(idx.esc_name());
	  conditions.append(" <= ");
	  conditions.append(high.esc_name());
	  if (elementwise && (i >= 1)) {
	    VarInfoName[] _boundv = (VarInfoName[]) qret.bound_vars.get(i-1);
	    VarInfoName _idx = _boundv[0], _low = _boundv[1];
	    conditions.append(" && ");
	    if (ZERO.equals(_low)) {
	      conditions.append(_idx);
	    } else {
	      conditions.append("(");
	      conditions.append(_idx.esc_name());
	      conditions.append("-(");
	      conditions.append(_low.esc_name());
	      conditions.append("))");
	    }
	    conditions.append(" == ");
	    if (ZERO.equals(low)) {
	      conditions.append(idx.esc_name());
	    } else {
	      conditions.append("(");
	      conditions.append(idx.esc_name());
	      conditions.append("-(");
	      conditions.append(low.esc_name());
	      conditions.append("))");
	    }
	  }
	}
      }
      result[0] = "(\\forall int " + int_list + "; (" + conditions + ") ==> ";
      result[result.length-1] = ")";

      // stringify the terms
      for (int i=0; i < roots.length; i++) {
	result[i+1] = qret.root_primes[i].esc_name();
      }

      return result;
    }

    // <root*> -> <string string*>
    /**
     * Given a list of roots, return a String array where the first
     * element is a simplify-style quantification over
     * newly-introduced bound variables, the last element is a closer,
     * and the other elements are simplify-named strings for the
     * provided roots (with sequenced subscripted by one of the new
     * bound variables).
     **/
    public static String[] format_simplify(VarInfoName[] roots) {
      return format_simplify(roots, false);
    }
    public static String[] format_simplify(VarInfoName[] roots, boolean elementwise) {
      Assert.assert(roots != null);

      QuantifyReturn qret = quantify(roots);

      // build the forall predicate
      String[] result = new String[roots.length+2];
      StringBuffer int_list, conditions;
      {
	// "i j ..."
	int_list = new StringBuffer();
	// "(AND (<= ai i) (<= i bi) (<= aj j) (<= j bj) ...)"
	// if elementwise, also insert "(EQ (- i ai) (- j aj)) ..."
	conditions = new StringBuffer();
	for (int i=0; i < qret.bound_vars.size(); i++) {
	  VarInfoName[] boundv = (VarInfoName[]) qret.bound_vars.get(i);
	  VarInfoName idx = boundv[0], low = boundv[1], high = boundv[2];
	  if (i != 0) {
	    int_list.append(" ");
	    conditions.append(" ");
	  }
	  int_list.append(idx.simplify_name());
	  conditions.append( "(<= " + low.simplify_name() + " " + idx.simplify_name() + ")");
	  conditions.append(" (<= " + idx.simplify_name() + " " + high.simplify_name() + ")");
	  if (elementwise && (i >= 1)) {
	    VarInfoName[] _boundv = (VarInfoName[]) qret.bound_vars.get(i-1);
	    VarInfoName _idx = _boundv[0], _low = _boundv[1];
	    conditions.append(" (EQ (- " + _idx.simplify_name() + " " + _low.simplify_name() + ")");
	    conditions.append(    " (- " + idx.simplify_name() + " " + low.simplify_name() + "))");
	  }
	}
      }
      result[0] = "(FORALL (" + int_list + ") (IMPLIES (AND " + conditions + ") ";
      result[result.length-1] = "))"; // close IMPLIES, FORALL

      // stringify the terms
      for (int i=0; i < roots.length; i++) {
	result[i+1] = qret.root_primes[i].simplify_name();
      }

      return result;
    }

  } // QuantHelper

}
