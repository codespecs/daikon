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

    // x or this.x
    if ((name.indexOf('[') == -1) && (name.indexOf('(') == -1)) {
      // checking for only leagal characters would be more robust
      return (new Simple(name)).intern();
    }

    // a[]
    if (name.endsWith("[]")) {
      return parse(name.substring(0, name.length()-2)).applyElements();
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
      name_cached = name_impl().intern();
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
      esc_name_cached = esc_name_impl().intern();
    }
    return esc_name_cached;
  }
  private String esc_name_cached = null;
  protected abstract String esc_name_impl();

  /**
   * @return the string representation (interned) of this name, in the
   * Simplify tool output format
   **/
  public String simplify_name() {
    if (simplify_name_cached == null) {
      simplify_name_cached = simplify_name_impl().intern();
    }
    return simplify_name_cached;
  }
  private String simplify_name_cached = null;
  protected abstract String simplify_name_impl();

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
  // The usual Object methods

  public boolean equals(Object o) {
    return (o instanceof VarInfoName) && equals((VarInfoName) o);
  }

  public boolean equals(VarInfoName other) {
    return (other == this) || ((other != null) && (this.name().equals(other.name())));
  }

  public int hashCode() {
    return name().hashCode();
  }

  public int compareTo(Object o) {
    return name().compareTo(((VarInfoName) o).name());
  }

  public String toString() {
    return name();
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
    protected String name_impl() {
      return name;
    }
    protected String esc_name_impl() {
      return "return".equals(name) ? "\\result" : name;
    }
    protected String simplify_name_impl() {
      // Field access with dots ("getters") are like lambda
      // applications so this.foo turns into (select foo this)
      String working = name;
      String prefix = "";
      String suffix = "";
      int dot = working.indexOf('.');
      while (dot >= 0) {
	String rest = working.substring(dot+1);
	working = working.substring(0, dot);
	prefix = "(select " + rest + " " + prefix;
	suffix = suffix + ")";
	dot = working.indexOf('.');
      }
      return prefix + working + suffix;
    }
    public Object accept(Visitor v) {
      return v.visitSimple(this);
    }
  }

  /**
   * Returns a name for the size of this (this object should be a
   * sequence).  Form is like "size(this)" or "this.length".
   **/
  public VarInfoName applySize() {
    return (new SizeOf(this)).intern();
  }

  /**
   * The size of a contained sequence; form is like "size(sequence)"
   * or "sequence.length".
   **/
  public static class SizeOf extends VarInfoName {
    public final VarInfoName sequence;
    public SizeOf(VarInfoName sequence) {
      Assert.assert(sequence != null);
      this.sequence = sequence;
    }
    protected String name_impl() {
      return "size(" + sequence.name() + ")";
    }
    protected String esc_name_impl() {
      return sequence.esc_name() + ".length";
    }
    protected String simplify_name_impl() {
      return "(arrayLength " + sequence.simplify_name() + ")";
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
    protected String name_impl() {
      return function + "(" + argument.name() + ")";
    }
    protected String esc_name_impl() {
      return name_impl();
    }
    protected String simplify_name_impl() {
      return "(" + function + " " + argument.simplify_name() + ")";
    }
    public Object accept(Visitor v) {
      return v.visitFunctionOf(this);
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
    protected String name_impl() {
      return term.name() + ".class";
    }
    protected String esc_name_impl() {
      return "\\typeof(" + term.esc_name() + ")";
    }
    protected String simplify_name_impl() {
      return "(typeof " + term.simplify_name() + ")";
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
    protected String name_impl() {
      return "orig(" + term.name() + ")";
    }
    protected String esc_name_impl() {
      return "\\old(" + term.esc_name() + ")";
    }
    protected String simplify_name_impl() {
      return "(orig " + term.simplify_name() + ")";
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
    protected String name_impl() {
      return "post(" + term.name() + ")";
    }
    protected String esc_name_impl() {
      return "\\new(" + term.esc_name() + ")";
    }
    protected String simplify_name_impl() {
      return "(post " + term.simplify_name() + ")";
    }
    public Object accept(Visitor v) {
      return v.visitPoststate(this);
    }
  }

  /**
   * Returns a name for the decrement of this term, like "this-1".
   **/
  public VarInfoName applyDecrement() {
    return (new Decrement(this)).intern();
  }

  /**
   * One less than some other value
   **/
  public static class Decrement extends VarInfoName {
    public final VarInfoName term;
    public Decrement(VarInfoName term) {
      Assert.assert(term != null);
      this.term = term;
    }
    protected String name_impl() {
      return term.name() + "-1";
    }
    protected String esc_name_impl() {
      return term.esc_name() + "-1";
    }
    protected String simplify_name_impl() {
      return "(- " + term.simplify_name() + " 1)";
    }
    public Object accept(Visitor v) {
      return v.visitDecrement(this);
    }
  }

  /**
   * Returns a name for the increment of this term, like "this+1".
   **/
  public VarInfoName applyIncrement() {
    return (new Increment(this)).intern();
  }

  /**
   * One more than some other value
   **/
  public static class Increment extends VarInfoName {
    public final VarInfoName term;
    public Increment(VarInfoName term) {
      Assert.assert(term != null);
      this.term = term;
    }
    protected String name_impl() {
      return term.name() + "+1";
    }
    protected String esc_name_impl() {
      return term.esc_name() + "+1";
    }
    protected String simplify_name_impl() {
      return "(+ " + term.simplify_name() + " 1)";
    }
    public Object accept(Visitor v) {
      return v.visitIncrement(this);
    }
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
    protected String name_impl() {
      return name_impl("");
    }
    protected String name_impl(String index) {
      return term.name() + "[" + index + "]";
    }
    protected String esc_name_impl() {
      throw new UnsupportedOperationException("ESC cannot format an unquantified sequence of elements");
    }
    protected String esc_name_impl(String index) {
      return term.name() + "[" + index + "]";
    }
    protected String simplify_name_impl() {
      return "(elements " + term.simplify_name() + ")";
    }
    public Object accept(Visitor v) {
      return v.visitElements(this);
    }
  }

  /**
   * Returns a name for an element selected from a sequence, like
   * "this[i]"
   **/
  public VarInfoName applySubscript(VarInfoName index) {
    Assert.assert(index != null);
    // orig(a[]) . orig(index) -> orig(a[index])
    // orig(a[]) . index       -> orig(a[post(index)])
    // a[]       . orig(index) -> a[orig(index)]
    // a[]       . index       -> a[index]
    ElementsFinder finder = new ElementsFinder(this);
    Elements elems = finder.elems();
    Assert.assert(elems != null);
    if (finder.inPre()) {
      if (index instanceof Prestate) {
	index = ((Prestate) index).term; // #1
      } else {
	index = index.applyPoststate();  // #2
      }
    }
    Replacer r = new Replacer(elems, (new Subscript(elems, index)).intern());
    return r.replace(this).intern();
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
    protected String name_impl() {
      return sequence.name_impl(index.name());
    }
    protected String esc_name_impl() {
      return sequence.esc_name_impl(index.esc_name());
    }
    protected String simplify_name_impl() {
      return "(select " + sequence.simplify_name() + " " + index.simplify_name() + ")";
    }
    public Object accept(Visitor v) {
      return v.visitSubscript(this);
    }
  }

//    public static String addSubscript(String base, String subscript) {
//      // This logic may belong elsewhere (and the heuristics are incorrect anyway).
//      String suffix = "";
//      if (base.endsWith("[]")) {
//        base = base.substring(0, base.length()-2);
//      } else if (base.startsWith("orig(") && base.endsWith("[])")) {
//        // This is heuristic; I think it's probably OK.
//        base = base.substring(0, base.length()-3);
//        suffix = ")";
//      } else if (base.startsWith("\\old(") && base.endsWith(")")) {
//        // This is even more heuristic; I think it's probably also OK.
//        base = base.substring(0, base.length()-1);
//        suffix = ")";
//        // Even more heuristic; starting to get scary.
//        int subold = subscript.indexOf("\\old(");
//        while (subold != -1) {
//          int oldcloseparen = subscript.indexOf(")", subold);
//          Assert.assert(oldcloseparen != -1);
//          subscript = (subscript.substring(0, subold)
//                       + subscript.substring(subold+5, oldcloseparen)
//                       + subscript.substring(oldcloseparen+1));
//          subold = subscript.indexOf("\\old(");
//        }
//      } else {
//        throw new Error("what base? addSubscript(" + base + ", " + subscript + ")");
//      }
//      Assert.assert(subscript.indexOf("]") == -1);
//      Assert.assert(suffix.indexOf("]") == -1);
//      return base + "[" + subscript + "]" + suffix;
//    }

//    // The caller must assure that if the base is \old, so is the subscript.
//    public static String addSubscript_esc(String base, String subscript) {
//      // This logic may belong elsewhere (and the heuristics are incorrect anyway).
//      String suffix = "";
//      Assert.assert (! base.endsWith("[]"));

//      if (base.startsWith("\\old(") && base.endsWith(")")) {
//        base = base.substring(0, base.length()-1);
//        suffix = ")";
//        int subold = subscript.indexOf("\\old(");
//        while (subold != -1) {
//          int oldcloseparen = subscript.indexOf(")", subold);
//          Assert.assert(oldcloseparen != -1);
//          subscript = (subscript.substring(0, subold)
//                       + subscript.substring(subold+5, oldcloseparen)
//                       + subscript.substring(oldcloseparen+1));
//          subold = subscript.indexOf("\\old(");
//        }
//      }
//      Assert.assert(subscript.indexOf("]") == -1);
//      Assert.assert(suffix.indexOf("]") == -1);
//      return base + "[" + subscript + "]" + suffix;
//    }

  /**
   * Returns a name for a slice of element selected from a sequence,
   * like "this[i..j]".  If an endpoint is null, it means "from the
   * start" or "to the end".
   **/
  public VarInfoName applySlice(VarInfoName i, VarInfoName j) {
    // a[] -> a[index]
    // orig(a[]) -> orig(a[post(index)])
    ElementsFinder finder = new ElementsFinder(this);
    Elements elems = finder.elems();
    Assert.assert(elems != null);
    if (finder.inPre()) {
      if ((i != null) && !(i instanceof Prestate)) {
	i = i.applyPrestate();
      }
      if ((j != null) && !(j instanceof Prestate)) {
	j = j.applyPrestate();
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
    protected String simplify_name_impl() {
      return "(select " + sequence.simplify_name() + " " + "i..j)";
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
    public Object visitTypeOf(TypeOf o);
    public Object visitPrestate(Prestate o);
    public Object visitPoststate(Poststate o);
    public Object visitIncrement(Increment o);
    public Object visitDecrement(Decrement o);
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
    public Object visitTypeOf(TypeOf o) {
      return o.term.accept(this);
    }
    public Object visitPrestate(Prestate o) {
      return o.term.accept(this);
    }
    public Object visitPoststate(Poststate o) {
      return o.term.accept(this);
    }
    public Object visitIncrement(Increment o) {
      return o.term.accept(this);
    }
    public Object visitDecrement(Decrement o) {
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
      // I don't think we can get here in practice
      throw new UnsupportedOperationException();
    }
    public Object visitSlice(Slice o) {
      // I don't think we can get here in practice
      throw new UnsupportedOperationException();
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
    public Object visitIncrement(Increment o) {
      return (o == old) ? _new :
	((VarInfoName) super.visitIncrement(o)).applyIncrement();
    }
    public Object visitDecrement(Decrement o) {
      return (o == old) ? _new :
	((VarInfoName) super.visitDecrement(o)).applyDecrement();
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
	applySlice((VarInfoName) o.i.accept(this),
		   (VarInfoName) o.j.accept(this));
    }
  }

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
    public Object visitSubscript(Subscript o) {
      o.index.accept(this);
      // don't visit the sequence; it is fixed with an exact
      // subscript, so we don't want to include it in the results
      return o.sequence.term.accept(this);
    }
    public Object visitSlice(Slice o) {
      unquant.add(this);
      o.i.accept(this);
      o.j.accept(this);
      // don't visit the sequence; we will replace the slice with the
      // subscript, so we want to leave the elements alone
      return o.sequence.term.accept(this);
    }
  }

  public static class QuantHelper {

    // <root, needy, index> -> <root', lower, upper>
    public static VarInfoName[] replace(VarInfoName root, VarInfoName needy, VarInfoName index) {
      Assert.assert(root != null);
      Assert.assert(needy != null);
      Assert.assert(index != null);
      Assert.assert((needy instanceof Elements) || (needy instanceof Slice));

      VarInfoName root_prime, lower, upper;

      VarInfoName replace_with;
      if (needy instanceof Elements) {
	VarInfoName sequence = ((Elements) needy).term;
	root_prime = sequence.applySubscript(index);
	lower = parse("0");
	upper = sequence.applySize().applyDecrement();
      } else if (needy instanceof Slice) {
	Slice slice = (Slice) needy;
	root_prime = slice.sequence.applySubscript(index);
	lower = slice.i;
	upper = slice.j;
      } else {
	// placate javac
	return null;
      }

      Assert.assert(root_prime != null);
      Assert.assert(lower != null);
      Assert.assert(upper != null);

      return new VarInfoName[] { root_prime, lower, upper };
    }

    // <root*> -> <<root', index, lower, upper>?*>
    public static VarInfoName[][] quantify(VarInfoName[] roots) {
      Assert.assert(roots != null);

      VarInfoName[][] result = new VarInfoName[roots.length][];

      // a Set[Simples] representing all of the simple identifiers
      // used by these roots
      Set simples = new HashSet();

      // build helper for each roots
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
	if (uq.size() > 0) {
	  Assert.assert(uq.size() == 1, "We can only handle 1D arrays for now");
	  VarInfoName uq_elt = (VarInfoName) uq.get(0);

	  VarInfoName idx = (new Simple(String.valueOf(tmp++))).intern();
	  Assert.assert(!simples.contains(idx), "Index variable unexpectedly used");

	  result[i] = replace(roots[i], uq_elt, idx);
	}
      }

      return result;
    }

    // <root*> -> <string string*>
    public static String[] format_esc(VarInfoName[] roots) {
      Assert.assert(roots != null);
	
      VarInfoName[][] intermediate = quantify(roots);

      // build the "\forall ..." predicate
      String[] result = new String[roots.length+1];
      StringBuffer forall;
      {
	forall = new StringBuffer("\\forall int ");
	// i,j,...
	boolean comma = false;
	for (int i=0; i < intermediate.length; i++) {
	  if (intermediate[i] != null) {
	    if (comma) { forall.append(", "); }
	    forall.append(intermediate[i][1]); // 1 = index
	    comma = true;
	  }
	}
	forall.append("; (");
	boolean andand = false;
	// ai <= i && i <= bi && aj <= j && j <= bj && ...
	for (int i=0; i < intermediate.length; i++) {
	  if (intermediate[i] != null) {
	  if (andand) { forall.append(" && "); }
	    forall.append(intermediate[i][2].esc_name()); // 2 = lower
	    forall.append(" <= ");
	    forall.append(intermediate[i][1].esc_name()); // 1 = index
	    forall.append(" && ");
	    forall.append(intermediate[i][1].esc_name()); // 1 = index
	    forall.append(" <= ");
	    forall.append(intermediate[i][3].esc_name()); // 3 = lower
	    andand = true;
	  }
	}
	forall.append(") ==> ");
      }
      result[0] = forall.toString();

      // stringify the terms
      for (int i=0; i < roots.length; i++) {
	if (intermediate[i] != null) {
	  result[i+1] = intermediate[i][0].esc_name(); // 0 = root'
	} else {
	  result[i+1] = roots[i].esc_name();
	}
      }

      return result;
    }

  }

}


//    /**
//     * @return three-element array indicating upper and lower (inclusive)
//     * bounds of the range of this array variable, and a canonical element at
//     * index i.
//     **/
//    public String[] index_range() {
//      String working_name = name.esc_name();
//      String pre_wrapper = "";
//      String post_wrapper = "";
//      while (working_name.startsWith("\\") && working_name.endsWith(")")) {
//        int open_paren_pos = working_name.indexOf("(");
//        pre_wrapper += working_name.substring(0, open_paren_pos+1);
//        post_wrapper += ")";
//        working_name = working_name.substring(open_paren_pos+1, working_name.length()-1);
//      }
//      String minindex;
//      String maxindex;
//      String arrayname;
//      if (working_name.endsWith("[]")) {
//        minindex = "";
//        maxindex = "";
//        arrayname = working_name.substring(0, working_name.length()-2);
//      } else if (! working_name.endsWith("]")) {
//        minindex = "";
//        maxindex = "";
//        arrayname = working_name;
//      } else {
//        int open_bracket_pos = working_name.lastIndexOf("[");
//        arrayname = working_name.substring(0, open_bracket_pos);
//        String subscripts = working_name.substring(open_bracket_pos+1, working_name.length()-1);
//        int dots_pos = subscripts.indexOf("..");
//        if (dots_pos == -1) {
//          throw new Error("can't find \"..\" in " + working_name);
//        }
//        minindex = subscripts.substring(0, dots_pos);
//        maxindex = subscripts.substring(dots_pos+2);

//      }
//      if (minindex.equals("")) minindex = "0";
//      if (maxindex.equals("")) maxindex = arrayname + ".length-1";
//      String arrayelt = pre_wrapper + arrayname + "[i]" + post_wrapper;
//      // System.out.println("index_range: " + name + " ( = " + esc_name + " ) ");
//      // System.out.println("    => " + minindex + ", " + maxindex + ", " + arrayelt);

//      return new String[] { minindex, maxindex, arrayelt };
//    }


//    /**
//     * Return an array of two strings:
//     * an esc forall quantifier, and
//     * the expression for the element at index i of the array
//     **/
//    public String[] esc_forall() {
//      String[] index_range = index_range();
//      if (index_range.length != 3) {
//        throw new Error("index_range failed for " + name());
//      }
//      return new String[] {
//        "\\forall int i; (" + index_range[0] + " <= i && i <= " + index_range[1] + ") ==> ",
//        index_range[2],
//      };
//    }

//    /**
//     * Return an array of three strings:
//     * an esc forall quantifier, and
//     * the expressions for the elements at index i of the two arrays
//     **/
//    public static String[] esc_forall_2(VarInfo var1, VarInfo var2) {
//      String[] index_range1 = var1.index_range();
//      String[] index_range2 = var2.index_range();
//      Assert.assert(index_range1.length == 3, "no index_range: " + var1.name);
//      Assert.assert(index_range2.length == 3, "no index_range: " + var2.name);
//      String[] esc_forall1 = var1.esc_forall();
//      String elt2 = index_range2[2];
//      if (! index_range1[0].equals(index_range2[0])) {
//        int i_pos = elt2.lastIndexOf("[i]");
//        elt2 = elt2.substring(0, i_pos+2)
//          + "-" + index_range1[0] + "+" + index_range2[0] + "]"
//          + elt2.substring(i_pos+3);
//      }
//      return new String[] {
//        esc_forall1[0],
//        esc_forall1[1],
//        elt2,
//      };
//    }
