// "Ppt" stands for "Program point" (but is easier to type).

package daikon;


import java.util.*;
import utilMDE.*;


// Types of Ppt (program point) objects:
//  Ppt:  abstract base class
//  PptTopLevel:  pointed to by top-level PptMap object.  Contains all variables
//    and all data for those variables.
// These Ppts are called "Views":
//  PptConditional:  contains only value tuples satisfying some condition.
//    Probably doesn't make sense for parent to be a PptSlice.
//  PptSlice:  contains a subset of variables.  Probably doesn't contain its
//    own data structure with all the values, but depends on its parent
//    (which may be any type of Ppt except a PptSlice, which wouldn't
//    make good sense).
// Actually, right now we assume all Views are Slices, which is a problem.


// Ppt is an abstract base class rather than an interface in part because
// interfaces cannot declare member variables.  I suspect that using
// members directly will be more efficient than calling accessor
// functions such as num_vars() and var_info_iterator().

// The common interface for all Ppt objects.
public abstract class Ppt implements java.io.Serializable {

  public final String name;
  public final PptName ppt_name;

  // It might make more sense to put the sorting into
  // PptMap.sortedIterator(), for example, but it's in here for now

  // Orders ppts by the name, except . and : are swapped
  //   so that Foo:::OBJECT and Foo:::CLASS are processed before Foo.method.
  // Also suffix "~" to ":::EXIT" to put it after the line-numbered exits.
  public static final Comparator NAME_COMPARATOR = new Comparator() {
      public int compare(Object o1, Object o2) {
	String name1 = ((Ppt) o1).name;
	String name2 = ((Ppt) o2).name;
	if (name1.endsWith(FileIO.exit_suffix))
	  name1 += "~";
	if (name2.endsWith(FileIO.exit_suffix))
	  name2 += "~";
	
	String swapped1 = swap(name1, '.', ':');
	String swapped2 = swap(name2, '.', ':');
	
	return swapped1.compareTo(swapped2);
      }
      
      static String swap(String s, char a, char b) {
	final char magic = '\255';
	return s.replace(a, magic).replace(b, a).replace(magic, b);
      }
    };

  protected Ppt(String name) {
    this.name = name;
    ppt_name = new PptName(name);
  }

  public VarInfo[] var_infos;

  // Do I want two collections here (one for slices and one for conditional?
  // This used to be a WeakHashMap; now it is a HashSet, because I'm not sure
  // where else these would be referred to.
  // // old comment:
  // //   This is actually a set, but is implemented as a WeakHashMap because
  // //   that is the only weak collection and I want the objects weakly held.
  // I'm not sure why this was originally a HashSet, but that fact is now
  // taken advantage of in instantiate_views, for fast checking of whether
  // an element is in the set.  (Simple ordering might have been enough there.)
  /**
   * All the Views on this.
   * Provided so that this Ppt can notify them when significant events
   * occur, such as receiving a new value, deriving variables, or
   * discarding data.
   **/
  HashSet views;

  // Temporarily have a separate collection for PptConditional views.
  // In the long run, I'm not sure whether the two collections will be
  // separate or not.
  // Right now, these are created only after all the values have been seen,
  // so I don't have to get too tense about installing them correctly and
  // iterating over them.  That should be fixed later.  For now, maybe have
  // two methods that add:  one that puts all the values in, one that doesn't.
  Vector views_cond;

  /** Add a new derived Ppt. */
  void addView(Ppt slice) {
    Vector slices = new Vector(1);
    slices.add(slice);
    addViews(slices);
  }
  /** This may be more efficient than repeated calls to addView. */
  abstract void addViews(Vector slices);


  abstract void removeView(Ppt slice);
  // A reasonable default implementation:
  // {
  //   boolean removed = views.remove(slice);
  //   Assert.assert(removed);
  // }

  /**
   * Typically one should use the dynamic_constant or canBeMissing slots,
   * which cache the invariants of most interest, instead of this function.
   **/
  public PptSlice1 getView(VarInfo vi) {
    for (Iterator itor = views.iterator(); itor.hasNext(); ) {
      PptSlice slice = (PptSlice) itor.next();
      if ((slice.arity == 1) && slice.usesVar(vi))
        return (PptSlice1) slice;
    }
    return null;
  }

  /**
   * Typically one should use the equal_to slot, which caches the
   * invariants of most interest, instead of this function.
   **/
  public PptSlice2 getView(VarInfo vi1, VarInfo vi2) {
    for (Iterator itor = views.iterator(); itor.hasNext(); ) {
      PptSlice slice = (PptSlice) itor.next();
      if ((slice.arity == 2) && slice.usesVar(vi1) && slice.usesVar(vi2))
        return (PptSlice2) slice;
    }
    return null;
  }

  public void clear_view_caches() {
    for (Iterator itor = views.iterator(); itor.hasNext(); ) {
      PptSlice slice = (PptSlice) itor.next();
      slice.clear_cache();
    }
  }

  /** Number of samples, not including missing values. */
  public abstract int num_samples();

  /**
   * Number of samples with mod bit set for at least one variable.  In
   * other words, this is recording tuplemod information, not regular mod
   * information.
   **/
  public abstract int num_mod_non_missing_samples();
  public abstract int num_values();
  // public abstract int num_missing

  // Perhaps also optionally provide an array of the Ppts.
  // That might be more direct, and it's not all that much
  // space (no more than used up by the iterator, etc., and
  // it need only be updated once.


  // This might include derived variables as well, or it might not.  Or
  // maybe it always does, and I elsewhere do something to extend the
  // ValueTuple appropriately.
  /** This might queue up data or it might process it immediately. */
  // This does something like the following, probably:
  //   this_var_values = this_fn_var_values[ppt_name];
  //   this_var_values[these_values] = this_var_values.get(these_values, 0) + 1;
  //   this_fn_samples[ppt_name] = this_fn_samples.get(ppt_name, 0) + 1;
  abstract void add(ValueTuple vt, int count);

  // This is rather confused.  I need a better notion of exactly what is
  // going on.
  // /**
  //  * Force processing of the data.  This might involve computing
  //  * invariants, notifying views of the arrival of data, etc.
  //  * A view might call this on its parent in order to update itself.
  //  * Or, it might be used before deriving new variables.
  //  */
  // abstract void process();

  // /**
  //  * Returns whether the ValueTuple appears in this map.
  //  * This can be more efficient than @link{count} because, for
  //  * views, it can stop after finding one occurrence.
  //  */
  // abstract boolean contains(ValueTuple vt);

  // /** Returns the number of occurrences of this ValueTuple in the map. */
  // abstract int count(ValueTuple vt);

  // /**
  //  * This oughtn't return a Set because it might be expensive to produce
  //  * such a thing (with no duplicates, that is).  And it might not even be
  //  * possible to return this set if we have already discarded some info.
  //  *
  //  * Maybe have another method that does return a Set.
  //  */
  // abstract Iterator entrySet();

  String fn_name() {
    return Ppt.fn_name(name);
  }

  static String fn_name(String ppt_name) {
    int fn_name_end = ppt_name.indexOf(FileIO.ppt_tag_separator);
    if (fn_name_end == -1)
      return null;
    return ppt_name.substring(0, fn_name_end).intern();
  }

  public static String varNames(VarInfo[] infos) {
    StringBuffer sb = new StringBuffer();
    sb.append("(");
    if (infos.length == 0) {
      sb.append("<implication slice>");
    } else {
      sb.append(infos[0].name);
      for (int i=1; i<infos.length; i++) {
        sb.append(", ");
        sb.append(infos[i].name);
      }
    }
    sb.append(")");
    return sb.toString();
  }

  // Cache, so the value doesn't have to be repeatedly recomputed.
  private String varNames = null;

  /** Return a string representation of the variable names. */
  public String varNames() {
    if (varNames == null) {
      varNames = varNames(var_infos);
    }
    return varNames;
  }

  public VarInfo findVar(String name) {
    return findVar_interned(name.intern());
  }

  public VarInfo findVar_interned(String name) {
    Assert.assert(Intern.isInterned(name));
    for (int i=0; i<var_infos.length; i++) {
      if (var_infos[i].name == name)
        return var_infos[i];
    }
    return null;
  }


  // Argument is a vector of PptTopLevel objects.
  // Result does NOT include static constants, as it will be used to
  // index into ValueTuple, which omits static constants.
  public static final VarInfo[] common_vars(Vector ppts) {
    Vector result = new Vector();
    Assert.assert(ppts.size() > 1);
    {
      PptTopLevel ppt = (PptTopLevel) ppts.elementAt(0);
      VarInfo[] vars = ppt.var_infos;
      for (int i=0; i<vars.length; i++) {
        if (vars[i].isStaticConstant())
          continue;

        result.add(vars[i]);
      }
    }
    for (int i=1; i<ppts.size(); i++) {
      PptTopLevel ppt = (PptTopLevel) ppts.elementAt(i);
      VarInfo[] vars = ppt.var_infos;
      // Remove from result any variables that do not occur in vars
      for (int rindex=result.size()-1; rindex>=0; rindex--) {
        VarInfo rvar = (VarInfo) result.elementAt(rindex);
        String rname = rvar.name;
        boolean found = false;
        for (int vindex=0; vindex<vars.length; vindex++) {
          VarInfo vvar = vars[vindex];
          if (rvar.compatible(vvar)) {
            // do not remove
            found = true;
            break;
          }
        }
        if (! found) {
          result.remove(rindex);
        }
      }
    }
    return (VarInfo[]) result.toArray(new VarInfo[] { });
  }


  /* It does not appear this is being used anywhere - mjh
  public static final class NameComparator implements Comparator {
    public int compare(Object o1, Object o2) {
      if (o1 == o2)
        return 0;
      PptSlice ppt1 = (PptSlice) o1;
      PptSlice ppt2 = (PptSlice) o2;
      // This class is used for comparing PptSlice objects.
      // (Should it be in PptSlice?)
      Assert.assert(ppt1.parent == ppt2.parent);
      return ppt1.name.compareTo(ppt2.name);
    }
  }
  */


}
