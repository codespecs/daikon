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


// I want to indicate that every Ppt has a public member var_infos.  In
// order to do that, I really need an abstract base class from which all
// Ppt objects derive.  If I didn't insist on that, or was willing to
// make clients call a method to get the array, then I could use an
// interface instead.

// What motivates this is that it's a bit gross, and may be inefficient in
// time without saving any space, to use num_vars and var_info_iterator
// instead of

// The common interface for all Ppt objects.
public abstract class Ppt {

  public String name;

  public VarInfo[] var_infos;

  // Do I want two collections here (one for slices and one for conditional?
  // This used to be a WeakHashMap; now it is a HashSet, because I'm not sure
  // where else these would be referred to.
  //  * This is actually a set, but is implemented as a WeakHashMap because
  //  * that is the only weak collection and I want the objects weakly held.
  // I'm not sure why this was originally a HashSet, but that fact is now
  // taken advantage of in instantiate_views.
  /**
   * All the Views on this.
   * Provided so that this Ppt can notify them when significant events
   * occur, such as receiving a new value, deriving variables, or
   * discarding data.
   */
  HashSet views;

  // Temporarily have a separate collection for PptConditional views.
  // In the long run, I'm not sure whether the two HashSets will be
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

  PptSliceGeneric getView(VarInfo vi) {
    for (Iterator itor = views.iterator(); itor.hasNext(); ) {
      PptSliceGeneric slice = (PptSliceGeneric) itor.next();
      if ((slice.arity == 1) && slice.usesVar(vi))
        return slice;
    }
    return null;
  }

  PptSliceGeneric getView(VarInfo vi1, VarInfo vi2) {
    for (Iterator itor = views.iterator(); itor.hasNext(); ) {
      PptSliceGeneric slice = (PptSliceGeneric) itor.next();
      if ((slice.arity == 2) && slice.usesVar(vi1) && slice.usesVar(vi2))
        return slice;
    }
    return null;
  }

  public void clear_view_caches() {
    for (Iterator itor = views.iterator(); itor.hasNext(); ) {
      PptSliceGeneric slice = (PptSliceGeneric) itor.next();
      slice.clear_cache();
    }
  }

  /** Number of samples, not including missing values. */
  public abstract int num_samples();

  // Do these make sense?  They have to do with individual
  // variables, not entire samples.
  /** Number of samples with mod bit set. */
  public abstract int num_mod_non_missing_samples();
  public abstract int num_values();
  // public abstract int num_missing

  // Perhaps also optionally provide an array of the Ppts.
  // That might be more direct, and it's not all that much
  // space (no more than used up by the iterator, etc., and
  // it need only be updated once.

  // /** The number of variables at this Ppt. */
  // int num_vars();
  // /** An iterator over the variables at this Ppt. */
  // Iterator var_info_iterator();



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

  /**
   * Returns whether the ValueTuple appears in this map.
   * This can be more efficient than @link{count} because, for
   * views, it can stop after finding one occurrence.
   */
  abstract boolean contains(ValueTuple vt);

  /** Returns the number of occurrences of this ValueTuple in the map. */
  abstract int count(ValueTuple vt);

  /**
   * This oughtn't return a Set because it might be expensive to produce
   * such a thing (with no duplicates, that is).  And it might not even be
   * possible to return this set if we have already discarded some info.
   *
   * Maybe have another method that does return a Set.
   */
  abstract Iterator entrySet();

  String fn_name() {
    return Ppt.fn_name(name);
  }

  static String fn_name(String ppt_name) {
    int fn_name_end = ppt_name.indexOf(FileIO.ppt_tag_separator);
    if (fn_name_end == -1)
      return null;
    return ppt_name.substring(0, fn_name_end).intern();
  }

  /** Put a string representation of the variable names in the StringBuffer. */
  public void varNames(StringBuffer sb) {
    // System.out.println("this=" + this);
    // System.out.println("in varNames(): var_infos = " + var_infos + " = " + this.var_infos);
    // System.out.println(var_infos);
    // System.out.println(var_infos[0]);
    // System.out.println(var_infos[0].name);
    sb.append("(");
    sb.append(var_infos[0].name);
    // System.out.println("about to loop");
    for (int i=1; i<var_infos.length; i++) {
      sb.append(", ");
      sb.append(var_infos[i].name);
    }
    sb.append(")");
  }

  /** Return a string representation of the variable names. */
  public String varNames() {
    StringBuffer sb = new StringBuffer();
    varNames(sb);
    return sb.toString();
  }

  public VarInfo findVar(String name) {
    for (int i=0; i<var_infos.length; i++) {
      if (var_infos[i].name.equals(name))
        return var_infos[i];
    }
    return null;
  }

}
