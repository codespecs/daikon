// "Ppt" stands for "Program point" (but is easier to type).

package daikon;

import java.util.*;
import java.io.Serializable;
import utilMDE.*;
import org.apache.log4j.Category;

// Types of Ppt (program point) objects:
//  Ppt:  abstract base class
//  PptTopLevel:  pointed to by top-level PptMap object.  Contains all variables
//    and all data for those variables.
//  PptConditional:  contains only value tuples satisfying some condition.
//    Probably doesn't make sense for parent to be a PptSlice.
//  PptSlice:  contains a subset of variables.  Probably doesn't contain its
//    own data structure with all the values, but depends on its parent
//    (which may be any type of Ppt except a PptSlice, which wouldn't
//    make good sense).
// Originally, both PptConditional and PptSlice were called "Views"; but
// presently (6/2002), only Slices are called Views.


// Ppt is an abstract base class rather than an interface in part because
// interfaces cannot declare member variables.  I suspect that using
// members directly will be more efficient than calling accessor
// functions such as num_vars() and var_info_iterator().

// The common interface for all Ppt objects.
public abstract class Ppt
  implements Serializable
{
  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20020122L;

  public final String name;
  public final PptName ppt_name;

  protected Ppt(String name) {
    this.name = name;
    ppt_name = new PptName(name);
  }

  public VarInfo[] var_infos;

  // [INCR] the views and cond_views fields and their corresponding
  // add/remove view methods were never used on a Ppt, always
  // on PptTopLevel, so move their declaration there.  (In fact they
  // don't make sense in PptSlice, so PptTopLevel is a better place
  // anyway.)

  /** Trim the collections used in this Ppt */
  public void trimToSize() {
    for (int i=0; i < var_infos.length; i++) {
      var_infos[i].trimToSize();
    }
  }

  /** Number of samples, not including missing values. */
  // public abstract int num_samples(); // [[INCR]]

  /**
   * Number of samples with mod bit set for at least one variable.  In
   * other words, this is recording tuplemod information, not regular mod
   * information.
   **/
  // public abstract int num_mod_non_missing_samples(); // [[INCR]]
  // public abstract int num_values(); // [[INCR]]
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
  /**
   * Give sample data to this ppt to process.
   * @return a List of Invariants that have weakened due to the
   * processing of the sample.
   **/
  abstract List add(ValueTuple vt, int count);

  protected static final List emptyList = new ArrayList();

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

  /**
   * The name of the function represented by this program point;
   * the program point name up to the ":::" separator.
   **/
  String fn_name() {
    return Ppt.fn_name(name);
  }

  /**
   * The name of the function represented by the specified program point name;
   * the program point name up to the ":::" separator.
   **/
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
      sb.append(infos[0].name.name());
      for (int i=1; i<infos.length; i++) {
        sb.append(", ");
        sb.append(infos[i].name.name());
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

  public VarInfo findVar(VarInfoName name) {
    for (int i=0; i<var_infos.length; i++) {
      if (name.equals(var_infos[i].name))
        return var_infos[i];
    }
    return null;
  }

  public VarInfo findVar_debugging(VarInfoName name) {
    for (int i=0; i<var_infos.length; i++) {
      System.out.println("Checking " + name.name() + " against " + var_infos[i].name.name());
      System.out.println("  Checking " + name + " against " + var_infos[i].name);
      if (name.equals(var_infos[i].name))
        return var_infos[i];
    }
    return null;
  }

  /**
   * This should never be used if the VarInfoName is available; it is a
   * convenience method used when parsing from programs or user input.
   **/
  public VarInfo findVar(String name) {
    return findVar(VarInfoName.parse(name));
  }

  /**
   * Return a list of all variables that appear in every Ppt in ppts.
   * The result does NOT include static constants, as it will be used to
   * index into ValueTuple, which omits static constants.
   * @param ppts a vector of PptTopLevel objects.
   **/
  public static final VarInfo[] common_vars(List ppts) {
    Vector result = new Vector();
    Assert.assertTrue(ppts.size() > 1);
    // First, get all the variables from the first program point.
    {
      PptTopLevel ppt = (PptTopLevel) ppts.get(0);
      VarInfo[] vars = ppt.var_infos;
      for (int i=0; i<vars.length; i++) {
        if (vars[i].isStaticConstant())
          continue;
        result.add(vars[i]);
      }
    }
    // Now, for each subsequent program point, remove variables that don't
    // appear in it.
    for (int i=1; i<ppts.size(); i++) {
      PptTopLevel ppt = (PptTopLevel) ppts.get(i);
      VarInfo[] vars = ppt.var_infos;
      // Remove from result any variables that do not occur in vars
      VARLOOP:
      for (int rindex=result.size()-1; rindex>=0; rindex--) {
        VarInfo rvar = (VarInfo) result.get(rindex);
        boolean found = false;
        for (int vindex=0; vindex<vars.length; vindex++) {
          VarInfo vvar = vars[vindex];
          if (rvar.isSimilarVarInfo(vvar)) {
            // do not remove
            continue VARLOOP;
          }
        }
        result.remove(rindex);
      }
    }
    return (VarInfo[]) result.toArray(new VarInfo[result.size()]);
  }

  public boolean containsVar (VarInfo vi) {
    // There's gotta be a faster way of doing this.  I don't want to
    // use a HashSet for var_infos because various things clobber
    // this.var_infos.
    int result = 0;
    for (int i = 0; i < var_infos.length; i++) {
      if (var_infos[i] == vi) {
        return true;
      }
    }
    return false;
  }

  // It might make more sense to put the sorting into
  // PptMap.sortedIterator(), for example, but it's in here for now

  // Check if o1 and o2 are both main exits (combined or only exits)
  // If so, compare their name without the EXIT[line]
  // If the name is the same, return 0, otherwise
  // Orders ppts by the name, except . and : are swapped
  //   so that Foo:::OBJECT and Foo:::CLASS are processed before Foo.method.
  public static final class NameComparator implements Comparator {
    public int compare(Object o1, Object o2) {

      if ((o1 instanceof PptTopLevel) && (o2 instanceof PptTopLevel)) {
        PptTopLevel p1 = (PptTopLevel) o1;
        PptTopLevel p2 = (PptTopLevel) o2;
        if (p1.ppt_name.isExitPoint() && p2.ppt_name.isExitPoint()) {
          // [INCR] if ((p1.combined_exit == null) && (p2.combined_exit == null)) {
            String fullName1 = p1.ppt_name.getNameWithoutPoint();
            String fullName2 = p2.ppt_name.getNameWithoutPoint();
            if (fullName1.equals(fullName2))
              return 0;
          // [INCR] }
        }
      }

      String name1 = ((Ppt) o1).name;
      String name2 = ((Ppt) o2).name;

      String swapped1 = swap(name1, '.', ':');
      String swapped2 = swap(name2, '.', ':');

      return swapped1.compareTo(swapped2);
    }

    static String swap(String s, char a, char b) {
      final char magic = '\255';
      return s.replace(a, magic).replace(b, a).replace(magic, b);
    }
  }

}
