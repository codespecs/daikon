package daikon;

import java.util.*;

import com.oroinc.text.regex.*;

import utilMDE.*;


public class ExplicitVarComparability extends VarComparability {

  // A (explicit) VarComparable is:
  //  * for scalar types:  a tuple of comparable variables
  //  * for array types:  a tuple of "array", a list of variables comparable to
  //    the element type, and some number of lists variables comparable to the
  //    index types
  //  * If the alias slot is set to a String,
  //    then treat this variable as if it has the specified name.
  //    The point of "alias" is that an explicit VarComparability lists names of
  //    other varibles to which it is comparable; but we introduce new variables
  //    which we want to be considered comparable to others.  So we have to
  //    indicate what names those newly-introduced varaibles are comparable
  //    (actually, identical in terms of VarComparability) to.
  //  * special:  the string "always" means always comparable
  //    I represent this by base == null

  // All strings that appear in ExplicitVarComparability objects are interned.

  String[] base;
  String[][] indices;
  int dimensions;		// indicates how many of the indices are in use;
				// there may be more indices than this
  String alias;

  ExplicitVarComparability baseType; // also access through elementType() accessor
  ExplicitVarComparability[] indexTypes; // access through indexType() accessor

  ExplicitVarComparability cached_element_type;

  ExplicitVarComparability(String[] base_, String[][] indices_, int dimensions_,
			   String alias_) {
    base = base_;
    indices = indices_;
    dimensions = dimensions_;
    alias = alias_;
    indexTypes = new ExplicitVarComparability[dimensions];
  }

  public static VarComparability makeAlias(VarInfo vi) {
    return vi.comparability.makeAlias(vi.name);
  }
  public VarComparability makeAlias(String name) {
    return new ExplicitVarComparability(base, indices, dimensions, name);
  }

  public boolean alwaysComparable() {
    return (base == null);
  }

  public VarComparability elementType() {
    if (cached_element_type == null) {
      cached_element_type = new ExplicitVarComparability(base, indices, dimensions-1, alias);
    }
    return cached_element_type;
  }

  public VarComparability indexType(int dim) {
    if (indexTypes[dim] == null) {
      indexTypes[dim] = new ExplicitVarComparability(indices[dim], null, 0,
						((alias == null)
						 ? null
						 : VarNames.indexVar(alias, dim)));
    }
    return indexTypes[dim];
  }

  static VarComparability parse(String rep_, ProglangType vartype) {
    String rep = rep_;

    PatternMatcher re_matcher = Global.regexp_matcher;

    if (vartype.isArray()) {
      // The VarComparability is of the form
      //  (var1 var2 var3)[var1 var2 var3][var1 var2 var3]
      int dims = vartype.dimensions();
      // Permit "[]" to appear in variable names.
      re_matcher.matches(rep, dims_regexp(dims));
      MatchResult match = re_matcher.getMatch();
      // "+2" because "groups" includes the whole match (group 0).
      Assert.assert(match.groups() == dims+2);

      String base_raw = match.group(1);
      String[] indices_raw = new String[dims];
      for (int i=0; i<dims; i++) {
	indices_raw[i] = match.group(i+2);
      }

      // I could consider interning the arrays themselves; is that
      // worthwhile?  Probably not...

      String[] new_base = ws_split_to_interned_array(base_raw);
      String[][] new_indices = new String[dims][];
      for (int i=0; i<dims; i++) {
	new_indices[i] = ws_split_to_interned_array(indices_raw[i]);
      }

      return new ExplicitVarComparability(new_base, new_indices, dims, null);

    } else {
      // scalar variable
      // would this be faster?
      //   ((rep.charAt(0) == '(') && (rep.charAt(rep.length-1) == '('))
      if (rep.startsWith("(") && rep.endsWith(")")) {
	rep = rep.substring(1, rep.length()-1);
      }
      String[] new_base = ws_split_to_interned_array(rep);
      return new ExplicitVarComparability(new_base, null, 0, null);
    }
  }

  static Vector dims_regexps = new Vector();
  static Perl5Pattern dims_regexp(int dims) {
    if (dims_regexps.size() <= dims) {
      // add all the elements up to the missing one
      for (int i=dims_regexps.size(); i<=dims; i++) {
	StringBuffer regexp = new StringBuffer("^\\(([^)]*)\\)");
	for (int j=0; j<dims; j++)
	  regexp.append("\\[\\(?((?:[^\\]\\)]|\\[\\])*)\\)?\\]");
	regexp.append("$");
	try {
	  dims_regexps.add(Global.regexp_compiler.compile(regexp.toString()));
	} catch (Exception e) {
	  throw new Error(e.toString());
	}
      }
    }
    return (Perl5Pattern) dims_regexps.elementAt(dims);
  }



  // Split on whitespace and return an array of the resulting words, interned.
  static final String[] ws_split_to_interned_array(String s) {
    PatternMatcher re_matcher = Global.regexp_matcher;

    Vector vec = Util.split(re_matcher, Global.ws_regexp, s);
    String[] result = (String[]) vec.toArray(new String[0]);
    Intern.internStrings(result);
    return result;
  }


  // def array_derived_vars(array, indices):
  //     return ("%s-element" % array,) + tuple(map(lambda i,a=array: "%s-index%d" % (a,i), range(1,indices+1)))


  // This is the key function of the class.
  // I could also add a member version.
  static boolean compatible(String name1_, ExplicitVarComparability type1,
			    String name2_, ExplicitVarComparability type2) {
    if (type1.alwaysComparable() || type2.alwaysComparable())
      return true;

    if (type1.dimensions != type2.dimensions)
      return false;
    int dims = type1.dimensions;

    String name1 = name1_, name2 = name2_;
    if (type1.alias != null)
      name1 = type1.alias;
    if (type2.alias != null)
      name2 = type2.alias;

    Assert.assert((ArraysMDE.indexOf(type1.base, name2) == -1)
		  == (ArraysMDE.indexOf(type2.base, name1) == -1));
    if (ArraysMDE.indexOf(type1.base, name2) == -1)
      return false;

    // Check each dimension of array

    for (int i=0; i<dims; i++) {
      String indexvar1 = VarNames.indexVar(name1, i);
      String indexvar2 = VarNames.indexVar(name2, i);
      if (!compatible(indexvar1, type1.indexType(i), indexvar2, type2.indexType(i)))
	return false;
    }

    return true;
  }

}
