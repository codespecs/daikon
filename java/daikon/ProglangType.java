package daikon;

import java.util.*;

import com.oroinc.text.regex.*;

import utilMDE.*;


// I could also consider using Class; however:
//  * that ties this to Java
//  * that loads the class (not obviously a problem)

// Problem:  this doesn't currently represent inheritance, coercability, or
// other relationships over the base types.

// integral_types = ("int", "char", "float", "double", "integral", "boolean")
// known_types = integral_types + ("pointer", "address")

public final class ProglangType {
  // Should I use a Set, possibly a HashSet, here?
  // Or a WeakHashMapWithHasher?  It depends on how big this can get...
  private static Vector all_known_types = new Vector();

  // Use == to compare, because ProglangType objects are interned.
  public final static ProglangType INT = ProglangType.intern("int", 0);
  public final static ProglangType STRING = ProglangType.intern("String", 0);
  public final static ProglangType INT_ARRAY = ProglangType.intern("int", 1);
  public final static ProglangType STRING_ARRAY = ProglangType.intern("String", 1);

  public final static ProglangType BOOLEAN = ProglangType.intern("boolean", 0);
  public final static ProglangType INTEGER = ProglangType.intern("Integer", 0);

  private String base;		// interned name of base type
  public String base() { return base; }
  private int dimensions;	// number of dimensions
  public int dimensions() { return dimensions; }
  public boolean isArray() { return dimensions > 0; }

  /**
   * No public constructor:  use parse() instead to get a canonical
   * representation.
   * basetype should be interned
   */
  private ProglangType(String basetype, int dims) {
    Assert.assert(basetype == basetype.intern());
    // Don't do this here; it messes up interning.
    // // Oooh, hack.  Yuck.
    // if (basetype == "boolean")  // interned
    //   basetype = "int";
    base = basetype;
    dimensions = dims;
  }

  /**
   * This can't be a constructor because it returns a canonical
   * representation (that can be compared with ==), not necessarily a new
   * object.
   */
  public static ProglangType parse(String rep_) {
    String rep = rep_;
    int dims = 0;
    while (rep.endsWith("[]")) {
      dims++;
      rep = rep.substring(0, rep.length() - 2);
    }
    // don't permit "array of" as prefix.

//             if not (base in known_types):
//                 // hack for Java.  (I want to avoid the short names if possible.)
//                 if (base == "java.lang.String") or (base == "String"): // interned strings
//                     base = "char"
//                     dimensionality = dimensionality+1
//                 elif (base == "java.lang.Vector") or (base == "Vector"): // interned strings
//                     base = "java_object"
//                     dimensionality = dimensionality+1

    String new_base = rep.intern();
    return intern(new_base, dims);
  }

  public boolean equals(Object o) {
    return this == o;
  }

  private boolean internal_equals(ProglangType other) {
    return ((base == other.base)
            && (dimensions == other.dimensions));
  }

  // t_base should be interned
  private static ProglangType find(String t_base_, int t_dims) {
    String t_base = t_base_;
    Assert.assert(t_base == t_base.intern());
    if (t_base == "boolean") { // interned
      t_base = "int";
    }
    // System.out.println("ProglangType.find(" + t_base + ", " + t_dims + ")");
    // System.out.println("All known types:");
    // for (int i=0; i<all_known_types.size(); i++) {
    //   ProglangType k = (ProglangType)all_known_types.elementAt(i);
    //   System.out.println("  " + k.base() + ", " + k.dimensions());
    // }
    for (int i=0; i<all_known_types.size(); i++) {
      ProglangType candidate = (ProglangType)all_known_types.elementAt(i);
      Assert.assert(candidate.base() == candidate.base().intern());
      // System.out.println("ProglangType.find checking #" + i + ": " + candidate.base() + "," + candidate.dimensions());
      if ((t_dims == candidate.dimensions())
	  && (t_base == candidate.base())) {
        // System.out.println("ProglangType.find succeeded at index " + i);
	return candidate;
      }
      // System.out.println("Candidate (" + t_base + ", " + t_dims + ") failed: "
      //                    + ((t_dims == candidate.dimensions()) ? "same" : "different")
      //                    + " dimensions, "
      //                    + ((t_base == candidate.base()) ? "same" : "different")
      //                    + " base");
    }
    // System.out.println("ProglangType.find(" + t_base + ", " + t_dims + ") failed: ");
    return null;
  }

  // private ProglangType intern() {
  //   return ProglangType.intern(this);
  // }

  // We shouldn't even get to the point of interning; we should not
  // have created the ProglangType if it isn't canonical.
  // private static ProglangType intern(ProglangType t) {
  //   ProglangType candidate = find(t.base, t.dimensions);
  //   if (candidate != null)
  //     return candidate;
  //   all_known_types.add(t);
  //   return t;
  // }

  private static ProglangType intern(String t_base, int t_dims) {
    ProglangType result = find(t_base, t_dims);
    if (result != null) {
      return result;
    }
    result = new ProglangType(t_base, t_dims);
    all_known_types.add(result);
    return result;
  }

//     def __cmp__(self, other):
//         if (self.base == other.base):
//             return self.dimensionality - other.dimensionality
//         elif (self.base < other.base):
//             return -1
//         else:
//             return 1
//
//     def compatible(self, other):
//         base1 = self.base
//         base2 = other.base
//         return ((self.dimensionality == other.dimensionality)
//                 and ((base1 == base2)
//                      or ((base1 == "integral") and (base2 in integral_types)) // interned strings
//                      or ((base2 == "integral") and (base1 in integral_types)))) // interned strings

  // This used to be private.  Why???
  public ProglangType elementType() {
    if (dimensions == 0)
      throw new Error("Called elementType on a non-array");
    return ProglangType.intern(base, dimensions-1);
  }

  // These used to all be private.  Why??
  // (Actually, why are they public?  Is that necessary?)

  // Primitive types
  final static String BASE_BOOLEAN = "boolean";
  final static String BASE_BYTE = "byte";
  final static String BASE_CHAR = "char";
  final static String BASE_DOUBLE = "double";
  final static String BASE_FLOAT = "float";
  final static String BASE_INT = "int";
  final static String BASE_LONG = "long";
  final static String BASE_SHORT = "short";

  // Nonprimitive types
  final static String BASE_ADDRESS = "address";
  // Hmmm, not sure about the difference between these two.
//   final static String BASE_JAVA_OBJECT = "java_object";
//   final static String BASE_OBJECT = "Object";
  // deprecated; use "address" in preference to "pointer"
  final static String BASE_POINTER = "pointer";
  final static String BASE_STRING = "String";
  final static String BASE_INTEGER = "Integer";

  // avoid duplicate allocations
  final static Integer Zero = Intern.internedInteger(0);
  final static Integer One = Intern.internedInteger(1);

  // Given a string representation of a value (of the type represented by
  // this ProglangType), return the interpretation of that value.
  // Canonicalize where possible.
  final Object parse_value(String value_) {
    String value = value_;
    // This only needs to deal with representation types, not with all
    // types in the underlying programming language.

    if (dimensions == 0) {
      if (base == BASE_STRING) {
	if (value.startsWith("\"") && value.endsWith("\""))
	  value = value.substring(1, value.length()-1);
	return value.intern();
      } else if ((base == BASE_ADDRESS) || (base == BASE_POINTER)) {
	return Intern.intern(Integer.valueOf(value, 16));
      } else if ((base == BASE_CHAR) || (base == BASE_INT)) {
        // Is this still necessary?
        // Hack for Java objects, fix later I guess.
        if (value.equals("null"))
          return Zero;
        // Hack for booleans
        if (value.equals("false"))
          return Zero;
        if (value.equals("true"))
          return One;
	return Intern.internedInteger(value);
	// Old implementation
	// if type(value) == types.IntType:
	//     pass
	// elif type(value) == types.StringType:
	//     try:
	// 	value = int(value)
	//     except:
	// 	assert len(value) == 1
	// 	value = ord(value)
	// else:
	//     raise "Bad character value in data trace file: " + value
      // } else if (base == BASE_BOOLEAN) {
      //   return new Boolean(value);
      } else {
	throw new Error("unrecognized type " + base);
      }
    } else if (dimensions == 1) {
      // variable is an array

      // // Hack for null (missing) string:  turn it into empty string
      // if ((this_base_type == "char")
      //     && (this_var_type.dimensionality == 1)
      //     && (value == "null"))
      //   value = "\"\"";

      // if (base == BASE_CHAR) {
      //   if ((value.length() > 1)
      //       && value.startsWith("\"") && value.endsWith("\"")) {
      //     // variable is a string
      //     // turn it into a tuple of *numbers* instead.
      //     // (Probably I want to retain it as a string; it's obscure
      //     // as a sequence of numbers.  The advantage to a sequence of
      //     // numbers is that already-written tests can work.)
      //     if (value.startsWith("\"") && value.endsWith("\""))
      //       value = value.substring(1, value.length()-1);
      //     return value.toCharArray();
      //   } else {
      //     throw new Error("To be written");
      //   }
      // } else {

      value = value.trim();

      // Deal with [] surrounding Java array output
      if (value.startsWith("[") && value.endsWith("]")) {
        value = value.substring(1, value.length() - 1).trim();
      }

      // This isn't right if a string contains embedded spaces.
      // I could instead use StreamTokenizer.
      Vector value_strings_vector
        = ((value.length() == 0)
           ? (new Vector(0))  // parens for Emacs indentation
           : Util.split(Global.regexp_matcher, Global.ws_regexp, value));
      String[] value_strings = (String[]) value_strings_vector.toArray(new String[0]);
      int len = value_strings.length;

      // This big if ... else should deal with all the primitive types --
      // or at least all the ones that can be rep_types.
      if (base == BASE_INT) {
        int[] result = new int[len];
        for (int i=0; i<len; i++) {
          if (value_strings[i].equals("null"))
            result[i] = 0;
          else
            result[i] = Integer.parseInt(value_strings[i]);
        }
        return Intern.intern(result);
      } else if (base == BASE_STRING) {
        // First, intern each String in the array ...
        Intern.internStrings(value_strings);
        // ... then, intern the entire array, and return it
        return Intern.intern(value_strings);
      } else {
        throw new Error("Can't deal with array of base type " + base);
      }

      // This is a more general technique; but when will we need
      // anything general?
      // // not elementType() because that interns; here, there is no
      // // need to do the work of interning (I think)
      // ProglangType elt_type = elementType();
      // Object[] result = new Object[len];
      // for (int i=0; i<len; i++)
      //   result[i] = ***;

    } else if (dimensions == 2) {
      if (base == BASE_CHAR) {
	// Array of strings
	throw new Error("To implement");
	// value = tuple(eval(value));
      } else {
	throw new Error("Can't parse a value of this type");
      }
    } else {
      throw new Error("Can't parse a value of this type");
    }
  }

  public boolean baseIsIntegral() {
    return (// (base == BASE_BOOLEAN) ||
            (base == BASE_BYTE)
            || (base == BASE_CHAR)
            || (base == BASE_INT)
            || (base == BASE_LONG)
            || (base == BASE_SHORT)
            || (base == BASE_INTEGER));
  }

  public boolean isIntegral() {
    if (dimensions != 0)
      return false;

    if (baseIsIntegral()) {
      return true;
    }
    return false;
  }

  public boolean isIndex() {
    return isIntegral();

    // Old implementation
    // ProglangType type = var.type;
    // if (type.isIntegral() && (type != ProglangType.BOOLEAN))
    //   return true;
    // return false;
  }

  public boolean comparable(ProglangType other) {
    if (this == other)          // ProglangType objects are interned
      return true;
    if (this.dimensions != other.dimensions)
      return false;
    if (this.baseIsIntegral() && other.baseIsIntegral())
      return true;
    return false;
  }

  public String format() {
    if (dimensions == 0)
      return base;

    StringBuffer sb = new StringBuffer();
    sb.append(base);
    for (int i=0; i<dimensions; i++)
      sb.append("[]");
    return sb.toString();
  }

}
