package daikon;

import java.util.*;

import com.oroinc.text.regex.*;

import utilMDE.*;


// I could also consider using Class; however:
//  * that ties this to a Java front end, as Class can't represent types of
//    (say) C variables.  (not a compelling problem)
//  * that loads the class, which requies that all classes available at
//    runtime be available at inference time.  (not a compelling problem)
//  * Class does not represent inheritance (but I can do that myself);
//    and see isAssignableFrom, which might do all I need.
//  * Class has no "dimensions" field.  isArray() exists, however, as does
//    getComponentType().  I can always determine the number of dimensions
//    from the number of leading "[" in the name, and dimensions is only
//    really needed for parse_value.  parse_value can do literal checks and
//    elementType() can use the name, I suppose.  Or maybe try to
//    instantiate something, though that seems dicier.


// Problem:  this doesn't currently represent inheritance, coercability, or
// other relationships over the base types.

// integral_types = ("int", "char", "float", "double", "integral", "boolean")
// known_types = integral_types + ("pointer", "address")

public final class ProglangType implements java.io.Serializable {
  // Should I use a Set, possibly a HashSet, here?
  // Or a WeakHashMapWithHasher?  It depends on how big this can get...
  private static Vector all_known_types = new Vector();

  // Use == to compare, because ProglangType objects are interned.
  public final static ProglangType INT = ProglangType.intern("int", 0);
  public final static ProglangType LONG_PRIMITIVE = ProglangType.intern("long", 0);
  public final static ProglangType DOUBLE = ProglangType.intern("double", 0);
  public final static ProglangType STRING = ProglangType.intern("String", 0);
  public final static ProglangType INT_ARRAY = ProglangType.intern("int", 1);
  public final static ProglangType LONG_PRIMITIVE_ARRAY = ProglangType.intern("long", 1);
  public final static ProglangType DOUBLE_ARRAY = ProglangType.intern("double", 1);
  public final static ProglangType STRING_ARRAY = ProglangType.intern("String", 1);

  public final static ProglangType BOOLEAN = ProglangType.intern("boolean", 0);
  public final static ProglangType INTEGER = ProglangType.intern("Integer", 0);
  public final static ProglangType LONG_OBJECT = ProglangType.intern("Long", 0);

  public final static ProglangType VECTOR = ProglangType.intern("Vector", 0);
  public final static ProglangType OBJECT = ProglangType.intern("Object", 0);

  private String base;		// interned name of base type
  public String base() { return base; }
  private int dimensions;	// number of dimensions
  public int dimensions() { return dimensions; }
  public boolean isArray() { return dimensions > 0; }

  /**
   * No public constructor:  use parse() instead to get a canonical
   * representation.
   * basetype should be interned
   **/
  private ProglangType(String basetype, int dims) {
    Assert.assert(basetype == basetype.intern());
    base = basetype;
    dimensions = dims;
  }

  /**
   * This can't be a constructor because it returns a canonical
   * representation (that can be compared with ==), not necessarily a new
   * object.
   *  @argument rep  the name of the type, optionally suffixed by
   *  (possibly multiple) "[]"
   **/
  public static ProglangType parse(String rep) {
    String new_base = rep;
    int dims = 0;
    while (new_base.endsWith("[]")) {
      dims++;
      new_base = new_base.substring(0, new_base.length() - 2);
    }
    new_base = new_base.intern();
    return intern(new_base, dims);
  }

  /** Like parse, but does certain conversions for representation types. **/
  public static ProglangType rep_parse(String rep) {
    ProglangType candidate = parse(rep);
    if (candidate.base == "boolean") {  // interned
      return intern("int", candidate.dimensions);
    } else if (candidate.base == "float") { // interned
      return intern("double", candidate.dimensions);
    } else {
      return candidate;
    }
  }


  public boolean equals(Object o) {
    return this == o;
  }

  private boolean internal_equals(ProglangType other) {
    return ((base == other.base)
            && (dimensions == other.dimensions));
  }

  // t_base should be interned
  private static ProglangType find(String t_base, int t_dims) {
    Assert.assert(t_base == t_base.intern());

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

//     def compatible(self, other):
//         base1 = self.base
//         base2 = other.base
//         return ((self.dimensionality == other.dimensionality)
//                 and ((base1 == base2)
//                      or ((base1 == "integral") and (base2 in integral_types)) // interned strings
//                      or ((base2 == "integral") and (base1 in integral_types)))) // interned strings

  // This used to be private.  Why???
  public ProglangType elementType() {
    if (this == VECTOR)
      return OBJECT;
    if (dimensions == 0)
      throw new Error("Called elementType on non-array type " + format());
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
  // No need for the Integer versions; use Long instead.
  // final static Integer IntegerZero = Intern.internedInteger(0);
  // final static Integer IntegerOne = Intern.internedInteger(1);
  final static Long LongZero = Intern.internedLong(0);
  final static Long LongOne = Intern.internedLong(1);
  final static Double DoubleZero = Intern.internedDouble(0);
  final static Double DoubleNaN = new Double(Double.NaN);

  // Given a string representation of a value (of the type represented by
  // this ProglangType), return the interpretation of that value.
  // Canonicalize where possible.
  final Object parse_value(String value) {
    String value_ = value;      // for debugging, I suppose
    // This only needs to deal with representation types, not with all
    // types in the underlying programming language.

    if (dimensions == 0) {
      if (base == BASE_STRING) {
	if (value.startsWith("\"") && value.endsWith("\""))
	  value = value.substring(1, value.length()-1);
	return value.intern();
      } else if ((base == BASE_ADDRESS) || (base == BASE_POINTER)) {
	return Intern.intern(Long.valueOf(value, 16));
      } else if (base == BASE_CHAR) {
        // This will fail if the character is output as an integer
        // (as I believe the C front end does).
        char c;
        int index;
        if (value.length() == 1)
          c = value.charAt(0);
        else if ((value.length() == 2) && (value.charAt(0) == '\\'))
          c = UtilMDE.unquote(value).charAt(0);
        else
          throw new Error("Bad character: " + value);
        return Intern.internedLong(Character.getNumericValue(c));
      } else if (base == BASE_INT) {
        // Is this still necessary?
        // Hack for Java objects, fix later I guess.
        if (value.equals("null"))
          return LongZero;
        // Hack for booleans
        if (value.equals("false"))
          return LongZero;
        if (value.equals("true"))
          return LongOne;
	return Intern.internedLong(value);
      // } else if (base == BASE_BOOLEAN) {
      //   return new Boolean(value);
      } else if (base == BASE_DOUBLE) {
        // Is this still necessary?
        // Hack for Java objects, fix later I guess.
        if (value.equals("null"))
          return DoubleZero;
        if (value.equals("NaN"))
          return DoubleNaN;
	return Intern.internedDouble(value);
      // } else if (base == BASE_BOOLEAN) {
      //   return new Boolean(value);
      } else {
	throw new Error("unrecognized type " + base);
      }
    } else if (dimensions == 1) {
      // variable is an array

      value = value.trim();

      if (value.equals("null"))
        return null;

      // Try requiring the square brackets around arrays (permits
      // distinguishing between null and an array containing just null).
      Assert.assert(value.startsWith("[") && value.endsWith("]"));
      // Deal with [] surrounding Java array output
      if (value.startsWith("[") && value.endsWith("]")) {
        value = value.substring(1, value.length() - 1).trim();
      }

      // This isn't right if a string contains embedded spaces.
      // I could instead use StreamTokenizer.
      String[] value_strings
        = ((value.length() == 0)
           ? (new String[0])  // parens for Emacs indentation
           : (String[]) (Util.split(Global.regexp_matcher, Global.ws_regexp, value)).toArray(new String[0]));
      int len = value_strings.length;

      // This big if ... else should deal with all the primitive types --
      // or at least all the ones that can be rep_types.
      if (base == BASE_INT) {
        long[] result = new long[len];
        for (int i=0; i<len; i++) {
          if (value_strings[i].equals("null"))
            result[i] = 0;
          else
            result[i] = Long.parseLong(value_strings[i]);
        }
        return Intern.intern(result);
      } else if (base == BASE_DOUBLE) {
        double[] result = new double[len];
        for (int i=0; i<len; i++) {
          if (value_strings[i].equals("null"))
            result[i] = 0;
          else
            result[i] = Double.parseDouble(value_strings[i]);
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
      // such generality?
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
	throw new Error("Can't parse a value of type " + format());
      }
    } else {
      throw new Error("Can't parse a value of type " + format());
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
    return ((dimensions == 0) && baseIsIntegral());
  }

  public boolean isIndex() {
    return isIntegral();
  }

  public boolean baseIsFloat() {
    return ((base == BASE_DOUBLE) || (base == BASE_FLOAT));
  }

  public boolean isFloat() {
    return ((dimensions == 0) && baseIsFloat());
  }

  public boolean isObject() {
    return ((dimensions == 0)
            && (! baseIsIntegral())
            && (! baseIsFloat())
            && (! (base == BASE_BOOLEAN)));
  }

  public boolean comparable(ProglangType other) {
    if (this == other)          // ProglangType objects are interned
      return true;
    if (this.dimensions != other.dimensions)
      return false;
    boolean thisIntegral = this.baseIsIntegral();
    boolean otherIntegral = other.baseIsIntegral();
    if (thisIntegral && otherIntegral)
      return true;
    // Make Object comparable to everything
    if (((this.base == "Object") && (! otherIntegral)) // interned strings
        || ((other.base == "Object") && (! thisIntegral))) // interned strings
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
