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
  // Use .equals(), not ==, to copmare to these types.
  public static ProglangType INT = new ProglangType("int", 0);
  public static ProglangType INT_ARRAY = new ProglangType("int", 1);

  private String base;		// interned name of base type
  public String base() { return base; }
  private int dimensions;	// number of dimensions
  public int dimensions() { return dimensions; }
  public boolean isArray() { return dimensions > 0; }

  // Should I use a Set, possibly a HashSet, here?
  // Or a WeakHashMapWithHasher?  It depends on how big this can get...
  private static Vector all_known_types = new Vector();

  /**
   * No public constructor:  use parse() instead to get a canonical
   * representation.
   * basetype should be interned
   */
  private ProglangType(String basetype, int dims) {
    Assert.assert(basetype == basetype.intern());
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
    if (!(o instanceof ProglangType))
      return false;
    ProglangType plt = (ProglangType)o;

    // System.out.println("Calling equals: " + this + " " + o);
    return ((base == plt.base)
            && (dimensions == plt.dimensions));
  }

  // t_base should be interned
  private static ProglangType find(String t_base, int t_dims) {
    Assert.assert(t_base == t_base.intern());
    int num_known_types = 0;
    for (int i=0; i<num_known_types; i++) {
      ProglangType candidate = (ProglangType)all_known_types.elementAt(i);
      if ((t_dims == candidate.dimensions())
	  && (t_base == candidate.base()))
	return candidate;
    }
    return null;
  }

  private ProglangType intern() {
    return ProglangType.intern(this);
  }

  private static ProglangType intern(ProglangType t) {
    ProglangType candidate = find(t.base, t.dimensions);
    if (candidate != null)
      return candidate;
    all_known_types.add(t);
    return t;
  }

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

  // Primitive types
  public final static String TYPE_BOOLEAN = "boolean";
  public final static String TYPE_BYTE = "byte";
  public final static String TYPE_CHAR = "char";
  public final static String TYPE_DOUBLE = "double";
  public final static String TYPE_FLOAT = "float";
  public final static String TYPE_INT = "int";
  public final static String TYPE_LONG = "long";
  public final static String TYPE_SHORT = "short";

  // Nonprimitive types
  public final static String TYPE_ADDRESS = "address";
  // Hmmm, not sure about the difference between these two.
//   public final static String TYPE_JAVA_OBJECT = "java_object";
//   public final static String TYPE_OBJECT = "Object";
  // deprecated; use "address" in preference to "pointer"
  public final static String TYPE_POINTER = "pointer";
  public final static String TYPE_STRING = "String";

  // Given a string representation of a value (of the type represented by
  // this ProglangType), return the interpretation of that value.
  final Object parse_value(String value_) {
    String value = value_;
    // This only needs to deal with representation types, not with all
    // types in the underlying programming language.

    if (dimensions == 0) {
      if (base == TYPE_STRING) {
	if (value.startsWith("\"") && value.endsWith("\""))
	  value = value.substring(1, value.length()-1);
	return value.intern();
      } else if ((base == TYPE_ADDRESS) || (base == TYPE_POINTER)) {
	return Integer.valueOf(value, 16);
      // } else if ((base == TYPE_JAVA_OBJECT) || (base == TYPE_OBJECT)) {
      //   return Integer.valueOf(value.substring(value.indexOf('@')+1));
      } else if ((base == TYPE_CHAR) || (base == TYPE_INT)) {
	return Integer.valueOf(value);
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
      } else if (base == TYPE_BOOLEAN) {
	return new Boolean(value);
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

      if (base == TYPE_CHAR) {
	if ((value.length() > 1)
	    && value.startsWith("\"") && value.endsWith("\"")) {
	  // variable is a string
	  // turn it into a tuple of *numbers* instead.
	  // (Probably I want to retain it as a string; it's obscure
	  // as a sequence of numbers.  The advantage to a sequence of
	  // numbers is that already-written tests can work.)
	  if (value.startsWith("\"") && value.endsWith("\""))
	    value = value.substring(1, value.length()-1);

	  return value.toCharArray();
	} else {
	  throw new Error("To be written");
	}
      } else {
	// Deal with [] surrounding Java array output
	if (value.startsWith("[") && value.endsWith("]")) {
	  value = value.substring(1, value.length() - 1);
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
	if (base == TYPE_INT) {
	  int[] result = new int[len];
	  for (int i=0; i<len; i++)
	    result[i] = new Integer(value_strings[i]).intValue();
	  return result;
        // } else if ((base == TYPE_JAVA_OBJECT) || (base == TYPE_OBJECT)) {
        //   int[] result = new int[len];
        //   for (int i=0; i<len; i++)
        //     result[i] = Integer.parseInt(value.substring(value.indexOf('@')+1));
        //   return result;
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
      }
    } else if (dimensions == 2) {
      if (base == TYPE_CHAR) {
	// Array of strings
	throw new Error("To implement");
	// value = tuple(eval(value));
      } else {
	throw new Error("Can't parse a value of this type.");
      }
    } else {
      throw new Error("Can't parse a value of this type.");
    }
  }


}
