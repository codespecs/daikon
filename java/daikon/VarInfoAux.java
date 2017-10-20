package daikon;

import java.io.*;
import java.util.*;
import java.util.logging.Level;
import java.util.logging.Logger;

/*>>>
import org.checkerframework.checker.interning.qual.*;
import org.checkerframework.checker.lock.qual.*;
import org.checkerframework.checker.nullness.qual.*;
import org.checkerframework.dataflow.qual.*;
*/

/**
 * Represents additional information about a VarInfo that front ends tell Daikon. For example,
 * whether order matters in a collection. This is immutable and interned.
 */
public final class VarInfoAux implements Cloneable, Serializable {
  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20020614L;

  /** General debug tracer. */
  public static final Logger debug = Logger.getLogger("daikon.VarInfoAux");

  public static final String TRUE = "true";
  public static final String FALSE = "false";

  // The below are all the possible keys for the map, and values tend to be
  // binary.  So could we make it a packed binary array?

  // All of the keys below should be @KeyFor("this.map") but that isn't a valid expression.
  // See https://tinyurl.com/cfissue/877

  /**
   * Whether the elements in this collection are all the meaningful elements, or whether there is a
   * null at the end of this collection that ends the collection.
   */
  public static final String NULL_TERMINATING = "nullTerminating";

  /**
   * Whether this variable is a parameter to a method, or derived from a parameter to a method. By
   * default, if p is a parameter, then some EXIT invariants related to p aren't printed. However,
   * this does not affect the computation of invariants.
   *
   * <p>Frontends are responsible for setting if p is a parameter and if p.a is a parameter. In
   * Java, p.a is not a parameter, whereas in IOA, it is.
   */
  public static final String IS_PARAM = "isParam";

  /** Whether repeated elements can exist in this collection. */
  public static final String HAS_DUPLICATES = "hasDuplicates";

  /** Whether order matters. */
  public static final String HAS_ORDER = "hasOrder";

  /** Whether taking the size of this matters. */
  public static final String HAS_SIZE = "hasSize";

  /** Whether null has a special meaning for this variable or its members. */
  public static final String HAS_NULL = "hasNull";

  /** Indicates the minimum size of the vector, if there's any. */
  public static final String MINIMUM_LENGTH = "minlength";

  /** Indicates the maximum size of the vector, if there's any. */
  public static final String MAXIMUM_LENGTH = "maxlength";

  /** Indicates the minimum value of the scalar variable or the vector elements, if there's any. */
  public static final String MINIMUM_VALUE = "minvalue";

  /** Indicates the maximum value of the scalar variable or the vector elements, if there's any. */
  public static final String MAXIMUM_VALUE = "maxvalue";

  /**
   * Indicates the valid values (using string representation) for the elements of the vector, if
   * there's any. Values are enclosed in square brackets, and each element is quoted separately,
   * e.g.: ["a" "b"]. Parsing will be done upon call of the getList() method.
   *
   * @see #getList(String)
   */
  public static final String VALID_VALUES = "validvalues";

  /**
   * Whether this variable is an inline structure. By default, a variable is a reference to a
   * structure (class). If it is an inlined structure (or array), it doesn't make sense to look for
   * invariants over its hashcode. Front ends include references to inlined structures as variables
   * because some tools that follow daikon need other information about the variable.
   */
  public static final String IS_STRUCT = "isStruct";

  /** Whether this variable is known to be non-null, such as "this" in a Java program. */
  public static final String IS_NON_NULL = "isNonNull";

  /**
   * Java-specific. The package name of the class that declares this variable, if the variable is a
   * field. If it's not a field of some class, the value of this key is "no_package_name_string".
   */
  public static final String PACKAGE_NAME = "declaringClassPackageName";

  public static final String NO_PACKAGE_NAME = "no_package_name_string";

  /**
   * Return an interned VarInfoAux that represents a given string. Elements are separated by commas,
   * in the form:
   *
   * <p>x = a, "a key" = "a value"
   *
   * <p>Parse allow for quoted elements. White space to the left and right of keys and values do not
   * matter, but inbetween does.
   */
  public static /*@Interned*/ VarInfoAux parse(String inString) throws IOException {
    Reader inStringReader = new StringReader(inString);
    StreamTokenizer tok = new StreamTokenizer(inStringReader);
    tok.resetSyntax();
    tok.wordChars(0, Integer.MAX_VALUE);
    tok.quoteChar('\"');
    tok.whitespaceChars(' ', ' ');
    tok.ordinaryChar('[');
    tok.ordinaryChar(']');
    tok.ordinaryChars(',', ',');
    tok.ordinaryChars('=', '=');
    Map</*@Interned*/ String, /*@Interned*/ String> map = theDefault.map;

    String key = "";
    String value = "";
    boolean seenEqual = false;
    boolean insideVector = false;
    for (int tokInfo = tok.nextToken();
        tokInfo != StreamTokenizer.TT_EOF;
        tokInfo = tok.nextToken()) {
      @SuppressWarnings("interning") // initialization-checking pattern
      boolean mapUnchanged = (map == theDefault.map);
      if (mapUnchanged) {
        // We use default values if none are specified.  We initialize
        // here rather than above to save time when there are no tokens.

        map = new HashMap</*@Interned*/ String, /*@Interned*/ String>(theDefault.map);
      }

      /*@Interned*/ String token;
      if (tok.ttype == StreamTokenizer.TT_WORD || tok.ttype == '\"') {
        assert tok.sval != null
            : "@AssumeAssertion(nullness): representation invariant of StreamTokenizer";
        token = tok.sval.trim().intern();
      } else {
        token = ((char) tok.ttype + "").intern();
      }

      debug.fine("Token info: " + tokInfo + " " + token);

      if (token == "[") { // interned
        if (!seenEqual) throw new IOException("Aux option did not contain an '='");
        if (insideVector) throw new IOException("Vectors cannot be nested in an aux option");
        if (value.length() > 0) throw new IOException("Cannot mix scalar and vector values");

        insideVector = true;
        value = "";
      } else if (token == "]") { // interned
        if (!insideVector) throw new IOException("']' without preceding '['");
        insideVector = false;
      } else if (token == ",") { // interned
        if (!seenEqual) throw new IOException("Aux option did not contain an '='");
        if (insideVector) throw new IOException("',' cannot be used inside a vector");
        map.put(key.intern(), value.intern());
        key = "";
        value = "";
        seenEqual = false;
      } else if (token == "=") { // interned
        if (seenEqual) throw new IOException("Aux option contained more than one '='");
        if (insideVector) throw new IOException("'=' cannot be used inside a vector");
        seenEqual = true;
      } else {
        if (!seenEqual) {
          key = (key + " " + token).trim();
        } else if (insideVector) {
          value = value + " \"" + token.trim() + "\"";
        } else {
          value = (value + " " + token).trim();
        }
      }
    }

    if (seenEqual) {
      map.put(key.intern(), value.intern());
    }

    // Interning
    VarInfoAux result = new VarInfoAux(map).intern();
    assert interningMap != null
        : "@AssumeAssertion(nullness):  application invariant:  postcondition of intern(), which was just called";
    if (debug.isLoggable(Level.FINE)) {
      debug.fine("New parse " + result);
      debug.fine("Intern table size: " + interningMap.size());
    }
    return result;
  }

  /** Interned default options. */
  private static /*@Interned*/ VarInfoAux theDefault = new VarInfoAux().intern();

  /** Create a new VarInfoAux with default options. */
  public static /*@Interned*/ VarInfoAux getDefault() {
    return theDefault;
  }

  /** Map for interning. */
  private static /*@MonotonicNonNull*/ Map<VarInfoAux, /*@Interned*/ VarInfoAux> interningMap =
      // Static fields might not be initialized before static methods (which
      // call instance methods) are called, so don't bother to initialize here.
      null;

  /** Special handler for deserialization. */
  private /*@Interned*/ Object readResolve() throws ObjectStreamException {
    isInterned = false;
    Map</*@Interned*/ String, /*@Interned*/ String> newMap =
        new HashMap</*@Interned*/ String, /*@Interned*/ String>();
    for (String key : map.keySet()) {
      newMap.put(key.intern(), map.get(key).intern());
    }
    map = newMap;
    return this.intern();
  }

  /** Contains the actual hashMap for this. */
  private Map</*@Interned*/ String, /*@Interned*/ String> map;

  /** Whether this is interned. */
  private boolean isInterned = false;

  /** Make the default map here. */
  private VarInfoAux() {
    HashMap</*@Interned*/ String, /*@Interned*/ String> defaultMap =
        new HashMap</*@Interned*/ String, /*@Interned*/ String>();
    // The following are default values.
    defaultMap.put(HAS_DUPLICATES, TRUE);
    defaultMap.put(HAS_ORDER, TRUE);
    defaultMap.put(HAS_SIZE, TRUE);
    defaultMap.put(HAS_NULL, TRUE);
    defaultMap.put(NULL_TERMINATING, TRUE);
    defaultMap.put(IS_PARAM, FALSE);
    defaultMap.put(PACKAGE_NAME, NO_PACKAGE_NAME);
    defaultMap.put(IS_STRUCT, FALSE);
    defaultMap.put(IS_NON_NULL, FALSE);
    this.map = defaultMap;
    this.isInterned = false;
  }

  /** Create a new VarInfoAux with default options. */
  private VarInfoAux(Map</*@Interned*/ String, /*@Interned*/ String> map) {
    this.map = map;
    this.isInterned = false;
  }

  /** Creates and returns a copy of this. */
  // Default implementation to quiet Findbugs.
  /*@SideEffectFree*/
  @Override
  public VarInfoAux clone(
      /*>>>@GuardSatisfied VarInfoAux this*/) throws CloneNotSupportedException {
    VarInfoAux result = (VarInfoAux) super.clone();
    result.isInterned = false;
    return result;
  }

  /*@SideEffectFree*/
  @Override
  public String toString(/*>>>@GuardSatisfied VarInfoAux this*/) {
    return map.toString();
  }

  /*@Pure*/
  @Override
  public int hashCode(/*>>>@GuardSatisfied VarInfoAux this*/) {
    return map.hashCode();
  }

  /*@EnsuresNonNullIf(result=true, expression="#1")*/
  /*@Pure*/
  @Override
  public boolean equals(
      /*>>>@GuardSatisfied VarInfoAux this,*/
      /*@GuardSatisfied*/ /*@Nullable*/ Object o) {
    if (o instanceof VarInfoAux) {
      return equalsVarInfoAux((VarInfoAux) o);
    } else {
      return false;
    }
  }

  /*@Pure*/
  public boolean equalsVarInfoAux(
      /*>>>@GuardSatisfied VarInfoAux this,*/
      /*@GuardSatisfied*/ VarInfoAux o) {
    return this.map.equals(o.map);
  }

  // Two variables should not be put in the same equality set unless they have the same flags.  For
  // example, suppose that variable "this" is known to be non-null, but it is initially equal to
  // another variable.  Then NonZero will not be instantiated over "this", and when the equality set
  // is broken, there will be no NonZero invariant to copy to the other variable.  We only need to
  // check equality for every aux field that might affect methods such as instantiate_ok.
  @SuppressWarnings("keyfor") // https://tinyurl.com/cfissue/877
  /*@Pure*/
  public boolean equals_for_instantiation(
      /*>>>@GuardSatisfied VarInfoAux this,*/
      /*@GuardSatisfied*/ VarInfoAux o) {
    return this.getValue(HAS_DUPLICATES).equals(o.getValue(HAS_DUPLICATES))
        && this.getValue(HAS_ORDER).equals(o.getValue(HAS_ORDER))
        && this.getValue(HAS_SIZE).equals(o.getValue(HAS_SIZE))
        && this.getValue(HAS_NULL).equals(o.getValue(HAS_NULL))
        && Objects.equals(this.getValueOrNull(MINIMUM_LENGTH), o.getValueOrNull(MINIMUM_LENGTH))
        && Objects.equals(this.getValueOrNull(MAXIMUM_LENGTH), o.getValueOrNull(MAXIMUM_LENGTH))
        && Objects.equals(this.getValueOrNull(MINIMUM_VALUE), o.getValueOrNull(MINIMUM_VALUE))
        && Objects.equals(this.getValueOrNull(MAXIMUM_VALUE), o.getValueOrNull(MAXIMUM_VALUE))
        && Objects.equals(this.getValueOrNull(VALID_VALUES), o.getValueOrNull(VALID_VALUES))
        && this.getValue(IS_STRUCT).equals(o.getValue(IS_STRUCT))
        && this.getValue(IS_NON_NULL).equals(o.getValue(IS_NON_NULL));
  }

  /**
   * Returns canonical representation of this. Doesn't need to be called by outside classes because
   * these are always interned.
   */
  @SuppressWarnings({"interning", "cast"}) // intern method
  private /*@Interned*/ VarInfoAux intern() {

    for (/*@Interned*/ String key : map.keySet()) {
      assert key == key.intern();
      assert map.get(key) == map.get(key).intern();
    }

    if (this.isInterned) {
      return (/*@Interned*/ VarInfoAux) this; // cast is redundant (except in JSR 308)
    }

    // Necessary because various static methods call intern(), possibly before static field
    // interningMap's initializer would be executed.
    if (interningMap == null) {
      interningMap = new HashMap<VarInfoAux, /*@Interned*/ VarInfoAux>();
    }

    /*@Interned*/ VarInfoAux result;
    if (interningMap.containsKey(this)) {
      result = interningMap.get(this);
    } else {
      // Intern values in map
      interningMap.put(
          this, (/*@Interned*/ VarInfoAux) this); // cast is redundant (except in JSR 308)
      result = (/*@Interned*/ VarInfoAux) this; // cast is redundant (except in JSR 308)
      this.isInterned = true;
    }
    return result;
  }

  /**
   * Returns the integer value associated with a key, assuming it is defined. It is recommended to
   * check that it is defined first with {@link #hasValue(String)}.
   *
   * @throws RuntimeException if the key is not defined
   * @throws NumberFormatException if the value of the key cannot be parsed as an integer
   * @see #hasValue(String)
   */
  public int getInt(/*@KeyFor("this.map")*/ String key) {
    if (!hasValue(key)) {
      throw new RuntimeException(String.format("Key '%s' is not defined", key));
    }
    return Integer.parseInt(getValue(key));
  }

  /**
   * Returns the string array associated with a key, assuming it is defined. It is recommended to
   * check that it is defined first with {@link #hasValue(String)}.
   *
   * @throws RuntimeException if the key is not defined
   * @see #hasValue(String)
   */
  public String[] getList(/*@KeyFor("this.map")*/ String key) {
    try {
      if (!hasValue(key)) {
        throw new RuntimeException(String.format("Key '%s' is not defined", key));
      }
      final String sValue = getValue(key);
      StreamTokenizer tok = new StreamTokenizer(new StringReader(sValue));
      tok.quoteChar('"');
      tok.whitespaceChars(' ', ' ');
      ArrayList<String> lValues = new ArrayList<String>();

      int tokInfo = tok.nextToken();
      while (tokInfo != StreamTokenizer.TT_EOF) {
        if (tok.ttype != '"') {
          continue;
        }
        assert tok.sval != null
            : "@AssumeAssertion(nullness)"; // tok.type == '"' guarantees not null
        lValues.add(tok.sval.trim());
        tokInfo = tok.nextToken();
      }
      return lValues.toArray(new String[] {});
    } catch (IOException ex) {
      throw new RuntimeException(String.format("Parsing for key '%s' failed", key), ex);
    }
  }

  /** Returns the value for the given key, which must be present in the map. */
  public String getValue(
      /*>>>@GuardSatisfied VarInfoAux this,*/
      /*@KeyFor("this.map")*/ String key) {
    return map.get(key);
  }

  /** Returns the value for the given key, or null if it is not present. */
  public /*@Nullable*/ String getValueOrNull(
      /*>>>@GuardSatisfied VarInfoAux this,*/
      String key) {
    return map.get(key);
  }

  /** Return {@code true} if the value for the given key is defined, and {@code false} otherwise. */
  public boolean hasValue(String key) {
    return map.containsKey(key);
  }

  public boolean getFlag(/*@KeyFor("this.map")*/ String key) {
    assert map.containsKey(key);
    Object value = map.get(key);
    assert value == TRUE || value == FALSE;
    return value.equals(TRUE);
  }

  /** Return a new VarInfoAux with the desired value set. Does not modify this. */
  public /*@Interned*/ VarInfoAux setValue(String key, String value) {
    HashMap</*@Interned*/ String, /*@Interned*/ String> newMap =
        new HashMap</*@Interned*/ String, /*@Interned*/ String>(this.map);
    newMap.put(key.intern(), value.intern());
    return new VarInfoAux(newMap).intern();
  }

  /**
   * Converts the integer {@code value} to a String before invoking {@link #setValue(String,
   * String)}.
   */
  public VarInfoAux setInt(String key, int value) {
    return setValue(key, Integer.toString(value));
  }

  /** @see #NULL_TERMINATING */
  @SuppressWarnings("keyfor") // NULL_TERMINATING is always a key
  /*@Pure*/
  public boolean nullTerminating() {
    return getFlag(NULL_TERMINATING);
  }

  /** @see #IS_PARAM */
  @SuppressWarnings("keyfor") // IS_PARAM is always a key
  /*@Pure*/
  public boolean isParam() {
    return getFlag(IS_PARAM);
  }

  /** @see #PACKAGE_NAME */
  @SuppressWarnings("keyfor") // PACKAGE_NAME is always a key
  /*@Pure*/
  public boolean packageName() {
    return getFlag(PACKAGE_NAME);
  }

  /** @see #HAS_DUPLICATES */
  @SuppressWarnings("keyfor") // HAS_DUPLICATES is always a key
  /*@Pure*/
  public boolean hasDuplicates() {
    return getFlag(HAS_DUPLICATES);
  }

  /** @see #HAS_ORDER */
  @SuppressWarnings("keyfor") // HAS_ORDER is always a key
  /*@Pure*/
  public boolean hasOrder() {
    return getFlag(HAS_ORDER);
  }

  /** @see #HAS_SIZE */
  @SuppressWarnings("keyfor") // HAS_SIZE is always a key
  /*@Pure*/
  public boolean hasSize() {
    return getFlag(HAS_SIZE);
  }

  /** @see #HAS_NULL */
  @SuppressWarnings("keyfor") // HAS_NULL is always a key
  /*@Pure*/
  public boolean hasNull() {
    return getFlag(HAS_NULL);
  }

  /** @see #IS_STRUCT */
  @SuppressWarnings("keyfor") // IS_STRUCT is always a key
  /*@Pure*/
  public boolean isStruct() {
    return getFlag(IS_STRUCT);
  }

  /** @see #IS_NON_NULL */
  @SuppressWarnings("keyfor") // IS_NON_NULL is always a key
  /*@Pure*/
  public boolean isNonNull() {
    return getFlag(IS_NON_NULL);
  }
}
