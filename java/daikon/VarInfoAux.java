package daikon;

import org.apache.log4j.Category;

import java.util.*;
import java.io.*;
import org.apache.log4j.Category;


/**
 * Represents additional information about a VarInfo that frontends
 * tell Daikon.  For example, whether order matters in a collection.
 *
 *
 **/

public final class VarInfoAux
  implements Cloneable, Serializable
{
  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20020614L;

  /**
   * General debug tracer.
   **/
  public static final Category debug = Category.getInstance("daikon.VarInfoAux");

  /**
   * Whether the elements in this collection are all the meaningful
   * elements, or whether there is a null at the end of this
   * collection that ends the collection.
   **/
  public static final String NULL_TERMINATING = "nullTerminating";

  /**
   * Whether this variable is a parameter to a method.
   **/
  public static final String IS_PARAM = "isParam";

  /**
   * Whether repeated elements can exist in this collection.
   **/
  public static final String HAS_DUPLICATES = "hasDuplicates";

  /**
   * Whether order matters.
   **/
  public static final String HAS_ORDER = "hasOrder";

  /**
   * Whether taking the size of this matters.
   **/
  public static final String HAS_SIZE = "hasSize";

  /**
   * Whether null has a special meaning for this variable or its members.
   **/
  public static final String HAS_NULL = "hasNull";

  public static final String TRUE = "true".intern();


  /**
   * Interned default options.
   **/
  private static VarInfoAux theDefault = null;

  /**
   * Interned hashMaps.  Maps HashMaps to VarInfoAux.
   **/
  private static Map/*[HashMap->VarInfoAux]*/ theMap = null;

  /**
   * Contains the actual hashMap for this.
   **/
  private Map map;

  static {
    if (theMap == null) {
      theMap = (new HashMap());
      theDefault = new VarInfoAux();
      theMap.put (theDefault.map, theDefault);
    }
  }

  /**
   * Return an interned VarInfoAux that represents a given string.
   * Elements are separated by commas, in the form:
   * <li> x = a, y = b
   * Parse rules do not allow for quoted elements.  White space
   * to the left and right of keys and values do not matter, but
   * within
   *
   **/
  public static VarInfoAux parse (String inString) throws IOException {
    Reader inStringReader = new StringReader(inString);
    StreamTokenizer tok = new StreamTokenizer (inStringReader);
    tok.resetSyntax();
    tok.wordChars(0, Integer.MAX_VALUE);
    tok.quoteChar('\"');
    tok.ordinaryChars(',', ',');
    tok.ordinaryChars('=', '=');
    Map map = theDefault.map;

    String key = "";
    String value = "";
    boolean seenEqual = false;
    for (int tokInfo = tok.nextToken(); tokInfo != StreamTokenizer.TT_EOF;
	 tokInfo = tok.nextToken()) {
      if (map == theDefault.map) {
	// We use default values if none are specified We initialize
	// here rather than above to save time when there are no
	// tokens.

	map = new HashMap(theDefault.map);
      }

      String token;
      if (tok.ttype == tok.TT_WORD || tok.ttype == '\"') {
	token = tok.sval.trim().intern();
      } else {
	token = ((char) tok.ttype + "").intern();
      }

      debug.debug ("Token info: " + tokInfo + " " + token);

      if (token == ",".intern()) {
	if (!seenEqual)
	  throw new IOException ("Aux option did not contain an '='");
	map.put (key.intern(), value.intern());
	key = "";
	value = "";
	seenEqual = false;
      } else if (token == "=".intern()) {
	if (seenEqual)
	  throw new IOException ("Aux option contained more than one '='");
	seenEqual = true;
      } else {
	if (!seenEqual) {
	  key = (key + " " + token).trim();
	} else {
	  value = (value + " " + token).trim();
	}
      }
    }

    if (seenEqual) {
      map.put (key.intern(), value.intern());
    }

    // Interning
    VarInfoAux result;
    if (theMap.containsKey(map)) {
      result = (VarInfoAux) theMap.get(map);
    } else {
      VarInfoAux newAux = new VarInfoAux(map);
      theMap.put (map, newAux);
      result = newAux;
    }
    if (debug.isDebugEnabled()) {
      debug.debug("New parse " + result);
      debug.debug ("Intern table size: " + new Integer(theMap.size()));
    }
    return result;
  }


  /**
   * Create a new VarInfoAux with default optiosn.
   **/
  public static VarInfoAux getDefault () {
    return theDefault;
  }

  /**
   * Make the default map here.
   **/
  private VarInfoAux () {
    HashMap defaultMap = new HashMap();
    // The following are default values.
    defaultMap.put (HAS_DUPLICATES, "true");
    defaultMap.put (HAS_ORDER, "true");
    defaultMap.put (HAS_SIZE, "true");
    defaultMap.put (HAS_NULL, "true");
    defaultMap.put (NULL_TERMINATING, "true");
    this.map = defaultMap;
  }

  /**
   * Create a new VarInfoAux with default optiosn.
   **/
  private VarInfoAux (Map map) {
    this.map = map;
  }


  public String toString() {
    return map.toString();
  }


  public boolean getFlag(String key) {
    Object value = map.get(key);
    if (value == TRUE) {
      return true;
    } else {
      return false;
    }
  }

}
