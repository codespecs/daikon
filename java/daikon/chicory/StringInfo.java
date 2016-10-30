package daikon.chicory;

import java.util.*;

/**
 * The StringInfo class is a subtype of DaikonVariableInfo used for variable types that can be
 * converted into strings (.toString()).
 */
public class StringInfo extends DaikonVariableInfo {
  public StringInfo(
      String theName, String typeName, String repTypeName, String receiverName, boolean arr) {
    super(theName, typeName, repTypeName, arr);

    function_args = receiverName;
  }

  @Override
  public Object getMyValFromParentVal(Object value) {
    if ((value == null) || (value instanceof NonsensicalObject)) {
      return NonsensicalObject.getInstance();
    }
    return value;
  }

  /** Returns a String that contains a string representation of val, used for dtrace information. */
  @SuppressWarnings("unchecked")
  public String getDTraceValueString(Object val) {
    if (isArray) {
      @SuppressWarnings("unchecked")
      List<?> valAsList = (List<?>) val;
      return getStringList(valAsList);
    } else {
      return getValueStringNonArr(val);
    }
  }

  /**
   * Returns a space-separated String of the elements in theValues. If theValues is null, returns
   * "null." If theValues is nonsensical, returns "nonsensical".
   *
   * @param theValues a list of values, each is a String or NonsensicalObject or NonsensicalList
   * @return a space-separated String of the elements in theValues
   */
  public static String getStringList(List<?> theValues) {
    if (theValues == null) {
      //buf.append("null");
      return "null" + DaikonWriter.lineSep + "1";
    }

    // assert !NonsensicalList.isNonsensicalList (theValues);
    if (NonsensicalList.isNonsensicalList(theValues)
    // How can this happen, given the declared type of theValues?
    // || theValues instanceof NonsensicalObject
    ) {
      //buf.append("nonsensical");
      return "nonsensical" + DaikonWriter.lineSep + "2";
    }

    StringBuffer buf = new StringBuffer();

    buf.append("[");
    for (Iterator<?> iter = theValues.iterator(); iter.hasNext(); ) {
      Object str = iter.next();

      if (str == null) {
        buf.append(str); // appends "null"
      } else if (str instanceof String) {
        buf.append("\"" + encodeString((String) str) + "\"");
      } else if (str instanceof NonsensicalObject || str instanceof NonsensicalList) {
        buf.append("nonsensical");
      } else {
        throw new Error("Impossible");
      }

      // Put space between elements in array
      if (iter.hasNext()) buf.append(" ");
    }
    buf.append("]");

    if (NonsensicalList.isNonsensicalList(theValues)) {
      buf.append(DaikonWriter.lineSep + "2");
    } else {
      buf.append(DaikonWriter.lineSep + "1");
    }

    return buf.toString();
  }

  /** Similar to showStringList, but used for non-array objects. */
  public String getValueStringNonArr(Object val) {
    String retString;

    if (val == null) {
      retString = "null";
    } else if (val instanceof NonsensicalObject) {
      retString = "nonsensical";
    } else {
      retString = getString((String) val);
    }
    retString += DaikonWriter.lineSep;

    if (val instanceof NonsensicalObject) {
      retString += ("2");
    } else {
      retString += ("1");
    }

    return retString;
  }

  //encodes a string: surrounds in quotes and removes line breaks
  private String getString(String stringRef) {
    return ("\"" + encodeString(stringRef) + "\"");
  }

  //removes endlines in string
  private static String encodeString(String input) {
    return Runtime.quote(input);
  }

  /** toString is a function */
  public VarKind get_var_kind() {
    return VarKind.FUNCTION;
  }

  /** Returns the name of this function */
  public String get_relative_name() {
    return "toString()";
  }

  public EnumSet<VarFlags> get_var_flags() {
    EnumSet<VarFlags> flags = super.get_var_flags().clone();
    flags.add(VarFlags.SYNTHETIC);
    flags.add(VarFlags.TO_STRING);
    return flags;
  }
}
