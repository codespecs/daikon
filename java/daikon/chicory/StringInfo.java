package daikon.chicory;

import java.util.EnumSet;
import java.util.Iterator;
import java.util.List;
import java.util.StringJoiner;

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
  @Override
  public String getDTraceValueString(Object val) {
    if (isArray) {
      List<?> valAsList = (List<?>) val;
      return getStringList(valAsList);
    } else {
      return getValueStringNonArr(val);
    }
  }

  /**
   * Returns a space-separated String of the elements in theValues. If theValues is null, returns
   * "null." If theValues is nonsensical, returns "nonsensical". Also contains the modbit, on a
   * separate line.
   *
   * @param theValues a list of values, each is a String or NonsensicalObject or NonsensicalList
   * @return a space-separated String of the elements in theValues
   */
  public static String getStringList(List<?> theValues) {
    if (theValues == null) {
      // buf.append("null");
      return "null" + DaikonWriter.lineSep + "1";
    }

    // assert !NonsensicalList.isNonsensicalList (theValues);
    if (NonsensicalList.isNonsensicalList(theValues)
    // How can this happen, given the declared type of theValues?
    // || theValues instanceof NonsensicalObject
    ) {
      // buf.append("nonsensical");
      return "nonsensical" + DaikonWriter.lineSep + "2";
    }

    StringJoiner buf = new StringJoiner(" ", "[", "]");

    for (Iterator<?> iter = theValues.iterator(); iter.hasNext(); ) {
      Object str = iter.next();

      if (str == null) {
        buf.add("null");
      } else if (str instanceof String) {
        buf.add("\"" + encodeString((String) str) + "\"");
      } else if (str instanceof NonsensicalObject || str instanceof NonsensicalList) {
        buf.add("nonsensical");
      } else {
        throw new Error("Impossible");
      }
    }

    if (NonsensicalList.isNonsensicalList(theValues)) {
      return buf.toString() + DaikonWriter.lineSep + "2";
    } else {
      return buf.toString() + DaikonWriter.lineSep + "1";
    }
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
      retString += "2";
    } else {
      retString += "1";
    }

    return retString;
  }

  // encodes a string: surrounds in quotes and removes line breaks
  private String getString(String stringRef) {
    return ("\"" + encodeString(stringRef) + "\"");
  }

  // removes endlines in string
  private static String encodeString(String input) {
    return Runtime.quote(input);
  }

  /** toString is a function. */
  @Override
  public VarKind get_var_kind() {
    return VarKind.FUNCTION;
  }

  /** Returns the name of this function. */
  @Override
  public String get_relative_name() {
    return "toString()";
  }

  @Override
  public EnumSet<VarFlags> get_var_flags() {
    EnumSet<VarFlags> flags = super.get_var_flags();
    flags.add(VarFlags.SYNTHETIC);
    flags.add(VarFlags.TO_STRING);
    return flags;
  }
}
