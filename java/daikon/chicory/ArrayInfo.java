package daikon.chicory;

/**
 * The ArrayInfo class is a subtype of DaikonVariableInfo used for variable types that are arrays
 * (i.e., their name ends with "[]").
 */
public class ArrayInfo extends DaikonVariableInfo {
  /** Component type of the array */
  Class<?> array_type;

  /**
   * Constructs an ArrayInfo object with the specified name and type.
   *
   * @param theName the variable name. Should end with "[]".
   * @param array_type component type of the array
   */
  public ArrayInfo(String theName, Class<?> array_type) {

    super(theName, array_type.getName() + "[]", getRepName(array_type, true) + "[]", true);
    this.array_type = array_type;
  }

  @Override
  public Object getMyValFromParentVal(Object value) {
    if (value == null) {
      return NonsensicalList.getInstance();
    } else if (value instanceof NonsensicalObject) {
      return NonsensicalList.getInstance();
    } else {
      // The "child" value of an array is the actual list of array values
      // as opposed to just the "hashcode" object.
      return DTraceWriter.getListFromArray(value);
    }
  }

  public Class<?> getType() {
    return array_type;
  }

  @Override
  public VarKind get_var_kind() {
    return VarKind.ARRAY;
  }
}
