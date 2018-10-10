/*
 * Created on May 3, 2005
 *
 */
package daikon.chicory;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.List;

/**
 * The ListInfo class is a subtype of DaikonVariableInfo used for variable types that implement
 * {@code java.util.List}.
 */
public class ListInfo extends DaikonVariableInfo {

  private Class<? extends List<?>> listType;

  public ListInfo(String theName, Class<? extends List<?>> theType) {
    super(theName, theType.getName(), "hashcode[]", true);
    listType = theType;
  }

  // use the "toArray" method to get an array
  // convert the array to a List
  @Override
  public Object getMyValFromParentVal(Object value) {

    Method arrayMethod;
    try {
      arrayMethod = listType.getMethod("toArray", new Class<?>[0]);
    } catch (NoSuchMethodException e) {
      throw new Error(
          listType.getName()
              + " seems to implement java.util.List, but method toArray() not found");
    }

    Object arrayVal;

    if (value != null && !(value instanceof NonsensicalObject)) {

      // TODO why can't we just cast to List and call toArray directly?

      try {
        arrayVal = arrayMethod.invoke(value, new Object[0]);
      } catch (IllegalArgumentException e1) {
        throw new Error(e1);
      } catch (IllegalAccessException e1) {
        throw new Error(e1);
      } catch (InvocationTargetException e1) {
        // We used to check for java.util.ConcurrentModificationException, but
        // now beleive than any InvocationTargetException should not fail and
        // thus should return Nonsensical.
        // Possibly, this should be extended to all exceptions.
        System.err.println(
            "Detected a InvocationTargetException in: " + listType.getName() + " " + getName());
        arrayVal = NonsensicalObject.getInstance();
      }
    } else {
      arrayVal = NonsensicalObject.getInstance();
    }

    @SuppressWarnings("nullness") // We just verified (or set) arrayVal in code above.
    Object tmp = DTraceWriter.getListFromArray(arrayVal);
    return tmp;
  }

  /** Lists are arrays from Daikon's point of view. */
  @Override
  public VarKind get_var_kind() {
    return VarKind.ARRAY;
  }
}
