package daikon.chicory;

import java.util.*;

/*>>>
import org.checkerframework.checker.lock.qual.*;
import org.checkerframework.dataflow.qual.*;
*/

/**
 * NonsensicalList is similar to NonsensicalObject but it is used for arrays whose value is
 * nonsensical.
 */
// It's problematic to make this generic:  what would "get" return?
public class NonsensicalList extends AbstractList<Object> implements List<Object> {

  private NonsensicalList() {
    super();
  }

  public static NonsensicalList getInstance() {
    return theList;
  }

  /*@Pure*/
  public Object get(/*>>>@GuardSatisfied NonsensicalList this,*/ int index) {
    return NonsensicalObject.getInstance();
  }

  /*@Pure*/
  public int size(/*>>>@GuardSatisfied NonsensicalList this*/) {
    return -1;
  }

  /*@SideEffectFree*/
  public String toString(/*>>>@GuardSatisfied NonsensicalList this*/) {
    return "NonsensicalList";
  }

  public static boolean isNonsensicalList(Object obj) {
    return (obj instanceof NonsensicalList);
  }

  private static final NonsensicalList theList = new NonsensicalList();
}
