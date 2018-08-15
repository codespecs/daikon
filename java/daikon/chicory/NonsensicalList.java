package daikon.chicory;

import java.util.AbstractList;
import java.util.List;
import org.checkerframework.checker.lock.qual.GuardSatisfied;
import org.checkerframework.dataflow.qual.Pure;
import org.checkerframework.dataflow.qual.SideEffectFree;

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

  @Pure
  @Override
  public Object get(@GuardSatisfied NonsensicalList this, int index) {
    return NonsensicalObject.getInstance();
  }

  @Pure
  @Override
  public int size(@GuardSatisfied NonsensicalList this) {
    return 0;
  }

  @SideEffectFree
  @Override
  public String toString(@GuardSatisfied NonsensicalList this) {
    return "NonsensicalList";
  }

  public static boolean isNonsensicalList(Object obj) {
    return (obj instanceof NonsensicalList);
  }

  private static final NonsensicalList theList = new NonsensicalList();
}
