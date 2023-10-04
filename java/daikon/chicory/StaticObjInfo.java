package daikon.chicory;

import org.checkerframework.checker.nullness.qual.Nullable;

/**
 * The StaticObjInfo class is a subtype of DaikonVariableInfo used as a root for static variables
 * within a class (which are the only variables visible to static methods). Nothing is printed for
 * this variable in either the decl or dtrace file, it exists only so that the static variables of a
 * class can be nested within it and not directly under the root.
 */
public class StaticObjInfo extends DaikonVariableInfo {
  /** The type of this variable. */
  public Class<?> type;

  /**
   * Create a new StaticObjInfo.
   *
   * @param type the variable's type
   */
  public StaticObjInfo(Class<?> type) {
    super("this", type.getName(), getRepName(type, false));
    this.type = type;
  }

  // See daikon.chicory.DaikonVariableInfo#getChildValue(java.lang.Object)
  @Override
  public @Nullable Object getMyValFromParentVal(Object val) {
    return null;
  }

  /** {@code this} is a top level variable. */
  @Override
  public VarKind get_var_kind() {
    return VarKind.VARIABLE;
  }
}
