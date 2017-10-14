package daikon.chicory;

import java.util.*;

/*>>>
import org.checkerframework.checker.nullness.qual.*;
*/

/**
 * The StaticObjInfo class is a subtype of DaikonVariableInfo used as a root for static variables
 * within a class (which are the only variables visible to static methods). Nothing is printed for
 * this variable in either the decl or dtrace file, it exists only so that the static variables of a
 * class can be nested within it and not directly under the root.
 */
public class StaticObjInfo extends DaikonVariableInfo {
  public Class<?> type;

  //     public StaticObjInfo(String typeName, String repTypeName)
  //     {
  //         super("this", typeName, repTypeName);
  //     }

  public StaticObjInfo(Class<?> type) {
    super("this", type.getName(), getRepName(type, false));
    this.type = type;
  }

  /* (non-Javadoc)
   * @see daikon.chicory.DaikonVariableInfo#getChildValue(java.lang.Object)
   */
  @Override
  public /*@Nullable*/ Object getMyValFromParentVal(Object val) {
    return null;
  }

  /** 'this' is a top level variable */
  @Override
  public VarKind get_var_kind() {
    return VarKind.VARIABLE;
  }
}
