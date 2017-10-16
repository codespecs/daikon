package daikon.chicory;

import java.util.*;

/*>>>
import org.checkerframework.checker.nullness.qual.*;
*/

/**
 * The ThisObjInfo class is a subtype of DaikonVariableInfo used for variable types which represent
 * the "this" object.
 */
public class ThisObjInfo extends DaikonVariableInfo {
  public Class<?> type;

  //     public ThisObjInfo(String typeName, String repTypeName)
  //     {
  //         super("this");
  //     }

  public ThisObjInfo(Class<?> type) {
    this("this", type);
  }

  /**
   * thisName is the name to be used to specify the variable. It's "this" except for outer classes,
   * as in "OuterClass.this".
   */
  public ThisObjInfo(String thisName, Class<?> type) {
    super(thisName, type.getName() + isNonNullParamString, getRepName(type, false));
    this.type = type;
  }

  /* (non-Javadoc)
   * @see daikon.chicory.DaikonVariableInfo#getChildValue(java.lang.Object)
   */
  @Override
  public /*@Nullable*/ Object getMyValFromParentVal(Object val) {
    throw new Error("shouldn't be called");
  }

  /** 'this' is a top level variable */
  @Override
  public VarKind get_var_kind() {
    return VarKind.VARIABLE;
  }

  /** Add IS_PARAM to list of variable flags, because the receiver "this" is a formal parameter. */
  @Override
  public EnumSet<VarFlags> get_var_flags() {
    // System.out.printf ("%s is a parameter%n", this);
    EnumSet<VarFlags> var_flags = super.get_var_flags().clone();
    var_flags.add(VarFlags.IS_PARAM);
    var_flags.add(VarFlags.NON_NULL);
    return var_flags;
  }
}
