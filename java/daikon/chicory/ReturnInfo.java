package daikon.chicory;

/** A subtype of DaikonVariableInfo used for variables that are returned from procedures. */
public class ReturnInfo extends DaikonVariableInfo {

  public ReturnInfo(Class<?> returnType) {
    super("return", stdClassName(returnType), getRepName(returnType, false));
  }

  @Override
  public Object getMyValFromParentVal(Object value) {
    throw new RuntimeException("Don't call getMyValFromParentVal on ReturnInfo objects");
  }

  @Override
  public VarKind get_var_kind() {
    return VarKind.RETURN;
  }
}
