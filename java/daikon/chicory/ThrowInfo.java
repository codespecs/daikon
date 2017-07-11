package daikon.chicory;

/** A subtype of DaikonVariableInfo used for variables that are returned from procedures. */
public class ThrowInfo extends DaikonVariableInfo {
  // Under what circumstances is this null?  Maybe it's unused. -MDE
  //    Class<?> return_type = null;

  //     public ReturnInfo()
  //     {
  //         super("return");
  //     }

  public ThrowInfo(Class<?> exceptionType) {
    super("exception", stdClassName(exceptionType), getRepName(exceptionType, false));

    //        this.return_type = returnType;
  }

  @Override
  public Object getMyValFromParentVal(Object value) {
    throw new RuntimeException("Don't call getMyValFromParentVal on ThrowInfo objects");
  }

  public VarKind get_var_kind() {
    return VarKind.RETURN;
  }
}
