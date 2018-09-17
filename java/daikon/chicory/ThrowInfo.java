package daikon.chicory;

/** A subtype of DaikonVariableInfo used for operands of throw statements. */
public class ThrowInfo extends DaikonVariableInfo {

  /**
   * Constructs an ThrowInfo object with the specified type.
   *
   * @param exceptionType the thrown exception
   */
  public ThrowInfo(Class<?> exceptionType) {
    super("exception", stdClassName(exceptionType), getRepName(exceptionType, false));
  }

  @Override
  public Object getMyValFromParentVal(Object value) {
    throw new RuntimeException("Don't call getMyValFromParentVal on ThrowInfo objects");
  }

  /** Returns the kind of this variable. We treat throws as a kind of RETURN. */
  public VarKind get_var_kind() {
    return VarKind.RETURN;
  }
}
