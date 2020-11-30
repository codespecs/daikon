package daikon.chicory;

import org.checkerframework.checker.nullness.qual.Nullable;

/**
 * The {@link #getComparability} method calculates a comparability value.
 *
 * <p>This interface permits both {@code DCRuntime} and {@code DeclWriter} to call {@code
 * DeclWriter.printDecl}; otherwise, they would need to duplicate code.
 */
public interface ComparabilityProvider {
  /**
   * Calculate a comparability value.
   *
   * @param dv variable to calculate comparability for
   * @param compare_ppt corresponding ppt from DynComp input; or null if none
   * @return comparability value
   */
  public String getComparability(DaikonVariableInfo dv, DeclReader.@Nullable DeclPpt compare_ppt);
}
