// This does not reproduce the problem!

package daikon;

/** A test class. */
public class AnnotateNullableSmall {
  /**
   * A test method.
   *
   * @param class_ppt its input
   * @return its output
   */
  public static String process_class(PptTopLevel class_ppt) {
    return String.format("%d", class_ppt.num_samples());
  }
}
