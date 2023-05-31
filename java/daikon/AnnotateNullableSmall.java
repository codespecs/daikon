package daikon;

public class AnnotateNullableSmall {
  public static String process_class(PptTopLevel class_ppt) {
    return String.format("%d", class_ppt.num_samples());
  }
}
