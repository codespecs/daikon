package daikon;

import org.checkerframework.checker.nullness.qual.NonNull;

public class AnnotateNullable2 {

  /** Returns a JVM signature for the method. */
  public static String jvm_signature(PptTopLevel ppt) {

    @SuppressWarnings("nullness") // Java method, so getMethodName() != null
    @NonNull String method = ppt.ppt_name.getMethodName();
    @SuppressWarnings("nullness") // Java method, so getSignature() != null
    @NonNull String java_sig = ppt.ppt_name.getSignature();
    String java_args = java_sig.replace(method, "");
    // System.out.printf("m/s/a = %s %s %s%n", method, java_sig, java_args);
    if (method.equals(ppt.ppt_name.getShortClassName())) {
      method = "<init>";
    }
    return method;
  }
}
