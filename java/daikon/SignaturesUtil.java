// TODO: When reflection-util >1.1.3 is released, delete this file and use
// Signatures.classGetNameToBinaryName() instead.

package daikon;

import org.checkerframework.checker.signature.qual.BinaryName;
import org.checkerframework.checker.signature.qual.ClassGetName;
import org.plumelib.reflection.Signatures;

/** Temporary utility about class name formats. */
public class SignaturesUtil {

  /** Do not instantiate. */
  private SignaturesUtil() {
    throw new Error("Do not instantiate");
  }

  /**
   * Convert a field descriptor to a binary name. For example, convert "[Ljava/util/Map$Entry;" to
   * "java.lang.Map$Entry[]" or "I" to "int".
   *
   * @param typename a field descriptor (the name of a type in JVML format)
   * @return the corresponding binary name
   */
  @SuppressWarnings("signature") // conversion routine
  public static @BinaryName String classGetNameToBinaryName(@ClassGetName String typename) {
    return Signatures.fieldDescriptorToBinaryName(typename);
  }
}
