package daikon.dcomp;

import java.lang.annotation.*;

/**
 * Indicates that the annotated method is one that provides a DataFlow
 * summary.  Takes a single string argument that describes the method that
 * this routine replaces.  This argument is in the form type-name, where
 * type is either static or instance and name is the fully qualified
 * method name.  For example, "static-java.lang.Short.valueOf" or
 * "instance-java.lang.StringBuffer.append"
 *
 * The arguments of the annotated methods must match exactly the arguments
 * of the method it replaces.  If the replaced method is an instance method,
 * the first argument should be of the receiver type.
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.METHOD)
public @interface DFSum {
  String value();
}
