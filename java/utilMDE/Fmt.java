package utilMDE;

import java.util.*;

/**
 * Routines for doing simple string formatting similar to printf/sprintf in C
 * All of the arguments must be objects.  Primitive types can be used in
 * several ways:
 *
 *      - Add them to a string:  "" + i
 *      - create a wrapper object:  new Integer(i)
 *      - use a fmt routine to create a wrapper object: Fmt.i(i)
 */
public class Fmt {

  /**
   * Replaces each instance of %s in format with the corresponding
   * object in args and writes the result to System.out.  Each
   * argument is converted to a string with toString()
   */
  static public void pf (String format, Object[] args) {
    System.out.println (spf (format, args));
  }

  /**
   * Replaces each instance of %s in format with the corresponding
   * object in args and returns the result.  Each argument is
   * converted to a string with toString()
   */
  static public String spf (String format, Object[] args) {

    StringBuffer result = new StringBuffer(format.length() + args.length*20);

    int current_arg = 0;
    for (int i = 0; i < format.length(); i++) {
      char c = format.charAt(i);

      if (c != '%') {
        result.append (c);
      } else {
        i++;
        char cmd = format.charAt(i);
        if (cmd == '%')
          result.append ('%');
        else if (cmd == 's') {
          result.append (args[current_arg].toString());
          current_arg++;
        }
      }
    }

    if (current_arg != args.length)
      throw new RuntimeException (spf ("spf: only %s of %s arguments used up",
                                        i(current_arg), i(args.length)));

    return (result.toString());
  }

  /** convenience routine for new Integer(val) **/
  static public Integer i (int val) {
    return new Integer (val);
  }

  static public String spf (String format, Object arg1) {
    return (spf (format, new Object[] {arg1}));
  }

  static public String spf (String format, Object arg1, Object arg2) {
    return (spf (format, new Object[] {arg1, arg2}));
  }

  static public String spf (String format, Object arg1, Object arg2,
                            Object arg3) {
    return (spf (format, new Object[] {arg1, arg2, arg3}));
  }

  static public String spf (String format, Object arg1, Object arg2,
                           Object arg3, Object arg4) {
    return (spf (format, new Object[] {arg1, arg2, arg3, arg4}));
  }

  static public void pf (String format) {
    pf (format, new Object[0]);
  }

  static public void pf (String format, Object arg1) {
    pf (format, new Object[] {arg1});
    return;
  }

  static public void pf (String format, Object arg1, Object arg2) {
    pf (format, new Object[] {arg1, arg2});
    return;
  }

  static public void pf (String format, Object arg1, Object arg2,
                            Object arg3) {
    pf (format, new Object[] {arg1, arg2, arg3});
    return;
  }

  static public void pf (String format, Object arg1, Object arg2,
                           Object arg3, Object arg4) {
    pf (format, new Object[] {arg1, arg2, arg3, arg4});
    return;
  }

}
