package utilMDE;

// From
//   Frequently Asked Questions (with answers) for Java programmers

/** Assertions:  test boolean expressions at runtime. */
public final class Assert {
  /** If false, the Assert class is disabled. */
  public final static boolean enabled = true;
  /** Throw AssertionException with the argument string if the condition does not hold. */
  public static final void assert(boolean b, String s) {
    if (enabled && !b)
      throw new AssertionException(s);
  }
  /** Throw AssertionException if the condition does not hold. */
  public static final void assert(boolean b) {
    assert(b, null);
  }
  /** Error class for failed assertions. */
  public static class AssertionException extends Error {
    public AssertionException(String s) {
      super(s);
    }
  }
}



//   6. (Sect. 17) How can I write C/C++ style assertions in Java?
//
//      [*] The two classes shown below provide an assertion facility in Java.
//      Set Assert.enabled to true to enable the assertions, and to false to
//      disable assertions in production code. The AssertionException is not
//      meant to be caught--instead, let it print a trace. Since the exception
//      is not meant to be caught, we just extend Error instead of
//      RuntimeException. As with RuntimeException, a method does not need to
//      declare that it throws Error. In addition programmers are less likely
//      to write "catch(Error) ..." than "catch(RuntimeException)".
//
//      With a good optimizing compiler there will be no run time overhead for
//      many uses of these assertions when Assert.enabled is set to false.
//      However, if the condition in the assertion may have side effects, the
//      condition code cannot be optimized away. For example, in the assertion
//
//           Assert.assert(size() <= maxSize, "Maximum size exceeded");
//
//      the call to size() cannot be optimized away unless the compiler can see
//      that the call has no side effects. C and C++ use the preprocessor to
//      guarantee that assertions will never cause overhead in production code.
//      Without a preprocessor, it seems the best we can do in Java is to write
//
//           Assert.assert(Assert.enabled && size() <= maxSize, "Too big");
//
//           Alternatively, use
//
//               if (Assert.enabled)
//                   Assert.assert( size() <= maxSize, "Too big" );
//
//      In this case, when Assert.enabled is false, the method call can always
//      be optimized away totally, even if it has side effects. The relevant
//      sections of the JLS are Section 13.4.8, final Fields and Constants and
//      Section 14.19, Unreachable Statements. 13.4.8 requires that primitive
//      constants ("a field that is static, final, and initialized with a
//      compile-time constant expression") be inlined. So everywhere
//      Assert.enabled is refered it is replaced at compile time with its
//      value. Writing:
//
//            if (Assert.enabled) Assert.assert(size() <= maxSize, "Too big");
//
//      is exactly the same as writing:
//
//            if (false) Assert.assert(size() <= maxSize, "Too big");
//
//      ... assuming Assert.enabled is false at compile time. Section 14.19
//      discusses compiling away such dead code. To sum up: the inlining of the
//      primitive constant is required by the spec. The subsequent optimization
//      of not generating code masked by (what turns into) an "if (false) ..."
//      is not required but is implemented by many existing Java compilers.
//
//           public class AssertionException extends Error {
//               public AssertionException(String s) {
//                  super(s);
//               }
//           }
//
//           final public class Assert {
//               public static final boolean enabled = true;
//               public static final void assert(boolean b, String s) {
//                   if (enabled && !b)
//                       throw new AssertionException(s);
//               }
//           }
