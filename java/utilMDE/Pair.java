package utilMDE;

/**
 * Simple pair class.
 **/
public class Pair<T1 extends /*@Nullable*/ Object,T2 extends /*@Nullable*/ Object> {
  public T1 a;
  public T2 b;

  public Pair(T1 a, T2 b) {
    this.a = a;
    this.b = b;
  }

  /** Factory method with short name and no need to name type parameters. */
  public static <A, B> Pair<A, B> of(A a, B b) {
    return new Pair<A, B>(a, b);
  }

  public String toString() {
    return "<" + String.valueOf(a) + "," + String.valueOf(b) + ">";
  }

  public boolean equals(/*@Nullable*/ Object obj) {
    if (obj instanceof Pair<?, ?>) { // generics are not checked at run time!
      @SuppressWarnings("unchecked")
      Pair<T1, T2> other = (Pair<T1, T2>) obj;
      boolean aEquals = ((this.a == other.a)
                         || (this.a != null && (this.a.equals(other.a))));
      boolean bEquals = ((this.b == other.b)
                         || (this.b != null && (this.b.equals(other.b))));
      return aEquals && bEquals;
    } else {
      return false;
    }
  }

  // If fields a and b were made final, then the hashcode could be cached.
  // (And if they aren't final, it's a bit odd to be calling hashCode.)
  // But then the class would not be useful for mutable pairs.
  public int hashCode() {
    return (((a == null) ? 0 : a.hashCode()) +
            ((b == null) ? 0 : b.hashCode()));
  }

}
