package six170;

// Taken from 6.170 Recitation 5, Spring 2001
// Author: Jeremy Nimmer

/**
 * A TinySet is a mutable set of integers drawn from the range [0..7]
 * @specfield elements : subset of { n : integer | 0 <= n <= 7 }
 */
public class TinySet {

  private int bits;

  // Rep Invariant:
  //   0 <= bits <= 255

  // Abstraction Function:  a = AF(t)
  //   a.elements = { n | ((t.bits & (1 &lt;&lt; n)) != 0) }

  /**
   * @effects Creates a new, empty TinySet (this.elements_post = {})
   */
  public TinySet() {
    this.bits = 0;
  }

  /**
   * @requires 0 <= n <= 7
   * @effects this.elements_post = this.elements U { n }
   */
  public void add(int n) {
    bits = bits | (1 << n);
  }

  /**
   * @requires 0 <= n <= 7
   * @effects this.elements_post = this.elements - { n }
   */
  public void remove(int n) {
    bits = bits & ~(1 << n);
  }

  /**
   * @requires 0 <= n <= 7
   * @returns true iff n in this.elements
   */
  public boolean contains(int n) {
    return (bits & (1 << n)) != 0;
  }

  /**
   * @requires other != null
   * @modifies this
   * @effects this.elements_post = this.elements ^ other.elements
   */
  public void intersect(TinySet other) {
    bits = bits & other.bits;
  }

  /**
   * @requires other != null
   * @modifies this
   * @effects this.elements_post = this.elements U other.elements
   */
  public void union(TinySet other) {
    bits = bits | other.bits;
  }

}
