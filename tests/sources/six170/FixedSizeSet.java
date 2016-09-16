package six170;

// Taken from 6.170 Recitation 5, Spring 2001
// Author: Jeremy Nimmer
// Change from a bitwise rep to a boolean array

/**
 * A FixedSizeSet is a mutable set of integers drawn from the range [0..7]
 * @specfield elements : subset of { n : integer | 0 <= n <= 7 }
 */
public class FixedSizeSet {

  private boolean[] bits;

  // Rep Invariant:
  //   bits != null && bits.length == 8

  // Abstraction Function:  a = AF(t)
  //   a.elements = { n | bits[n] }

  /**
   * @effects Creates a new, empty FixedSizeSet (this.elements_post = {})
   */
  public FixedSizeSet() {
    this.bits = new boolean[8]; 
  }

  /**
   * @requires 0 <= n <= 7
   * @effects this.elements_post = this.elements U { n }
   */
  public void add(int n) {
    bits[n] = true;
  }

  /**
   * @requires 0 <= n <= 7
   * @effects this.elements_post = this.elements - { n }
   */
  public void remove(int n) {
    bits[n] = false;
  }

  /**
   * @requires 0 <= n <= 7
   * @returns true iff n in this.elements
   */
  public boolean contains(int n) {
    return bits[n];
  }

  /**
   * @requires other != null
   * @modifies this
   * @effects this.elements_post = this.elements ^ other.elements
   */
  public void intersect(FixedSizeSet other) {
    for (int i=0; i<bits.length; i++) {
      if (! other.bits[i])
	bits[i] = false;
    }
  }

  /**
   * @requires other != null
   * @modifies this
   * @effects this.elements_post = this.elements U other.elements
   */
  public void union(FixedSizeSet other) {
    for (int i=0; i<bits.length; i++) {
      if (other.bits[i])
	bits[i] = true;
    }
  }

}
