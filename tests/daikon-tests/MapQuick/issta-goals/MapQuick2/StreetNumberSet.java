package MapQuick2;

import MapQuick.*;
import java.util.StringTokenizer;

/**
 * StreetNumberSet is immutible.
 */
public class StreetNumberSet {
  
  private final SimpleParitySet[] sets;
  
  /**
   * Creates a StreetNumberSet containing the numbers indicated in the
   * argument.
   *
   * @requires numbers != null && numbers is a space-free comma-delimited
   * list of one or more parity ranges.
   *
   * <p> A parity range is either a single nonnegative integer "n" or a
   * hyphen-separated pair of nonnegative integers "m-n", where m and n
   * have the same parity and m is no greater than n.  For instance, legal
   * arguments include "5", "22,253", "3-101", and "1914-1918,1939-1945".
   */
  public StreetNumberSet(String numbers) {
    StringTokenizer items = new StringTokenizer(numbers, ",", false);

    sets = new SimpleParitySet[items.countTokens()];

    int index = 0;
    
    while (items.hasMoreTokens()) {
      String item = items.nextToken();
      int dashLoc = item.indexOf('-');
      if (dashLoc == -1) {
        sets[index] = new SimpleParitySet(Integer.parseInt(item),
                                          Integer.parseInt(item));
      } else {
        sets[index] =
          new SimpleParitySet(Integer.parseInt(item.substring(0,dashLoc)),
                              Integer.parseInt(item.substring(dashLoc+1)));
      }
      index++;

    }
  }

  /**
   * Creates a new StreetNumberSet from the given array of sets.
   */
  private StreetNumberSet(SimpleParitySet[] sets) {
    this.sets = sets;
  }

  /** @return true iff i is in this */
  public boolean contains(int sn) {
    checkRep();
    for (int i=0; i<sets.length; i++) {
      if (sets[i].contains(sn)) return true;
    }
    return false;
  }

  /** @return the number of elements less than i in this */
  public int orderStatistic(int sn) {
    int result = 0;
    for (int i=0; i<sets.length; i++) {
      result += sets[i].orderStatistic(sn);
    }
    return result;
  }

  /**
   * @return the number of elements in this
   */
  public int size() {
    int result = 0;
    for (int i=0; i<sets.length; i++) {
      result += sets[i].size();
    }
    return result;
  }
  
  /**
   * @requires numbers be well formed as in the constructor.
   * @return a new StreetNumberSet which is the union
   *         of this and the numbers specified in the argument.
   */
  public StreetNumberSet union(String numbers) {
    StreetNumberSet givenNumbers = new StreetNumberSet(numbers);
    return union(givenNumbers);
  }

  /**
   * @return a new StreetNumberSet which is the union of this
   *         and the argument.
   */
  public StreetNumberSet union(StreetNumberSet numbers) {
    SimpleParitySet[] newSets =
      new SimpleParitySet[numbers.sets.length +
                         this.sets.length];
    System.arraycopy(numbers.sets, 0, newSets, 0, numbers.sets.length);
    System.arraycopy(sets, 0, newSets, numbers.sets.length, sets.length);
    return new StreetNumberSet(newSets);
  }


  private void checkRep() {
    try {
      for (int i=0; i<sets.length; i++) {
        sets[i].checkRep();
      }
    } catch (RuntimeException re) {
      System.err.println("Error in: " + toString());
      throw re;
    }

  }

  public String toString() {
    String result = "";
    try {
      for (int i=0; i<sets.length; i++) {
        result += sets[i].toString();
        if (i < sets.length-1) result += ",";
      }
    } catch (Throwable t) {
      result += "*Error*";
    }
    return result;
  }
    
  
  class SimpleParitySet {
    /**
     * @specfield: low  // lowest value in this
     * @specfield: high // highest value in this
     * @endspec
     * A SimpleParitySet contains all the numbers from low to high
     * of the same parity.  If low is even then it contains all the even
     * numbers from low to high inclusive.  If low is odd, then it contains
     * all of the odd numbers from low to high.  Low and high must either
     * both be even, or both be odd.
     */
    
    public final int low;
    public final int high;

    /**
     * @requires low and high are of the same parity
     * @effects Creates a new SimpleParitySet ranging from low to high.
     */
    public SimpleParitySet(int low, int high) {
      if ((low & 1) != (high & 1))
        throw new RuntimeException("low and high have different parities.");
      this.low = low;
      this.high = high;
    }
    
    /** @return true iff i is in this */
    public boolean contains(int i) {
      if ((low & 1) != (i & 1)) return false;
      if (i > high) return false;
      if (i < low) return false;
      
      return true;
    }
    
    /** @return the number of elements less than i in this */
    public int orderStatistic(int i) {
      if (i <= low) return 0;
      if (i > high) return (high - low) / 2 + 1;
      return ((i - low + 1) >> 1);
    }
    
    /** @return the number of elements in this */
    public int size() {
      return ((high-low) >> 1) + 1;
    }

    private void checkRep() {
    }
 
    public String toString() {
      return "" + low + "-" + high;
    }
    
    
  }
}

