package six170;

/**
 * A HuffmanSymbol represents a single symbol in the unencoded string.
 * For instance one is using Huffman encoding to encode a sequence of
 * characters, each HuffmanSymbol would match to each letter of the
 * alphabet.
 */

public class HuffmanSymbol {
  
  private Object symbol;
  private int frequency;

  // Representation Invariant
  //    symbol != null && frequency >= 0

  /**
   * @requires: symbol != null && frequency >= 0
   * @effects: constructs a new HuffmanSymbol for the given symbol and
   * frequency.
   */
  public HuffmanSymbol(Object symbol, int frequency) {
    this.symbol = symbol;
    this.frequency = frequency;
  }

  /**
   * @return the symbol associated with this
   */
  public Object symbol() {
    return symbol;
  }

  /**
   * @return the frequency associated with this
   */
  public int frequency() {
    return frequency;
  }

  public String toString() {
    return "[HuffmanSymbol: symbol=" + symbol + " freq=" + frequency + "]";
  }

  private void checkRep() {
    if (symbol == null) {
      throw new IllegalStateException("HuffmanSymbol: null symbol");
    }
    if (frequency < 0) {
      throw new IllegalStateException("HuffmanSymbol: frequency less than zero");
    }
  }
}
