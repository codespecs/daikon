package six170;

/**
 * A HuffmanTree is used to represent the decoding tree.  The leaves
 * of the tree should each contain a reference to a HuffmanSymbol,
 * while the intermediate nodes do not.  The tree is a binary tree and
 * each node either has two children or none at all.
 */

public class HuffmanTree {

  private HuffmanSymbol symbol;
  private HuffmanTree left, right;

  // Representation invariant
  //    (symbol != null && left == null && right == null) ||
  //    (symbol == null && left != null && right != null)
  
  /**
   * @requires: symbol != null
   * @effects: creates a new HuffmanTree with only one leaf node
   * containing <code>symbol</code>.  
   */
  public HuffmanTree(HuffmanSymbol symbol) {
    if (symbol == null) {
      throw new IllegalArgumentException("null symbols are not allowed for leaf nodes");
    }
    this.symbol = symbol;
  }

  /**
   * @requires: left != null && right != null
   * @effects: consntructs a new HuffmanTree which contains
   * <code>left</code> as the left subtree and <code>right</code> as
   * the right subtree.
   */
  public HuffmanTree(HuffmanTree left, HuffmanTree right) {
    if (left == null || right == null) {
      throw new IllegalArgumentException("null subtrees are not allowed");
    }
    this.left = left;
    this.right = right;
  }

  /**
   * @return the total frequency of all HuffmanSymbols in this tree
   * (including all children).  
   */
  public int frequency() {
    if (symbol != null) {
      return symbol.frequency();
    } else {
      return left.frequency() + right.frequency();
    }
  }	

  /**
   * @return the symbol which is located at this node or null of this
   * node is not a leaf of the tree.  
   */
  public Object symbol() {
    if (symbol == null) {
      return null;
    } else {
      return symbol.symbol();
    }
  }

  /**
   * @return the left subtree of this tree.  If no such subtree exists
   * (it is already a leaf node) returns null.  
   */
  public HuffmanTree left() {
    return left;
  }

  /**
   * @return the right subtree of this tree.  If no such subtree exists
   * (it is already a leaf node) returns null.  
   */
  public HuffmanTree right() {
    return right;
  }

  private void checkRep() {
    if (symbol == null) {
      if (left == null ||
	  right == null) {
	throw new IllegalStateException("non-leaf node has null child");
      }
    } else {
      if (left != null ||
	  right != null) {
	throw new IllegalStateException("non-leaf node has symbol");
      }
    }
  }
}
