package six170;

import java.util.*;

/**
 * A HuffmanCodec represents a particular Huffman encoding/decoding
 * scheme.  It is build from a list of symbols which are to be
 * encoding as well as their corresponding frequencies.
 */
public class HuffmanCodec {

  private Map encodingMap;
  private HuffmanTree decodingTree;

  /**
   * @requires: huffmanSymbols contains elements of type HuffmanSymbol
   *
   * @effects: constructs a new HuffmanCodec capable of encoding the
   * symbols contained in <code>huffmanSymbols</code>.
   */
  public HuffmanCodec(List huffmanSymbols) {
    decodingTree = buildDecodingTree(huffmanSymbols);
    encodingMap = buildEncodingMap(decodingTree);
  }

  /**
   * @requires: huffmanSymbols contains elements of type HuffmanSymbol
   *
   * @effects: builds an decoding tree of the symbols contained in
   * <code>huffmanSymbols</code> using a Huffman scheme based on the
   * frequencies reported by the symbols.
   */
  private static HuffmanTree buildDecodingTree(List huffmanSymbols) {
    // this method works by creating a list of subtrees and repeatedly
    // merging the two with the highest frequencies under one parent
    // until only one tree exists with all of the nodes as leaves.

    List nodeList = new ArrayList();

    Iterator symbolIterator = huffmanSymbols.iterator();

    while (symbolIterator.hasNext()) {
      HuffmanSymbol hs = (HuffmanSymbol) symbolIterator.next();
      nodeList.add(new HuffmanTree(hs));
    }

    while (nodeList.size() > 1) {
      HuffmanTree h1 = extractSmallest(nodeList);
      HuffmanTree h2 = extractSmallest(nodeList);

      nodeList.add(new HuffmanTree(h1, h2));
    }

    return (HuffmanTree) nodeList.get(0);
  }

  /**
   * @requires: nodeList contains elements of type HuffmanTree
   *
   * @modifies: nodeList
   *
   * @effects: removes and returns the element of
   * <code>nodeList</code> which has the smallest
   * <code>frequency</code> value.
   */
  private static HuffmanTree extractSmallest(List nodeList) {
    HuffmanTree smallest = null;
    Iterator iter = nodeList.iterator();
    while (iter.hasNext()) {
      HuffmanTree current = (HuffmanTree) iter.next();
      if (smallest == null ||
          current.frequency() < smallest.frequency()) {
        smallest = current;
      }
    }
    nodeList.remove(smallest);
    return smallest;
  }

  /**
   * @requires: decodingTree != null
   * @effects: builds a Map which maps Symbols from
   * <code>decodingTree</code> to the bit sequencies used to encode
   * them.  The values in the returned map will be
   * <code>java.util.List</code> objects containing
   * <code>Boolean</code> elements.  A true element maps to a right
   * hand subtree in <code>decodingTree</code> and a false element
   * maps to a left hand subtree.
   */
  private static Map buildEncodingMap(HuffmanTree decodingTree) {
    Map encodingMap = new LinkedHashMap();
    List stack = new ArrayList();
    addToMap(encodingMap, decodingTree, stack);
    return encodingMap;
  }

  /**
   * implements the recursive portion of buildEncodingMap
   */
  private static void addToMap(Map encodingMap,
                               HuffmanTree subTree,
                               List bitStack) {
    if (subTree.symbol() != null) {
      encodingMap.put(subTree.symbol(),
                      new ArrayList(bitStack));
    }

    if (subTree.left() != null) {
      bitStack.add(new Boolean(false));
      addToMap(encodingMap,
               subTree.left(),
               bitStack);
      bitStack.remove(bitStack.size()-1);
    }

    if (subTree.right() != null) {
      bitStack.add(new Boolean(true));
      addToMap(encodingMap,
               subTree.right(),
               bitStack);
      bitStack.remove(bitStack.size()-1);
    }
  }

  /**
   * @requires: inputStream != null && the elements of
   * <code>inputStream</code> are valid symbols for this codec.
   *
   * @effects: encodes <code>inputStream</code> into a bit stream
   * using the huffman encoding specified by this codec.
   *
   * @return a List containing Boolean objects representing the bits
   * of the encoded data.
   */
  public List encode(List inputStream) {
    List outputStream = new ArrayList();
    Iterator iter = inputStream.iterator();
    while (iter.hasNext()) {
      Object v = iter.next();
      outputStream.addAll((List)encodingMap.get(v)); // appends to the list
    }
    return outputStream;
  }

  /**
   * @requires: inputBitStream != null
   * @effects: decodes the bitStream using the huffman encoding
   * specified by this codec.
   * @return a List containing the symbols resulting from the decoding
   */
  public List decode(List inputBitStream) {
    int offset = 0;
    List outputList = new ArrayList();
    while (offset < inputBitStream.size()) {
      HuffmanTree subTree = decodingTree;

      while (offset < inputBitStream.size()) {
        if (((Boolean) inputBitStream.get(offset)).booleanValue()) {
          subTree = subTree.right();
        } else {
          subTree = subTree.left();
        }
        if (subTree == null) {
          throw new IllegalStateException("Unrecognized Symbol in stream");
        }
        if (subTree.symbol() != null) {
          outputList.add(subTree.symbol());
          break;
        }
        offset++;
      }
      offset++;
    }
    return outputList;
  }
}
