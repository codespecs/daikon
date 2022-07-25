package six170;

import java.util.*;

public class HuffmanMain {

  private final static String[] strings = new String[] {
    "massachusetts institute of technology",
    "the quick brown fox jumped over the small dog",
    "now is the time for all good men to come to the aid of their country",
    "I know some new tricks \\ Said the Cat in the Hat. \\ A lot of good tricks. \\ I will show them to you. \\ Your mother \\ Will not mind at all if I do. \\ - Dr. Seuss, The Cat In The Hat",
    "Keep in mind always the two constant Laws of Frisbee: 1. The most powerful force in the world is that of a discstraining to land under a car, just out of reach (this force is technically termed 'car suck').  2.  Never precede any maneuver by a comment more predictive than 'Watch this!'",
    "The goal of the Oxygen project is to create a system that fits this vision.  The Oxygen system will bring an abundance of computation and communication to users through natural spoken and visual interfaces, making it easy for them to collaborate, access knowledge, and automate repetitive tasks.  The Oxygen system must be pervasive--it must be everywhere, with every portal reaching into the same information base; embedded--it must live in our world, sensing and affecting it; nomadic--its users and computations must be free to move around according to their needs; eternal--it must never shut down or reboot; components may come and go in response to demand, errors, and upgrades, but Oxygen as a whole must be non-stop and forever. Oxygen will help people do more by doing less by blending into their lives, customizing itself to meet their needs, being accessible through natural perceptual interfaces, and making it easy for people to do the tasks they want."
  };

  public static void main (String args[]) {
    for (int i=0; i<strings.length; i++) {
      once(strings[i]);
    }
  }

  public static void once(String arg) {
    List huffmanSymbols = getHuffmanSymbols(arg);
    HuffmanCodec hc = new HuffmanCodec(huffmanSymbols);
    List bools = hc.encode(stringToList(arg));
    String encoding = booleanListToString(bools);
    String original = listToString(hc.decode(bools));
  }

  public static void original() {
    String mit = "massachusetts institute of technology";
    String mit2 = "mit";

    List huffmanSymbols = getHuffmanSymbols(mit);
    HuffmanCodec hc = new HuffmanCodec (huffmanSymbols);

    List b1 = hc.encode(stringToList(mit));
    List b2 = hc.encode(stringToList(mit2));

    System.out.println(booleanListToString(b1));
    System.out.println(booleanListToString(b2));

    System.out.println("");

    System.out.println(listToString(hc.decode(b1)));
    System.out.println(listToString(hc.decode(b2)));
  }

  /**
   * @requires: s != null
   * @effects: creates a List of the characters in the same order in
   * <code>s</code> as <code>Character</code> objects.
   */
  private static List stringToList(String s) {
    List l = new ArrayList();
    for (int i=0; i < s.length(); i++) {
      l.add(Character.valueOf(s.charAt(i)));
    }
    return l;
  }

  /**
   * @requires: l is a list containing Character objects
   * @return a string of the concatenation of the Character objects in
   * <code>l</code> into a String.
   */
  private static String listToString(List l) {
    StringBuffer sb = new StringBuffer();
    Iterator iter = l.iterator();
    while (iter.hasNext()) {
      sb.append(iter.next());
    }
    return sb.toString();
  }

  /**
   * @requires: l is a list containing Boolean objects
   * @return a string of the same length as <code>l</code> which
   * contains a 1 for each true Boolean in <code>l</code> and a 0 for
   * each false Boolean.
   */
  private static String booleanListToString(List l) {
    StringBuffer sb = new StringBuffer();
    Iterator iter = l.iterator();
    while (iter.hasNext()) {
      Boolean b = (Boolean) iter.next();
      if (b.booleanValue()) {
	sb.append("1");
      } else {
	sb.append("0");
      }
    }
    return sb.toString();
  }

  /**
   * @requires: s != null
   * @effects: computes the frequencies of characters in
   * <code>s</code>.  Returns a Map mapping Characters to Integers
   * where the Integer represent the number of times that Character
   * appears in <code>s</code>.
   */
  private static List getHuffmanSymbols(String s) {
    LinkedHashMap map = new LinkedHashMap();

    for (int i=0; i < s.length(); i++) {
      Character c = Character.valueOf(s.charAt(i));
      if (map.containsKey(c)) {
        Integer j = ((Integer) map.get(c));
        map.put(c, Integer.valueOf(j.intValue() + 1));
      } else {
        map.put(c, Integer.valueOf(1));
      }
    }

    List allSymbols = new ArrayList();

    Iterator keys = map.keySet().iterator();
    while (keys.hasNext()) {
      Character c = (Character) keys.next();
      int i = ((Integer) map.get(c)).intValue();

      allSymbols.add(new HuffmanSymbol(c, i));
    }

    return allSymbols;
  }
}
