package daikon;

import java.io.*;
import java.util.*;
import utilMDE.*;

public class Diff {

  /** Read two PptMap objects from their respective files and diff them. */
  public static void main(String[] args) throws FileNotFoundException, StreamCorruptedException, OptionalDataException, IOException, ClassNotFoundException {

    if (args.length != 2) {
      System.out.println("Supply two filename arguments to Diff.");
      System.exit(1);
    }

    FileInputStream istream1 = new FileInputStream(args[0]);
    ObjectInputStream oistream1 = new ObjectInputStream(istream1);
    FileInputStream istream2 = new FileInputStream(args[1]);
    ObjectInputStream oistream2 = new ObjectInputStream(istream2);

    PptMap all_ppts1 = (PptMap) oistream1.readObject();
    PptMap all_ppts2 = (PptMap) oistream2.readObject();

    System.out.println("Done.");
  }

  // Void return type:  for now it prints its output.
  public static void diffPptMap(PptMap pm1, PptMap pm2) {
    // I could have used set intersection/difference instead of the
    // OrderedPairIterator; but the latter works, too.
    Iterator itor1 = new TreeSet(pm1.nameStringSet()).iterator();
    Iterator itor2 = new TreeSet(pm2.nameStringSet()).iterator();
    for (Iterator opi = new OrderedPairIterator(itor1, itor2); opi.hasNext(); ) {
      OrderedPairIterator.Pair pair = (OrderedPairIterator.Pair) opi.next();
      if (pair.b == null) {
        System.out.println("Program point " + pair.a + " only in first set of invariants");
      } else if (pair.a == null) {
        System.out.println("Program point " + pair.b + " only in first set of invariants");
      } else {
        String ppt_name = (String) pair.a;
        Assert.assert(ppt_name.equals(pair.b));
        PptTopLevel ppt1 = (PptTopLevel) pm1.get(ppt_name);
        PptTopLevel ppt2 = (PptTopLevel) pm2.get(ppt_name);
        ppt1.diff(ppt2);
      }
    }
  }

}
