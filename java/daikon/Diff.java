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

}
