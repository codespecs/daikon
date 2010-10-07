package DataStructures;

class DisjSetsTest {

  // Test main; all finds on same output line should be identical
  public static void main( String [ ] args ) {
    testRandoms();
    testRemoveFrivolous();
    testSmall();
  }

  public static void testSmall() {
    DisjSets ds = new DisjSets(1);
    ds.find(0);
    ds.unionCareful(0, 0);
    ds = new DisjSets(3);
    ds.unionCareful(0, 1);
    ds.unionCareful(0, 2);
    for (int i=0; i<30; i++) { ds.find(1); ds.find(2); }
  }

  public static void testRandoms() {
    // Need to get lots of samples in the range 0..115
    int[] sizes = { 1, 5, 50, 10, 65, 25, 35 };
    for (int i=0; i < sizes.length; i++) {
      int size = sizes[i];
      System.out.println("size[" + i + "] = " + size);
      DisjSets ds = new DisjSets(size);
      observe(ds, size);
      for (int j=0; j<size*3; j++) {
	ds.unionCareful(rnd(size), rnd(size));
	ds.find(0);
      }
      //ds.unionCareful(0, size-1);
    }
  }

  public static void testRemoveFrivolous() {
    // we want 1 to be a lower bound on the constructor
    for (int i=0; i<20; i++) {
      new DisjSets(1);
    }
  }

  private static java.util.Random rnd = new java.util.Random(1000);
  private static int rnd(int lessThan) {
    return Math.abs(rnd.nextInt(lessThan));
  }

  private static void observe(DisjSets s, int size) {
    for (int i=0; i<size; i++)
      s.find(i);
  }
}
