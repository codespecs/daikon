package DataStructures;

import java.util.*;

class DisjSetsTest
{
  // Test main; all finds on same output line should be identical
  public static void main( String [ ] args )
  {
    DisjSetsTest t = new DisjSetsTest();
    t.testFromDsaaMain();
    t.testRandoms();
    t.testRemoveFrivolous();
    t.testSmall();
  }

  public void testSmall() {
    DisjSets ds = new DisjSets(1);
    ds.find(0);
    ds.unionCareful(0, 0);
    ds = new DisjSets(2);
    ds.unionCareful(0, 1);
    ds.find(1);
  }

  public void testRandoms() {
    int[] sizes = { 0, 1, 5, 50, 10 };
    for (int i=0; i < sizes.length; i++) {
      int size = sizes[i];
      System.out.println("size[" + i + "] = " + size);
      DisjSets ds = new DisjSets(size);
      observe(ds, size);
      for (int j=0; j<size*3; j++) {
	ds.unionCareful(rnd(size), rnd(size));
      }
      //ds.unionCareful(0, size-1);
    }
  }

  public void testRemoveFrivolous() {
    DisjSets ds;

    // we want 0 to be a lower bound on the constructor
    for (int i=0; i<20; i++) {
      new DisjSets(0);
    }

    // we want elts to be maxed at length-1
    for (int i=5; i<10; i++) {
      ds = new DisjSets(i);
      for (int j=0; j<i-1; j++) {
	ds.unionCareful(i-1, j);
	observe(ds, i);
      }
    }
    
    ds = new DisjSets(200);
    ds.find(197);

    ds = new DisjSets(204);
    ds.unionCareful(191, 195);

    ds = new DisjSets(210);
    ds.unionDisjoint(200, 209);
    ds.find(209);
    ds.unionDisjoint(200, 0);
    ds.find(0);  

    // this kills some spurious array endpoint invariants
    ds = new DisjSets(210);
    ds.unionCareful(208, 209);
    ds.unionCareful(207, 208);
    ds.unionCareful(206, 207);
    ds.find(208);

    ds = new DisjSets(215);
    ds.unionCareful(214, 0);
    ds.unionCareful(213, 214);
    ds.unionCareful(214, 214);
  }

  private static Random rnd = new Random(1000);
  private static int rnd(int lessThan)
  {
    int result = Math.abs(rnd.nextInt()) % lessThan;
    if ((0 <= result) && (result < lessThan))
      return result;
    else
      throw new Error();
  }

  private static void observe(DisjSets s, int size)
  {
    for (int i=0; i<size; i++)
      s.find(i);
  }

  public void testFromDsaaMain() {
    int numElements = 128;
    int numInSameSet = 16;

    DisjSets ds = new DisjSets( numElements );
    int set1, set2;

    for( int k = 1; k < numInSameSet; k *= 2 )
      {
	for( int j = 0; j + k < numElements; j += 2 * k )
	  {
	    ds.unionCareful( j , j+ k);
	    // set1 = ds.find( j );
	    // set2 = ds.find( j + k );
	    // ds.union( set1, set2 );
	  }
      }

    for( int i = 0; i < numElements; i++ )
      {
	System.out.print( ds.find( i )+ "*" );
	if( i % numInSameSet == numInSameSet - 1 )
	  System.out.println( );
      }
    System.out.println( );
  }
}
