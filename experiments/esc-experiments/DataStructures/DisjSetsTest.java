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
  }

  public void testRandoms() {
    int[] sizes = { 0, 1, 5, 100, 10 };
    for (int i=0; i < sizes.length; i++) {
      int size = sizes[i];
      System.out.println("size[" + i + "] = " + size);
      DisjSets ds = new DisjSets(size);
      observe(ds, size);
      for (int j=0; j<size; j++) {
	ds.unionCareful(rnd(size), rnd(size));
      }
      //ds.unionCareful(0, size-1);
    }
  }

  public void testRemoveFrivolous() {
    DisjSets ds;

    // we want 0 to be a lower bound
    for (int i=0; i<20; i++) {
      new DisjSets(0);
    }

    ds = new DisjSets(2000);
    ds.find(123);

    ds = new DisjSets(500);
    ds.unionCareful(67, 89);
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
