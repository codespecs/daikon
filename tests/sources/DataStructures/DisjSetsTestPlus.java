// Created in 3 minutes (including Daikon runs); only took 1 minute to write the code.

package DataStructures;

public class DisjSetsTestPlus {

  // Test main; all finds on same output line should be identical
  public static void main(String[] args)
  {
    foo(10, 3);
    foo(30, 9);
    foo(50, 33);
    foo(99, 19);
    foo(128, 16);
  }

  public static void foo(int numElements, int numInSameSet) {
    DisjSets ds = new DisjSets( numElements );
    int set1, set2;

    for( int k = 1; k < numInSameSet; k *= 2 ) {
      for( int j = 0; j + k < numElements; j += 2 * k ) {
	set1 = ds.find( j );
	set2 = ds.find( j + k );
	ds.unionCareful( set1, set2 );
      }
    }

    for( int i = 0; i < numElements; i++ ) {
      System.out.print( ds.find( i )+ "*" );
      if( i % numInSameSet == numInSameSet - 1 )
	System.out.println( );
    }
    System.out.println( );
  }

}
