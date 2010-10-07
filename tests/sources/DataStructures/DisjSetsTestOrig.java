package DataStructures;

public class DisjSetsTestOrig {

  // Test main; all finds on same output line should be identical
  public static void main(String[] args )
  {
    int numElements = 128;
    int numInSameSet = 16;

    DisjSets ds = new DisjSets( numElements );
    int set1, set2;

    for( int k = 1; k < numInSameSet; k *= 2 ) {
      for( int j = 0; j + k < numElements; j += 2 * k ) {
	set1 = ds.find( j );
	set2 = ds.find( j + k );
	ds.unionDisjoint( set1, set2 );
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
