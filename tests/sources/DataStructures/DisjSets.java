package DataStructures;

// DisjSets class
//
// CONSTRUCTION: with int representing initial number of sets
//
// ******************PUBLIC OPERATIONS*********************
// void union( root1, root2 ) --> Merge two sets
// int find( x )              --> Return set containing x
// ******************ERRORS********************************
// No error checking is performed

/**
 * Disjoint set class. (Package friendly so not used accidentally)
 * Does not use union heuristics or path compression.
 * Elements in the set are numbered starting at 0.
 * @author Mark Allen Weiss
 * @see DisjSetsFast
 */
class DisjSets
{
  
  /**
   * Construct the disjoint sets object.
   * @param numElements the initial number of disjoint sets.
   */
  public DisjSets( int numElements )
  {
    s = new int [ numElements ];
    for( int i = 0; i < s.length; i++ )
      s[ i ] = -1;
  }

  /**
   * Union two disjoint sets.  For simplicity, we assume root1 and
   * root2 are distinct and represent set names.
   * 
   * @param root1 the root of set 1.
   * @param root2 the root of set 2.
   */
  public void unionDisjoint( int root1, int root2 )
  {
    s[ root2 ] = root1;
  }

  /**
   * Union any two sets.
   * @param set1 element in set 1.
   * @param set2 element in set 2.
   */
  public void unionCareful( int set1, int set2 )
  {
    int root1 = find(set1);
    int root2 = find(set2);
    if (root1 != root2)
      unionDisjoint(root1, root2);
  }
  
  /**
   * Perform a find.
   * Error checks omitted again for simplicity.
   * @param x the element being searched for.
   * @return the set containing x.
   */
  public int find( int x )
  {
    if( s[ x ] < 0 )
      return x;
    else
      return find( s[ x ] );
  }

  private int [ ] s;

}
