package DataStructures;

// Basic node stored in skip lists
// Note that this class is not accessible outside
// of package DataStructures

class SkipNode
{
  // Constructors
  SkipNode( Comparable theElement )
  {
    this( theElement, null, null );
  }
  
  SkipNode( Comparable theElement, SkipNode rt, SkipNode dt )
  {
    element  = theElement;
    right    = rt;
    down     = dt;
  }
  
  // Friendly data; accessible by other package routines
  Comparable element;      // The data in the node
  SkipNode   right;        // Right link 
  SkipNode   down;         // Down link
}
