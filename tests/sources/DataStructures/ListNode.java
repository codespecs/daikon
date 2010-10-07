package DataStructures;

// Basic node stored in a linked list
// Note that this class is not accessible outside
// of package DataStructures

class ListNode
{
  // Constructors
  ListNode( Object theElement )
  {
    this( theElement, null );
  }
  
  ListNode( Object theElement, ListNode n )
  {
    element = theElement;
    next    = n;
  }
  
  // Friendly data; accessible by other package routines
  Object   element;
  ListNode next;
}

