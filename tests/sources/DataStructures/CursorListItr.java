package DataStructures;

// CursorListItr class; maintains "current position"
//
// CONSTRUCTION: Package friendly only, with a CursorNode
//
// ******************PUBLIC OPERATIONS*********************
// void advance( )        --> Advance
// boolean isPastEnd( )   --> True if at valid position in list
// Object retrieve        --> Return item in current position

/**
 * Linked list implementation of the list iterator
 *    using a header node; cursor version.
 * @author Mark Allen Weiss
 * @see CursorList
 */
public class CursorListItr
{
  /**
   * Construct the list iterator
   * @param theNode any node in the linked list.
   */
  CursorListItr( int theNode )
  {
    current = theNode;
  }

  /**
   * Test if the current position is past the end of the list.
   * @return true if the current position is null-equivalent.
   */
  public boolean isPastEnd( )
  {
    return current == 0;
  }

  /**
   * Return the item stored in the current position.
   * @return the stored item or null if the current position
   * is not in the list.
   */
  public Object retrieve( )
  {
    return isPastEnd( ) ? null : CursorList.cursorSpace[ current ].element;
  }

  /**
   * Advance the current position to the next node in the list.
   * If the current position is null, then do nothing.
   */
  public void advance( )
  {
    if( !isPastEnd( ) )
      current = CursorList.cursorSpace[ current ].next;
  }

  /*@ spec_public */ int current;    // Current position
}
