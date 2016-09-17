package DataStructures;

// LinkedList class
//
// CONSTRUCTION: with no initializer
// Access is via LinkedListItr class
//
// ******************PUBLIC OPERATIONS*********************
// boolean isEmpty( )     --> Return true if empty; else false
// void makeEmpty( )      --> Remove all items
// LinkedListItr zeroth( )--> Return position to prior to first
// LinkedListItr first( ) --> Return first position
// void insert( x, p )    --> Insert x after current iterator position p
// void remove( x )       --> Remove x
// LinkedListItr find( x )
//                        --> Return position that views x
// LinkedListItr findPrevious( x )
//                        --> Return position prior to x
// ******************ERRORS********************************
// No special errors

/**
 * Linked list implementation of the list
 *    using a header node.
 * Access to the list is via LinkedListItr.
 * @author Mark Allen Weiss
 * @see LinkedListItr
 */
public class LinkedList
{
  /**
   * Construct the list
   */
  public LinkedList( )
  {
    header = new ListNode( null );
  }

  /**
   * Test if the list is logically empty.
   * @return true if empty, false otherwise.
   */
  public boolean isEmpty( )
  {
    return header.next == null;
  }

  /**
   * Make the list logically empty.
   */
  public void makeEmpty( )
  {
    header.next = null;
  }


  /**
   * Return an iterator representing the header node.
   */
  public LinkedListItr zeroth( )
  {
    return new LinkedListItr( header );
  }

  /**
   * Return an iterator representing the first node in the list.
   * This operation is valid for empty lists.
   */
  public LinkedListItr first( )
  {
    return new LinkedListItr( header.next );
  }

  /**
   * Insert after p.
   * @param x the item to insert.
   * @param p the position prior to the newly inserted item.
   */
  public void insert( Object x, LinkedListItr p )
  {
    if( p != null && p.current != null )
      p.current.next = new ListNode( x, p.current.next );
  }

  /**
   * Return iterator corresponding to the first node containing an item.
   * @param x the item to search for.
   * @return an iterator; iterator isPastEnd if item is not found.
   */
  public LinkedListItr find( Object x )
  {
    ListNode itr = header.next;

    while( itr != null && !itr.element.equals( x ) )
      itr = itr.next;

    return new LinkedListItr( itr );
  }

  /**
   * Return iterator prior to the first node containing an item.
   * @param x the item to search for.
   * @return appropriate iterator if the item is found. Otherwise, the
   * iterator corresponding to the last element in the list is returned.
   */
  public LinkedListItr findPrevious( Object x )
  {
    ListNode itr = header;

    while( itr.next != null && !itr.next.element.equals( x ) )
      itr = itr.next;

    return new LinkedListItr( itr );
  }

  /**
   * Remove the first occurrence of an item.
   * @param x the item to remove.
   */
  public void remove( Object x )
  {
    LinkedListItr p = findPrevious( x );

    if( p.current.next != null )
      p.current.next = p.current.next.next;  // Bypass deleted node
  }

  // Simple print method
  public static void printList( LinkedList theList )
  {
    if( theList.isEmpty( ) )
      System.out.print( "Empty list" );
    else
      {
      LinkedListItr itr = theList.first( );
      for( ; !itr.isPastEnd( ); itr.advance( ) )
      {Object junk = itr.retrieve( ); /*System.out.print( itr.retrieve( ) + " " );*/ }
      }

    System.out.println( );
  }

  private ListNode header;

}
