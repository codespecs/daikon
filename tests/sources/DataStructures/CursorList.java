package DataStructures;

// CursorList class
//
// CONSTRUCTION: with no initializer
// Access is via CursorListItr class
//
// ******************PUBLIC OPERATIONS*********************
// boolean isEmpty( )     --> Return true if empty; else false
// void makeEmpty( )      --> Remove all items
// CursorListItr zeroth( )--> Return position to prior to first
// CursorListItr first( ) --> Return first position
// void insert( x, p )    --> Insert x after current iterator position p
// void remove( x )       --> Remove x
// CursorListItr find( x )
//                        --> Return position that views x
// CursorListItr findPrevious( x )
//                        --> Return position prior to x
// ******************ERRORS********************************
// No special errors

/**
 * Linked list implementation of the list
 *    using a header node; cursor version.
 * Access to the list is via CursorListItr.
 * @author Mark Allen Weiss
 * @see CursorListItr
 */
public class CursorList
{
  private static int alloc( )
  {
    int p = cursorSpace[ 0 ].next;
    cursorSpace[ 0 ].next = cursorSpace[ p ].next;
    if( p == 0 )
      throw new OutOfMemoryError( );
    return p;
  }

  private static void free( int p )
  {
    cursorSpace[ p ].element = null;
    cursorSpace[ p ].next = cursorSpace[ 0 ].next;
    cursorSpace[ 0 ].next = p;
  }

  /**
   * Construct the list.
   */
  public CursorList( )
  {
    header = alloc( );
    cursorSpace[ header ].next = 0;
  }

  /**
   * Test if the list is logically empty.
   * @return true if empty, false otherwise.
   */
  public boolean isEmpty( )
  {
    return cursorSpace[ header ].next == 0;
  }

  /**
   * Make the list logically empty.
   */
  public void makeEmpty( )
  {
    while( !isEmpty( ) )
      remove( first( ).retrieve( ) );
  }


  /**
   * Return an iterator representing the header node.
   */
  public CursorListItr zeroth( )
  {
    return new CursorListItr( header );
  }

  /**
   * Return an iterator representing the first node in the list.
   * This operation is valid for empty lists.
   */
  public CursorListItr first( )
  {
    return new CursorListItr( cursorSpace[ header ].next );
  }

  /**
   * Insert after p.
   * @param x the item to insert.
   * @param p the position prior to the newly inserted item.
   */
  public void insert( Object x, CursorListItr p )
  {
    if( p != null && p.current != 0 )
      {
	int pos = p.current;
	int tmp = alloc( );

	cursorSpace[ tmp ].element = x;
	cursorSpace[ tmp ].next = cursorSpace[ pos ].next;
	cursorSpace[ pos ].next = tmp;
      }
  }

  /**
   * Return iterator corresponding to the first node containing an item.
   * @param x the item to search for.
   * @return an iterator; iterator isPastEnd if item is not found.
   */
  public CursorListItr find( Object x )
  {
    int itr = cursorSpace[ header ].next;

    while( itr != 0 && !cursorSpace[ itr ].element.equals( x ) )
      itr = cursorSpace[ itr ].next;

    return new CursorListItr( itr );
  }

  /**
   * Return iterator prior to the first node containing an item.
   * @param x the item to search for.
   * @return appropriate iterator if the item is found. Otherwise, the
   * iterator corresponding to the last element in the list is returned.
   */
  public CursorListItr findPrevious( Object x )
  {
    int itr = header;

    while( cursorSpace[ itr ].next != 0 &&
	   !cursorSpace[ cursorSpace[ itr ].next ].element.equals( x ) )
      itr = cursorSpace[ itr ].next;

    return new CursorListItr( itr );
  }

  /**
   * Remove the first occurrence of an item.
   * @param x the item to remove.
   */
  public void remove( Object x )
  {
    CursorListItr p = findPrevious( x );
    int pos = p.current;

    if( cursorSpace[ pos ].next != 0 )
      {
	int tmp = cursorSpace[ pos ].next;
	cursorSpace[ pos ].next = cursorSpace[ tmp ].next;
	free( tmp );
      }
  }

  // Simple print method
  static public void printList( CursorList theList )
  {
    if( theList.isEmpty( ) )
      System.out.print( "Empty list" );
    else
      {
	CursorListItr itr = theList.first( );
	for( ; !itr.isPastEnd( ); itr.advance( ) )
      {Object junk = itr.retrieve( ); /*System.out.print( itr.retrieve( ) + " " );*/ }
      }

    System.out.println( );
  }

  private int header;
  static CursorNode[ ] cursorSpace;

  private static final int SPACE_SIZE = 100;

  static
  {
    cursorSpace = new CursorNode[ SPACE_SIZE ];
    for( int i = 0; i < SPACE_SIZE; i++ )
      cursorSpace[ i ] = new CursorNode( null, i + 1 );
    cursorSpace[ SPACE_SIZE - 1 ].next = 0;
  } 

}
