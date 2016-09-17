package DataStructures;

// BinarySearchTree class
//
// CONSTRUCTION: with a value at least as large as all others
//
// ******************PUBLIC OPERATIONS*********************
// void insert( x )       --> Insert x
// void remove( x )       --> Remove x (unimplemented)
// Comparable find( x )   --> Return item that matches x
// Comparable findMin( )  --> Return smallest item
// Comparable findMax( )  --> Return largest item
// boolean isEmpty( )     --> Return true if empty; else false
// void makeEmpty( )      --> Remove all items

/**
 * Implements a deterministic skip list.
 * Note that all "matching" is based on the compareTo method.
 * @author Mark Allen Weiss
 */
public class DSL
{
  /**
   * Construct the DSL.
   * @param inf the largest Comparable.
   */
  public DSL( Comparable inf )
  {
    infinity = inf;
    bottom = new SkipNode( null );
    bottom.right = bottom.down = bottom;
    tail   = new SkipNode( infinity );
    tail.right = tail;
    header = new SkipNode( infinity, tail, bottom );
  }

  /**
   * Insert into the DSL.
   * @param x the item to insert.
   */
  public void insert( Comparable x )
  {
    SkipNode current = header;

    bottom.element = x;
    while( current != bottom )
      {
	while( current.element.compareTo( x ) < 0 )
	  current = current.right;

	// If gap size is 3 or at bottom level and
	// must insert, then promote middle element
	if( current.down.right.right.element.compareTo( current.element ) < 0 )
	  {
	    current.right = new SkipNode( current.element, current.right,
					  current.down.right.right );
	    current.element = current.down.right.element;
	  }
	else
	  current = current.down;
      }

    // Raise height of DSL if necessary
    if( header.right != tail )
      header = new SkipNode( infinity, tail, header );
  }

  /**
   * Remove from the DSL. Unimplemented.
   * @param x the item to remove.
   */
  public void remove( Comparable x )
  {
    System.out.println( "Sorry, remove unimplemented" );
  }

  /**
   * Find the smallest item in the DSL.
   * @return smallest item, or null if empty.
   */
  public Comparable findMin( )
  {
    if( isEmpty( ) )
      return null;

    SkipNode current = header;
    while( current.down != bottom )
      current = current.down;

    return elementAt( current );
  }

  /**
   * Find the largest item in the DSL.
   * @return the largest item, or null if empty.
   */
  public Comparable findMax( )
  {
    if( isEmpty( ) )
      return null;

    SkipNode current = header;
    for( ; ; )
      if( current.right.right != tail )
	current = current.right;
      else if( current.down != bottom )
	current = current.down;
      else
	return elementAt( current );
  }

  /**
   * Find an item in the DSL.
   * @param x the item to search for.
   * @return the matching item, or null if not found.
   */
  public Comparable find( Comparable x )
  {
    SkipNode current = header;

    bottom.element = x;
    for( ; ; )
      if( x.compareTo( current.element ) < 0 )
	current = current.down;
      else if( x.compareTo( current.element ) > 0 )
	current = current.right;
      else
	return elementAt( current );
  }

  /**
   * Make the DSL logically empty.
   */
  public void makeEmpty( )
  {
    header.right = tail;
    header.down = bottom;
  }

  /**
   * Test if the DSL is logically empty.
   * @return true if empty, false otherwise.
   */
  public boolean isEmpty( )
  {
    return header.right == tail && header.down == bottom;
  }

  /**
   * Internal method to get element field.
   * @param t the node.
   * @return the element field, or null if t is null.
   */
  private Comparable elementAt( SkipNode t )
  {
    return t == bottom ? null : t.element;
  }

  /**
   * Print the DSL.
   */
  private void printList( )
  {
    SkipNode current = header;

    while( current.down != bottom )
      ;

    while( current.right != tail )
      {
	System.out.println( current.element );
	current = current.right;
      }
  }

  /** The DSL header. */
  private SkipNode header;
  private Comparable infinity;
  private SkipNode bottom = null;
  private SkipNode tail   = null;
       

  // Test program
  public static void main( String [ ] args )
  {
    DSL t = new DSL( new MyInteger( 100000000 ) );
    final int NUMS = 4000;
    final int GAP  =   37;

    System.out.println( "Checking... (no more output means success)" );

    for( int i = GAP; i != 0; i = ( i + GAP ) % NUMS )
      t.insert( new MyInteger( i ) );

    if( NUMS < 40 )
      t.printList( );
    if( ((MyInteger)(t.findMin( ))).intValue( ) != 1 ||
	((MyInteger)(t.findMax( ))).intValue( ) != NUMS - 1 )
      System.out.println( "FindMin or FindMax error!" );

    for( int i = 1; i < NUMS; i++ )
      if( ((MyInteger)(t.find( new MyInteger( i ) ))).intValue( ) != i )
	System.out.println( "Find error1!" );
    if( t.find( new MyInteger( 0 ) ) != null )
      System.out.println( "Find error2!" );
    if( t.find( new MyInteger( NUMS + 10 ) ) != null )
      System.out.println( "Find error2!" );
  }
}
