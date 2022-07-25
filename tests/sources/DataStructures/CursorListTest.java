package DataStructures;

public class CursorListTest
{

  public static void main(String[] args) {
    original_main();
    observe(new CursorList());
    observe(new CursorList());
    observe(new CursorList());
    for (int i=0; i<2; i++) {
      (new CursorListTest()).run();
    }
  }

  private static int itemno = 0;
  private static Object item() {
    itemno = (itemno + 1) % 10;
    switch (itemno) {
    case 0: return new Object();
    case 1: return Integer.valueOf(1978);
    case 2: return new String("Hi MoM");
    case 3: return new MyInteger(12345);
    case 4: return Double.valueOf(42.0);
    case 5: return Long.valueOf(101010);
    case 6: return new String("I wish I were an Oscar Meyer weiner");
    case 7: return new String("Then everyone would be in love with me");
    case 8: return System.out;
    case 9: return CursorListTest.class;
    }
    throw new Error();
  }

  CursorList ls = new CursorList();
  public void run() {
    observe();

    System.out.println("run loop 1");
    CursorListItr theItr = ls.zeroth();
    for(int i = 0; i < 10; i++) {
      ls.insert(item(), theItr);
      observe();
      theItr.advance( );
    }

    System.out.println("run loop 2");
    for(int i = 0; i < 10; i += 2 ) {
      ls.remove(ls.first().retrieve());
      observe();
    }
    ls.makeEmpty();
    observe();
    
    System.out.println("run loop 3");
    theItr = ls.zeroth();
    for(int i = 0; i < 20; i += 1) {
      ls.insert(item(), theItr);
      observe();
      ls.insert(item(), theItr);
      observe();
      ls.remove(ls.first().retrieve());
      observe();
    }
    ls.makeEmpty();
    observe();
  }

  public void observe() {
    observe(ls);
  }
  public static void observe(CursorList ls) {
    CursorList.printList(ls);
    ls.isEmpty();
    for (CursorListItr itr = ls.first(); !itr.isPastEnd(); itr.advance()) {
      itr.retrieve();
      ls.find(item()).retrieve();
      ls.findPrevious(item()).retrieve();
    }
    for (CursorListItr itr = ls.zeroth(); !itr.isPastEnd(); itr.advance()) {
      itr.retrieve();
      ls.findPrevious(item()).retrieve();
    }
  }
    
  public static void original_main() {
    CursorList    theList = new CursorList( );
    CursorListItr theItr;
    int i;

    theItr = theList.zeroth( );
    CursorList.printList( theList );

    for( i = 0; i < 10; i++ )
      {
	theList.insert( new MyInteger( i ) , theItr );
	CursorList.printList( theList );
	theItr.advance( );
      }

    for( i = 0; i < 10; i += 2 )
      theList.remove( new MyInteger( i ) );

    for( i = 0; i < 10; i++ )
      if( ( i % 2 == 0 ) != ( theList.find( new MyInteger( i ) ).isPastEnd( ) ) )
	System.out.println( "Find fails!" );

    System.out.println( "Finished deletions" );
    CursorList.printList( theList );
  }

}

