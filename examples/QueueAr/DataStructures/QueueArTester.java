package DataStructures;

import java.util.*;

public class QueueArTester
{
  /*@ spec_public */ private QueueAr q = null;

  private QueueArTester()
  {
    q = new QueueAr(0);
  }

  public void doNew(int size)
  {
    q = new QueueAr(size);
    observe();
  }

  public Object createItem(int i)
  {
    return new MyInteger(i);
  }

  public void observe()
  {
    isEmpty();
    isFull();
    getFront();
  }
  public void isEmpty()
  { q.isEmpty(); }
  public void isFull()
  { q.isFull(); }
  public void getFront()
  { q.getFront(); }

  public void makeEmpty()
  { observe(); q.makeEmpty(); observe(); }
  public void dequeue()
  { observe(); q.dequeue(); observe(); }
  public void enqueue(int x)
  { enqueue(createItem(x)); }
  public void enqueue(Object o)
  { observe(); try { q.enqueue(o); } catch (Exception e) { } observe(); }

  public void repEnqueue(int n)
  {
    for (int i=0; i < n; i++) {
      int x = (int)(1000 * Math.random());
      enqueue(x);
    }
  }

  public void dequeueAll()
  {
    while( !q.isEmpty( ) ) {
      dequeue();
    }
  }

  public void fillAndEmpty(int n)
  {
    // System.out.println("fae " + n);
    doNew(n);
    repEnqueue(n);
    dequeueAll();
  }

  public void walkAround(int n)
  {
    // System.out.println("wa " + n);
    doNew(n);
    for (int i=0; i < n/2; i++) {
      int x = (int)(1000 * Math.random());
      enqueue(x);
    }
    for (int i=0; i < 5*n; i++) {
      int x = (int)(1000 * Math.random());
      enqueue(x);
      dequeue();
    }
  }

  public void variedTypes()
  {
    // System.out.println("varied");
    doNew(10);
    enqueue("Hello world.");
    enqueue(new Double(5.5));
    enqueue(new Integer(10));
    enqueue(new StringBuffer("Hello again"));
    enqueue(new ArrayList());
    enqueue(new LinkedList());
    dequeue();
    dequeue();
    dequeue();
    dequeue();
    dequeue();
    dequeue();
  }

  public void run()
  {
    variedTypes();
    fillAndEmpty(0);
    fillAndEmpty(1);
    fillAndEmpty(2);
    fillAndEmpty(5);
    fillAndEmpty(10);
    fillAndEmpty(20);
    fillAndEmpty(30);
    fillAndEmpty(50);
    walkAround(5);
    walkAround(10);
    walkAround(20);
    walkAround(30);
  }

  public static void main(String[] args)
  {
    (new QueueArTester()).run();
  }

  /*
  public void testOriginal()
  {
    QueueAr q = new QueueAr( );

    try
      {
	for( int i = 0; i < 10; i++ )
	  q.enqueue( new MyInteger( i ) );

	while( !q.isEmpty( ) )
	  System.out.println( q.dequeue( ) );

	int num = 68;
	q.enqueue(new MyInteger(num++));
	q.enqueue(new MyInteger(num++));
	q.enqueue(new MyInteger(num++));
	q.enqueue(new MyInteger(num++));
	q.enqueue(new MyInteger(num++));
	q.enqueue(new MyInteger(num++));
	// 6
	System.out.println( q.dequeue( ) );
	System.out.println( q.dequeue( ) );
	System.out.println( q.dequeue( ) );
	System.out.println( q.dequeue( ) );
	// 2
	q.enqueue(new MyInteger(num++));
	q.enqueue(new MyInteger(num++));
	q.enqueue(new MyInteger(num++));
	// 5
	System.out.println( q.dequeue( ) );
	System.out.println( q.dequeue( ) );
	// 3
	q.enqueue(new MyInteger(num++));
	q.enqueue(new MyInteger(num++));
	// 5
	System.out.println( q.dequeue( ) );
	System.out.println( q.dequeue( ) );
	System.out.println( q.dequeue( ) );
	// 2
	q.enqueue(new MyInteger(num++));
	q.enqueue(new MyInteger(num++));
	q.enqueue(new MyInteger(num++));
	q.enqueue(new MyInteger(num++));
	q.enqueue(new MyInteger(num++));
	q.enqueue(new MyInteger(num++));
	q.enqueue(new MyInteger(num++));
	q.enqueue(new MyInteger(num++));
	// 10
	System.out.println( q.dequeue( ) );
	System.out.println( q.dequeue( ) );
	System.out.println( q.dequeue( ) );
	System.out.println( q.dequeue( ) );
	System.out.println( q.dequeue( ) );
	System.out.println( q.dequeue( ) );
	System.out.println( q.dequeue( ) );
	System.out.println( q.dequeue( ) );
	System.out.println( q.dequeue( ) );
	// 1
	q.enqueue(new MyInteger(num++));
	q.enqueue(new MyInteger(num++));
	q.enqueue(new MyInteger(num++));
	// 4
	System.out.println( q.dequeue( ) );
	System.out.println( q.dequeue( ) );
	// 2
	q.enqueue(new MyInteger(num++));
	q.enqueue(new MyInteger(num++));
	// 4
	System.out.println( q.dequeue( ) );
      }
    catch( Overflow e ) { System.out.println( "Unexpected overflow" ); }
  }
  */

}
