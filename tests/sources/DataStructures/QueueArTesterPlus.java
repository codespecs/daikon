package DataStructures;

// Created in 3 minutes starting from QATOrig

// This was extracted from QueueAr.main in the original dsaa
// and then augmented
public class QueueArTesterPlus
{

  public static void main(String[] args) {
    foo(4);
    foo(9);
    foo(15);
    foo(20);
    foo(23);
  }

  public static void foo(int size) {
    QueueAr q = new QueueAr(size);
    
    try {
      for(int i = 0; i < size; i++)
	q.enqueue(new MyInteger(i));
    } catch(Overflow e) {
      System.out.println( "Unexpected overflow" );
    }
    
    while( !q.isEmpty( ) )
      System.out.println( q.dequeue( ) );

    try {
      for(int i = 0; i < size / 2; i++)
	q.enqueue(new MyInteger(i));
    } catch(Overflow e) {
      System.out.println( "Unexpected overflow" );
    }
    
    try {
      for(int i = 0; i < size / 3; i++) {
	q.enqueue(new MyInteger(i));
	System.out.println( q.dequeue( ) );
      }
    } catch(Overflow e) {
      System.out.println( "Unexpected overflow" );
    }

    try {
      for(int i = 0; i < size; i++)
	q.enqueue(new MyInteger(i));
    } catch(Overflow e) {
      System.out.println( "Unexpected overflow" );
    }

    while( !q.isEmpty( ) )
      System.out.println( q.dequeue( ) );

  }    

}
