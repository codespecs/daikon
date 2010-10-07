package DataStructures;

// This was extracted from QueueAr.main in the original dsaa
public class QueueArTesterOrig
{

  public static void main(String[] args)
  {
    QueueAr q = new QueueAr(10);
    
    try {
      for(int i = 0; i < 10; i++)
	q.enqueue(new MyInteger(i));
    } catch(Overflow e) {
      System.out.println( "Unexpected overflow" );
    }
    
    while( !q.isEmpty( ) )
      System.out.println( q.dequeue( ) );
  }    

}
