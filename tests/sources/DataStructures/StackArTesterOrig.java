package DataStructures;

public class StackArTesterOrig {

  public static void main(String[] args) {
    StackAr s = new StackAr( 12 );
    
    try
      {
	for( int i = 0; i < 10; i++ )
	  s.push( new MyInteger( i ) );
      }
    catch( Overflow e ) { System.out.println( "Unexpected overflow" ); }
    
    while( !s.isEmpty( ) )
      System.out.println( s.topAndPop( ) );
  }

}
