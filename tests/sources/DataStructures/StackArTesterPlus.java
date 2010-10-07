package DataStructures;

// Created in 5 minutes time, starting with the original tests.

public class StackArTesterPlus {

  public static void main(String[] args) {
    again(2);
    again(5);
    again(8);
    again(12);
    again(15);
  }

  public static void again(int size) {
    StackAr s = new StackAr(size);
    
    try {
      for( int i = 0; i < size; i++ ) {
	s.push( ((i & 1) == 0) ? new Object() : new MyInteger(i) );
      }
    } catch( Overflow e ) {
      System.out.println( "Unexpected overflow" );
    }
    
    while( !s.isEmpty() ) {
      s.topAndPop();
    }
    s.topAndPop();
  }

}
