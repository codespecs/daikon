package misc;

class Precedence {

  public static void main(String args[]) {
    Precedence e = new Precedence();
    e.i = 10;
    System.out.println(e.lets_calculate(4));
    // prints 6
  }

  int i;

  int lets_calculate(int j)
  {
    i = i++ % j ; // this one prints 14 in the instrumented version
    //i = (i++) % j ; // this one prints 6 in the instrumented version
    return i + j;
  }

}
