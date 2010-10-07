package misc;
import java.util.Random;

public class Compar1 {

  static public void main(String[] args) {

    Random gen = new Random(20010709);

    // for (int i=0; i<100; i++) {
    //   int j = gen.nextInt();
    //   foo(i, i+1, j, j+1);
    // }

    for (int i=0, j=2; i<100; i++, j++) {
      bar(i, i+1, j, j+1);
    }

  }

  // static public void foo(int i, int j, int k, int l) {
  //   System.out.println(i + " " + j + " " + k + " " + l);
  // }

  static public void bar(int i, int j, int k, int l) {
    String s = "" + i + " " + j + " " + k + " " + l;
    // Avoid extraneous output.
    // System.out.println(s);
  }

}
