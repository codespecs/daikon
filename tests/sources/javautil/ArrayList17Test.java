package javautil;

public class ArrayList17Test {

  public static void main(String[] argv) {
    ArrayList17<Integer> li;
    li = new ArrayList17<Integer>(22);
    for (int i=0; i<100; i++) {
      li.add(i);
    }
    for (int i=99; i>0; i-=3) {
      li.remove(i);
    }
    li = new ArrayList17<Integer>();
  }

}
