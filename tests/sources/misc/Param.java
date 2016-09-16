package misc;

/**
 * For testing of isParam flag to Daikon.  We change the param with
 * "2" in it and see what Daikon produces.
 */

public class Param
{
  public int gi1;
  public int gi2;
  public ParamType gs1;
  public ParamType gs2;

  public static void main(String[] args)
  {
    Param me = new Param();
    me.main();
  }


  public void main() {
    for (int i = 0; i < 100; i++) {
      work (gi1, gi2, gs1, gs2);
    }
  }

  /**
   *
   */
  public void work(int pi1, int pi2, ParamType ps1, ParamType ps2)
  {
    pi2 ++;

    ps2 = new ParamType();

    ps1.a ++;
    ps2.a ++;

    
  }

  public Param() {
    gi1 = 0;
    gi2 = 5;

    gs1 = new ParamType();
    gs1.a = 10;
    gs1.b = 15;

    gs2 = new ParamType();
    gs2.a = 20;
    gs2.b = 25;


  }

}


class ParamType {

  public int a;
  public int b;

  public ParamType () {
    a = 0;
    b = 0;
  }

}
