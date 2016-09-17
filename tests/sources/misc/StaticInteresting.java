package misc;
import java.util.*;

/**
 * For testing of static checks for whether something is interesting.
 * If behavior is incorrect, the following should be reported:
 * 
 * <p>
 * this.inner[this.x] in this.outer[]  
 *
 * <p>
 * Why is this code so complicated?  Because we want
 * this.inner[this.x] == this.outer[something] for the above invariant
 * to be true (it is true).  However, we order inner, outer, x and y
 * such that outer[this.y] comes first in the VarInfo index ordering.
 * Hence initially, this.outer[this.y] == this.inner[this.x] with
 * this.outer[this.y] being the leader.  Since Daikon thinks it's
 * obvious that this.outer[this.y] is in this.outer[] and it doesn't
 * look at the equality relationship to see if other variables may be
 * interesting, the invariant is never instantiated.  We make sure the
 * invariant *is* something that's interesting to the user by breaking
 * the equality halfway.  So correct behavior should print the
 * invariant.
 *
 * <p> Requises Ajax to work properly, because otherwise the equality
 * leader initially is y, since it's even earlier in the VarInfo index
 * ordering.  In fact, this pathology was very hard to reproduce since
 * regular variables always come first and are never statically
 * unintersting.
 *
 * <p> Member is just one example of this.  Others include
 * subsequence.
 */

public class StaticInteresting {
  public int[] outer = new int[100];
  public int[] inner = new int[1];
  public int y = 25;
  public int x = 0;
  public static final Random rand = new Random(10);

  public static void main(String[] args)
  {
    // s.outer[s.y] is in s.outer[] (obviously)
    StaticInteresting s = new StaticInteresting();
    for (int i = 0; i < 100; i++) {
      s.inner[s.x] = s.outer[s.y];
      // so s.inner[s.x] is in s.outer[] too (not obvious)
      s.work ();
    }
    // Now break the equality
    for (int i = 0; i < 100; i++) {
      s.inner[s.x] = s.outer[s.y - 1];
      
      // Still true that s.inner[s.x] is in s.outer[]
      s.work ();
    }

    StaticInteresting2.main(null);
  }


  /**
   *
   */
  public  void work() {
    for (int i = 0; i < outer.length; i++) {
      outer[i] = rand.nextInt();
    }
    inner[x] = outer[y];
  }

  public StaticInteresting() {
    for (int i = 0; i < outer.length; i++) {
      outer[i] = i*i + rand.nextInt();
    }
    inner[x] = outer[y];    
  }
}

/**
 * Alternative version, where the key invariant
 * <p>
 *  this.inner[this.x] in this.outer[]
 * <p>
 * should not be reported because this.inner[this.x] == this.outer[this.y]
 */
class StaticInteresting2 {
  public int[] outer = new int[100];
  public int[] inner = new int[1];
  public int y = 25;
  public int x = 0;
  public static final Random rand = new Random(10);

  public static void main(String[] args)
  {
    // s.outer[s.y] is in s.outer[] (obviously)
    StaticInteresting2 s = new StaticInteresting2();
    for (int i = 0; i < 100; i++) {
      s.inner[s.x] = s.outer[s.y];
      // so s.inner[s.x] is in s.outer[] too (not obvious)
      s.work ();
    }
  }


  /**
   *
   */
  public  void work() {
    for (int i = 0; i < outer.length; i++) {
      outer[i] = rand.nextInt();
    }
    inner[x] = outer[y];
  }

  public StaticInteresting2() {
    for (int i = 0; i < outer.length; i++) {
      outer[i] = i*i + rand.nextInt();
    }
    inner[x] = outer[y];    
  }
}

