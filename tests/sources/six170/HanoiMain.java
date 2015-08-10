package six170;

//
// Towers of Hanoi -- Text based animation
//
// Created:       Sat Feb  3 15:48
// Last Modified: Sat Feb  6 21:54
//

/**
   HanoiMain is used to create and execute a Hanoi tower animation.
*/
public class HanoiMain {
  /**
     @requires:
     @modifies: System.out
     @effects:  Shows a text animation of solving the Towers of Hanoi
                problem with a tower of height 3.
  */
  public static void main(String argv[]) {
    moveAndShow(6, true, 3);
    moveAndShow(5, true, 3);
    moveAndShow(4, true, 3);
    moveAndShow(3, true, 3);
    moveAndShow(2, true, 3);
    moveAndShow(1, true, 3);
  }

  // numMoves indicates how many times to move the bottommost disk
  private static void moveAndShow(int numDisks, boolean noOutput, int numMoves) {
    // Create a new tower
    Hanoi h = new Hanoi(numDisks, noOutput);
    // Move it from 0 to 1, and show the animation.
    h.moveTower(0,1,numDisks);
    // Show the last frame.
    h.showTowers();
    if (numMoves > 1) {
      h.moveTower(1,2,numDisks);
      h.showTowers();
    }
    if (numMoves > 2) {
      h.moveTower(2,0,numDisks);
      h.showTowers();
    }

  }

}
