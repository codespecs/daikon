package six170;

// Towers of Hanoi -- Text based animation
//
// Author:	  Matt Deeds
// Created:	  Sat Feb  3 14:55
// Last Modified: Sat Feb 6 21:50

/**
   @author Matt Deeds

   Hanoi represents a towers of Hanoi puzzle.  It has the ability to
   solve itself and print out the animation of the process.  */

public class Hanoi {

  // Abstraction function: each disk of size j is located on peg
  // diskLocation[j].  Disks range in size from 0 to height-1.
  private int diskLocation[];
  private int height;

  private boolean noOutput;

  // Representation invariant: Every disk is on peg 0, 1, or 2.	 */

  /**
     @requires: height > 0
     @modifies: this
     @effects: Creates a new tower of Hanoi puzzle with all disks on
     peg 0.  There are height pegs.
  */
  public Hanoi(int height) {
    diskLocation = new int[height];
    for (int i=0; i<height; i++) {
      diskLocation[i] = 0;
    }
    this.height = height;
  }

  public Hanoi(int height, boolean noOutput) {
    this(height);
    this.noOutput = noOutput;
  }

  /**
     @requires: fromPeg and toPeg are each 0, 1, or 2.
     @modifies: this
     @effects: moves the smallest disk on peg fromPeg to toPeg.
  */
  private void moveDisk(int fromPeg, int toPeg) {
    for (int i=0; i<height; i++) {
      if (diskLocation[i] == fromPeg) {
        diskLocation[i] = toPeg;
        break;
      }
    }
  }

  /**
     @requires: pegA and pegB are each 0, 1, or 2. pegA != pegB.
     @modifies: nothing
     @effects: returns the peg (0, 1, or 2) which is not pegA or pegB.
  */
  private static int otherPeg(int pegA, int pegB) {
    if ((pegA == 0) && (pegB == 1)) return 2;
    if ((pegA == 1) && (pegB == 0)) return 2;
    if ((pegA == 0) && (pegB == 2)) return 1;
    if ((pegA == 2) && (pegB == 0)) return 1;
    if ((pegA == 1) && (pegB == 2)) return 0;
    if ((pegA == 2) && (pegB == 1)) return 0;
    return -1;

    // Cool but obfuscated: this function may be implemented as:
    // return 3-(pegA+pegB);

  }

  /**
     @requires: fromPeg and toPeg are each 0, 1, or 2.	 There is a
                tower of height towerHeight on peg fromPeg, and it is legal
                to move it to toPeg.  towerHeight > 0.
     @modifies: this, System.out
     @effects:	moves a tower of height towerHeight from fromPeg to toPeg.
                Displays the animation of the process, excluding the
                last frame.
  */
  public void moveTower(int fromPeg, int toPeg, int towerHeight) {
    if (towerHeight == 1) {
      // Degenerate case: tower of towerHeight 1 is a disk.
      showTowers();
      moveDisk(fromPeg, toPeg);
    } else {
      moveTower(fromPeg, otherPeg(fromPeg,toPeg), towerHeight-1);
      showTowers();
      moveDisk(fromPeg, toPeg);
      moveTower(otherPeg(fromPeg, toPeg), toPeg, towerHeight-1);
    }
  }

  /**
     @requires:
     @modifies: System.out
     @effects: Produces a text representation of the current state of this.
  */
  public void showTowers() {
    if (!noOutput) {
      // Producing nice looking text output requires a lot of
      // uninteresting code.
      System.out.println("");
      for (int i=0; i<height; i++) {
        for (int j=0; j<3; j++) {
          for (int k=height-1; k>=0; k--) {
            if ((diskLocation[i] == j) && (k <= i)) {
              System.out.print("=");
            } else {
              System.out.print(" ");
            }
          }
          System.out.print("|");
          for (int k=0; k<height; k++) {
            if ((diskLocation[i] == j) && (k <= i)) {
              System.out.print("=");
            } else {
              System.out.print(" ");
            }
          }
          System.out.print(" ");
        }
        System.out.println("");
      }
      for (int i=0; i<((height*2+2)*3); i++) {
        System.out.print("-");
      }
      System.out.println("");
    }
  }
}
