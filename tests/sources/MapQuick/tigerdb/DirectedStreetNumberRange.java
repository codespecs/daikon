package MapQuick.tigerdb;

public class DirectedStreetNumberRange {
  public final IntSet s;
    
  private final int isLowToHigh; // 0 means false, 1 means true, -1 means don't know

  public DirectedStreetNumberRange(IntSet s, 
				   boolean isLowToHigh) {
    this.s = s;
    if (isLowToHigh) {
      this.isLowToHigh = 1;
    } else {
      this.isLowToHigh = 0;
    }
  }

  public DirectedStreetNumberRange() {
    this.s = new IntSet();
    this.isLowToHigh = -1; // don't-know value
  }

  public boolean contains(int i) { return s.contains(i); }

  public String toString() { return "set:"+s+" low2high:"+unparse(isLowToHigh); }

  public int size() { return s.size(); }

  private String unparse(int l2h) {
    switch(l2h) {
    case 0: return "false";
    case 1: return "true";
    default: return "don't-know";
    }
  }

  public boolean sameDir(DirectedStreetNumberRange d) {
    return d.isLowToHigh == -1 || this.isLowToHigh == -1 ||
      this.isLowToHigh == d.isLowToHigh;
  }

  public boolean couldBeLowToHigh() {
    return (isLowToHigh == 1 || isLowToHigh == -1) ;
  }

}
