/*********************************************
 * This IntDoublePair represents a connected int and double.
 * This is pretty much a struct + constructor.
 * However this also implements Comparator
 * so that it can be used in a TreeSet or Sorted.
 *
 * Created by Yuriy Brun, 6/1/2002
 *********************************************/
import java.util.*;

public class IntDoublePair implements Comparable{
  //public fields
  public int number;
  public double value;

  //returns a new fresh Pair with number set to num and value set to val
  public IntDoublePair(int num, double val) {
    number = num;
    value = val;
  }

  //Compares an Object to this
  //Throws ClassCastException if o is not an IntDoublePair
  public int compareTo(Object o) throws ClassCastException{
    IntDoublePair p;
    try {
      p = (IntDoublePair) o;
    }
    catch (ClassCastException e) {
      throw e;
    }
    return this.number - p.number;
  }
}









