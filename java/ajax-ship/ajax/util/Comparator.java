/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.util;

public interface Comparator {
/**
E.g. to get integers sorted into ascending order, return V(left) - V(right).

@return <0 if left comes before right, 0 if left==right, and >0 if left>right
*/
    public int compareElements(int left, int right);
}
