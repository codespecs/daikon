/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc.util;

import ajax.jbc.LineNumberData;
import ajax.util.*;

public class LineNumberSorter implements Comparator {
    private LineNumberData[] lineNumbers;
    
    private LineNumberSorter(LineNumberData[] lineNumbers) {
        this.lineNumbers = lineNumbers;
    }
    
    public static void sort(LineNumberData[] lineNumbers) {
        int[] perm = Sorter.identityPermutation(lineNumbers.length);
        
        Sorter.sort(perm, new LineNumberSorter(lineNumbers));
        
        LineNumberData[] source = (LineNumberData[])lineNumbers.clone();
        
        for (int i = 0; i < lineNumbers.length; i++) {
            lineNumbers[i] = source[perm[i]];
        }
    }
    
    public int compareElements(int a, int b) {
        return lineNumbers[a].getStartingPC() - lineNumbers[b].getStartingPC();
    }
}
