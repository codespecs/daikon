/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc.util;

import ajax.jbc.CatchBlockData;
import ajax.util.*;

public class CatchBlockSorter implements Comparator {
    private CatchBlockData[] catchBlocks;
    
    private CatchBlockSorter(CatchBlockData[] catchBlocks) {
        this.catchBlocks = catchBlocks;
    }
    
    public static void sort(CatchBlockData[] catchBlocks) {
        int[] perm = Sorter.identityPermutation(catchBlocks.length);
        
        Sorter.sort(perm, new CatchBlockSorter(catchBlocks));
        
        CatchBlockData[] source = (CatchBlockData[])catchBlocks.clone();
        
        for (int i = 0; i < catchBlocks.length; i++) {
            catchBlocks[i] = source[perm[i]];
        }
    }
    
    public int compareElements(int a, int b) {
        return catchBlocks[a].getStartPC() - catchBlocks[b].getStartPC();
    }
}
