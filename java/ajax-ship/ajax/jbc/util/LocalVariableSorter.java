/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc.util;

import ajax.jbc.LocalVariableData;
import ajax.util.*;

public class LocalVariableSorter implements Comparator {
    private LocalVariableData[] localVariables;
    
    private LocalVariableSorter(LocalVariableData[] localVariables) {
        this.localVariables = localVariables;
    }
    
    public static void sort(LocalVariableData[] localVariables) {
        int[] perm = Sorter.identityPermutation(localVariables.length);
        
        Sorter.sort(perm, new LocalVariableSorter(localVariables));
        
        LocalVariableData[] source = (LocalVariableData[])localVariables.clone();
        
        for (int i = 0; i < localVariables.length; i++) {
            localVariables[i] = source[perm[i]];
        }
    }
    
    public int compareElements(int a, int b) {
        int indexDiff = localVariables[a].getVarIndex() - localVariables[b].getVarIndex();
        
        if (indexDiff != 0) {
            return indexDiff;
        } else {
            return localVariables[a].getScopeStartPC() - localVariables[b].getScopeStartPC();
        }
    }
}
