/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.util;

public class StringSorter implements Comparator {
    private String[] array;
    
    private StringSorter(String[] array) {
        this.array = array;
    }
    
    public static void sort(String[] array) {
        int[] perm = Sorter.identityPermutation(array.length);
        
        Sorter.sort(perm, new StringSorter(array));
        
        String[] source = (String[])array.clone();
        
        for (int i = 0; i < array.length; i++) {
            array[i] = source[perm[i]];
        }
    }
    
    public int compareElements(int a, int b) {
        return array[a].compareTo(array[b]);
    }
}
