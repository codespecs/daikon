/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.util;

public class Sorter {
    public static int[] identityPermutation(int length) {
        int[] result = new int[length];
        
        for (int i = 0; i < result.length; i++) {
            result[i] = i;
        }
        
        return result;
    }
    
    public static void sort(int[] perm, Comparator cmp) {
        int[] buf = new int[perm.length];        
        
        sort(perm, buf, cmp, 0, perm.length);
    }
    
    private static void sort(int[] perm, int[] buf, Comparator cmp, int start, int length) {
        if (length > 1) {
            int firstHalfLength = length/2;
            int secondHalfLength = length - firstHalfLength;
            int secondHalfStart = start + firstHalfLength;
            
            sort(perm, buf, cmp, start, firstHalfLength);
            sort(perm, buf, cmp, secondHalfStart, secondHalfLength);
            
            if (cmp.compareElements(perm[secondHalfStart - 1],
                perm[secondHalfStart]) > 0) {
                int left = start;
                int right = secondHalfStart;
                int secondHalfEnd = start + length;
                int leftElem = perm[left];
                int rightElem = perm[right];
                int i = 0;
                
                while (true) {
                    if (cmp.compareElements(leftElem, rightElem) <= 0) {
                        buf[i] = leftElem;
                        i++;
                        left++;
                        if (left < secondHalfStart) {
                            leftElem = perm[left];
                        } else {
                            break;
                        }
                    } else {
                        buf[i] = rightElem;
                        i++;
                        right++;
                        if (right < secondHalfEnd) {
                            rightElem = perm[right];
                        } else {
                            break;
                        }
                    }
                }
                
                if (left < secondHalfStart) {
                    System.arraycopy(perm, left, perm, start + i, length - i);
                }
                
                System.arraycopy(buf, 0, perm, start, i);
            }
        }
    }
}
