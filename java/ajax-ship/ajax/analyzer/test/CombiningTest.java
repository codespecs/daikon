/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.test;

public class CombiningTest {
    public static void main(String[] args) {
        int j = 100, l = 50, m = 60;
        
        for (int i = 0; i < j; i++) {
        }
        
        for (int k = 0; k < 30; k++) {
            l += k;
            m *= k;
        }
    }
}
