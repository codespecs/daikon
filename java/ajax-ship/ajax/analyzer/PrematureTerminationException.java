/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer;

public class PrematureTerminationException extends Exception {
    PrematureTerminationException(String s) {
        super(s);
    }
}
