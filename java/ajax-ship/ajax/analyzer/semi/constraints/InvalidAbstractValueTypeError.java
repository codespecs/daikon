/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semi.constraints;

import ajax.jbc.*;

class InvalidAbstractValueTypeError extends InvalidClassDataError {
    InvalidAbstractValueTypeError(String s) {
        super(s);
    }
}
