/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer;

import ajax.Globals;
import ajax.jbc.util.*;
import ajax.jbc.*;

/**
This is where we classify code according to how the analysis will deal
with it.
<p>
The code disposition flags determine how the analysis will react to code
it cannot analyze. There are two flags:
<li>
<ul>If <b>WARN</b> is set, then warnings are emitted when necessary. If
it is not set, no warnings are emitted.
<ul>If <b>OPTIMISTIC</b> is set, then the analysis makes optimistic
assumptions, otherwise it maintains a conservative, pessimistic
approximation.
</li>
The default is WARN on and OPTIMISTIC on. We should change things so
that the default is OPTIMISTIC off...
*/
public class CoveragePolicy {
    public static final int WARN       = 0x01;
    public static final int OPTIMISTIC = 0x02;
    
    public CoveragePolicy() {
    }
    
    public boolean isMethodKnowable(JBCMethod m) {
        return true;
    }
    
    public int getUnknownMethodDisposition(JBCMethod m) {
        return WARN | OPTIMISTIC; // TBD make OPTIMISTIC off by default
    }
    
/**
Forces code matching the class mask to be treated as unknown, even if the
analysis is capable of handling it. When the analysis looks at this
code, it consults the disposition.
*/
    public void addUnknownCode(ClassMask mask, int disposition) {
        throw new Error("Unimplemented");
    }
    
/**
If the analysis is unable to handle code matching the mask, then
it takes action specified in the disposition.
*/
    public void addFailureUnknownCodeDisposition(ClassMask mask, int disposition) {
        throw new Error("Unimplemented");
    }
    
    public void notifyUnknownCode(JBCMethod m) {
        if ((getUnknownMethodDisposition(m) & WARN) != 0) {
            Globals.writeLog(this, "Unknown method encountered: " + m);
        }
    }
}
