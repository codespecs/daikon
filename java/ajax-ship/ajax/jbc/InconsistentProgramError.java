/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc;

/**
The InconsistentProgramError is thrown when the analysis of a JBC method or
the flowgraph representation of some external code could not be completed
because there is an inconsistency between different parts of the program.

The error records some dependency information about which parts contributed
to the inconsistency. This information is used to determine when analysis should
be retried (when one of the contributing parts has changed).
*/
public class InconsistentProgramError extends Error {
    private Object[] dependencies;
    
/**
Create the InconsistentProgramError exception.

@param s the message explaining why it's inconsistent
@param dependencies the JBCClass, JBCMethod and String objects for the classes,
native code methods and native code functions that might avoid triggering this
error if changed (the class, method or function that was being analyzed does
not need to be included)
*/
    public InconsistentProgramError(String s, Object[] dependencies) {
        super(s);
        
        this.dependencies = dependencies;
    }
    
    public Object[] getDependencies() {
        return dependencies;
    }
}
