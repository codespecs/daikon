/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer;

import ajax.Globals;
import ajax.solver.*;
import ajax.jbc.*;
import java.util.Vector;
import ajax.jbc.util.*;
import ajax.util.IdentityManager;

/**
A query family specifies a partial map S from bytecode expressions
(method, offset, JBCExpression) to 'intermediates', a partial map T from
bytecode expressions to 'target cookies', and abstract "union" and "intersection"
operators with respect to which the intermediates form a lattice. The lattice always contains
a bottom element such that for all x, x union bottom = x and x intersection bottom = bottom.
The interface is set up so that this bottom element is never seen directly by the query family;
when bottom occurs as the answer to a query, no callback with the query result is ever issued.

We essentially compute, for each r \elem range(T),
LatticeUnion { S(s) | s <-> t, T(t) = r }
where <-> represents a conservative approximation to the value-point relation.

LatticeIntersection <b>can</b> be used when we compose analyses by taking their intersection. E.g.
if we were taking the intersection of two analyses, the system might actually use an expression
like this:
LatticeIntersect { LatticeUnion { S(s) | s <->1 t, T(t) = r },
                   LatticeUnion { S(s) | s <->2 t, T(t) = r } }
where <->1 and <->2 are both sound approximations to the true value-point relation.

The intermediates are assumed to be immutable. The equals method should make
sense for them.

Bytecode expressions accepted by query families are extended with "query field expressions".
A query field is just like a normal object field except that it is never accessed by code in
the program. If Q is the name of a query field, then obj1.Q <-> obj2.Q iff obj1 <-> obj2. This
turns out to be really useful for tagging objects with "hidden information" and doing queries on
that.

Current examples of JBCQueryFamilys are:
<UL>
<LI><strong>ajax.analyzer.LivenessQueryFamily<strong> In this family, an intermediate
is simply the value "LIVE" (or bottom). LIVE union LIVE = LIVE, and LIVE intersect LIVE = LIVE.
A target cookie is an identifier M for a method body. The idea is that when a target cookie for
a method body M gets the intermediate "LIVE", that method body is callable by some virtual
method call. We create a query field Q_m for each method signature m, which represents the
virtual method with signature m defined by that object. At a virtual method call on object obj
with method signature m, the source map S maps obj.Q_m to LIVE. At a new object expression that
creates object obj with class C, the target map T maps obj.Q_m to M for each method signature m
implemented by class C, where M is the method body that C uses to implement m.
</LI>
</UL>
*/
public abstract class JBCQueryFamily {
    private Analyzer analyzer;
    private boolean started = false;
    
    /* These variables are set only temporarily. */
    private ExternalFlowgraph flowgraph;
    private String flowgraphName;
    private JBCMethod method;

/**
Construct a query family.

@param analyzer the analyzer to extract information from
*/
    protected JBCQueryFamily(Analyzer analyzer) {
        this.analyzer = analyzer;
    }

/**
Call this once construction is complete.
*/
    public void start() {
        if (!started) {
            analyzer.addQueryFamily(this);
            started = true;
        }
    }
    
/**
This is the M function. This method must be overriden to specify how two intermediates
should be joined.

This function must be commutative, associative and idempotent. In other words:
<UL>
<LI>joinIntermediates(o1, o2) = joinIntermediates(o2, o1)</LI>
<LI>joinIntermediates(o1, joinIntermediates(o2, o3)) = joinIntermediates(joinIntermediates(o1, o2), o3)</LI>
<LI>joinIntermediates(o1, o1) = o1</LI>
</UL>
If these are violated, then odd and nondeterministic errors may occur in the query results.
*/
    public abstract Object joinIntermediates(Object o1, Object o2);
    
    public abstract Object intersectIntermediates(Object o1, Object o2);

/**
This method is called when the analyzer determines that a particular method may be called.
The query family should examine the method code to determine which "value-points" it's
interested in. It returns an Enumeration of JBCQueryDatums, one for each value-point of
interest, which specifies S and T. T is the set of value-points for which
the corresponding JBCQueryDatum's "isTarget()" returns true. S is defined by each
JBCQueryDatum's "getSourceIntermediate()" method. The value-point given by a JBCQueryDatum
is specified by the JBCQueryDatum's getOffset() and getExpression() methods.

The real S map and T set are formed by merging the S maps and T sets for each live method.
If a method is known to be never called, then the query family will never see it here.
Any value-points in such a "dead" method could never be related to any other
value-points anyway. Applications that want to have queries involving this "dead" code
must handle the situation in an application-specific way. For example, if the user
asks the "jgrep" tool for the field writes matching a field read that happens to be
dead, then jgrep will print no results (the correct answer) but issue a warning.
*/
    protected abstract void identifyQueryData(JBCMethod method, byte[] code, boolean[] instructions);
    
/**
As above, but for native code specifications.

@return an Enumeration of FlowgraphQueryDatums
*/
    protected abstract void identifyQueryData(String name, ExternalFlowgraph fg, Vector externalNodes);
    
/**
As above, but for native methods.

@return an Enumeration of FlowgraphQueryDatums
*/
    protected abstract void identifyQueryData(JBCMethod method, ExternalFlowgraph fg, Vector externalNodes);
    
    protected abstract void updateResult(Object targetCookie, Object intermediate);
    
    protected void notifyAnalysisComplete() {
    }
    
    protected void addSourceDatum(int offset, JBCExpression expression, Object intermediate) {
        addSourceDatum(offset, null, expression, intermediate);
    }
    
    protected void addSourceDatum(ExternalCFGNode node, JBCExpression expression, Object intermediate) {
        addSourceDatum(node, null, expression, intermediate);
    }
    
    protected void addTargetDatum(int offset, JBCExpression expression, Object targetCookie) {
        addTargetDatum(offset, null, expression, targetCookie);
    }
    
    protected void addTargetDatum(ExternalCFGNode node, JBCExpression expression, Object targetCookie) {
        addTargetDatum(node, null, expression, targetCookie);
    }

    protected void addSourceDatum(int offset, JBCExpressionContext context, JBCExpression expression, Object intermediate) {
        analyzer.addSourceDatum(this, method, offset, context, expression, intermediate);
    }
    
    protected void addSourceDatum(ExternalCFGNode node, JBCExpressionContext context, JBCExpression expression, Object intermediate) {
        if (method != null) {
            analyzer.addSourceDatum(this, method, node, context, expression, intermediate);
        } else {
            analyzer.addSourceDatum(this, flowgraphName, node, context, expression, intermediate);
        }
    }
    
    protected void addTargetDatum(int offset, JBCExpressionContext context, JBCExpression expression, Object targetCookie) {
        analyzer.addTargetDatum(this, method, offset, context, expression, targetCookie);
    }
    
    protected void addTargetDatum(ExternalCFGNode node, JBCExpressionContext context, JBCExpression expression, Object targetCookie) {
        if (method != null) {
            analyzer.addTargetDatum(this, method, node, context, expression, targetCookie);
        } else {
            analyzer.addTargetDatum(this, flowgraphName, node, context, expression, targetCookie);
        }
    }
    
    void examineCode(JBCMethod method, byte[] code, boolean[] instructions) {
        this.method = method;
        
        identifyQueryData(method, code, instructions);
        
        this.method = null;
    }
    
    void examineCode(JBCMethod method, ExternalFlowgraph fg, Vector externalNodes) {
        this.method = method;
        this.flowgraph = fg;
        
        identifyQueryData(method, fg, externalNodes);
        
        this.flowgraph = null;
        this.method = null;
    }
    
    void examineCode(String name, ExternalFlowgraph fg, Vector externalNodes) {
        this.flowgraphName = name;
        
        identifyQueryData(name, fg, externalNodes);
        
        this.flowgraph = null;
        this.flowgraphName = null;
    }
    
    protected void dispose() {
        analyzer.removeQueryFamily(this);
    }
    
    protected Analyzer getAnalyzer() {
        return analyzer;
    }

/**
Convenience function using Locations
*/
    protected void addSourceDatum(Location loc, JBCExpression e, Object intermediate) {
        if (loc instanceof JBCLocation) {
            JBCLocation l = (JBCLocation)loc;
            
            if (Globals.debug && !l.getMethod().equals(method)) {
                Globals.nonlocalError("Method mismatch in location: " + l);
            }
            
            analyzer.addSourceDatum(this, method, l.getOffset(), null, e, intermediate);
        } else if (!Globals.debug || loc instanceof ExternalLocation) {
            ExternalLocation l = (ExternalLocation)loc;
            
            if (method != null) {
                if (Globals.debug && !method.equals(l.getMethod())) {
                    Globals.nonlocalError("Method mismatch in location: " + l);
                }
                
                analyzer.addSourceDatum(this, method, l.getNode(), null, e, intermediate);
            } else {
                if (Globals.debug && !flowgraphName.equals(l.getFlowgraphName())) {
                    Globals.nonlocalError("Function mismatch in location: " + l);
                }
                
                analyzer.addSourceDatum(this, flowgraphName, l.getNode(), null, e, intermediate);
            }
        } else {
            Globals.nonlocalError("Unknown location type: " + loc);
        }
    }
    
/**
Convenience function using Locations
*/
    protected void addTargetDatum(Location loc, JBCExpression e, Object cookie) {
        if (loc instanceof JBCLocation) {
            JBCLocation l = (JBCLocation)loc;
            
            if (Globals.debug && !l.getMethod().equals(method)) {
                Globals.nonlocalError("Method mismatch in location: " + l);
            }
            
            analyzer.addTargetDatum(this, method, l.getOffset(), null, e, cookie);
        } else if (!Globals.debug || loc instanceof ExternalLocation) {
            ExternalLocation l = (ExternalLocation)loc;
            
            if (method != null) {
                if (Globals.debug && !method.equals(l.getMethod())) {
                    Globals.nonlocalError("Method mismatch in location: " + l);
                }
                
                analyzer.addTargetDatum(this, method, l.getNode(), null, e, cookie);
            } else {
                if (Globals.debug && !flowgraphName.equals(l.getFlowgraphName())) {
                    Globals.nonlocalError("Function mismatch in location: " + l);
                }
                
                analyzer.addTargetDatum(this, flowgraphName, l.getNode(), null, e, cookie);
            }
        } else {
            Globals.nonlocalError("Unknown location type: " + loc);
        }
    }
    
    public int hashCode() {
        return IdentityManager.getIdentityHashCode(this);
    }
}
