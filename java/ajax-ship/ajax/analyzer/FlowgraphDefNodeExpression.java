/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer;

import ajax.Globals;
import ajax.jbc.*;

public class FlowgraphDefNodeExpression extends JBCExpression {
    private static final FlowgraphDefNodeExpression expression = new FlowgraphDefNodeExpression();
    
    private FlowgraphDefNodeExpression() {
    }

    static FlowgraphDefNodeExpression get() {
        return expression;
    }
}
