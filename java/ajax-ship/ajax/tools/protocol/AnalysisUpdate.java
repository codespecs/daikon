/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.tools.protocol;

import java.io.*;

public class AnalysisUpdate implements Serializable {
    private Integer analysisToken;
    
    public AnalysisUpdate(Integer analysisToken) {
        this.analysisToken = analysisToken;
    }
    
    public Integer getAnalysisToken() {
        return analysisToken;
    }
}
