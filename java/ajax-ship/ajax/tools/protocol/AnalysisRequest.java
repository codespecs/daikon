/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.tools.protocol;

import java.io.*;

public class AnalysisRequest implements Serializable {
    private static int UID = 1940011;
    
    private String analysisName;
    private Integer analysisToken;
    
    public AnalysisRequest(String name) {
        analysisName = name;
        analysisToken = new Integer(UID);
        UID++;
    }
    
    public String getAnalysisName() {
        return analysisName;
    }
    
    public Integer getAnalysisToken() {
        return analysisToken;
    }
    
    public String toString() {
        return analysisName + " (" + analysisToken.intValue() + ")";
    }
}
