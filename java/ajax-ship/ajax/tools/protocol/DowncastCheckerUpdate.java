/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.tools.protocol;

public class DowncastCheckerUpdate extends AnalysisUpdate {
    private LocationDescriptor location;
    private ClassDescriptor actualBound = null;
    private ClassDescriptor foundBound = null;
    private int state;
    
    public static final int UNKNOWN = 0;
    public static final int SAFE    = 1;
    public static final int UNSAFE  = 2;
    
    public DowncastCheckerUpdate(Integer analysisToken, LocationDescriptor location, int state) {
        super(analysisToken);
        
        this.location = location;
        this.state = state;
    }
    
    public int getState() {
        return state;
    }
    
    public LocationDescriptor getLocation() {
        return location;
    }
    
    public ClassDescriptor getActualBound() {
        return actualBound;
    }
    
    public ClassDescriptor getFoundBound() {
        return foundBound;
    }
    
    public void setActualBound(ClassDescriptor b) {
        actualBound = b;
    }
    
    public void setFoundBound(ClassDescriptor b) {
        foundBound = b;
    }
    
    public String toString() {
        return location.toString() + ": state=" + state;
    }
}
