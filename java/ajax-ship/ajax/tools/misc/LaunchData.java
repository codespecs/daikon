/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.tools.misc;

import java.io.*;

public class LaunchData implements Serializable {
    private String programTitle;
    private String JARFile;
    private String mainClass;
    
    public LaunchData(String programTitle, String JARFile, String mainClass) {
        this.programTitle = programTitle;
        this.JARFile = JARFile;
        this.mainClass = mainClass;
    }
    
    public String getProgramTitle() {
        return programTitle;
    }
    
    public String getMainClass() {
        return mainClass;
    }
    
    public String getJARFile() {
        return JARFile;
    }
    
    public String toString() {
        return "LaunchData: " + programTitle + " (" + JARFile + ":" + mainClass + ")";
    }
}
