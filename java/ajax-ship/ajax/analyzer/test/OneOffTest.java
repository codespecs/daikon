/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.test;

import ajax.jbc.*;
import ajax.jbc.util.*;
import java.io.*;
import java.util.*;
import java.text.*;
import ajax.analyzer.semi.*;

public class OneOffTest {
    public static void main(String[] args) {
        SEMIAnalyzer analyzer = new SEMIAnalyzer((JBCWorld)null);
        
        analyzer.addQueryFamily(null);
        analyzer.addTargetDatum(null, null, 0, null, null, null);
    }
}
