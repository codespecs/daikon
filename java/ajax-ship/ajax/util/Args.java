/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.util;

import ajax.Globals;
import java.io.*;

public class Args {
    private String[] args;
    private PrintWriter err = null;
    private String usage;
    
    protected PrintWriter getErr() {
        if (err == null) {
            err = new PrintWriter(System.err, true);
        }
        
        return err;
    }
    
    private void deleteArgAt(int index) {
        String[] newArgs = new String[args.length - 1];
        
        System.arraycopy(args, 0, newArgs, 0, index);
        System.arraycopy(args, index + 1, newArgs, index, newArgs.length - index);
        args = newArgs;
    }
    
    public Args(String[] args, String usage) {
        this.args = (String[])args.clone();
        this.usage = usage;
    }
    
    public Error printUsageErrorAndDie(String s) {
        getErr().println(s);
        return printUsageAndDie();
    }
    
    public Error printUsageAndDie() {
        getErr().println(usage);
        System.exit(1);
        
        return new Error(); // this is just to make control flow clear
    }
    
    public boolean extractBoolOption(String option) {
        for (int i = 0; i < args.length; i++) {
            if (args[i].equals(option)) {
                deleteArgAt(i);
                return true;
            }
        }
        
        return false;
    }
    
    public int extractIntOption(String option, int def) {
        String s = extractStringOption(option, null);
        
        if (s == null) {
            return def;
        } else {
            try {
                return Integer.parseInt(s);
            } catch (NumberFormatException ex) {
                getErr().println("Bad numeric argument to " + option + " option: " + s);
                throw printUsageAndDie();
            }
        }
    }
    
    public String extractStringOption(String option, String def) {
        for (int i = 0; i < args.length; i++) {
            if (args[i].equals(option)) {
                if (i + 1 < args.length) {
                    String s = args[i + 1];
                    
                    deleteArgAt(i);
                    deleteArgAt(i);
                    return s;
                } else {
                    getErr().println("Expected parameter to " + option + " option");
                    throw printUsageAndDie();
                }
            }
        }
        
        return def;
    }
    
    public void checkDone() {
        if (args.length > 0) {
            getErr().println("Unexpected argument: " + args[0]);
            throw printUsageAndDie();
        }
    }
    
    public String[] getRemaining() {
        return args;
    }
    
    public String extractNextArg(String name) {
        if (args.length > 0) {
            String s = args[0];
            
            deleteArgAt(0);
            return s;
        } else {
            getErr().println("Required " + name + " argument");
            throw printUsageAndDie();
        }
    }

    public String extractNextOptionalArg() {
        if (args.length > 0) {
            String s = args[0];
            
            deleteArgAt(0);
            return s;
        } else {
            return null;
        }
    }
}
