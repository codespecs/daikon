/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc;

public interface CatchBlockData {
    public int getStartPC();
    public int getEndPC();
    public int getHandlerPC();
    public String getCatchType();
}
