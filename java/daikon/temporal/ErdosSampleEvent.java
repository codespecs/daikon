package daikon.temporal;
import daikon.*;
import java.util.*;

public class ErdosSampleEvent extends Event {
  Ppt ppt; ValueTuple vt;

  public ErdosSampleEvent(Ppt ppt, ValueTuple vt) {
    this.ppt=ppt; this.vt=vt;
  }
  public boolean forwardMatches(Event parm1) {
    return false;
  }
  public boolean forwardSharesTypeWith(Event parm1) {
    return true;
  }

  public Vector generateBasicEvents(){
    Vector rez=new Vector();
    VarInfo vars[]=ppt.var_infos;

    //System.out.println("DEBUG: vars.length = " + String.valueOf(vars.length));

    for(int i=0;i<vars.length;i++){ //vars.length
      VarInfo var=vars[i];
      if(var.name.name().endsWith("_c")){
        Object value=vt.getValue(var);
        rez.add(ErdosEvent.getEvent(var.name.name(), new Object[]{value}));
      }
    }
    return rez;
  }
}
