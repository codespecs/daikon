package daikon.temporal;
import daikon.*;
import java.util.*;

public class ErdosEvent extends Event {
  String msgname;
  Object values[];
  static Vector events=new Vector();

  private ErdosEvent(String varname, Object values[]) {
    this.msgname=varname; this.values=values;
  }

  public boolean forwardMatches(Event e) {
    if(e instanceof ErdosEvent){
      ErdosEvent ev=(ErdosEvent)e;
      if(!ev.msgname.equals(msgname)) return false;
      if(!(ev.values.length==values.length)) return false;
      for(int i=0;i<values.length;i++) if(!values[i].equals(ev.values[i])) return false;
      return true;
    }else if(e instanceof ErdosSampleEvent){
      ErdosSampleEvent esm=(ErdosSampleEvent)e;
      ValueTuple vt=esm.vt;
      VarInfo vars[]=esm.ppt.var_infos;
      Object value=null;
      for(int i=0;i<vars.length;i++)
        if(vars[i].name.name().equals(msgname)) value=vt.getValue(vars[i]);
      return values[0].equals(value);
    }
    return false;
  }

  public boolean forwardSharesTypeWith(Event e) {
    if(e instanceof ErdosSampleEvent) return true;
    if(!(e instanceof ErdosEvent)) return false;
    return msgname.equals(((ErdosEvent)e).msgname);
  }

  public String toString(){
    StringBuffer sb=new StringBuffer(msgname+"(");
    for(int i=0;i<values.length;i++)
      sb.append(values[i]+(i==values.length-1?"":","));
    sb.append(")");
    return sb.toString();
  }

  public static ErdosEvent getEvent(String varname, Object par[]){
    ErdosEvent ee=new ErdosEvent(varname, par);
    for(int i=0;i<events.size();i++)  if(ee.forwardMatches((Event)events.elementAt(i))) {
  //    System.out.print("H");
      return (ErdosEvent)events.elementAt(i);
    }
    System.out.print("N");
    events.add(ee);
    return ee;
  }

}
