package daikon.repair;
import daikon.*;
import java.util.*;

/**
 * This class stores the state used to generate a repair
 * specification.  In particular, it stores quantifiers, mappings from
 * fields to relations, and mappings from variables to sets.
 **/

public class Repair {
    private static Repair repair;

    public static Repair getRepair() {
	if (repair==null)
	    repair=new Repair();
	return repair;
    }
    Hashtable settable=new Hashtable();
    Hashtable relationtable=new Hashtable();
    Hashtable definitiontable=new Hashtable();
    int tagnumber=0;
    boolean forceset=false;
    Hashtable quantifiers=new Hashtable();

    Definition d=new Definition();

    public void noForceSet() {
	forceset=false;
    }
    public void setForceSet() {
	forceset=true;
    }
    /** This method returns a the range set for the relation given by
     * the second parameter.*/
    public String getRange(Ppt ppt, String relation) {
	Definition d=(Definition)definitiontable.get(ppt);
	if (d==null)
	    return null;
	return (String)d.rangetable.get(relation);
    }

    /** This method resets the quantifier table. */
    public void reset() {
	quantifiers=new Hashtable();
	forceset=false;
    }

    /** The repair system is designed to capture equality constriants
     * using relations.  For constraints involving local variables, we
     * may need to synthesize an "special" object.  */
    public void addSpecial() {
	appendQuantifier("s_quant","Special");
    }

    /** This method generates the current quantifier string. */
    public String getQuantifiers() {
	String str="";
	for(Iterator it=quantifiers.keySet().iterator();it.hasNext();) {
	    String key=(String)it.next();
	    if (!str.equals(""))
		str+=",";
	    str+="forall "+key+" in "+((String)quantifiers.get(key));
	}
	return str;
    }

    /** This method appends a quantifier to the quantifier list. */
    public void appendQuantifier(String q, String set) {
	if (!quantifiers.containsKey(q))
	    quantifiers.put(q,set);
    }

    /** This method takes in a program point, the domain set, a
     * field, and the full field name fld, and returns the
     * corresponding relation. */

    public String getRelation(Ppt ppt, String set, String field, String fld) {
	Tuple t=new Tuple(set,field,ppt);
	if (relationtable.containsKey(t))
	    return (String)relationtable.get(t);
	String relationname=generateRelationName(field);
	String rangeset=generateSetName("R"+field);
	String newrule="[forall s in "+set+"], true => <s,s."+field+"> in "+relationname+";";
	String newrule2="[forall s in "+set+"], true => s."+field+" in "+rangeset+";";
	Definition d=(Definition)definitiontable.get(ppt);
	d.rangetable.put(relationname,rangeset);
	String setdef=relationname+": "+set+"->"+rangeset+";";
	appendModelRule(ppt,newrule);
	appendModelRule(ppt,newrule2);
	appendSetRelation(ppt,setdef);
	appendSetRelation(ppt,"set "+rangeset+"("+getTypedef(ppt,fld)+");");
	relationtable.put(t,relationname);
	return relationname;
    }

    /** This method takes in a local program variable and a program
     * point, and returns a relation. */

    public String getRelation(String programvar, Ppt ppt) {
	Tuple t=new Tuple(programvar,ppt);
	if (relationtable.containsKey(t))
	    return (String)relationtable.get(t);
	String relationname=generateRelationName(programvar);

	Tuple t2=new Tuple(programvar,ppt);
	boolean generatesetdef=true;
	if (settable.containsKey(t2))
	    generatesetdef=false;
	String setname=generateSetName(programvar);
	String newrule="[forall s in Special], true => <s,"+programvar+"> in "+relationname+";";

	appendModelRule(ppt,newrule);

	if (generatesetdef) {
	    //Generate set definition
	    String setdef="set "+setname+"("+getTypedef(ppt, programvar)+");";
	    appendSetRelation(ppt,setdef);
	    settable.put(t,setname);
	}

	{
	    //Generate relation definition
	    String relationdef=relationname+": Special->"+setname+";";
	    appendSetRelation(ppt,relationdef);
	}

	{
	    //Generate global definition
	    String vardef="";
	    if (!getType(ppt,programvar).equals("int"))
		vardef=getType(ppt,programvar)+programvar+";";
	    else
		vardef="int "+programvar+";";
	    appendGlobal(ppt,vardef);
	}

	if (!definitiontable.containsKey(ppt)) {
	    definitiontable.put(ppt,new Definition());
	}
	Definition d=(Definition)definitiontable.get(ppt);
	if (!d.generatespecial) {
	    //Generate special set
	    d.generatespecial=true;
	    appendSetRelation(ppt,"set Special(int);");
	    appendModelRule(ppt,"[],true => 0 in Special");
	}
	//store relation in table and return it
	relationtable.put(t,relationname);
	return relationname;
    }

    /** This method takes in a program variable and a program point
     * and returns the corresponding setname or a variable that
     * quantifies over that set. */

    public String getSet(String programvar, Ppt ppt) {
	String setname=getRealSet(programvar,ppt);
	if (forceset)
	    return setname;
	else {
	    appendQuantifier(programvar,setname);
	    return programvar;
	}
    }

    /** This method takes in a program variable and a program point
     * and returns the corresponding setname. */

    public String getRealSet(String programvar, Ppt ppt) {
	Tuple t=new Tuple(programvar,ppt);

	if (settable.containsKey(t)) {
	    String setname=(String)settable.get(t);
	    return setname;
	}
	String setname=generateSetName(programvar);
	String newrule="[], true => "+programvar+" in "+setname+";";
	String setdef="set "+setname+"("+getTypedef(ppt, programvar)+");";
	appendModelRule(ppt,newrule);
	appendSetRelation(ppt,setdef);
	String vardef="";
	if (!getType(ppt,programvar).equals("int"))
	    vardef=getType(ppt,programvar)+" "+programvar+";";
	else
	    vardef="int "+programvar+";";
	appendGlobal(ppt,vardef);

	settable.put(t,setname);
	return setname;
    }

    /** This method returns the model definition rules for a given
     * program point. */

    public String getRules(Ppt ppt) {
	Definition d=(Definition)definitiontable.get(ppt);
	if (d==null)
	    return null;
	return d.modelrule;
    }

    /** This method returns the global definitions for a given program
     * point. */

    public String getGlobals(Ppt ppt) {
	Definition d=(Definition)definitiontable.get(ppt);
	if (d==null)
	    return null;
	return d.globaldecls;
    }

    /** This method returns the set and relation definitions for a
     * given program point. */

    public String getSetRelation(Ppt ppt) {
	Definition d=(Definition)definitiontable.get(ppt);
	if (d==null)
	    return null;
	return d.setrelation;
    }

    /** This method returns the type of a variable. */

    static public String getType(Ppt ppt, String var) {
	VarInfo vi=ppt.findVar(var);
	String str=vi.type.toString();
	while(str.indexOf("[]")!=-1) {
	    int location=str.indexOf("[]");
	    str=str.substring(0,location)+"*"+str.substring(location+2,str.length());
	}
	return str;
    }

    /** This method returns the type of a variable once
     * dereferenced. */

    static public String getTypedef(Ppt ppt, String var) {
	String str=getType(ppt,var);
	int last=str.lastIndexOf("*");
	if (last!=-1) {
	    return str.substring(0,last);
	} else return str;
    }

    /** This method generates a set name from a program variable. */

    private String generateSetName(String programvar) {
	String setname="S"+programvar+tagnumber;
	tagnumber++;
	return setname;
    }

    /** This method generates a relation name from a program
     * variable. */

    private String generateRelationName(String fieldvar) {
	String setname="R"+fieldvar+tagnumber;
	tagnumber++;
	return setname;
    }

    /** This method appents a set or relation definition for a given
     * program point. */

    void appendSetRelation(Ppt p, String s) {
	if (!definitiontable.containsKey(p)) {
	    definitiontable.put(p,new Definition());
	}
	Definition d=(Definition)definitiontable.get(p);
	d.appendSetRelation(s);
    }

    /** This method appends a model definition rule for a given
     * program point. */

    void appendModelRule(Ppt p, String s) {
	if (!definitiontable.containsKey(p)) {
	    definitiontable.put(p,new Definition());
	}
	Definition d=(Definition)definitiontable.get(p);
	d.appendModelRule(s);
    }


    /** This method appends a global definition for a given program
     * point. */
    void appendGlobal(Ppt p, String s) {
	if (!definitiontable.containsKey(p)) {
	    definitiontable.put(p,new Definition());
	}
	Definition d=(Definition)definitiontable.get(p);
	d.appendGlobal(s);
    }

    /** Generic tuple class.  Implements hashcode and equals.  */
    public static class Tuple {
	Object a;
	Object b;
	Object c;

	Tuple(Object a, Object b) {
	    this.a=a;
	    this.b=b;
	    this.c=null;
	}
	Tuple(Object a, Object b,Object c) {
	    this.a=a;
	    this.b=b;
	    this.c=c;
	}
	public int hashCode() {
	    int h=a.hashCode()^b.hashCode();
	    if (c!=null)
		h^=c.hashCode();
	    return h;
	}
	public boolean equals(Object o) {
	    if (!((o instanceof Tuple)&&
		((Tuple)o).a.equals(a)&&
		((Tuple)o).b.equals(b)))
		return false;
	    if (c==null&&((Tuple)o).c==null)
		return true;
	    if (c==null||((Tuple)o).c==null)
		return false;
	    return ((Tuple)o).c.equals(c);
	}
    }

    /** This class stores information on a given program point. */

    public static class Definition {
	String setrelation="";
	String modelrule="";
	String globaldecls="";
	boolean generatespecial=false;

	Hashtable rangetable=new Hashtable();

	void appendGlobal(String g) {
	    globaldecls+=g+"\n";
	}

	void appendSetRelation(String sr) {
	    setrelation+=sr+"\n";
	}
	void appendModelRule(String mr) {
	    modelrule+=mr+"\n";
	}
    }
}
