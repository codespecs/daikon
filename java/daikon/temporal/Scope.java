package daikon.temporal;

import java.util.*;

//FIXME: In the Future(TM), actually use this structure to store
//scope state. Right now this is a trick for rollbacks. Do the same
//thing to InvariantState
class ScopeState
{
    EventRecord eventsSeen;
    boolean isActive;
    boolean enteredBefore;
    boolean exitedBefore;
}

public abstract class Scope extends EventReceptor
{
    protected EventRecord mEventsSeen;

    protected Collection mChildScopes;
    protected Collection mChildInvariants;

    protected boolean isActive;

    protected boolean enteredBefore;
    protected boolean exitedBefore;

    Scope()
    {
	super();
	
	mEventsSeen = new EventRecord();

	mChildScopes = new Vector();
	mChildInvariants = new Vector();

	isActive = false;
	enteredBefore = false;
	exitedBefore = false;
    }

    //FIXME: Do I want to save/restore isActive?
    Object generateStateSnapshot()
    {
	ScopeState out = new ScopeState();
	
	out.eventsSeen = mEventsSeen.duplicate();
	out.isActive = isActive;
	out.enteredBefore = enteredBefore;
	out.exitedBefore = exitedBefore;

	return out;
    }

    void restoreState(Object snapshot)
    {
	ScopeState s = (ScopeState)snapshot;

	mEventsSeen = s.eventsSeen.duplicate();
	isActive = s.isActive;
	enteredBefore = s.enteredBefore;
	exitedBefore = s.exitedBefore;
    }

    Hashtable generateSubtreeSnapshots()
    {
	Hashtable out = new Hashtable();

	out.put(this, generateStateSnapshot());

	for(Iterator i = mChildInvariants.iterator(); i.hasNext(); )
	    {
		EventReceptor r = (EventReceptor)i.next();

		out.put(r, r.generateStateSnapshot());
	    }

	for(Iterator i = mChildScopes.iterator(); i.hasNext(); )
	    {
		Scope s = (Scope)i.next();

		out.putAll(s.generateSubtreeSnapshots());
	    }

	return out;
    }

    boolean seenEvent(Event e)
    {
	return mEventsSeen.hasEventMatching(e);
    }

    void addChild(EventReceptor r)
    {
	r.mParent = this;

	if (r instanceof Scope)
	    {
		mChildScopes.add(r);
	    }
	else
	    {
		mChildInvariants.add(r);
	    }
    }

    void deleteChild(EventReceptor r)
    {
	//FIXME: Do I need to be careful about garbage collection here? Don't think so, but.

	if (r instanceof Scope)
	    {
		mChildScopes.remove(r);
	    }
	else
	    {
		mChildInvariants.remove(r);
	    }
    }

    void delete()
    {
	super.delete();

	for(Iterator i = mChildScopes.iterator(); i.hasNext(); )
	    {
		((EventReceptor)i.next()).delete();
	    }

	for(Iterator i = mChildInvariants.iterator(); i.hasNext(); )
	    {
		((EventReceptor)i.next()).delete();
	    }
    }

    void setParent(Scope s)
    {
	s.addChild(this);
    }

    void addChildren(Vector v)
    {
	for(Iterator i = v.iterator(); i.hasNext(); )
	    {
		addChild((EventReceptor)i.next());
	    }
    }

    void sendEventToKids(Event e)
    {
	mEventsSeen.add(e);

	for(Iterator i = mChildScopes.iterator(); i.hasNext(); )
	    {
		((EventReceptor)i.next()).processEvent(e);
	    }

	for(Iterator i = mChildInvariants.iterator(); i.hasNext(); )
	    {
		((EventReceptor)i.next()).processEvent(e);
	    }
    }

    void enter()
    {
	isActive = true;

	for(Iterator i = mChildScopes.iterator(); i.hasNext(); )
	    {
		((EventReceptor)i.next()).parentScopeEntering();
	    }

	for(Iterator i = mChildInvariants.iterator(); i.hasNext(); )
	    {
		((EventReceptor)i.next()).parentScopeEntering();
	    }
    }		

    void exit()
    {
	isActive = false;
	exitedBefore = true;

	for(Iterator i = mChildScopes.iterator(); i.hasNext(); )
	    {
		((EventReceptor)i.next()).parentScopeExiting();
	    }

	for(Iterator i = mChildInvariants.iterator(); i.hasNext(); )
	    {
		((EventReceptor)i.next()).parentScopeExiting();
	    }
    }

    void parentScopeExiting()
    {
	if (isActive())
	    {
		exit();
	    }
    }

    boolean isActive()
    {
	return isActive;
    }

    Vector duplicateChildren()
    {
	Vector out = new Vector();

	for(Iterator i = mChildScopes.iterator(); i.hasNext(); )
	    {
		out.add(((EventReceptor)i.next()).produceDuplicate());
	    }

	for(Iterator i = mChildInvariants.iterator(); i.hasNext(); )
	    {
		out.add(((EventReceptor)i.next()).produceDuplicate());
	    }

	return out;
    }

    //FIXME: Use a different collection here?
    //This routine returns a Hashtable mapping nodes in the
    //EventReceptor tree to Vectors of new nodes which should be added
    //off of them. This is done so that all the new nodes can be added
    //at once, without separate computations interfering with one another.
    Hashtable generateNewCandidates(Event e)
    {
	Hashtable out = new Hashtable();

	if (!isActive())
	    return out;

	//Recurse first, damn you	
	//Is this correct (in particular re children with stuff
	//depending on things which begin or close this scope)
	for(Iterator i = mChildScopes.iterator(); i.hasNext(); )
	    {
		out.putAll(((Scope)i.next()).generateNewCandidates(e));
	    }

	if (seenEvent(e))
	    return out;

	Vector newKids = new Vector();

	if (mEventsSeen.noEventsConflictWith(e))
	    {
		newKids.add(new AlwaysInvariant(e));
	    }

	//FIXME: Possible subtle semantics error here?
	if (!exitedBefore)
	    {
		newKids.add(new EventuallyInvariant(e));
	    }

	ScopeAfter afterE = new ScopeAfter(e);
	afterE.isActive = true;
	newKids.add(afterE);

	ScopeBefore newBeforeE = new ScopeBefore(e);
	newKids.add(newBeforeE);

	for(Iterator i = mChildScopes.iterator(); i.hasNext(); )
	    {
		EventReceptor r = (EventReceptor)i.next();
	    
		//		//Add "before e, r" for each child scope r
		//		ScopeBefore newBeforeE = new ScopeBefore(e);

		newBeforeE.addChild(r.produceDuplicate());
		//		newKids.add(newBeforeE);

		if (r instanceof ScopeAfter)
		    {
			//r is a ScopeAfter, aka our marker for the other
			//events seen in here
			ScopeAfter af = (ScopeAfter)r;

			//Create a scope which is after r's event, (weak)until e
			ScopeAfterUntil newAfterUntil = new ScopeAfterUntil(af.mEvent, e);
			newAfterUntil.addChildren(af.duplicateChildren());

			EventuallyInvariant erespondstor = new EventuallyInvariant(newAfterUntil, e);

			newKids.add(newAfterUntil);

			ScopeBetween newBetween = new ScopeBetween(af.mEvent, e);
			newBetween.addChildren(af.duplicateChildren());

			newKids.add(newBetween);
		    }
	    }

	for(Iterator i = mChildInvariants.iterator(); i.hasNext(); )
	    {
		EventReceptor r = (EventReceptor)i.next();

		//		ScopeBefore newBeforeE = new ScopeBefore(e);

		newBeforeE.addChild(r.produceDuplicate());
		
		//		newKids.add(newBeforeE);
	    }

	out.put(this, newKids);

	return out;
    }

    public String toString()
    {
	return toString(0);
    }
    
    public abstract String getNameString();

    private static String makeSpacing(int l)
    {
	StringBuffer space = new StringBuffer();

	for(int i = 0; i < l; i++)
	    {
		space.append("    ");
	    }
    
	return space.toString();
    }
	
    public String toString(int level)
    {
	StringBuffer res = new StringBuffer();

	String spaceString = makeSpacing(level);

	res.append(spaceString + getNameString() + "\n");

	for(Iterator i = mChildInvariants.iterator(); i.hasNext(); )
	    {
		res.append(spaceString + i.next().toString() + "\n");
	    }

	for (Iterator i = mChildScopes.iterator(); i.hasNext(); )
	    {
		res.append(((Scope)i.next()).toString(level + 1));
	    }

	res.append("\n");

	return res.toString();
    }

    //override me!
    Scope instantiateDuplicateScope()
    {
	return null;
    }

    EventReceptor produceDuplicate()
    {
	Scope s = instantiateDuplicateScope();

	tiedTo.add(s);

	s.mEventsSeen = mEventsSeen.duplicate();
	s.enteredBefore = enteredBefore;
	s.exitedBefore = exitedBefore;
	s.addChildren(duplicateChildren());

	return s;
    }
}

class ScopeGlobal extends Scope
{
    public boolean doDynamicInstantiation;

    public static ScopeGlobal GLOBAL_SCOPE = null;

    ScopeGlobal()
    {
	doDynamicInstantiation = true;

	ScopeGlobal.GLOBAL_SCOPE = this;

	mParent = null;
    }

    EventReceptor produceDuplicate()
    {
	throw new RuntimeException("Whatchoo talkin' bout, willis?");
    }

    public void printState()
    {
	System.out.println(toString());
    }

    public String getNameString()
    {
	return "GLOBAL:";
    }

    public void processEvent(Event e)
    {
	Vector v = new Vector();

	v.add(e);

	processEvent(e, v);
    }

    public void processEvent(Event sampleEvent, Vector basicEvents)
    {
	Vector newStuffHashes = null;

	if (doDynamicInstantiation)
	    {
		for(Iterator i = basicEvents.iterator(); i.hasNext(); )
		    {
			newStuffHashes.add(generateNewCandidates((Event)i.next()));
		    }
	    }

	sendEventToKids(sampleEvent);

	if (doDynamicInstantiation)
	    {		
		for(Iterator i = newStuffHashes.iterator(); i.hasNext(); )
		    {
			Hashtable res = (Hashtable)i.next();

			for(Iterator j = res.keySet().iterator(); j.hasNext(); )
			    {
				Scope s = (Scope)j.next();

				s.addChildren((Vector)res.get(s));
			    }
		    }
	    }
    }
}

class ScopeBefore extends Scope
{
    Event mEvent;

    ScopeBefore(Event e)
    {
	super();

	mEvent = e;
    }

    void parentScopeEntering()
    {
	enter();
    }

    void processEvent(Event e)
    {
	if (isActive())
	    {
		if (mEvent.matches(e))
		    {
			exit();
		    }
		else
		    {
			sendEventToKids(e);
		    }
	    }
    }

    Scope instantiateDuplicateScope()
    {
	return new ScopeBefore(mEvent);
    }

    public String getNameString()
    {
	return "(BEFORE " + mEvent.toString() + "):";
    }

}

//Note: Could have implemented this as a ScopeNot with a ScopeBefore.
//Should I?
class ScopeAfter extends Scope
{
    Event mEvent;
    
    ScopeAfter(Event e)
    {
	super();
	
	mEvent = e;
    }

    void processEvent(Event e)
    {
	if (!isActive())
	    {
		if (mEvent.matches(e))
		    {
			enter();
		    }
	    }
	else
	    {
		sendEventToKids(e);
	    }
    }

    Scope instantiateDuplicateScope()
    {
	return new ScopeAfter(mEvent);
    }

    public String getNameString()
    {
	return "(AFTER " + mEvent.toString() + "):";
    }
}			

//FIXME: MAJOR: Semantics are wrong. Scope should be limited to smallest
//space between A and B (i.e. AAB only in scope between the second A and the
//b, or rather that is the only scope which needs to be checked, by inclusion)
//This same problem probably also applies to afteruntil (find out!)
//Could probably also be correctly written as ScopeAfterBefore, but who cares
class ScopeBetween extends Scope
{
    Event mEventA;
    Event mEventB;

    private Hashtable savedStates;
    private Vector kidsCreated;

    ScopeBetween(Event a, Event b)
    {
	super();

	mEventA = a;
	mEventB = b;
    }

    void enter()
    {
	savedStates = new Hashtable();
	kidsCreated = new Vector();

	savedStates.putAll(generateSubtreeSnapshots());

	super.enter();
    }

    Hashtable generateNewCandidates(Event e)
    {
	Hashtable res = super.generateNewCandidates(e);

	for(Iterator i = res.keySet().iterator(); i.hasNext(); )
	    {
		EventReceptor r = (EventReceptor)i.next();

		if (r.equals(this) || r.isChildOf(this))
		    {
			kidsCreated.addAll((Vector)res.get(r));
		    }
	    }

	return res;
    }
    
    void rollback()
    {
	for(Iterator i = kidsCreated.iterator(); i.hasNext(); )
	    {
		((EventReceptor)i.next()).delete();
	    }

	for(Iterator i = savedStates.keySet().iterator(); i.hasNext(); )
	    {
		EventReceptor r = (EventReceptor)i.next();

		r.restoreState(savedStates.get(r));
	    }

	//FIXME: May be unnecessary, given that "this" is stored in savedStates by generateSubtreeSnapshots.
	//Seems kinda sketchy.
	isActive = false;
    }


    void parentScopeExiting()
    {
	if (isActive())
	    {
		//Parent scope is closing. This means we never saw our close event,
		//so we never actually were active. ha! we were just kidding		
		rollback();
	    }
	//	else
	//	    {
	//		exit();
	//	    }
    }

    void processEvent(Event e)
    {
	if (!isActive())
	    {
		if (mEventA.matches(e))
		    {
			enter();
		    }
	    }
	else
	    {
		if (mEventB.matches(e))
		    {
			exit();
		    }
		else
		    {
			sendEventToKids(e);
		    }
	    }
    }

    Scope instantiateDuplicateScope()
    {
	return new ScopeBetween(mEventA, mEventB);
    }

    public String getNameString()
    {
	return "(BETWEEN " + mEventA.toString() + " and " + mEventB.toString() + "):";
    }
}

//This scope has an exception - its ending event can be the same thing as an event
//contained within (eg in an eventually invariant) - to allow sensible response
//invariants
class ScopeAfterUntil extends Scope
{
    Event mEventA;
    Event mEventB;

    ScopeAfterUntil(Event a, Event b)
    {
	super();

	mEventA = a;
	mEventB = b;

	//This may not be right. This is done to suppress generation of subscopes/etc
	//which depend on event B (except for the explicitly generated response invariant).
	//In fact, this is ugly/hackish enough that it's almost certainly not right..
	//but that's why it's a FIXME.
	mEventsSeen.add(b);
    }

    void processEvent(Event e)
    {
	if (!isActive())
	    {
		if (mEventA.matches(e))
		    {
			isActive = true;
		    }
	    }
	else
	    {
		sendEventToKids(e);

		if (mEventB.matches(e))
		    {
			exit();
		    }
	    }
    }

    Scope instantiateDuplicateScope()
    {
	return new ScopeAfterUntil(mEventA, mEventB);
    }
	
    public String getNameString()
    {
	return "(AFTER " + mEventA.toString() + " UNTIL " + mEventB.toString() + "):";
    }	
}

	
    
