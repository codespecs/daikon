package daikon.temporal;

import java.util.*;

/**
 * This class captures behavior common to all scopes. Each scope must
 * keep track of which events it has seen already (for the candidate
 * invariant instantiation process), what its children are, and some
 * self-explanatory state information. This class also provides various
 * utility methods used in the myriad tree-walks which are part of the
 * temporal invariant detection process.
 *
 * A key method is generateNewCandidates, which produces the list
 * of new invariants to create, based on a new event.
 *
 * Specific scope types are defined later in the file.
 **/

public abstract class Scope extends EventReceptor
{
    // A structure to record what events have been already seen
    protected EventRecord mEventsSeen;

    // Collections of child scopes and invariants
    protected Collection mChildScopes;
    protected Collection mChildInvariants;

    // Is the scope currently active (i.e. entered but not yet exited)
    protected boolean isActive;

    // Has the scope been entered/exited before?
    protected boolean enteredBefore;
    protected boolean exitedBefore;

    // This is the maximum tree depth for dynamic invariant instantiation. Controls
    // how complicated the invariants detected by the system will be.
    public static int MAX_DEPTH = 2;

    Scope()
    {
	super();

	mEventsSeen = new EventRecord();

	mChildScopes = new Vector(0,0);
	mChildInvariants = new Vector(0,0);

	isActive = false;
	enteredBefore = false;
	exitedBefore = false;
    }

    // This class is used to save scope state (for restoring purposes).
    // This may be used by the generateStateSnapshot/restoreStateSnapshot
    // routines.
    static private class ScopeState
    {
        EventRecord eventsSeen;
        boolean isActive;
        boolean enteredBefore;
        boolean exitedBefore;
    }


    /**
     * This method generates a snapshot of scope state for rollback
     * purposes. Rollback occurs when a between scope never encounters
     * its closing event.
     **/

    // FIXME: Do I want to save/restore isActive? Yes, I think.
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

    // This generates snapshots for all descendants of this scope.
    Hashtable generateSubtreeSnapshots()
    {
	Hashtable out = new Hashtable();

	out.put(this, generateStateSnapshot());

	for (Iterator i = mChildInvariants.iterator(); i.hasNext(); )
	    {
		EventReceptor r = (EventReceptor)i.next();

		out.put(r, r.generateStateSnapshot());
	    }

	for (Iterator i = mChildScopes.iterator(); i.hasNext(); )
	    {
		Scope s = (Scope)i.next();

		out.putAll(s.generateSubtreeSnapshots());
	    }

	return out;
    }

    // Checks if the given event has been seen by this scope.
    public boolean seenEvent(Event e)
    {
	return mEventsSeen.hasEventMatching(e);
    }

    // Add a child to the appropriate collection.
    public void addChild(EventReceptor r)
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

    // Delete the child from the collection.
    void deleteChild(EventReceptor r)
    {
	// FIXME: Do I need to be careful about garbage collection here? Don't think so, but.

	if (r instanceof Scope)
	    {
		mChildScopes.remove(r);
	    }
	else
	    {
		mChildInvariants.remove(r);
	    }
    }

    // Delete this scope and all its children.
    void delete()
    {
	super.delete();

	for (Iterator i = mChildScopes.iterator(); i.hasNext(); )
	    {
		((EventReceptor)i.next()).delete();
	    }

	for (Iterator i = mChildInvariants.iterator(); i.hasNext(); )
	    {
		((EventReceptor)i.next()).delete();
	    }
    }

    // More scope-tree bookkeeping.
    void setParent(Scope s)
    {
	s.addChild(this);
    }

    public void addChildren(Vector v)
    {
	for (Iterator i = v.iterator(); i.hasNext(); )
	    {
		addChild((EventReceptor)i.next());
	    }
    }

    // This sends the supplied events to all kids of the scope (and records the fact that the
    // event has now been seen).
    void sendEventToKids(Event e)
    {
	// FIXME: Restructure so that the basic events aren't recomputed
	// (and so that this backwards dependency is removed). Not too hard;
	// just need to always pass around both the sample event and the
	// basic events..
	mEventsSeen.add(e);

	mEventsSeen.addAll(TemporalInvariantManager.Manager.generateBasicEventsFromSample(e));


	for (Iterator i = mChildScopes.iterator(); i.hasNext(); )
	    {
		((EventReceptor)i.next()).processEvent(e);
	    }

	for (Iterator i = mChildInvariants.iterator(); i.hasNext(); )
	    {
		((EventReceptor)i.next()).processEvent(e);
	    }
    }

    /**
     * When you enter a scope, tell your kids about it (and remember
     * that you are now active).
     **/
    public void enter()
    {
	isActive = true;

	for (Iterator i = mChildScopes.iterator(); i.hasNext(); )
	    {
		((EventReceptor)i.next()).parentScopeEntering();
	    }

	for (Iterator i = mChildInvariants.iterator(); i.hasNext(); )
	    {
		((EventReceptor)i.next()).parentScopeEntering();
	    }
    }

    /**
     * When you exit, tell your kids about it and fix your state.
     **/
    public void exit()
    {
	isActive = false;
	exitedBefore = true;

	for (Iterator i = mChildScopes.iterator(); i.hasNext(); )
	    {
		((EventReceptor)i.next()).parentScopeExiting();
	    }

	for (Iterator i = mChildInvariants.iterator(); i.hasNext(); )
	    {
		((EventReceptor)i.next()).parentScopeExiting();
	    }
    }

    // If your parent exits, you exit too.
    void parentScopeExiting()
    {
	if (isActive())
	    {
		exit();
	    }
    }

    public boolean isActive()
    {
	return isActive;
    }

    /**
     * This is called during the candidate invariant generation
     * process to duplicate child scopes and invariants and produce
     * the proper records. This is needed whenever a new event E is
     * seen in a scope, as it might be true that everything that is
     * true so far is only true before E is seen.
     **/
    Vector duplicateChildren()
    {
	// FIXME: Pass level around so that redundant getDepth() computations
	// can be spared.

	if (getDepth() >= MAX_DEPTH)
	{
		return new Vector();
	}

	Vector out = new Vector();

	for (Iterator i = mChildScopes.iterator(); i.hasNext(); )
	    {
		out.add(((EventReceptor)i.next()).produceDuplicate());
	    }

	for (Iterator i = mChildInvariants.iterator(); i.hasNext(); )
	    {
		out.add(((EventReceptor)i.next()).produceDuplicate());
	    }

	return out;
    }

    // A wrapper to take care of the recursion depth issue - we don't want
    // to generate invariants specific to particular executions
    Hashtable generateNewCandidates(Event e)
    {
	return generateNewCandidates(e, 0);
    }

    /**
     * This hairy routine does the vast bulk of the candidate invariant
     * generation. The basic idea is that it collects together a Hashtable
     * containing (scope . vector-of-new-children) pairs which the global
     * scope then adds to the invariant tree. The new invariants are collected
     * in this way (and not immediately added) so that they don't interfere
     * with the regular processing of the event which caused their creation.
     *
     * It does this by first asking its kids what new invariants they want
     * to create, then following a set of rules (outlined elsewhere, FIXME -
     * describe them here) which determine what new invariants are possible
     * as immediate children.
     **/

    // FIXME: Use a different collection here?
    // This routine returns a Hashtable mapping nodes in the
    // EventReceptor tree to Vectors of new nodes which should be added
    // off of them. This is done so that all the new nodes can be added
    // at once, without separate computations interfering with one another.
    Hashtable generateNewCandidates(Event e, int level)
    {
	Hashtable out = new Hashtable();

	if (!isActive() || level >= MAX_DEPTH)
	    return out;

	// Recurse first, damn you
	// Is this correct (in particular re children with stuff
	// depending on things which begin or close this scope)
	for (Iterator i = mChildScopes.iterator(); i.hasNext(); )
	    {
		out.putAll(((Scope)i.next()).generateNewCandidates(e, level + 1));
	    }

	if (seenEvent(e))
	    return out;

	//	System.out.println("Generating new candidate invariants at level " + String.valueOf(level) + " given event " + e.toString());

	Vector newKids = new Vector(0,0);

	if (mEventsSeen.noEventsConflictWith(e))
	    {
		newKids.add(new TemporalInvariant.AlwaysInvariant(e));
	    }

	// FIXME: Possible subtle semantics error here?
	if (!exitedBefore)
	    {
		newKids.add(new TemporalInvariant.EventuallyInvariant(e));
	    }

	ScopeAfter afterE = new ScopeAfter(e);
	afterE.isActive = true;
	newKids.add(afterE);

	// FIXME: Add scope creation routines for creating scopes from
	// other scopes (to automatically correctly copy seen events, etc)
	ScopeBefore newBeforeE = new ScopeBefore(e);

	newBeforeE.mEventsSeen.add(mEventsSeen);

	newKids.add(newBeforeE);

	for (Iterator i = mChildScopes.iterator(); i.hasNext(); )
	    {
		EventReceptor r = (EventReceptor)i.next();

		//		// Add "before e, r" for each child scope r
		//		ScopeBefore newBeforeE = new ScopeBefore(e);

		newBeforeE.addChild(r.produceDuplicate());
		//		newKids.add(newBeforeE);

		if (r instanceof ScopeAfter)
		    {
			// FIXME: Correct numConfirmingSamples stuff here
			// (as in some cases it must be reset)

			// r is a ScopeAfter, aka our marker for the other
			// events seen in here
			ScopeAfter af = (ScopeAfter)r;

			// Create a scope which is after r's event, (weak)until e
			ScopeAfterUntil newAfterUntil = new ScopeAfterUntil(af.mEvent, e);
			newAfterUntil.mEventsSeen.add(af.mEventsSeen);

			newAfterUntil.addChildren(af.duplicateChildren());

			TemporalInvariant.EventuallyInvariant erespondstor = new TemporalInvariant.EventuallyInvariant(newAfterUntil, e);

			newKids.add(newAfterUntil);

			/*
			ScopeBetween newBetween = new ScopeBetween(af.mEvent, e);
			newBetween.addChildren(af.duplicateChildren());
			newBetween.mEventsSeen.add(af.mEventsSeen);

			newKids.add(newBetween);
			*/
		    }
	    }

	for (Iterator i = mChildInvariants.iterator(); i.hasNext(); )
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

    // Return a string representing the kind of scope we are.
    public abstract String getNameString();

    // Utility function to help make printouts legible
    private static String makeSpacing(int l)
    {
	StringBuffer space = new StringBuffer();

	for (int i = 0; i < l; i++)
	    {
		space.append("    ");
	    }

	return space.toString();
    }

    // Return a string representation properly indented (used to make pritned scope trees
    // legible)
    public String toString(int level)
    {
	StringBuffer res = new StringBuffer();

	String spaceString = makeSpacing(level);

	res.append(spaceString + getNameString() + " " + isActive() + "\n");

	for (Iterator i = mChildInvariants.iterator(); i.hasNext(); )
	    {
		res.append(spaceString + "    " + i.next().toString() + "\n");
	    }

	for (Iterator i = mChildScopes.iterator(); i.hasNext(); )
	    {
		res.append(((Scope)i.next()).toString(level+1));
	    }

	//	res.append("\n");

	return res.toString();
    }

    // This method is overridden by child scopes to instantiate a scope object
    // of the proper class (which is then used in produceDuplicate())
    Scope instantiateDuplicateScope()
    {
	return null;
    }

    // Duplicate this scope and tie the duplicate.
    EventReceptor produceDuplicate()
    {
	Scope s = instantiateDuplicateScope();

	setTiedTo(s);

	s.mEventsSeen = mEventsSeen.duplicate();
	s.enteredBefore = enteredBefore;
	s.exitedBefore = exitedBefore;
	s.addChildren(duplicateChildren());

	return s;
    }

    Vector getAllChildInvariants()
    {
	Vector out = new Vector(0,0);

	out.addAll(mChildInvariants);

	for (Iterator i = mChildScopes.iterator(); i.hasNext(); )
	{
		out.addAll(((Scope)i.next()).getAllChildInvariants());
	}

	return out;
    }

    /* ***************************************************************************
     * Inner classes
     */

    /**
     * This scope is special. It is entered and exited explicitly. It
     * also does some basic bookkeeping for candidate instantiation.
     **/
    public static class ScopeGlobal extends Scope
    {
	// This boolean flag controls whether dynamic invariant instantiation is turned on
	// in the system or not.
        public boolean doDynamicInstantiation;

	// The global scope is basically a singleton; each new instance of the global scope
	// makes itself the 'official' instance. This is mostly for convenience (one can reset
	// the state of the detector easily by creating a new global scope, which will then be
	// garbage collected). For safety, it might be better to instead require the caller to
	// explicitly deallocate the old global scope instead, first.
	// FIXME: Can be removed, I think, given the global_scope reference in TemporalInvariantManager.
        public static ScopeGlobal GLOBAL_SCOPE = null;

        public ScopeGlobal()
        {
            doDynamicInstantiation = true;

            ScopeGlobal.GLOBAL_SCOPE = this;

            mParent = null;
        }

        public void printState()
        {
            System.out.println(toString());
        }

        public String getNameString()
        {
            return "GLOBAL:";
        }

	// If supplied a single event, assume it is both a sample and a basic event. For convenience
	// in test suite creation (and to allow ScopeGlobal to descend from Scope).
        public void processEvent(Event e)
        {
            Vector v = new Vector();

            v.add(e);

            processEvent(e, v);
        }

	// Run the given sample through the scope/invariant hierarchy.
        public void processEvent(Event sampleEvent, Vector basicEvents)
        {
            Vector newStuffHashes = null;

            if (doDynamicInstantiation)
                {
                    newStuffHashes = new Vector();

                    for (Iterator i = basicEvents.iterator(); i.hasNext(); )
                        {
                            Event e = (Event)i.next();

                            System.out.print("DEALING WITH BASIC EVENT: " + e.toString());

                            if (!mEventsSeen.hasEventMatching(e))
                            {
                                System.out.println(" -- NEW");
                            } else {
                                System.out.println();
                            }

                            newStuffHashes.add(generateNewCandidates(e));
                        }
                }

            sendEventToKids(sampleEvent);

            //	System.out.println("\n\nNEW INVARIANTS\n\n");

            if (doDynamicInstantiation)
                {
                    for (Iterator i = newStuffHashes.iterator(); i.hasNext(); )
                        {
                            Hashtable res = (Hashtable)i.next();

                            for (Iterator j = res.keySet().iterator(); j.hasNext(); )
                                {
                                    Scope s = (Scope)j.next();

                                    s.addChildren((Vector)res.get(s));

                                    for (Iterator k = ((Vector)res.get(s)).iterator(); k.hasNext(); )
                                        {
                                            EventReceptor rec = (EventReceptor)k.next();

                                            if (rec instanceof TemporalInvariant)
                                                {
                                                    TemporalInvariant n = (TemporalInvariant)rec;

                                                    //												System.out.println(n.outputString());
                                                }
                                        }
                                }
                        }
                }

            //	System.out.println("\n\nOVERALL STATE:\n\n");

            //	printState();

        }
    }

    // This class implements scopes which are active before a given event
    public static class ScopeBefore extends Scope
    {
        Event mEvent;

        public ScopeBefore(Event e)
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

    // Note: Could have implemented this as a ScopeNot with a ScopeBefore.
    // Should I?
    public static class ScopeAfter extends Scope
    {
        Event mEvent;

        public ScopeAfter(Event e)
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

    /**
     * This scope has some tricky logic. It should only be active if both
     * its start event and its end event are seen by it. However, to properly
     * handle candidate instantiation (without ridiculous backtracking overhead),
     * it assumes that its closing event will be seen, instantiates candidates/etc,
     * then reverts state as needed if its parent exits before its closing event
     * is seen.
     **/

    // FIXME: MAJOR: Semantics are wrong. Scope should be limited to smallest
    // space between A and B (i.e. AAB only in scope between the second A and the
    // b, or rather that is the only scope which needs to be checked, by inclusion)
    // This same problem probably also applies to afteruntil (find out!)
    // Could probably also be correctly written as ScopeAfterBefore, but who cares
    public static class ScopeBetween extends Scope
    {
	// Bounding events
        Event mEventA;
        Event mEventB;

        private Hashtable savedStates; // States of all the kids; must be saved in case
	// state entry must be rolled back

	// Vector of all new kids created based on scope entry
        private Vector kidsCreated;

        public ScopeBetween(Event a, Event b)
        {
            super();

            mEventA = a;
            mEventB = b;
        }

        public void enter()
        {
            savedStates = new Hashtable();
            kidsCreated = new Vector();

            savedStates.putAll(generateSubtreeSnapshots());

            super.enter();
        }

	// Make sure to store newly created kids (so they can be destroyed if the
	// scope is later determined never to have been active).
        Hashtable generateNewCandidates(Event e)
        {
            Hashtable res = super.generateNewCandidates(e);

            for (Iterator i = res.keySet().iterator(); i.hasNext(); )
                {
                    EventReceptor r = (EventReceptor)i.next();

                    if (r.equals(this) || r.isChildOf(this))
                        {
                            kidsCreated.addAll((Vector)res.get(r));
                        }
                }

            return res;
        }

	// Do the rollback.
        void rollback()
        {
            for (Iterator i = kidsCreated.iterator(); i.hasNext(); )
                {
                    ((EventReceptor)i.next()).delete();
                }

            for (Iterator i = savedStates.keySet().iterator(); i.hasNext(); )
                {
                    EventReceptor r = (EventReceptor)i.next();

                    r.restoreState(savedStates.get(r));
                }

            // FIXME: May be unnecessary, given that "this" is stored in savedStates by generateSubtreeSnapshots.
            // Seems kinda sketchy.
            isActive = false;
        }


        void parentScopeExiting()
        {
            if (isActive())
                {
                    // Parent scope is closing. This means we never saw our close event,
                    // so we never actually were active.
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

    /**
     * This scope is active after its event A, and until either its parent scope
     * closes or until its event B is seen, whichever comes first.
     **/

    // This scope has an exception - its ending event can be the same thing as an event
    // contained within (eg in an eventually invariant) - to allow sensible response
    // invariants
    public static class ScopeAfterUntil extends Scope
    {
        Event mEventA;
        Event mEventB;

        public ScopeAfterUntil(Event a, Event b)
        {
            super();

            mEventA = a;
            mEventB = b;

            // This may not be right. This is done to suppress generation of subscopes/etc
            // which depend on event B (except for the explicitly generated response invariant).
            // In fact, this is ugly/hackish enough that it's almost certainly not right..
            // but that's why it's a FIXME.
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

}
