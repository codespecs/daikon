package daikon.temporal;

import junit.framework.*;
import java.util.*;

class LetterEvent extends Event
{
    String name;

    LetterEvent(String n)
    {
	name = n;
    }

    public boolean forwardSharesTypeWith(Event e)
    {
	return (e instanceof LetterEvent);
    }

    public boolean forwardMatches(Event e)
    {
	LetterEvent l = (LetterEvent)e;

	return l.name.equals(name);
    }

    public String toString()
    {
	return name;
    }
}

//FIXME: Needs some major restructuring for efficiency/clarity.
public class TemporalTests extends TestCase
{
    Event A = new LetterEvent("A");
    Event B = new LetterEvent("B");
    Event C = new LetterEvent("C");
    Event D = new LetterEvent("D");

    public static void main(String[] args)
    {
	junit.textui.TestRunner.run(new TestSuite(TemporalTests.class));
    }

    public TemporalTests(String name)
    {
	super(name);
    }

    ScopeGlobal global;

    ScopeBefore beforeA;
    ScopeAfter afterA;

    ScopeBefore beforeB;
    ScopeAfter afterB;

    ScopeBefore beforeC;
    ScopeAfter afterC;

    ScopeBefore beforeD;
    ScopeAfter afterD;

    public void generateBasicScopes()
    {
	global = new ScopeGlobal();

	beforeA = new ScopeBefore(A);
	afterA = new ScopeAfter(A);

	beforeB = new ScopeBefore(B);
	afterB = new ScopeAfter(B);

	beforeC = new ScopeBefore(C);
	afterC = new ScopeAfter(C);

	beforeD = new ScopeBefore(D);
	afterD = new ScopeAfter(D);
    }

    public void generateSimpleScopes()
    {
	generateBasicScopes();

	global.doDynamicInstantiation = false;

	global.addChild(beforeA);
	global.addChild(afterA);
    }

    public void generateSimpleNestedScopes()
    {
	generateBasicScopes();

	global.doDynamicInstantiation = false;

	global.addChild(beforeA);
	beforeA.addChild(beforeB);
	beforeA.addChild(afterC);

	global.addChild(afterA);
	afterA.addChild(beforeD);
	afterA.addChild(afterD);
    }
	

    public void testBasicEnterAndExit()
    {
	//Can we do very basic entering and exiting without drooling
	//(or at least without drooling on ourselves)?
	generateSimpleNestedScopes();

	assert(!global.isActive());
	assert(!beforeA.isActive());
	assert(!afterA.isActive());

	global.enter();

	assert(global.isActive());
	assert(beforeA.isActive());
	assert(!afterA.isActive());

	global.exit();

	assert(!global.isActive());
	assert(!beforeA.isActive());
	assert(!afterA.isActive());
    }

    public void testBasicScopeEventProcessing()
    {
	generateSimpleNestedScopes();

	global.enter();

	assert(!global.seenEvent(A));
	assert(!global.seenEvent(B));
	assert(!global.seenEvent(C));

	assert(!beforeA.seenEvent(A));
	assert(!beforeA.seenEvent(B));
	assert(!beforeA.seenEvent(C));

	assert(!afterA.seenEvent(A));
	assert(!afterA.seenEvent(B));
	assert(!afterA.seenEvent(C));

	global.processEvent(B);

	assert(global.isActive());
	assert(beforeA.isActive());
	assert(!afterA.isActive());

	assert(global.seenEvent(B));
	assert(beforeA.seenEvent(B));
	assert(!afterA.seenEvent(B));
	
	global.processEvent(A);

	assert(global.isActive());
	assert(!beforeA.isActive());
	assert(afterA.isActive());

	assert(global.seenEvent(A));
	assert(!beforeA.seenEvent(A));
	assert(!afterA.seenEvent(A));

	global.processEvent(C);

	assert(global.isActive());
	assert(!beforeA.isActive());
	assert(afterA.isActive());

	assert(global.seenEvent(C));
	assert(!beforeA.seenEvent(C));
	assert(afterA.seenEvent(C));

	global.exit();
    }

    public void testNestedScopeEventProcessing()
    {
	generateSimpleNestedScopes();

	global.enter();

	assert(beforeA.isActive());
	assert(beforeB.isActive());

	global.processEvent(D);

	assert(!beforeD.seenEvent(D));
	assert(!afterD.seenEvent(D));
	assert(!afterA.seenEvent(D));
	
	assert(beforeA.seenEvent(D));
	assert(beforeB.seenEvent(D));
	assert(!afterC.seenEvent(D));

	global.processEvent(C);

	assert(afterC.isActive());

	global.processEvent(D);

	assert(afterC.seenEvent(D));

	global.processEvent(B);

	assert(afterC.isActive());
	assert(!beforeB.isActive());

	assert(!afterA.seenEvent(C));

	global.processEvent(A);

	assert(beforeD.isActive());
	assert(!afterD.isActive());

	global.processEvent(C);

	assert(afterA.seenEvent(C));
	assert(beforeD.seenEvent(C));
	assert(!afterD.seenEvent(C));

	global.processEvent(D);

	assert(!beforeD.isActive());
	assert(afterD.isActive());
	
	global.exit();
    }

    public void testSimpleInvariants()
    {
	generateSimpleScopes();

	TemporalInvariant g_alwaysB, g_eventuallyB, bA_alwaysB, bA_eventuallyB, aA_alwaysB, aA_eventuallyB;

	g_alwaysB = new AlwaysInvariant(global, B);
	g_eventuallyB = new EventuallyInvariant(global, B);
	bA_alwaysB = new AlwaysInvariant(beforeA, B);
	bA_eventuallyB = new EventuallyInvariant(beforeA, B);
	aA_alwaysB = new AlwaysInvariant(afterA, B);
	aA_eventuallyB = new EventuallyInvariant(afterA, B);

	assert(!g_alwaysB.isFalsified);
	assert(g_alwaysB.numConfirmingSequences == 0);

	//Sequence 1: CABB

	global.enter();

	assert(!g_alwaysB.isFalsified);
	assert(g_alwaysB.numConfirmingSequences == 0);

	global.processEvent(C);

	assert(g_alwaysB.isFalsified);
	assert(bA_alwaysB.isFalsified);
	assert(!g_eventuallyB.isFalsified);

	global.processEvent(A);

	assert(bA_eventuallyB.isFalsified);
	
	global.processEvent(B);

	assert(!aA_alwaysB.isFalsified);
	assert(!aA_eventuallyB.isFalsified);
	assert(!g_eventuallyB.isFalsified);
	assert(g_alwaysB.isFalsified);

	assert(aA_alwaysB.numConfirmingSequences == 0);
	assert(g_alwaysB.numConfirmingSequences == 0);

	global.processEvent(B);

	assert(!aA_alwaysB.isFalsified);
	assert(!aA_eventuallyB.isFalsified);
	assert(g_alwaysB.isFalsified);

	global.exit();

	assert(g_eventuallyB.numConfirmingSequences == 1);
	assert(aA_alwaysB.numConfirmingSequences == 1);
	assert(aA_eventuallyB.numConfirmingSequences == 1);

	global.enter();
	global.exit();

	assert(g_eventuallyB.isFalsified);

	//Could probably do more here
    }

    public void testCoreBetweenScope()
    {
	ScopeBetween betAB;

	global = new ScopeGlobal();
	global.doDynamicInstantiation = false;
	betAB = new ScopeBetween(A, B);

	//Test 1 - very simple

	global.addChild(betAB);

	global.enter();

	assert(!betAB.isActive());

	global.processEvent(C);

	assert(!betAB.isActive());

	global.processEvent(A);

	assert(betAB.isActive()); //thinks its active now

	global.processEvent(C);

	global.processEvent(B);

	assert(!betAB.isActive());

	assert(betAB.seenEvent(C)); //did see the C

	global.processEvent(A);

	assert(betAB.isActive());

	global.processEvent(D);

	global.exit();

	assert(!betAB.isActive());
	assert(!betAB.seenEvent(D)); //scope didn't ever enter

	//Test 2 - now with an invariant

	global = new ScopeGlobal();
	global.doDynamicInstantiation = false;
	betAB = new ScopeBetween(A, B);
	
	AlwaysInvariant alwaysC = new AlwaysInvariant(betAB, C);

	//to be added later
	EventuallyInvariant eventuallyD = new EventuallyInvariant(D);

	global.addChild(betAB);

	global.enter();

	global.processEvent(A);

	global.processEvent(C);

	global.processEvent(B);

	assert(!alwaysC.isFalsified);
	assert(alwaysC.numConfirmingSequences == 1);

	betAB.addChild(eventuallyD);

	global.processEvent(A);

	global.processEvent(D);

	assert(alwaysC.isFalsified); //thinks its falsified

	assert(!eventuallyD.isFalsified);
	assert(eventuallyD.happenedOnceInScope);

	global.exit();

	assert(!alwaysC.isFalsified); //should still be kosher
	assert(!betAB.seenEvent(D));
	assert(!eventuallyD.isFalsified);

	assert(eventuallyD.numConfirmingSequences == 0);
    }

    public void testCoreAfterUntilScope()
    {
	ScopeAfterUntil auAB;

	global = new ScopeGlobal();
	global.doDynamicInstantiation = false;
	auAB = new ScopeAfterUntil(A, B);

	global.addChild(auAB);

	global.enter();

	assert(!auAB.isActive());

	global.processEvent(B);

	assert(!auAB.isActive());

	global.processEvent(A);

	assert(auAB.isActive());

	global.processEvent(C);

	assert(auAB.isActive());

	global.processEvent(B);

	assert(!auAB.isActive());

	global.processEvent(A);

	assert(auAB.isActive());

	global.exit();

	assert(!auAB.isActive());

	EventuallyInvariant eventuallyC = new EventuallyInvariant(auAB, C);
	EventuallyInvariant eventuallyB = new EventuallyInvariant(auAB, B);

	//Sequence ACB satisfies both

	global.enter();
	global.processEvent(A);

	assert(auAB.isActive());

	global.processEvent(C);

	assert(eventuallyC.happenedOnceInScope);
	assert(!eventuallyC.isFalsified);

	global.processEvent(B);

	assert(!auAB.isActive());
	assert(!eventuallyC.isFalsified);
	assert(eventuallyC.numConfirmingSequences == 1);

	global.exit();

	assert(!eventuallyC.isFalsified);

	assert(eventuallyC.numConfirmingSequences == 1);
	assert(!eventuallyB.isFalsified);
	assert(eventuallyB.numConfirmingSequences == 1);

	//Sequence ABCACB falsifies first, satisfies second, with count 3

	//FIXME: A little uncertain about these happenedOnceInScope tests... hmm..
	assert(!eventuallyC.happenedOnceInScope);

	global.enter();
	
	assert(!auAB.isActive());

	assert(!eventuallyC.happenedOnceInScope);

	global.processEvent(A);

	assert(auAB.isActive());

	assert(!eventuallyC.happenedOnceInScope);

	global.processEvent(B);

	assert(!auAB.isActive());

	assert(eventuallyC.isFalsified);
	
	global.processEvent(C);
	global.processEvent(A);
	global.processEvent(C);
	global.processEvent(B);
	global.exit();

	assert(eventuallyC.isFalsified);
	assert(!eventuallyB.isFalsified);
	assert(eventuallyB.numConfirmingSequences == 3);
	
	//Sequence ABA falsifies second
	
	global.enter();
	global.processEvent(A);
	global.processEvent(B);
	global.processEvent(A);
	global.exit();

	assert(eventuallyB.isFalsified);
    }

    //FIXME: Build a querying system for the invariant tree, and make
    //some approximate correctness tests, etc.
    //FIXME: These should be fully automated! no really!
    public void testBasicDynamicInstantiation()
    {
	global = new ScopeGlobal();

	global.enter();


	//Event A creates after A *, eventually A, always A
	global.processEvent(A);
	//	global.printState();

	/*Event B creates:
	    before b, after a,
	    between a and b,
	    after a until b,

	    after a, *
	    after b, *

	    b responds to a
	    before b, eventually a
	    eventually B
	    (falsifies always A)
	*/
	global.processEvent(B);
	//	global.printState();

	/*Event B changes nothing*/
	global.processEvent(B);
	//	global.printState();

	/*Event C:
	    after a, eventually c
	    after a, NO ALWAYS C

	    eventually C
	    c responds to a

	    before c, after a,
	    between a and c,
	    after a until c,

	    and more.. argh
	*/

	global.processEvent(C);
	//	global.printState();

	global.exit();
    }

}
	
	




    
	
