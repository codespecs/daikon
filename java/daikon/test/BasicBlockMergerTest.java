package daikon.test;

import static org.junit.Assert.*;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

//import com.sun.xml.internal.bind.v2.TODO;

import junit.framework.TestCase;
import daikon.BasicBlockMerger;
import daikon.PptTopLevel;
import daikon.SearchNodeUtility;
import daikon.VarInfo;

public class BasicBlockMergerTest extends TestCase{

	/**
	 *        1
	 *      /   \
	 *     v     v
	 *     2     3
	 *      \   /
	 *       v v
	 *        4
	 *        
	 * @throws Exception
	 */
	public void testOneFork() throws Exception {
        PptTopLevel ppt1 = new PptTopLevel(":::1", new VarInfo[] {});
        PptTopLevel ppt2 = new PptTopLevel(":::2", new VarInfo[] {});
        PptTopLevel ppt3 = new PptTopLevel(":::3", new VarInfo[] {});
        PptTopLevel ppt4 = new PptTopLevel(":::4", new VarInfo[] {});

        List<PptTopLevel> successorOfPpt1 = new ArrayList<PptTopLevel>();
        successorOfPpt1.add(ppt1); // this indicates the head node
        successorOfPpt1.add(ppt2); // these are the successor nodes
        successorOfPpt1.add(ppt3);

        List<PptTopLevel> successorOfPpt2 = new ArrayList<PptTopLevel>();
        successorOfPpt2.add(ppt2);
        successorOfPpt2.add(ppt4);

        List<PptTopLevel> successorOfPpt3 = new ArrayList<PptTopLevel>();
        successorOfPpt3.add(ppt3);
        successorOfPpt3.add(ppt4);

        List<PptTopLevel> successorOfPpt4 = new ArrayList<PptTopLevel>();
        successorOfPpt4.add(ppt4);

        List<List<PptTopLevel>> successors = new ArrayList<List<PptTopLevel>>();
        successors.add(successorOfPpt1);
        successors.add(successorOfPpt2);
        successors.add(successorOfPpt3);
        successors.add(successorOfPpt4);

        System.out.println("input= " + successors);

        List<PptTopLevel> mergedForPpt1 = Arrays.asList(new PptTopLevel[] {
                ppt1, ppt1 });
        List<PptTopLevel> mergedForPpt2 = Arrays.asList(new PptTopLevel[] {
                ppt2, ppt1, ppt2 });
        List<PptTopLevel> mergedForPpt3 = Arrays.asList(new PptTopLevel[] {
                ppt3, ppt1, ppt3 });
        List<PptTopLevel> mergedForPpt4 = Arrays.asList(new PptTopLevel[] {
                ppt4, ppt1, ppt4 });

        List<List<PptTopLevel>> expectedMergedBlocks = new ArrayList<List<PptTopLevel>>();
        expectedMergedBlocks.add(mergedForPpt1);
        expectedMergedBlocks.add(mergedForPpt2);
        expectedMergedBlocks.add(mergedForPpt3);
        expectedMergedBlocks.add(mergedForPpt4);

        System.out.println("output= " + expectedMergedBlocks);
        List<List<PptTopLevel>> actualMergedBlocks = new BasicBlockMerger<PptTopLevel>(
                successors).mergeBasicBlocks();

        System.out.println();

        assertEquals(expectedMergedBlocks, actualMergedBlocks);

    }
    
    
    /**
     *        1
     *      /   \
     *     v     v
     *     2     3
     *      \   /
     *       v v
     *        4
     *      /   \ 
     *     v     v
     *     5     6
     * @throws Exception
     */
    
    public void testNodeIsBothForkAndJoin() throws Exception {
        PptTopLevel ppt1 = new PptTopLevel(":::1", new VarInfo[] {});
        PptTopLevel ppt2 = new PptTopLevel(":::2", new VarInfo[] {});
        PptTopLevel ppt3 = new PptTopLevel(":::3", new VarInfo[] {});
        PptTopLevel ppt4 = new PptTopLevel(":::4", new VarInfo[] {});
        PptTopLevel ppt5 = new PptTopLevel(":::5", new VarInfo[] {});
        PptTopLevel ppt6 = new PptTopLevel(":::6", new VarInfo[] {});
        
        List<PptTopLevel> successorOfPpt1 = new ArrayList<PptTopLevel>();
        successorOfPpt1.add(ppt1); // this indicates the head node
        successorOfPpt1.add(ppt2); // these are the successor nodes
        successorOfPpt1.add(ppt3);

        List<PptTopLevel> successorOfPpt2 = new ArrayList<PptTopLevel>();
        successorOfPpt2.add(ppt2);
        successorOfPpt2.add(ppt4);

        List<PptTopLevel> successorOfPpt3 = new ArrayList<PptTopLevel>();
        successorOfPpt3.add(ppt3);
        successorOfPpt3.add(ppt4);

        List<PptTopLevel> successorOfPpt4 = new ArrayList<PptTopLevel>();
        successorOfPpt4.add(ppt4);
        successorOfPpt4.add(ppt5);
        successorOfPpt4.add(ppt6);
        

        List<PptTopLevel> successorOfPpt5 = new ArrayList<PptTopLevel>();
        successorOfPpt5.add(ppt5);
        
        List<PptTopLevel> successorOfPpt6 = new ArrayList<PptTopLevel>();
        successorOfPpt6.add(ppt6);
        
        List<List<PptTopLevel>> successors = new ArrayList<List<PptTopLevel>>();
        successors.add(successorOfPpt1);
        successors.add(successorOfPpt2);
        successors.add(successorOfPpt3);
        successors.add(successorOfPpt4);
        successors.add(successorOfPpt5);
        successors.add(successorOfPpt6);
        
        System.out.println("input= " + successors);

        List<PptTopLevel> mergedForPpt1 = Arrays.asList(new PptTopLevel[] {
                ppt1, ppt1 });
        List<PptTopLevel> mergedForPpt2 = Arrays.asList(new PptTopLevel[] {
                ppt2, ppt1, ppt2 });
        List<PptTopLevel> mergedForPpt3 = Arrays.asList(new PptTopLevel[] {
                ppt3, ppt1, ppt3 });
        List<PptTopLevel> mergedForPpt4 = Arrays.asList(new PptTopLevel[] {
                ppt4, ppt1, ppt4 });

        List<PptTopLevel> mergedForPpt5 = Arrays.asList(new PptTopLevel[] {
                ppt5, ppt1, ppt4, ppt5 });
        
        List<PptTopLevel> mergedForPpt6 = Arrays.asList(new PptTopLevel[] {
                ppt6, ppt1, ppt4, ppt6 });
        
        List<List<PptTopLevel>> expectedMergedBlocks = new ArrayList<List<PptTopLevel>>();
        expectedMergedBlocks.add(mergedForPpt1);
        expectedMergedBlocks.add(mergedForPpt2);
        expectedMergedBlocks.add(mergedForPpt3);
        expectedMergedBlocks.add(mergedForPpt4);
        expectedMergedBlocks.add(mergedForPpt5);
        expectedMergedBlocks.add(mergedForPpt6);
        
        System.out.println("output= " + expectedMergedBlocks);
        List<List<PptTopLevel>> actualMergedBlocks = new BasicBlockMerger<PptTopLevel>(
                successors).mergeBasicBlocks();

        System.out.println();

        assertEquals(expectedMergedBlocks, actualMergedBlocks);
        
    }
    
    
    /**
     *        1
     *      /   \
     *     v     v
     *     2     3
     *    /    /   \
     *    v   v     \
     *    4   5     |
     *    \   |     |
     *     v  v     |
     *        7 \   |
     *        |  v  v
     *        v    6
     *        8
     * @throws Exception
     */
    public void testBB_2e69_FromSung() throws Exception {
        PptTopLevel ppt1 = new PptTopLevel(":::1", new VarInfo[] {});
        PptTopLevel ppt2 = new PptTopLevel(":::2", new VarInfo[] {});
        PptTopLevel ppt3 = new PptTopLevel(":::3", new VarInfo[] {});
        PptTopLevel ppt4 = new PptTopLevel(":::4", new VarInfo[] {}); 
        PptTopLevel ppt5 = new PptTopLevel(":::5", new VarInfo[] {});
        PptTopLevel ppt6 = new PptTopLevel(":::6", new VarInfo[] {}); 
        PptTopLevel ppt7 = new PptTopLevel(":::7", new VarInfo[] {});
        PptTopLevel ppt8 = new PptTopLevel(":::8", new VarInfo[] {});
        
        List<PptTopLevel> successorOfPpt1 = new ArrayList<PptTopLevel>();
        successorOfPpt1.add(ppt1); // this indicates the head node
        successorOfPpt1.add(ppt2); // these are the successor nodes
        successorOfPpt1.add(ppt3);

        List<PptTopLevel> successorOfPpt2 = new ArrayList<PptTopLevel>();
        successorOfPpt2.add(ppt2);
        successorOfPpt2.add(ppt4);

        List<PptTopLevel> successorOfPpt3 = new ArrayList<PptTopLevel>();
        successorOfPpt3.add(ppt3);
        successorOfPpt3.add(ppt5);
        successorOfPpt3.add(ppt6);
        

        List<PptTopLevel> successorOfPpt4 = new ArrayList<PptTopLevel>();
        successorOfPpt4.add(ppt4);
        successorOfPpt4.add(ppt7);

        List<PptTopLevel> successorOfPpt5 = new ArrayList<PptTopLevel>();
        successorOfPpt5.add(ppt5);
        successorOfPpt5.add(ppt7);
        
        List<PptTopLevel> successorOfPpt6 = new ArrayList<PptTopLevel>();
        successorOfPpt6.add(ppt6);
        
        List<PptTopLevel> successorOfPpt7 = new ArrayList<PptTopLevel>();
        successorOfPpt7.add(ppt7);
        successorOfPpt7.add(ppt6);
        successorOfPpt7.add(ppt8);
        
        List<PptTopLevel> successorOfPpt8 = new ArrayList<PptTopLevel>();
        successorOfPpt8.add(ppt8);
        
        List<List<PptTopLevel>> successors = new ArrayList<List<PptTopLevel>>();
        successors.add(successorOfPpt1);
        successors.add(successorOfPpt2);
        successors.add(successorOfPpt3);
        successors.add(successorOfPpt4);
        successors.add(successorOfPpt5);
        successors.add(successorOfPpt6);
        successors.add(successorOfPpt7);
        successors.add(successorOfPpt8);
        
        System.out.println("input= " + successors);

        List<PptTopLevel> mergedForPpt1 = Arrays.asList(new PptTopLevel[] {
                ppt1, ppt1 });
        List<PptTopLevel> mergedForPpt2 = Arrays.asList(new PptTopLevel[] {
                ppt2, ppt1, ppt2 });
        List<PptTopLevel> mergedForPpt3 = Arrays.asList(new PptTopLevel[] {
                ppt3, ppt1, ppt3 });
        List<PptTopLevel> mergedForPpt4 = Arrays.asList(new PptTopLevel[] {
                ppt4, ppt1, ppt2, ppt4 });
        List<PptTopLevel> mergedForPpt5 = Arrays.asList(new PptTopLevel[] {
                ppt5, ppt1, ppt3, ppt5 });

        List<PptTopLevel> mergedForPpt6 = Arrays.asList(new PptTopLevel[] {
                ppt6, ppt1, ppt6 });
        
        List<PptTopLevel> mergedForPpt7 = Arrays.asList(new PptTopLevel[] {
                ppt7, ppt1, ppt7 });
        
        List<PptTopLevel> mergedForPpt8 = Arrays.asList(new PptTopLevel[] {
                ppt8, ppt1, ppt7, ppt8 });
        
        
        List<List<PptTopLevel>> expectedMergedBlocks = new ArrayList<List<PptTopLevel>>();
        expectedMergedBlocks.add(mergedForPpt1);
        expectedMergedBlocks.add(mergedForPpt2);
        expectedMergedBlocks.add(mergedForPpt3);
        expectedMergedBlocks.add(mergedForPpt4);
        expectedMergedBlocks.add(mergedForPpt5);
        expectedMergedBlocks.add(mergedForPpt6);
        expectedMergedBlocks.add(mergedForPpt7);
        expectedMergedBlocks.add(mergedForPpt8);
        
        System.out.println("output= " + expectedMergedBlocks);
        List<List<PptTopLevel>> actualMergedBlocks = new BasicBlockMerger<PptTopLevel>(
                successors).mergeBasicBlocks();

        System.out.println();

        assertEquals(expectedMergedBlocks, actualMergedBlocks);

    }
	
	/**
	 *        1
	 *      /   \
	 *     v     \
	 *     2      | 
	 *    /  \    | 
	 *    v   v   |
	 *    3   4   |
	 *    \   /   |
	 *     v v    |
	 *      5     |
	 *      |    /
	 *      v   v
	 *        6 
	 *      
	 * @throws Exception
	 */
	public void testTwoNestedForks() throws Exception {
        PptTopLevel ppt1 = new PptTopLevel(":::1", new VarInfo[] {});
        PptTopLevel ppt2 = new PptTopLevel(":::2", new VarInfo[] {});
        PptTopLevel ppt3 = new PptTopLevel(":::3", new VarInfo[] {});
        PptTopLevel ppt4 = new PptTopLevel(":::4", new VarInfo[] {});
        PptTopLevel ppt5 = new PptTopLevel(":::5", new VarInfo[] {});
        PptTopLevel ppt6 = new PptTopLevel(":::6", new VarInfo[] {});

        List<PptTopLevel> successorOfPpt1 = Arrays.asList(new PptTopLevel[] {
                ppt1, ppt2, ppt6 });

        List<PptTopLevel> successorOfPpt2 = Arrays.asList(new PptTopLevel[] {
                ppt2, ppt3, ppt4 });

        List<PptTopLevel> successorOfPpt3 = Arrays.asList(new PptTopLevel[] {
                ppt3, ppt5 });

        List<PptTopLevel> successorOfPpt4 = Arrays.asList(new PptTopLevel[] {
                ppt4, ppt5 });

        List<PptTopLevel> successorOfPpt5 = Arrays.asList(new PptTopLevel[] {
                ppt5, ppt6 });

        List<PptTopLevel> successorOfPpt6 = Arrays
                .asList(new PptTopLevel[] { ppt6 });

        List<List<PptTopLevel>> successors = new ArrayList<List<PptTopLevel>>();

        successors.add(successorOfPpt1);
        successors.add(successorOfPpt2);
        successors.add(successorOfPpt3);
        successors.add(successorOfPpt4);
        successors.add(successorOfPpt5);
        successors.add(successorOfPpt6);

        System.out.println("input= " + successors);

        List<PptTopLevel> mergedForPpt1 = Arrays.asList(new PptTopLevel[] {
                ppt1, ppt1 });
        List<PptTopLevel> mergedForPpt2 = Arrays.asList(new PptTopLevel[] {
                ppt2, ppt1, ppt2 });
        List<PptTopLevel> mergedForPpt3 = Arrays.asList(new PptTopLevel[] {
                ppt3, ppt1, ppt2, ppt3 });
        List<PptTopLevel> mergedForPpt4 = Arrays.asList(new PptTopLevel[] {
                ppt4, ppt1, ppt2, ppt4 });
        List<PptTopLevel> mergedForPpt5 = Arrays.asList(new PptTopLevel[] {
                ppt5, ppt1, ppt2, ppt5 });
        List<PptTopLevel> mergedForPpt6 = Arrays.asList(new PptTopLevel[] {
                ppt6, ppt1, ppt6 });

        List<List<PptTopLevel>> expectedMergedBlocks = new ArrayList<List<PptTopLevel>>();
        expectedMergedBlocks.add(mergedForPpt1);
        expectedMergedBlocks.add(mergedForPpt2);
        expectedMergedBlocks.add(mergedForPpt3);
        expectedMergedBlocks.add(mergedForPpt4);
        expectedMergedBlocks.add(mergedForPpt5);
        expectedMergedBlocks.add(mergedForPpt6);

        List<List<PptTopLevel>> actualMergedBlocks = new BasicBlockMerger<PptTopLevel>(
                successors).mergeBasicBlocks();
        System.out.println("output= " + actualMergedBlocks);
        System.out.println();

        assertEquals(expectedMergedBlocks, actualMergedBlocks);
    }
	
	/**
	 *        0
	 *        |
	 *        v 
	 *        1
	 *       /  ^
	 *      v   |
	 *        2   
	 *        |
	 *        v
	 *        3
	 *       
	 * @throws Exception
	 */
	
	public void testOneCycle() throws Exception {
        PptTopLevel ppt0 = new PptTopLevel(":::0", new VarInfo[] {});
        PptTopLevel ppt1 = new PptTopLevel(":::1", new VarInfo[] {});
        PptTopLevel ppt2 = new PptTopLevel(":::2", new VarInfo[] {});
        PptTopLevel ppt3 = new PptTopLevel(":::3", new VarInfo[] {});

        List<PptTopLevel> successorOfPpt0 = Arrays.asList(new PptTopLevel[] {
                ppt0, ppt1 });

        List<PptTopLevel> successorOfPpt1 = Arrays.asList(new PptTopLevel[] {
                ppt1, ppt2 });

        List<PptTopLevel> successorOfPpt2 = Arrays.asList(new PptTopLevel[] {
                ppt2, ppt1, ppt3 });

        List<PptTopLevel> successorOfPpt3 = Arrays
                .asList(new PptTopLevel[] { ppt3 });

        List<List<PptTopLevel>> successors = new ArrayList<List<PptTopLevel>>();

        successors.add(successorOfPpt0);
        successors.add(successorOfPpt1);
        successors.add(successorOfPpt2);
        successors.add(successorOfPpt3);

        System.out.println("input= " + successors);

        List<PptTopLevel> mergedForPpt0 = Arrays.asList(new PptTopLevel[] {
                ppt0, ppt0 });
        List<PptTopLevel> mergedForPpt1 = Arrays.asList(new PptTopLevel[] {
                ppt1, ppt0, ppt1 });
        List<PptTopLevel> mergedForPpt2 = Arrays.asList(new PptTopLevel[] {
                ppt2, ppt0, ppt1, ppt2 });
        List<PptTopLevel> mergedForPpt3 = Arrays.asList(new PptTopLevel[] {
                ppt3, ppt0, ppt1, ppt2, ppt3 });

        List<List<PptTopLevel>> expectedMergedBlocks = new ArrayList<List<PptTopLevel>>();
        expectedMergedBlocks.add(mergedForPpt0);
        expectedMergedBlocks.add(mergedForPpt1);
        expectedMergedBlocks.add(mergedForPpt2);
        expectedMergedBlocks.add(mergedForPpt3);

        List<List<PptTopLevel>> actualMergedBlocks = new BasicBlockMerger<PptTopLevel>(
                successors).mergeBasicBlocks();
        System.out.println("output= " + actualMergedBlocks);
        System.out.println();

        assertEquals(expectedMergedBlocks, actualMergedBlocks);

    }
    
    /**
     *        0
     *     ^  |  ^
     *     |  v   \
     *     \  1   |
     *        |   |
     *        v  / 
     *        2   
     *        
     *        
     *        
     *       
     * @throws Exception
     */
    
    public void testFirstEntryNodeIsAlsoJoin() throws Exception {
        PptTopLevel ppt0 = new PptTopLevel(":::0", new VarInfo[] {});
        PptTopLevel ppt1 = new PptTopLevel(":::1", new VarInfo[] {});
        PptTopLevel ppt2 = new PptTopLevel(":::2", new VarInfo[] {});

        List<PptTopLevel> successorOfPpt0 = Arrays.asList(new PptTopLevel[] {
                ppt0, ppt1 });

        List<PptTopLevel> successorOfPpt1 = Arrays.asList(new PptTopLevel[] {
                ppt1, ppt0, ppt2 });

        List<PptTopLevel> successorOfPpt2 = Arrays.asList(new PptTopLevel[] {
                ppt2, ppt0 });


        List<List<PptTopLevel>> successors = new ArrayList<List<PptTopLevel>>();

        successors.add(successorOfPpt0);
        successors.add(successorOfPpt1);
        successors.add(successorOfPpt2);

        System.out.println("input= " + successors);

        List<PptTopLevel> mergedForPpt0 = Arrays.asList(new PptTopLevel[] {
                ppt0, ppt0 });
        List<PptTopLevel> mergedForPpt1 = Arrays.asList(new PptTopLevel[] {
                ppt1, ppt0, ppt1 });
        List<PptTopLevel> mergedForPpt2 = Arrays.asList(new PptTopLevel[] {
                ppt2, ppt0, ppt1, ppt2 });

        List<List<PptTopLevel>> expectedMergedBlocks = new ArrayList<List<PptTopLevel>>();
        expectedMergedBlocks.add(mergedForPpt0);
        expectedMergedBlocks.add(mergedForPpt1);
        expectedMergedBlocks.add(mergedForPpt2);

        List<List<PptTopLevel>> actualMergedBlocks = new BasicBlockMerger<PptTopLevel>(
                successors).mergeBasicBlocks();
        System.out.println("output= " + actualMergedBlocks);
        System.out.println();

        assertEquals(expectedMergedBlocks, actualMergedBlocks);

    }
	
	public void testFindDFS() throws Exception {
        PptTopLevel ppt1 = new PptTopLevel(":::1", new VarInfo[] {});
        PptTopLevel ppt2 = new PptTopLevel(":::2", new VarInfo[] {});
        PptTopLevel ppt3 = new PptTopLevel(":::3", new VarInfo[] {});
        PptTopLevel ppt4 = new PptTopLevel(":::4", new VarInfo[] {});

        List<PptTopLevel> successorOfPpt1 = new ArrayList<PptTopLevel>();
        successorOfPpt1.add(ppt1); // this indicates the head node
        successorOfPpt1.add(ppt2); // these are the successor nodes
        successorOfPpt1.add(ppt3);

        List<PptTopLevel> successorOfPpt2 = new ArrayList<PptTopLevel>();
        successorOfPpt2.add(ppt2);
        successorOfPpt2.add(ppt4);

        List<PptTopLevel> successorOfPpt3 = new ArrayList<PptTopLevel>();
        successorOfPpt3.add(ppt3);
        successorOfPpt3.add(ppt4);

        List<PptTopLevel> successorOfPpt4 = new ArrayList<PptTopLevel>();
        successorOfPpt4.add(ppt4);

        List<List<PptTopLevel>> successors = new ArrayList<List<PptTopLevel>>();
        successors.add(successorOfPpt1);
        successors.add(successorOfPpt2);
        successors.add(successorOfPpt3);
        successors.add(successorOfPpt4);

        SearchNodeUtility<PptTopLevel> searcher = new SearchNodeUtility<PptTopLevel>(
                successors);
        assertTrue(searcher.findDepthFirst(ppt1, ppt4,
                new ArrayList<PptTopLevel>()));

        assertFalse(searcher.findDepthFirst(ppt2, ppt3,
                new ArrayList<PptTopLevel>()));

        assertFalse(searcher.findDepthFirst(ppt4, ppt1,
                new ArrayList<PptTopLevel>()));

        assertTrue(searcher.findDepthFirst(ppt3, ppt4,
                new ArrayList<PptTopLevel>()));

    }
    
    
    /**
     *
     *        1
     *       / \
     *      /   v
     *      |   2
     *      |   |
     *      |   v
     *      |   3
     *      |  / \
     *      v v   v
     *       4    5
     *            |
     *            v
     *            6
     *          
     * @throws Exception
     */
    public void testFindDFS_with2Forks() throws Exception {
        PptTopLevel ppt1 = new PptTopLevel(":::1", new VarInfo[] {});
        PptTopLevel ppt2 = new PptTopLevel(":::2", new VarInfo[] {});
        PptTopLevel ppt3 = new PptTopLevel(":::3", new VarInfo[] {});
        PptTopLevel ppt4 = new PptTopLevel(":::4", new VarInfo[] {});
        PptTopLevel ppt5 = new PptTopLevel(":::5", new VarInfo[] {});
        PptTopLevel ppt6 = new PptTopLevel(":::6", new VarInfo[] {});
        
        List<PptTopLevel> successorOfPpt1 = new ArrayList<PptTopLevel>();
        successorOfPpt1.add(ppt1); // this indicates the head node
        successorOfPpt1.add(ppt2); // these are the successor nodes
        successorOfPpt1.add(ppt4);

        List<PptTopLevel> successorOfPpt2 = new ArrayList<PptTopLevel>();
        successorOfPpt2.add(ppt2);
        successorOfPpt2.add(ppt3);

        List<PptTopLevel> successorOfPpt3 = new ArrayList<PptTopLevel>();
        successorOfPpt3.add(ppt3);
        successorOfPpt3.add(ppt5);
        successorOfPpt3.add(ppt4);
        
        List<PptTopLevel> successorOfPpt4 = new ArrayList<PptTopLevel>();
        successorOfPpt4.add(ppt4);

        List<PptTopLevel> successorOfPpt5 = new ArrayList<PptTopLevel>();
        successorOfPpt5.add(ppt5);
        successorOfPpt5.add(ppt6);
        
        List<PptTopLevel> successorOfPpt6 = new ArrayList<PptTopLevel>();
        successorOfPpt6.add(ppt6);


        List<List<PptTopLevel>> successors = new ArrayList<List<PptTopLevel>>();
        successors.add(successorOfPpt1);
        successors.add(successorOfPpt2);
        successors.add(successorOfPpt3);
        successors.add(successorOfPpt4);
        successors.add(successorOfPpt5);
        successors.add(successorOfPpt6);


        
        SearchNodeUtility<PptTopLevel> searcher = new SearchNodeUtility<PptTopLevel>(
                successors);
        List<PptTopLevel> visitedNodes = new ArrayList<PptTopLevel>();
        visitedNodes.add(ppt4);
        
        assertTrue(searcher.findDepthFirst(ppt1, ppt4,
                visitedNodes ));

        

    }
    

    public void testDepthFirstSearch() throws Exception {
        PptTopLevel ppt1 = new PptTopLevel(":::1", new VarInfo[] {});
        PptTopLevel ppt2 = new PptTopLevel(":::2", new VarInfo[] {});
        PptTopLevel ppt3 = new PptTopLevel(":::3", new VarInfo[] {});
        PptTopLevel ppt4 = new PptTopLevel(":::4", new VarInfo[] {});

        List<PptTopLevel> successorOfPpt1 = new ArrayList<PptTopLevel>();
        successorOfPpt1.add(ppt1); // this indicates the head node
        successorOfPpt1.add(ppt2); // these are the successor nodes
        successorOfPpt1.add(ppt3);

        List<PptTopLevel> successorOfPpt2 = new ArrayList<PptTopLevel>();
        successorOfPpt2.add(ppt2);
        successorOfPpt2.add(ppt4);

        List<PptTopLevel> successorOfPpt3 = new ArrayList<PptTopLevel>();
        successorOfPpt3.add(ppt3);
        successorOfPpt3.add(ppt4);

        List<PptTopLevel> successorOfPpt4 = new ArrayList<PptTopLevel>();
        successorOfPpt4.add(ppt4);

        List<List<PptTopLevel>> successors = new ArrayList<List<PptTopLevel>>();
        successors.add(successorOfPpt1);
        successors.add(successorOfPpt2);
        successors.add(successorOfPpt3);
        successors.add(successorOfPpt4);

        BasicBlockMerger<PptTopLevel> blockMerger = new BasicBlockMerger<PptTopLevel>(
                successors);
        blockMerger.depthFirstSearch();
        List<PptTopLevel> preorderTraversal = blockMerger
                .getPreorderTraversal();

        assertEquals(4, preorderTraversal.size());

        // DFS traversal 1,2,4,3
        assertEquals(ppt1, preorderTraversal.get(0));
        assertEquals(ppt2, preorderTraversal.get(1));
        assertEquals(ppt4, preorderTraversal.get(2));
        assertEquals(ppt3, preorderTraversal.get(3));

        // System.out.println("preoder traversal= " + preorderTraversal);

    }

    public void testComputeTransposedGraph() throws Exception {
        PptTopLevel ppt1 = new PptTopLevel(":::1", new VarInfo[] {});
        PptTopLevel ppt2 = new PptTopLevel(":::2", new VarInfo[] {});
        PptTopLevel ppt3 = new PptTopLevel(":::3", new VarInfo[] {});
        PptTopLevel ppt4 = new PptTopLevel(":::4", new VarInfo[] {});

        List<PptTopLevel> successorOfPpt1 = new ArrayList<PptTopLevel>();
        successorOfPpt1.add(ppt1); // this indicates the head node
        successorOfPpt1.add(ppt2); // these are the successor nodes
        successorOfPpt1.add(ppt3);

        List<PptTopLevel> successorOfPpt2 = new ArrayList<PptTopLevel>();
        successorOfPpt2.add(ppt2);
        successorOfPpt2.add(ppt4);

        List<PptTopLevel> successorOfPpt3 = new ArrayList<PptTopLevel>();
        successorOfPpt3.add(ppt3);
        successorOfPpt3.add(ppt4);

        List<PptTopLevel> successorOfPpt4 = new ArrayList<PptTopLevel>();
        successorOfPpt4.add(ppt4);

        List<List<PptTopLevel>> successors = new ArrayList<List<PptTopLevel>>();
        successors.add(successorOfPpt1);
        successors.add(successorOfPpt2);
        successors.add(successorOfPpt3);
        successors.add(successorOfPpt4);

        BasicBlockMerger<PptTopLevel> blockMerger = new BasicBlockMerger<PptTopLevel>(
                successors);

        List<PptTopLevel> transposedForPpt1 = Arrays
                .asList(new PptTopLevel[] { ppt1 });
        List<PptTopLevel> transposedForPpt2 = Arrays.asList(new PptTopLevel[] {
                ppt2, ppt1 });
        List<PptTopLevel> transposedForPpt3 = Arrays.asList(new PptTopLevel[] {
                ppt3, ppt1 });
        List<PptTopLevel> transposedForPpt4 = Arrays.asList(new PptTopLevel[] {
                ppt4, ppt2, ppt3 });

        List<List<PptTopLevel>> expectedTransposedGraph = new ArrayList<List<PptTopLevel>>();
        expectedTransposedGraph.add(transposedForPpt1);
        expectedTransposedGraph.add(transposedForPpt2);
        expectedTransposedGraph.add(transposedForPpt3);
        expectedTransposedGraph.add(transposedForPpt4);

        blockMerger.computeTransposedGraph();
        List<List<PptTopLevel>> transposedGrap = blockMerger
                .getTransposedGraph();

        assertEquals(expectedTransposedGraph, transposedGrap);
    }

    public void testFindMatchingFork() throws Exception {
        PptTopLevel ppt1 = new PptTopLevel(":::1", new VarInfo[] {});
        PptTopLevel ppt2 = new PptTopLevel(":::2", new VarInfo[] {});
        PptTopLevel ppt3 = new PptTopLevel(":::3", new VarInfo[] {});
        PptTopLevel ppt4 = new PptTopLevel(":::4", new VarInfo[] {});

        List<PptTopLevel> successorOfPpt1 = new ArrayList<PptTopLevel>();
        successorOfPpt1.add(ppt1); // this indicates the head node
        successorOfPpt1.add(ppt2); // these are the successor nodes
        successorOfPpt1.add(ppt3);

        List<PptTopLevel> successorOfPpt2 = new ArrayList<PptTopLevel>();
        successorOfPpt2.add(ppt2);
        successorOfPpt2.add(ppt4);

        List<PptTopLevel> successorOfPpt3 = new ArrayList<PptTopLevel>();
        successorOfPpt3.add(ppt3);
        successorOfPpt3.add(ppt4);

        List<PptTopLevel> successorOfPpt4 = new ArrayList<PptTopLevel>();
        successorOfPpt4.add(ppt4);

        List<List<PptTopLevel>> successors = new ArrayList<List<PptTopLevel>>();
        successors.add(successorOfPpt1);
        successors.add(successorOfPpt2);
        successors.add(successorOfPpt3);
        successors.add(successorOfPpt4);

        BasicBlockMerger<PptTopLevel> merger = new BasicBlockMerger<PptTopLevel>(
                successors);
        merger.computeTransposedGraph();
        merger.computeJoinNodes();
        merger.computeForkNodes();
        PptTopLevel matchingFork = merger.findMatchingForkNode(ppt4);

        assertEquals(ppt1, matchingFork);

    }

    /**
     * 
     * @throws Exception
     */
    public void testLargerExampleFromJeff() throws Exception {
        PptTopLevel ppt1 = new PptTopLevel(":::1", new VarInfo[] {});
        PptTopLevel ppt2 = new PptTopLevel(":::2", new VarInfo[] {});
        PptTopLevel ppt3 = new PptTopLevel(":::3", new VarInfo[] {});
        PptTopLevel ppt4 = new PptTopLevel(":::4", new VarInfo[] {});
        PptTopLevel ppt5 = new PptTopLevel(":::5", new VarInfo[] {});
        PptTopLevel ppt6 = new PptTopLevel(":::6", new VarInfo[] {});
        PptTopLevel ppt7 = new PptTopLevel(":::7", new VarInfo[] {});
        PptTopLevel ppt8 = new PptTopLevel(":::8", new VarInfo[] {});
        PptTopLevel ppt9 = new PptTopLevel(":::9", new VarInfo[] {});
        PptTopLevel ppt10 = new PptTopLevel(":::10", new VarInfo[] {});
        PptTopLevel ppt11 = new PptTopLevel(":::11", new VarInfo[] {});
        PptTopLevel ppt12 = new PptTopLevel(":::12", new VarInfo[] {});
        PptTopLevel ppt13 = new PptTopLevel(":::13", new VarInfo[] {});
        PptTopLevel ppt14 = new PptTopLevel(":::14", new VarInfo[] {});

        List<PptTopLevel> successorOfPpt1 = Arrays.asList(new PptTopLevel[] {
                ppt1, ppt2 });

        List<PptTopLevel> successorOfPpt2 = Arrays.asList(new PptTopLevel[] {
                ppt2, ppt3, ppt4 });

        List<PptTopLevel> successorOfPpt3 = Arrays.asList(new PptTopLevel[] {
                ppt3, ppt5 });

        List<PptTopLevel> successorOfPpt4 = Arrays.asList(new PptTopLevel[] {
                ppt4, ppt5 });

        List<PptTopLevel> successorOfPpt5 = Arrays.asList(new PptTopLevel[] {
                ppt5, ppt6 });

        List<PptTopLevel> successorOfPpt6 = Arrays.asList(new PptTopLevel[] {
                ppt6, ppt7 });

        List<PptTopLevel> successorOfPpt7 = Arrays.asList(new PptTopLevel[] {
                ppt7, ppt8, ppt13 });

        List<PptTopLevel> successorOfPpt8 = Arrays.asList(new PptTopLevel[] {
                ppt8, ppt9, ppt10 });

        List<PptTopLevel> successorOfPpt9 = Arrays.asList(new PptTopLevel[] {
                ppt9, ppt11 });

        List<PptTopLevel> successorOfPpt10 = Arrays.asList(new PptTopLevel[] {
                ppt10, ppt11 });

        List<PptTopLevel> successorOfPpt11 = Arrays.asList(new PptTopLevel[] {
                ppt11, ppt12 });

        List<PptTopLevel> successorOfPpt12 = Arrays.asList(new PptTopLevel[] {
                ppt12, ppt13, ppt7 });

        List<PptTopLevel> successorOfPpt13 = Arrays.asList(new PptTopLevel[] {
                ppt13, ppt14 });

        List<PptTopLevel> successorOfPpt14 = Arrays
                .asList(new PptTopLevel[] { ppt14 });

        List<List<PptTopLevel>> successors = new ArrayList<List<PptTopLevel>>();

        successors.add(successorOfPpt1);
        successors.add(successorOfPpt2);
        successors.add(successorOfPpt3);
        successors.add(successorOfPpt4);
        successors.add(successorOfPpt5);
        successors.add(successorOfPpt6);
        successors.add(successorOfPpt7);
        successors.add(successorOfPpt8);
        successors.add(successorOfPpt9);
        successors.add(successorOfPpt10);
        successors.add(successorOfPpt11);
        successors.add(successorOfPpt12);
        successors.add(successorOfPpt13);
        successors.add(successorOfPpt14);

        System.out.println("input= " + successors);

        List<PptTopLevel> mergedForPpt1 = Arrays.asList(new PptTopLevel[] {
                ppt1, ppt1 });
        List<PptTopLevel> mergedForPpt2 = Arrays.asList(new PptTopLevel[] {
                ppt2, ppt1, ppt2 });
        List<PptTopLevel> mergedForPpt3 = Arrays.asList(new PptTopLevel[] {
                ppt3, ppt1, ppt2, ppt3 });
        List<PptTopLevel> mergedForPpt4 = Arrays.asList(new PptTopLevel[] {
                ppt4, ppt1, ppt2, ppt4 });
        List<PptTopLevel> mergedForPpt5 = Arrays.asList(new PptTopLevel[] {
                ppt5, ppt1, ppt2, ppt5 });
        List<PptTopLevel> mergedForPpt6 = Arrays.asList(new PptTopLevel[] {
                ppt6, ppt1, ppt2, ppt5, ppt6 });
        List<PptTopLevel> mergedForPpt7 = Arrays.asList(new PptTopLevel[] {
                ppt7, ppt1, ppt2, ppt5, ppt6, ppt7 });
        List<PptTopLevel> mergedForPpt8 = Arrays.asList(new PptTopLevel[] {
                ppt8, ppt1, ppt2, ppt5, ppt6, ppt7, ppt8 });
        List<PptTopLevel> mergedForPpt9 = Arrays.asList(new PptTopLevel[] {
                ppt9, ppt1, ppt2, ppt5, ppt6, ppt7, ppt8, ppt9 });
        List<PptTopLevel> mergedForPpt10 = Arrays.asList(new PptTopLevel[] {
                ppt10, ppt1, ppt2, ppt5, ppt6, ppt7, ppt8, ppt10 });
        List<PptTopLevel> mergedForPpt11 = Arrays.asList(new PptTopLevel[] {
                ppt11, ppt1, ppt2, ppt5, ppt6, ppt7, ppt8, ppt11 });
        List<PptTopLevel> mergedForPpt12 = Arrays.asList(new PptTopLevel[] {
                ppt12, ppt1, ppt2, ppt5, ppt6, ppt7, ppt8, ppt11, ppt12 });
        List<PptTopLevel> mergedForPpt13 = Arrays.asList(new PptTopLevel[] {
                ppt13, ppt1, ppt2, ppt5, ppt6, ppt7, ppt13 });
        List<PptTopLevel> mergedForPpt14 = Arrays.asList(new PptTopLevel[] {
                ppt14, ppt1, ppt2, ppt5, ppt6, ppt7, ppt13, ppt14 });

        List<List<PptTopLevel>> expectedMergedBlocks = new ArrayList<List<PptTopLevel>>();
        expectedMergedBlocks.add(mergedForPpt1);
        expectedMergedBlocks.add(mergedForPpt2);
        expectedMergedBlocks.add(mergedForPpt3);
        expectedMergedBlocks.add(mergedForPpt4);
        expectedMergedBlocks.add(mergedForPpt5);
        expectedMergedBlocks.add(mergedForPpt6);
        expectedMergedBlocks.add(mergedForPpt7);
        expectedMergedBlocks.add(mergedForPpt8);
        expectedMergedBlocks.add(mergedForPpt9);
        expectedMergedBlocks.add(mergedForPpt10);
        expectedMergedBlocks.add(mergedForPpt11);
        expectedMergedBlocks.add(mergedForPpt12);
        expectedMergedBlocks.add(mergedForPpt13);
        expectedMergedBlocks.add(mergedForPpt14);

        BasicBlockMerger<PptTopLevel> basicBlockMerger = new BasicBlockMerger<PptTopLevel>(
                successors);
        List<List<PptTopLevel>> actualMergedBlocks = basicBlockMerger
                .mergeBasicBlocks();
        System.out.println("output= " + actualMergedBlocks);
        System.out.println();

        assertEquals(expectedMergedBlocks, actualMergedBlocks);

        System.out.println("subsummed Information "
                + basicBlockMerger.getSubsummedList());
        List<PptTopLevel> expectedSubsumedList = Arrays.asList(ppt2, ppt5,
                null, null, ppt6, ppt7, ppt13, ppt11, null, null, ppt12, null,
                ppt14, null);

        assertEquals(expectedSubsumedList, basicBlockMerger.getSubsummedList());

    }
}
