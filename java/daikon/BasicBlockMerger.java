package daikon;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public class BasicBlockMerger<T> {

    private List<List<T>> graph;

    private List<T> preorderTraversal;

    private List<T> indexNodes;

    private List<List<T>> transposedGraph;

    private List<T> forks;

    private List<T> joins;

    private List<List<T>> mergedGraph;

    private List<T> mergedNodes;

    private List<T> subsummedBy;

    public BasicBlockMerger(List<List<T>> graph) {
        this.graph = graph;
        preorderTraversal = new ArrayList<T>();
        indexNodes = new ArrayList<T>();

        for (List<T> nodeAdjacents : graph) {
            indexNodes.add(nodeAdjacents.get(0));
        }
    }

    /**
     * initial implementation to merge Basic Blocks
     * 
     * @return A structure indicating what are the other PPTs that need to be
     *         combined with the current PPT
     */
    public List<List<T>> mergeBasicBlocks() {

        computeTransposedGraph();

        computeForkNodes();

        computeJoinNodes();

        initializeMergedGraph();

        List<T> predecessorList = new ArrayList<T>();

        fillUpMergedGraph(graph.get(0).get(0), predecessorList);

        return mergedGraph;
    }

    // DFS traversal of the whole graph
    private void fillUpMergedGraph(T currentNode, List<T> predecessorList) {
        mergedNodes.add(currentNode);

        int currentNodeIndex = indexNodes.indexOf(currentNode);
        List<T> extendedPredecessorList = new ArrayList<T>();

        if (!joins.contains(currentNode)) {
            extendedPredecessorList.addAll(predecessorList);
            extendedPredecessorList.add(currentNode);
            if (predecessorList.size() > 0)
                fillUpSubsumedListInformation(currentNode, predecessorList
                        .get(predecessorList.size() - 1));
        } else {
            // treat the case when the node is a join

            T matchingForkNode = findMatchingForkNode(currentNode);

            int indexOfFork;
            if ((matchingForkNode == null)
                    || (currentNode.equals(matchingForkNode)))
                indexOfFork = indexNodes.indexOf(predecessorList
                        .get(predecessorList.size() - 1)); // findPreviousNodeIndex(currentNode);
            else
                indexOfFork = indexNodes.indexOf(matchingForkNode);

            // since we do a DFS grap traversal, this ensures that by the time
            // we process a node, its direct predecessors (e.g., forks) have
            // been already processed
            List<T> predecessorsOfFork = mergedGraph.get(indexOfFork);

            // skip the first element since it is the forkNode itself
            for (int i = 1; i < predecessorsOfFork.size(); i++)
                extendedPredecessorList.add(predecessorsOfFork.get(i));

            extendedPredecessorList.add(currentNode);
            if (predecessorsOfFork.size() > 0)
                fillUpSubsumedListInformation(currentNode, predecessorsOfFork
                        .get(predecessorsOfFork.size() - 1));
        }

        mergedGraph.get(currentNodeIndex).addAll(extendedPredecessorList);

        List<T> nodeAdjacents = graph.get(currentNodeIndex);
        Iterator<T> iterator = nodeAdjacents.iterator();
        // skip first element since nodeAdjacents[0] == currentNode
        iterator.next();

        while (iterator.hasNext()) {
            T adjacentNode = iterator.next();

            if (!mergedNodes.contains(adjacentNode)) {
                fillUpMergedGraph(adjacentNode, extendedPredecessorList);
            }
        }

    }

    private void fillUpSubsumedListInformation(T currentNode, T predecessorNode) {

        if (forks.contains(predecessorNode) && !joins.contains(currentNode))
            return; // do not fill the information for the fork because it will
        // be filled up when processing its correspoding join

        subsummedBy.set(indexNodes.indexOf(predecessorNode), currentNode);

    }

    private int findPreviousNodeIndex(T currentNode) {
        SearchNodeUtility<T> searchUtil = new SearchNodeUtility<T>(graph);
        List<T> visitedNodes = new ArrayList<T>();
        searchUtil.findDepthFirst(graph.get(0).get(0), currentNode,
                visitedNodes);

        T predecessorNode = visitedNodes.get(visitedNodes.size() - 1);

        return indexNodes.indexOf(predecessorNode);
    }

    public T findMatchingForkNode(T joinNode) {
        SearchNodeUtility<T> predecessorSearch = new SearchNodeUtility<T>(
                transposedGraph);
        predecessorSearch.initializeStopConditions(forks);
        SearchNodeUtility<T> successorSearch = new SearchNodeUtility<T>(graph);

        List<T> visitedNodes = new ArrayList<T>();
        
        // initialize the visitedNodes with the adjacents of the joinNode.
        // This forces the search to look only in the true predecessors
        // of the joinNode (otherwise, the search would look into successor
        // nodes that happen to be predecessors as well (for example see 
        // BasicBlockMergerTest.testOneCycle() )
        List<T> adjacentsToJoinNode = graph.get(indexNodes.indexOf(joinNode));
        for (int i = 0; i < adjacentsToJoinNode.size(); i++)
            visitedNodes.add(adjacentsToJoinNode.get(i));
        
        T earlierFork = predecessorSearch.findDepthFirstStopAtCondition(
                joinNode, visitedNodes);

        

        boolean foundEarliestFork = false;
        while (!foundEarliestFork) {
            // for a scenario when a join node does not match
            // any fork, see BasicBlockMerger.testOneCycle
            if (earlierFork == null)
                return null;
            
            List<T> copyVisitedNodes = new ArrayList<T>();
            copyVisitedNodes.addAll(visitedNodes);
            // if it is possible to come back to the join, without visiting
            // previously seen nodes, we found the earliest matching fork
            foundEarliestFork = successorSearch.findDepthFirst(earlierFork,
                    joinNode, copyVisitedNodes);
            if (!foundEarliestFork)
                earlierFork = predecessorSearch.findDepthFirstStopAtCondition(
                        earlierFork, copyVisitedNodes); 
        }

        return earlierFork;
    }

    public void depthFirstSearch() {

        List<T> firstNodeRow = graph.get(0);
        doDepthFirstSearch(firstNodeRow.get(0));

    }

    public void doDepthFirstSearch(T node) {

        preorderTraversal.add(node);

        int index = indexNodes.indexOf(node);
        List<T> nodeAdjacents = graph.get(index);

        Iterator<T> iterator = nodeAdjacents.iterator();
        // skip first element since nodeAdjacents[0] == node
        iterator.next();

        while (iterator.hasNext()) {
            T adjacentNode = iterator.next();
            if (!preorderTraversal.contains(adjacentNode)) {
                doDepthFirstSearch(adjacentNode);
            }
        }
    }

    private List<List<T>> initializeMergedGraph() {
        mergedNodes = new ArrayList<T>();

        mergedGraph = new ArrayList<List<T>>();

        subsummedBy = new ArrayList<T>(indexNodes.size());
        for (int i = 0; i < indexNodes.size(); i++) {
            subsummedBy.add(null); // TODO ask Jeff if he wants a different
                                    // value to indicate that this list is not
                                    // subsummed by any other list
        }

        for (List<T> list : graph) {
            List<T> dominators = new ArrayList<T>();
            dominators.add(list.get(0));
            mergedGraph.add(dominators);
        }

        return mergedGraph;
    }

    /**
     * precondition: This method can be called ONLY after calling
     * {@link BasicBlockMerger.computeTransposedGraph()}
     * 
     */
    public void computeJoinNodes() {
        joins = new ArrayList<T>();
        computeForks(transposedGraph, joins);
    }

    public void computeForks(List<List<T>> graph, List<T> forkNodes) {
        for (List<T> nodeRow : graph) {
            if (nodeRow.size() > 2)
                forkNodes.add(nodeRow.get(0));
        }

    }

    public void computeForkNodes() {
        forks = new ArrayList<T>();
        computeForks(graph, forks);
    }

    public List<T> getPreorderTraversal() {
        return preorderTraversal;
    }

    public List<List<T>> getTransposedGraph() {
        return transposedGraph;
    }

    public void computeTransposedGraph() {
        transposedGraph = new ArrayList<List<T>>();

        for (T node : indexNodes) {
            List<T> nodeAdjacents = new ArrayList<T>();
            nodeAdjacents.add(node);
            transposedGraph.add(nodeAdjacents);
        }

        for (int indexNode = 0; indexNode < graph.size(); indexNode++) {
            List<T> nodeRow = graph.get(indexNode);
            T firstNode = nodeRow.get(0);
            for (int indexAdjacents = 1; indexAdjacents < nodeRow.size(); indexAdjacents++) {
                T adjacentNode = nodeRow.get(indexAdjacents);
                int generalIndexForNode = indexNodes.indexOf(adjacentNode);
                transposedGraph.get(generalIndexForNode).add(firstNode);
            }
        }
    }

    public List<T> getSubsummedList() {
        return subsummedBy;
    }

    public List<T> getIndexes() {
        return indexNodes;
    }

}
