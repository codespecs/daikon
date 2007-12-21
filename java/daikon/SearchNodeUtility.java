/**
 * 
 */
package daikon;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public class SearchNodeUtility<T> {

    private List<List<T>> graph;

    private List<T> indexNodes;

    private List<T> stopConditions;
    
    boolean found = false;
    
    T foundNode = null;

    public SearchNodeUtility(List<List<T>> graph) {
        this.graph = graph;
        indexNodes = new ArrayList<T>();

        for (List<T> nodeRow : graph) {
            indexNodes.add(nodeRow.get(0));
        }
    }

    public void initializeStopConditions(List<T> stopConditions) {
        this.stopConditions = stopConditions;
    }

    /**
     * 
     * @param start
     *            Non-null start node
     * @param end
     *            Non-null end node
     * @param visited
     *            the list of nodes that have been already visited
     * @return Boolean indicating whether it is possible to reach from the start
     *         node to the end node without visiting nodes that were already
     *         visited
     */
    
    public boolean findDepthFirst(T start, T end, List<T> visited) {
        found = false;
        doFindDepthFirst(start, end, visited);
        return found;
    }
    
    public void doFindDepthFirst(T start, T end, List<T> visited) {
        if (start.equals(end)) {
            found = true;
            return;
        }

        visited.add(start);

        int index = indexNodes.indexOf(start);
        List<T> nodeAdjacents = graph.get(index);

        Iterator<T> iterator = nodeAdjacents.iterator();
        // skip first element since nodeAdjacents[0] == node
        iterator.next();

        while (iterator.hasNext()) {
            T adjacentNode = iterator.next();
            if (adjacentNode.equals(end)) {
                found = true;
                return;
            }
            if (!visited.contains(adjacentNode) && found == false) {
                doFindDepthFirst(adjacentNode, end, visited);
            }
        }

    }

    public T findDepthFirstStopAtCondition(T start, List<T> visited) {
        foundNode = null;
        doFindDepthFirstStopAtCondition(start, visited);
        return foundNode;
    }
    
    public void doFindDepthFirstStopAtCondition(T start, List<T> visited) {
        if (!visited.contains(start) && stopConditions.contains(start)) {
            foundNode = start;
            return;
        }

        visited.add(start);

        int index = indexNodes.indexOf(start);
        List<T> nodeAdjacents = graph.get(index);

        Iterator<T> iterator = nodeAdjacents.iterator();
        // skip first element since nodeAdjacents[0] == node
        iterator.next();

        while (iterator.hasNext()) {
            T adjacentNode = iterator.next();
            if (!visited.contains(adjacentNode) && foundNode == null) {
                doFindDepthFirstStopAtCondition(adjacentNode, visited);
            }
        }
    }

}