    package DataStructures;

    // Basic node stored in leftist heaps
    // Note that this class is not accessible outside
    // of package DataStructures

    class LeftHeapNode
    {
            // Constructors
        LeftHeapNode( Comparable theElement )
        {
            this( theElement, null, null );
        }

        LeftHeapNode( Comparable theElement, LeftHeapNode lt, LeftHeapNode rt )
        {
            element = theElement;
            left    = lt;
            right   = rt;
            npl     = 0;
        }

            // Friendly data; accessible by other package routines
        Comparable   element;      // The data in the node
        LeftHeapNode left;         // Left child
        LeftHeapNode right;        // Right child
        int          npl;          // null path length
    }
