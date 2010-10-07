    package DataStructures;

    // Basic node stored in AVL trees
    // Note that this class is not accessible outside
    // of package DataStructures

    class AANode
    {
            // Constructors
        AANode( Comparable theElement )
        {
            this( theElement, null, null );
        }

        AANode( Comparable theElement, AANode lt, AANode rt )
        {
            element  = theElement;
            left     = lt;
            right    = rt;
            level    = 1;
        }

            // Friendly data; accessible by other package routines
        Comparable element;      // The data in the node
        AANode     left;         // Left child
        AANode     right;        // Right child
        int        level;        // Level
    }
