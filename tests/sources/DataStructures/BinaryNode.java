    package DataStructures;

    // Basic node stored in unbalanced binary search trees
    // Note that this class is not accessible outside
    // of package DataStructures

    class BinaryNode<E extends Comparable>
    {
            // Constructors
        BinaryNode( E theElement )
        {
            this( theElement, null, null );
        }

        BinaryNode( E theElement, BinaryNode lt, BinaryNode rt )
        {
            element  = theElement;
            left     = lt;
            right    = rt;
        }

            // Friendly data; accessible by other package routines
            E element;      // The data in the node
        BinaryNode left;         // Left child
        BinaryNode right;        // Right child
    }
