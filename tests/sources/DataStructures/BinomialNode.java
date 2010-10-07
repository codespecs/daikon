    package DataStructures;

    // Basic node stored in binomial queues
    // Note that this class is not accessible outside
    // of package DataStructures

    class BinomialNode
    {
            // Constructors
        BinomialNode( Comparable theElement )
        {
            this( theElement, null, null );
        }

        BinomialNode( Comparable theElement, BinomialNode lt, BinomialNode nt )
        {
            element     = theElement;
            leftChild   = lt;
            nextSibling = nt;
        }

            // Friendly data; accessible by other package routines
        Comparable   element;     // The data in the node
        BinomialNode leftChild;   // Left child
        BinomialNode nextSibling; // Right child
    }
