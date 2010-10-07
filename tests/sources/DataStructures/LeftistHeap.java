    package DataStructures;

    // LeftistHeap class
    //
    // CONSTRUCTION: with a negative infinity sentinel
    //
    // ******************PUBLIC OPERATIONS*********************
    // void insert( x )       --> Insert x
    // Comparable deleteMin( )--> Return and remove smallest item
    // Comparable findMin( )  --> Return smallest item
    // boolean isEmpty( )     --> Return true if empty; else false
    // boolean isFull( )      --> Return false in this implementation
    // void makeEmpty( )      --> Remove all items
    // void merge( rhs )      --> Absorb rhs into this heap

    /**
     * Implements a leftist heap.
     * Note that all "matching" is based on the compareTo method.
     * @author Mark Allen Weiss
     */
    public class LeftistHeap
    {
        /**
         * Construct the leftist heap.
         */
        public LeftistHeap( )
        {
            root = null;
        }

        /**
         * Merge rhs into the priority queue.
         * rhs becomes empty. rhs must be different from this.
         * @param rhs the other leftist heap.
         */
        public void merge( LeftistHeap rhs )
        {
            if( this == rhs )    // Avoid aliasing problems
                return;
    
            root = merge( root, rhs.root );
            rhs.root = null;
        }

        /**
         * Internal static method to merge two roots.
         * Deals with deviant cases and calls recursive merge1.
         */
        private static LeftHeapNode merge( LeftHeapNode h1, LeftHeapNode h2 )
        {
            if( h1 == null )
                return h2;
            if( h2 == null )
                return h1;
            if( h1.element.compareTo( h2.element ) < 0 )
                return merge1( h1, h2 );
            else
                return merge1( h2, h1 );
        }

        /**
         * Internal static method to merge two roots.
         * Assumes trees are not empty, and h1's root contains smallest item.
         */
        private static LeftHeapNode merge1( LeftHeapNode h1, LeftHeapNode h2 )
        {
            if( h1.left == null )   // Single node
               h1.left = h2;       // Other fields in h1 already accurate
            else
            {
                h1.right = merge( h1.right, h2 );
                if( h1.left.npl < h1.right.npl )
                    swapChildren( h1 );
                h1.npl = h1.right.npl + 1;
            }
            return h1;
        }

        /**
         * Swaps t's two children.
         */
        private static void swapChildren( LeftHeapNode t )
        {
            LeftHeapNode tmp = t.left;
            t.left = t.right;
            t.right = tmp;
        }

        /**
         * Insert into the priority queue, maintaining heap order.
         * @param x the item to insert.
         */
        public void insert( Comparable x )
        {
            root = merge( new LeftHeapNode( x ), root );
        }

        /**
         * Find the smallest item in the priority queue.
         * @return the smallest item, or null, if empty.
         */
        public Comparable findMin( )
        {
            if( isEmpty( ) )
                return null;
            return root.element;
        }

        /**
         * Remove the smallest item from the priority queue.
         * @return the smallest item, or null, if empty.
         */
        public Comparable deleteMin( )
        {
            if( isEmpty( ) )
                return null;

            Comparable minItem = root.element;
            root = merge( root.left, root.right );

            return minItem;
        }

        /**
         * Test if the priority queue is logically empty.
         * @return true if empty, false otherwise.
         */
        public boolean isEmpty( )
        {
            return root == null;
        }

        /**
         * Test if the priority queue is logically full.
         * @return false in this implementation.
         */
        public boolean isFull( )
        {
            return false;
        }

        /**
         * Make the priority queue logically empty.
         */
        public void makeEmpty( )
        {
            root = null;
        }

        private LeftHeapNode root;    // root

        public static void main( String [ ] args )
        {
            int numItems = 100;
            LeftistHeap h  = new LeftistHeap( );
            LeftistHeap h1 = new LeftistHeap( );
            int i = 37;

                for( i = 37; i != 0; i = ( i + 37 ) % numItems )
                    if( i % 2 == 0 )
                        h1.insert( new MyInteger( i ) );
                    else
                        h.insert( new MyInteger( i ) );

                h.merge( h1 );
                for( i = 1; i < numItems; i++ )
                    if( ((MyInteger)( h.deleteMin( ) )).intValue( ) != i )
                        System.out.println( "Oops! " + i );
        }
    }
