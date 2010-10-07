    package DataStructures;

    // Treap class
    //
    // CONSTRUCTION: with no initializer
    //
    // ******************PUBLIC OPERATIONS*********************
    // void insert( x )       --> Insert x
    // void remove( x )       --> Remove x
    // Comparable find( x )   --> Return item that matches x
    // Comparable findMin( )  --> Return smallest item
    // Comparable findMax( )  --> Return largest item
    // boolean isEmpty( )     --> Return true if empty; else false
    // void makeEmpty( )      --> Remove all items
    // void printTree( )      --> Print tree in sorted order

    /**
     * Implements a treap.
     * Note that all "matching" is based on the compareTo method.
     * @author Mark Allen Weiss
     */
    public class Treap
    {
        /**
         * Construct the treap.
         */
        public Treap( )
        {
            root = nullNode;
        }

        /**
         * Insert into the tree. Does nothing if x is already present.
         * @param x the item to insert.
         */
        public void insert( Comparable x )
        {
            root = insert( x, root );
        }

        /**
         * Remove from the tree. Does nothing if x is not found.
         * @param x the item to remove.
         */
        public void remove( Comparable x )
        {
            root = remove( x, root );
        }

        /**
         * Find the smallest item in the tree.
         * @return the smallest item, or null if empty.
         */
        public Comparable findMin( )
        {
            if( isEmpty( ) )
                return null;

            TreapNode ptr = root;

            while( ptr.left != nullNode )
                ptr = ptr.left;

            return ptr.element;
        }

        /**
         * Find the largest item in the tree.
         * @return the largest item, or null if empty.
         */
        public Comparable findMax( )
        {
            if( isEmpty( ) )
                return null;

            TreapNode ptr = root;

            while( ptr.right != nullNode )
                ptr = ptr.right;

            return ptr.element;
        }

        /**
         * Find an item in the tree.
         * @param x the item to search for.
         * @return the matching item, or null if not found.
         */
        public Comparable find( Comparable x )
        {
            TreapNode current = root;
            nullNode.element = x;

            for( ; ; )
            {
                if( x.compareTo( current.element ) < 0 )
                    current = current.left;
                else if( x.compareTo( current.element ) > 0 ) 
                    current = current.right;
                else if( current != nullNode )
                    return current.element;
                else
                    return null;
            }
        }

        /**
         * Make the tree logically empty.
         */
        public void makeEmpty( )
        {
            root = nullNode;
        }

        /**
         * Test if the tree is logically empty.
         * @return true if empty, false otherwise.
         */
        public boolean isEmpty( )
        {
            return root == nullNode;
        }

        /**
         * Print the tree contents in sorted order.
         */
        public void printTree( )
        {
            if( isEmpty( ) )
                System.out.println( "Empty tree" );
            else
                printTree( root );
        }

        /**
         * Internal method to insert into a subtree.
         * @param x the item to insert.
         * @param t the node that roots the tree.
         * @return the new root.
         */
        private TreapNode insert( Comparable x, TreapNode t )
        {
            if( t == nullNode )
                t = new TreapNode( x, nullNode, nullNode );
            else if( x.compareTo( t.element ) < 0 )
            {
                t.left = insert( x, t.left );
                if( t.left.priority < t.priority )
                    t = rotateWithLeftChild( t );
            }
            else if( x.compareTo( t.element ) > 0  )
            {
                t.right = insert( x, t.right );
                if( t.right.priority < t.priority )
                    t = rotateWithRightChild( t );
            }
            // Otherwise, it's a duplicate; do nothing

            return t;
        }

        /**
         * Internal method to remove from a subtree.
         * @param x the item to remove.
         * @param t the node that roots the tree.
         * @return the new root.
         */
        private TreapNode remove( Comparable x, TreapNode t )
        {
            if( t != nullNode )
            {
                if( x.compareTo( t.element ) < 0 )
                    t.left = remove( x, t.left );
                else if( x.compareTo( t.element ) > 0 )
                    t.right = remove( x, t.right );
                else
                {
                        // Match found
                    if( t.left.priority < t.right.priority )
                        t = rotateWithLeftChild( t );
                    else
                        t = rotateWithRightChild( t );

                    if( t != nullNode )     // Continue on down
                        t = remove( x, t );
                    else
                        t.left = nullNode;  // At a leaf
                }
            }
            return t;
        }

        /**
         * Internal method to print a subtree in sorted order.
         * @param t the node that roots the tree.
         */
        private void printTree( TreapNode t )
        {
            if( t != t.left )
            {
                printTree( t.left );
                System.out.println( t.element.toString( ) );
                printTree( t.right );
            }
        }

        /**
         * Rotate binary tree node with left child.
         */
        static TreapNode rotateWithLeftChild( TreapNode k2 )
        {
            TreapNode k1 = k2.left;
            k2.left = k1.right;
            k1.right = k2;
            return k1;
        }

        /**
         * Rotate binary tree node with right child.
         */
        static TreapNode rotateWithRightChild( TreapNode k1 )
        {
            TreapNode k2 = k1.right;
            k1.right = k2.left;
            k2.left = k1;
            return k2;
        }

        private TreapNode root;
        private static TreapNode nullNode;
            static         // Static initializer for NullNode
            {
                nullNode = new TreapNode( null );
                nullNode.left = nullNode.right = nullNode;
                nullNode.priority = Integer.MAX_VALUE;
            }

            // Test program
        public static void main( String [ ] args )
        {
            Treap t = new Treap( );
            final int NUMS = 40000;
            final int GAP  =   307;

            System.out.println( "Checking... (no bad output means success)" );

            for( int i = GAP; i != 0; i = ( i + GAP ) % NUMS )
                t.insert( new MyInteger( i ) );
            System.out.println( "Inserts complete" );

            for( int i = 1; i < NUMS; i+= 2 )
                t.remove( new MyInteger( i ) );
            System.out.println( "Removes complete" );

            if( NUMS < 40 )
                t.printTree( );
            if( ((MyInteger)(t.findMin( ))).intValue( ) != 2 ||
                ((MyInteger)(t.findMax( ))).intValue( ) != NUMS - 2 )
                System.out.println( "FindMin or FindMax error!" );

            for( int i = 2; i < NUMS; i+=2 )
                if( ((MyInteger)t.find( new MyInteger( i ) )).intValue( ) != i )
                    System.out.println( "Error: find fails for " + i );

            for( int i = 1; i < NUMS; i+=2 )
                if( t.find( new MyInteger( i ) )  != null )
                    System.out.println( "Error: Found deleted item " + i );
        }
    }
