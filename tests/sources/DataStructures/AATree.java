    package DataStructures;

    // AATree class
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
     * Implements an AA-tree.
     * Note that all "matching" is based on the compareTo method.
     * @author Mark Allen Weiss
     */
    public class AATree
    {
        /**
         * Construct the tree.
         */
        public AATree( )
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
            deletedNode = nullNode;
            root = remove( x, root );
        }

        /**
         * Find the smallest item in the tree.
         * @return the smallest item or null if empty.
         */
        public Comparable findMin( )
        {
            if( isEmpty( ) )
                return null;

            AANode ptr = root;

            while( ptr.left != nullNode )
                ptr = ptr.left;

            return ptr.element;
        }

        /**
         * Find the largest item in the tree.
         * @return the largest item or null if empty.
         */
        public Comparable findMax( )
        {
            if( isEmpty( ) )
                return null;

            AANode ptr = root;

            while( ptr.right != nullNode )
                ptr = ptr.right;

            return ptr.element;
        }

        /**
         * Find an item in the tree.
         * @param x the item to search for.
         * @return the matching item of null if not found.
         */
        public Comparable find( Comparable x )
        {
            AANode current = root;
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
        private AANode insert( Comparable x, AANode t )
        {
            if( t == nullNode )
                t = new AANode( x, nullNode, nullNode );
            else if( x.compareTo( t.element ) < 0 )
                t.left = insert( x, t.left );
            else if( x.compareTo( t.element ) > 0 )
                t.right = insert( x, t.right );
            else
                return t;

            t = skew( t );
            t = split( t );
            return t;
        }

        /**
         * Internal method to remove from a subtree.
         * @param x the item to remove.
         * @param t the node that roots the tree.
         * @return the new root.
         */
        private AANode remove( Comparable x, AANode t )
        {
            if( t != nullNode )
            {
                // Step 1: Search down the tree and set lastNode and deletedNode
                lastNode = t;
                if( x.compareTo( t.element ) < 0 )
                    t.left = remove( x, t.left );
                else
                {
                    deletedNode = t;
                    t.right = remove( x, t.right );
                }

                // Step 2: If at the bottom of the tree and
                //         x is present, we remove it
                if( t == lastNode )
                {
                    if( deletedNode == nullNode || x.compareTo( deletedNode.element ) != 0 )
                        return t;   // Item not found; do nothing
                    deletedNode.element = t.element;
                    t = t.right;
                }

                // Step 3: Otherwise, we are not at the bottom; rebalance
                else
                    if( t.left.level < t.level - 1 || t.right.level < t.level - 1 )
                    {
                        if( t.right.level > --t.level )
                            t.right.level = t.level;
                        t = skew( t );
                        t.right = skew( t.right );
                        t.right.right = skew( t.right.right );
                        t = split( t );
                        t.right = split( t.right );
                    }
            }
            return t;
        }

        /**
         * Internal method to print a subtree in sorted order.
         * @param t the node that roots the tree.
         */
        private void printTree( AANode t )
        {
            if( t != t.left )
            {
                printTree( t.left );
                System.out.println( t.element.toString( ) );
                printTree( t.right );
            }
        }

        /**
         * Skew primitive for AA-trees.
         * @param t the node that roots the tree.
         * @return the new root after the rotation.
         */
        private AANode skew( AANode t )
        {
            if( t.left.level == t.level )
                t = rotateWithLeftChild( t );
            return t;
        }

        /**
         * Split primitive for AA-trees.
         * @param t the node that roots the tree.
         * @return the new root after the rotation.
         */
        private AANode split( AANode t )
        {
            if( t.right.right.level == t.level )
            {
                t = rotateWithRightChild( t );
                t.level++;
            }
            return t;
        }

        /**
         * Rotate binary tree node with left child.
         */
        static AANode rotateWithLeftChild( AANode k2 )
        {
            AANode k1 = k2.left;
            k2.left = k1.right;
            k1.right = k2;
            return k1;
        }

        /**
         * Rotate binary tree node with right child.
         */
        static AANode rotateWithRightChild( AANode k1 )
        {
            AANode k2 = k1.right;
            k1.right = k2.left;
            k2.left = k1;
            return k2;
        }

        private AANode root;
        private static AANode nullNode;
            static         // static initializer for nullNode
            {
                nullNode = new AANode( null );
                nullNode.left = nullNode.right = nullNode;
                nullNode.level = 0;
            }

        private static AANode deletedNode;
        private static AANode lastNode;

            // Test program; should print min and max and nothing else
        public static void main( String [ ] args )
        {
            AATree t = new AATree( );
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
