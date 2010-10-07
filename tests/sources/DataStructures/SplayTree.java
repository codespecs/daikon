    package DataStructures;

    // SplayTree class
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
     * Implements a top-down splay tree.
     * Note that all "matching" is based on the compareTo method.
     * @author Mark Allen Weiss
     */
    public class SplayTree
    {
        /**
         * Construct the tree.
         */
        public SplayTree( )
        {
            root = nullNode;
        }

        /**
         * Insert into the tree.
         * @param x the item to insert.
         */
        public void insert( Comparable x )
        {
            if( newNode == null )
                newNode = new BinaryNode( null );
            newNode.element = x;

            if( root == nullNode )
            {
                newNode.left = newNode.right = nullNode;
                root = newNode;
            }
            else
            {
                root = splay( x, root );
                if( x.compareTo( root.element ) < 0 )
                {
                    newNode.left = root.left;
                    newNode.right = root;
                    root.left = nullNode;
                    root = newNode;
                }
                else
                if( x.compareTo( root.element ) > 0 )
                {
                    newNode.right = root.right;
                    newNode.left = root;
                    root.right = nullNode;
                    root = newNode;
                }
                else
                    return;
            }
            newNode = null;   // So next insert will call new
        }

        /**
         * Remove from the tree.
         * @param x the item to remove.
         */
        public void remove( Comparable x )
        {
            BinaryNode newTree;

                // If x is found, it will be at the root
            root = splay( x, root );
            if( root.element.compareTo( x ) != 0 )
                return;   // Item not found; do nothing

            if( root.left == nullNode )
                newTree = root.right;
            else
            {
                // Find the maximum in the left subtree
                // Splay it to the root; and then attach right child
                newTree = root.left;
                newTree = splay( x, newTree );
                newTree.right = root.right;
            }
            root = newTree;
        }

        /**
         * Find the smallest item in the tree.
         * Not the most efficient implementation (uses two passes), but has correct
         *     amortized behavior.
         * A good alternative is to first call Find with parameter
         *     smaller than any item in the tree, then call findMin.
         * @return the smallest item or null if empty.
         */
        public Comparable findMin( )
        {
            if( isEmpty( ) )
                return null;

            BinaryNode ptr = root;

            while( ptr.left != nullNode )
                ptr = ptr.left;

            root = splay( ptr.element, root );
            return ptr.element;
        }

        /**
         * Find the largest item in the tree.
         * Not the most efficient implementation (uses two passes), but has correct
         *     amortized behavior.
         * A good alternative is to first call Find with parameter
         *     larger than any item in the tree, then call findMax.
         * @return the largest item or null if empty.
         */
        public Comparable findMax( )
        {
            if( isEmpty( ) )
                return null;

            BinaryNode ptr = root;

            while( ptr.right != nullNode )
                ptr = ptr.right;

            root = splay( ptr.element, root );
            return ptr.element;
        }

        /**
         * Find an item in the tree.
         * @param x the item to search for.
         * @return the matching item or null if not found.
         */
        public Comparable find( Comparable x )
        {
            root = splay( x, root );

            if( isEmpty( ) || root.element.compareTo( x ) != 0 )
                return null;

            return root.element;
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
         * Internal method to perform a top-down splay.
         * The last accessed node becomes the new root.
         * @param x the target item to splay around.
         * @param t the root of the subtree to splay.
         * @return the subtree after the splay.
         */
        private BinaryNode splay( Comparable x, BinaryNode t )
        {
            BinaryNode leftTreeMax, rightTreeMin;

            header.left = header.right = nullNode;
            leftTreeMax = rightTreeMin = header;

            nullNode.element = x;   // Guarantee a match

            for( ; ; )
                if( x.compareTo( t.element ) < 0 )
                {
                    if( x.compareTo( t.left.element ) < 0 )
                        t = rotateWithLeftChild( t );
                    if( t.left == nullNode )
                        break;
                    // Link Right
                    rightTreeMin.left = t;
                    rightTreeMin = t;
                    t = t.left;
                }
                else if( x.compareTo( t.element ) > 0 )
                {
                    if( x.compareTo( t.right.element ) > 0 )
                        t = rotateWithRightChild( t );
                    if( t.right == nullNode )
                        break;
                    // Link Left
                    leftTreeMax.right = t;
                    leftTreeMax = t;
                    t = t.right;
                }
                else
                    break;

            leftTreeMax.right = t.left;
            rightTreeMin.left = t.right;
            t.left = header.right;
            t.right = header.left;
            return t;
        }

        /**
         * Rotate binary tree node with left child.
         */
        static BinaryNode rotateWithLeftChild( BinaryNode k2 )
        {
            BinaryNode k1 = k2.left;
            k2.left = k1.right;
            k1.right = k2;
            return k1;
        }

        /**
         * Rotate binary tree node with right child.
         */
        static BinaryNode rotateWithRightChild( BinaryNode k1 )
        {
            BinaryNode k2 = k1.right;
            k1.right = k2.left;
            k2.left = k1;
            return k2;
        }

        /**
         * Internal method to print a subtree in sorted order.
         * WARNING: This is prone to running out of stack space.
         * @param t the node that roots the tree.
         */
        private void printTree( BinaryNode t )
        {
            if( t != t.left )
            {
                printTree( t.left );
                System.out.println( t.element.toString( ) );
                printTree( t.right );
            }
        }


        private BinaryNode root;
        private static BinaryNode nullNode;
            static         // Static initializer for nullNode
            {
                nullNode = new BinaryNode( null );
                nullNode.left = nullNode.right = nullNode;
            }

        private static BinaryNode newNode = null;  // Used between different inserts
        private static BinaryNode header = new BinaryNode( null ); // For splay


            // Test program; should print min and max and nothing else
        public static void main( String [ ] args )
        {
            SplayTree t = new SplayTree( );
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
