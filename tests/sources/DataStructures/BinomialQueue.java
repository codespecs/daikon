    package DataStructures;

    // BinomialQueue class
    //
    // CONSTRUCTION: with a negative infinity sentinel
    //
    // ******************PUBLIC OPERATIONS*********************
    // void insert( x )       --> Insert x
    // Comparable deleteMin( )--> Return and remove smallest item
    // Comparable findMin( )  --> Return smallest item
    // boolean isEmpty( )     --> Return true if empty; else false
    // boolean isFull( )      --> Return true if full; else false
    // void makeEmpty( )      --> Remove all items
    // vod merge( rhs )       --> Absord rhs into this heap
    // ******************ERRORS********************************
    // Overflow if CAPACITY is exceeded

    /**
     * Implements a binomial queue.
     * Note that all "matching" is based on the compareTo method.
     * @author Mark Allen Weiss
     */
    public class BinomialQueue
    {
        /**
         * Construct the binomial queue.
         */
        public BinomialQueue( )
        {
            theTrees = new BinomialNode[ MAX_TREES ];
            makeEmpty( );
        }

        /**
         * Merge rhs into the priority queue.
         * rhs becomes empty. rhs must be different from this.
         * @param rhs the other binomial queue.
         * @exception Overflow if result exceeds capacity.
         */
        public void merge( BinomialQueue rhs ) throws Overflow
        {
            if( this == rhs )    // Avoid aliasing problems
                return;

            if( currentSize + rhs.currentSize > capacity( ) )
                throw new Overflow( );

            currentSize += rhs.currentSize;

            BinomialNode carry = null;
            for( int i = 0, j = 1; j <= currentSize; i++, j *= 2 )
            {
                BinomialNode t1 = theTrees[ i ];
                BinomialNode t2 = rhs.theTrees[ i ];

                int whichCase = t1 == null ? 0 : 1;
                whichCase += t2 == null ? 0 : 2;
                whichCase += carry == null ? 0 : 4;

                switch( whichCase )
                {
                  case 0: /* No trees */
                  case 1: /* Only this */
                    break;
                  case 2: /* Only rhs */
                    theTrees[ i ] = t2;
                    rhs.theTrees[ i ] = null;
                    break;
                  case 4: /* Only carry */
                    theTrees[ i ] = carry;
                    carry = null;
                    break;
                  case 3: /* this and rhs */
                    carry = combineTrees( t1, t2 );
                    theTrees[ i ] = rhs.theTrees[ i ] = null;
                    break;
                  case 5: /* this and carry */
                    carry = combineTrees( t1, carry );
                    theTrees[ i ] = null;
                    break;
                  case 6: /* rhs and carry */
                    carry = combineTrees( t2, carry );
                    rhs.theTrees[ i ] = null;
                    break;
                  case 7: /* All three */
                    theTrees[ i ] = carry;
                    carry = combineTrees( t1, t2 );
                    rhs.theTrees[ i ] = null;
                    break;
                }
            }

            for( int k = 0; k < rhs.theTrees.length; k++ )
                rhs.theTrees[ k ] = null;
            rhs.currentSize = 0;
        }        

        /**
         * Return the result of merging equal-sized t1 and t2.
         */
        private static BinomialNode combineTrees( BinomialNode t1,
                                                  BinomialNode t2 )
        {
            if( t1.element.compareTo( t2.element ) > 0 )
                return combineTrees( t2, t1 );
            t2.nextSibling = t1.leftChild;
            t1.leftChild = t2;
            return t1;
        }

        /**
         * Insert into the priority queue, maintaining heap order.
         * This implementation is not optimized for O(1) performance.
         * @param x the item to insert.
         * @exception Overflow if capacity exceeded.
         */
        public void insert( Comparable x ) throws Overflow
        {
            BinomialQueue oneItem = new BinomialQueue( );
            oneItem.currentSize = 1;
            oneItem.theTrees[ 0 ] = new BinomialNode( x );

            merge( oneItem );
        }

        /**
         * Find the smallest item in the priority queue.
         * @return the smallest item, or null, if empty.
         */
        public Comparable findMin( )
        {
            if( isEmpty( ) )
                return null;

            return theTrees[ findMinIndex( ) ].element;
        }

    
        /**
         * Find index of tree containing the smallest item in the priority queue.
         * The priority queue must not be empty.
         * @return the index of tree containing the smallest item.
         */
        private int findMinIndex( )
        {
            int i;
            int minIndex;

            for( i = 0; theTrees[ i ] == null; i++ )
                ;

            for( minIndex = i; i < theTrees.length; i++ )
                if( theTrees[ i ] != null &&
                    theTrees[ i ].element.compareTo( theTrees[ minIndex ].element ) < 0 )
                    minIndex = i;

            return minIndex;
        }

        /**
         * Remove the smallest item from the priority queue.
         * @return the smallest item, or null, if empty.
         */
        public Comparable deleteMin( )
        {
            if( isEmpty( ) )
                return null;

            int minIndex = findMinIndex( );
            Comparable minItem = theTrees[ minIndex ].element;

            BinomialNode deletedTree = theTrees[ minIndex ].leftChild;

            BinomialQueue deletedQueue = new BinomialQueue( );
            deletedQueue.currentSize = ( 1 << minIndex ) - 1;
            for( int j = minIndex - 1; j >= 0; j-- )
            {
                deletedQueue.theTrees[ j ] = deletedTree;
                deletedTree = deletedTree.nextSibling;
                deletedQueue.theTrees[ j ].nextSibling = null;
            }

            theTrees[ minIndex ] = null;
            currentSize -= deletedQueue.currentSize + 1;

            try
              { merge( deletedQueue ); }
            catch( Overflow e ) { }
            return minItem;
        }

        /**
         * Test if the priority queue is logically empty.
         * @return true if empty, false otherwise.
         */
        public boolean isEmpty( )
        {
            return currentSize == 0;
        }

        /**
         * Test if the priority queue is logically full.
         * @return true if full, false otherwise.
         */
        public boolean isFull( )
        {
            return currentSize == capacity( );
        }

        /**
         * Make the priority queue logically empty.
         */
        public void makeEmpty( )
        {
            currentSize = 0;
            for( int i = 0; i < theTrees.length; i++ )
                theTrees[ i ] = null;
        }


        private static final int MAX_TREES = 14;

        private int currentSize;            // # items in priority queue
        private BinomialNode [ ] theTrees;  // An array of tree roots
    

        /**
         * Return the capacity.
         */
        private int capacity( )
        {
            return ( 1 << theTrees.length ) - 1;
        }

        public static void main( String [ ] args )
        {
            int numItems = 10000;
            BinomialQueue h  = new BinomialQueue( );
            BinomialQueue h1 = new BinomialQueue( );
            int i = 37;

            System.out.println( "Starting check." );
            try
            {
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
            catch( Overflow e ) { System.out.println( "Unexpected overflow" ); } 
            System.out.println( "Check done." );
        }
    }
