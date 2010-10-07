    package DataStructures;

    // SeparateChainingHashTable class
    //
    // CONSTRUCTION: with an approximate initial size or default of 101
    //
    // ******************PUBLIC OPERATIONS*********************
    // void insert( x )       --> Insert x
    // void remove( x )       --> Remove x
    // Hashable find( x )     --> Return item that matches x
    // void makeEmpty( )      --> Remove all items
    // int hash( String str, int tableSize )
    //                        --> Static method to hash strings
    // ******************ERRORS********************************
    // insert overrides previous value if duplicate; not an error

    /**
     * Separate chaining table implementation of hash tables.
     * Note that all "matching" is based on the equals method.
     * @author Mark Allen Weiss
     */
    public class SeparateChainingHashTable
    {
        /**
         * Construct the hash table.
         */
        public SeparateChainingHashTable( )
        {
            this( DEFAULT_TABLE_SIZE );
        }

        /**
         * Construct the hash table.
         * @param size approximate table size.
         */
        public SeparateChainingHashTable( int size )
        {
            theLists = new LinkedList[ nextPrime( size ) ];
            for( int i = 0; i < theLists.length; i++ )
                theLists[ i ] = new LinkedList( );
        }

        /**
         * Insert into the hash table. If the item is
         * already present, then do nothing.
         * @param x the item to insert.
         */
        public void insert( Hashable x )
        {
            LinkedList whichList = theLists[ x.hash( theLists.length ) ];
            LinkedListItr itr = whichList.find( x );

            if( itr.isPastEnd( ) )
                whichList.insert( x, whichList.zeroth( ) );
        }

        /**
         * Remove from the hash table.
         * @param x the item to remove.
         */
        public void remove( Hashable x )
        {
           theLists[ x.hash( theLists.length ) ].remove( x );
        }

        /**
         * Find an item in the hash table.
         * @param x the item to search for.
         * @return the matching item, or null if not found.
         */
        public Hashable find( Hashable x )
        {
            return (Hashable)theLists[ x.hash( theLists.length ) ].find( x ).retrieve( );
        }

        /**
         * Make the hash table logically empty.
         */
        public void makeEmpty( )
        {
            for( int i = 0; i < theLists.length; i++ )
                theLists[ i ].makeEmpty( );
        }

        /**
         * A hash routine for String objects.
         * @param key the String to hash.
         * @param tableSize the size of the hash table.
         * @return the hash value.
         */
        public static int hash( String key, int tableSize )
        {
            int hashVal = 0;

            for( int i = 0; i < key.length( ); i++ )
                hashVal = 37 * hashVal + key.charAt( i );

            hashVal %= tableSize;
            if( hashVal < 0 )
                hashVal += tableSize;

            return hashVal;
        }

        private static final int DEFAULT_TABLE_SIZE = 101;

            /** The array of Lists. */
        private LinkedList [ ] theLists; 

        /**
         * Internal method to find a prime number at least as large as n.
         * @param n the starting number (must be positive).
         * @return a prime number larger than or equal to n.
         */
        private static int nextPrime( int n )
        {
            if( n % 2 == 0 )
                n++;

            for( ; !isPrime( n ); n += 2 )
                ;

            return n;
        }

        /**
         * Internal method to test if a number is prime.
         * Not an efficient algorithm.
         * @param n the number to test.
         * @return the result of the test.
         */
        private static boolean isPrime( int n )
        {
            if( n == 2 || n == 3 )
                return true;

            if( n == 1 || n % 2 == 0 )
                return false;

            for( int i = 3; i * i <= n; i += 2 )
                if( n % i == 0 )
                    return false;

            return true;
        }


            // Simple main
        public static void main( String [ ] args )
        {
            SeparateChainingHashTable H = new SeparateChainingHashTable( );

            final int NUMS = 4000;
            final int GAP  =   37;

            System.out.println( "Checking... (no more output means success)" );

            for( int i = GAP; i != 0; i = ( i + GAP ) % NUMS )
                H.insert( new MyInteger( i ) );
            for( int i = 1; i < NUMS; i+= 2 )
                H.remove( new MyInteger( i ) );

            for( int i = 2; i < NUMS; i+=2 )
                if( ((MyInteger)(H.find( new MyInteger( i ) ))).intValue( ) != i )
                    System.out.println( "Find fails " + i );

            for( int i = 1; i < NUMS; i+=2 )
            {
                if( H.find( new MyInteger( i ) ) != null )
                    System.out.println( "OOPS!!! " +  i  );
            }
    }

}

