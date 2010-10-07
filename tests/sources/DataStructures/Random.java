    package DataStructures;

    // Random class
    //
    // CONSTRUCTION: with (a) no initializer or (b) an integer
    //     that specifies the initial state of the generator
    //
    // ******************PUBLIC OPERATIONS*********************
    //     Return a random number according to some distribution:
    // int randomInt( )                     --> Uniform, 1 to 2^31-1
    // int random0_1( )                     --> Uniform, 0 to 1
    // int randomInt( int low, int high )   --> Uniform low..high
    // long randomLong( long low, long high ) --> Uniform low..high
    // void permute( Object [ ] a )         --> Randomly permutate

    /**
     * Random number class, using a 31-bit
     * linear congruential generator.
     * Note that java.util contains a class Random,
     * so watch out for name conflicts.
     * @author Mark Allen Weiss
     */
    public class Random
    {
        private static final int A = 48271;
        private static final int M = 2147483647;
        private static final int Q = M / A;
        private static final int R = M % A;

        /**
         * Construct this Random object with
         * initial state obtained from system clock.
         */
        public Random( )
        {
            this( (int) ( System.currentTimeMillis( ) % Integer.MAX_VALUE ) );
        }

        /**
         * Construct this Random object with
         * specified initial state.
         * @param initialValue the initial state.
         */
        public Random( int initialValue )
        {
            if( initialValue < 0 )
                initialValue += M;

            state = initialValue;
            if( state == 0 )
                state = 1;
        }

        /**
         * Return a pseudorandom int, and change the
         * internal state.
         * @return the pseudorandom int.
         */
        public int randomInt( )
        {
            int tmpState = A * ( state % Q ) - R * ( state / Q );
            if( tmpState >= 0 )
                state = tmpState;
            else
                state = tmpState + M;

            return state;
        }

        /**
         * Return a pseudorandom int, and change the
         * internal state. DOES NOT WORK.
         * @return the pseudorandom int.
         */
        public int randomIntWRONG( )
        {
            return state = ( A * state ) % M;
        }

        /**
         * Return a pseudorandom double in the open range 0..1
         * and change the internal state.
         * @return the pseudorandom double.
         */
        public double random0_1( )
        {
            return (double) randomInt( ) / M;
        }

        /**
         * Return an int in the closed range [low,high], and
         * change the internal state.
         * @param low the minimum value returned.
         * @param high the maximum value returned.
         * @return the pseudorandom int.
         */
        public int randomInt( int low, int high )
        {
            double partitionSize = (double) M / ( high - low + 1 );

            return (int) ( randomInt( ) / partitionSize ) + low;
        }

        /**
         * Return an long in the closed range [low,high], and
         * change the internal state.
         * @param low the minimum value returned.
         * @param high the maximum value returned.
         * @return the pseudorandom long.
         */
        public long randomLong( long low, long high )
        {
            long longVal =  ( (long) randomInt( ) << 31 ) + randomInt( );
            long longM =  ( (long) M << 31 ) + M;

            double partitionSize = (double) longM / ( high - low + 1 );
            return (long) ( longVal / partitionSize ) + low;
        }
        /**
         * Randomly rearrange an array.
         * The random numbers used depend on the time and day.
         * @param a the array.
         */
        public static final void permute( Object [ ] a )
        {
            Random r = new Random( );

            for( int j = 1; j < a.length; j++ )
                Sort.swapReferences( a, j, r.randomInt( 0, j ) );
        }

        private int state;

            // Test program
        public static void main( String [ ] args )
        {
            Random r = new Random( 1 );

            for( int i = 0; i < 20; i++ )
                System.out.println( r.randomInt( ) );
        }
    }
