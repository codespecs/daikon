    package DataStructures;

    /**
     * Wrapper class for use with generic data structures.
     * Mimics Integer.
     * In Java 1.2, you can use Integer if Comparable is needed.
     * @author Mark Allen Weiss
     */
    public final class MyInteger implements Comparable, Hashable
    {
        /**
         * Construct the MyInteger object with initial value 0.
         */
        public MyInteger( )
        {
            this( 0 );
        }

        /**
         * Construct the MyInteger object.
         * @param x the initial value.
         */
        public MyInteger( int x )
        {
            value = x;
        }

        /**
         * Gets the stored int value.
         * @return the stored value.
         */
        public int intValue( )
        {
            return value;
        }

        /**
         * Implements the toString method.
         * @return the String representation.
         */
        public String toString( )
        {
            return Integer.toString( value );
        }

        /**
         * Implements the compareTo method.
         * @param rhs the other MyInteger object.
         * @return 0 if two objects are equal;
         *     less than zero if this object is smaller;
         *     greater than zero if this object is larger.
         * @exception ClassCastException if rhs is not
         *     a MyInteger.
         */
        public int compareTo( Comparable rhs )
        {
            return value < ((MyInteger)rhs).value ? -1 :
                   value == ((MyInteger)rhs).value ? 0 : 1;
        }

        /**
         * Implements the equals method.
         * @param rhs the second MyInteger.
         * @return true if the objects are equal, false otherwise.
         * @exception ClassCastException if rhs is not
         *     a MyInteger.
         */
        public boolean equals( Object rhs )
        {
            return rhs != null && value == ((MyInteger)rhs).value;
        }

        /**
         * Implements the hash method.
         * @param tableSize the hash table size.
         * @return a number between 0 and tableSize-1.
         */
        public int hash( int tableSize )
        {
            if( value < 0 )
                return -value % tableSize;
            else
                return value % tableSize;
        }

        private int value;
    }
