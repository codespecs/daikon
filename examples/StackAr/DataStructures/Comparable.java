    package DataStructures;

    /**
     * Protocol for Comparable objects.
     * In Java 1.2, you can remove this file.
     * @author Mark Allen Weiss
     */
    public interface Comparable
    {
        /**
         * Compare this object with rhs.
         * @param rhs the second Comparable.
         * @return 0 if two objects are equal;
         *     less than zero if this object is smaller;
         *     greater than zero if this object is larger.
         */
        int     compareTo( Comparable rhs );
    }
