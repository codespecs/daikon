// RandomSelector.java
package utilMDE;
import java.util.*;


/**
 *
 * <b>RandomSelector</b> selects k elements from
 * an Iteration over n elements using only O(k) space instead of O(n)
 * space.  For example, selecting 1 element from a FileStream containing
 * 1000 elements will take O(1) space.
 *
 * <p>A second mode allows for a fixed probability of randomly keeping
 *  each item as opposed to a fix number of samples.
 *
 * <P>current_values --> The values chosen based on the Objects observed
 * <BR>number_observed --> The number of Objects observed
 * <BR>number_to_take --> The number of elements to choose ('k' above)
 * <BR>keep_probability --> The percentage of elements to keep
 * <BR>selector_mode --> either fixed amount of samples or fixed percent.
 */

public class RandomSelector {

    // Rep Invariant: values != null && values.size() <= num_elts &&
    //                ((num_elts == -1 && coin_toss_mode == true) ||
    //                 (keep_probability == -1.0 && coin_toss_mode == false))

    // Abstraction Function:
    // 1. for all elements, 'val' of AF(current_values),
    //    this.values.indexOf (val) != -1
    // 2. AF(number_observed) = this.observed
    // 3. AF(number_to_take) = this.num_elts
    // 4. AF(keep_probability) = this.keep_probability
    // 5. AF(selector_mode) = fixed amount if coin_toss_mode == true
    //                        fixed percentage if coin_toss_mode == false

    private int num_elts = -1;
    private int observed;
    private Random seed;
    public ArrayList values;
    private boolean coin_toss_mode = false;
    private double keep_probability = -1.0;

    /** @param num_elts The number of elements intended to be selected
     * from the oncoming Iteration. Same as 'number_to_take'
     * @param r The seed to give for random number generation.
     **/
    public RandomSelector (int num_elts, Random r) {
        values = new ArrayList();
        this.num_elts = num_elts;
        observed = 0;
        seed = r;
    }

    /** @param num_elts The number of elements intended to be selected
     * from the oncoming Iteration. Same as 'number_to_take'
     * @param r The seed to give for random number generation.
     **/
    public RandomSelector (double keep_probability, Random r) {
        values = new ArrayList();
        this.keep_probability = keep_probability;
        coin_toss_mode = true;
        observed = 0;
        seed = r;
    }

    /** @param num_elts The number of elements intended to be selected
     * from the oncoming Iteration
     **/
    public RandomSelector (int num_elts) {
        this (num_elts, new Random());
    }

    /** Increments the number of observed_elements, then
     * with probability 1 / observed_elements, the Object 'next' will
     * be added to current_values. If the size of current_values exceeds
     * number_to_take, then one of the existing current_values will
     * be removed at random.
     **/
    public void accept (Object next) {

        // if we are in coin toss mode, then we want to keep
        // with probability == keep_probability.
        if (coin_toss_mode) {
            if (seed.nextDouble() < keep_probability) {
                values.add (next);
                //                System.out.println ("ACCEPTED " + keep_probability );
            }
            else {
                // System.out.println ("didn't accept " + keep_probability );
            }
            return;
        }

        // in fixed sample mode, the i-th element has a 1/i chance
        // of being accepted.
        if (seed.nextDouble() < ((double) num_elts / (++observed))) {
            if (values.size() < num_elts) {
                values.add (next);
            } else {
                int rem = (int) (values.size() * seed.nextDouble());
                values.set (rem, next);
            }
        }
        // do nothing if the probability condition is not met
    }

    /** Returns current_values, modifies none  **/
    public ArrayList getValues() {
        // avoid concurrent mod errors
        ArrayList ret = new ArrayList();
        ret.addAll (values);
        return ret;
    }

}
