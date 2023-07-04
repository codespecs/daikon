package daikon.inv.unary.string.dates;

import daikon.PptSlice;
import daikon.inv.Invariant;
import daikon.inv.InvariantStatus;
import daikon.inv.OutputFormat;
import daikon.inv.unary.string.SingleString;
import org.checkerframework.checker.lock.qual.GuardSatisfied;
import org.checkerframework.dataflow.qual.Pure;
import org.checkerframework.dataflow.qual.SideEffectFree;
import typequals.prototype.qual.Prototype;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Indicates that the value of a string variable is always a date following the format
 * DD/MM/YYYY (the separator can be “/” or “-“). Prints as {@code x is a Date. Format: DD/MM/YYYY}.
 */
public class IsDateDDMMYYYY extends SingleString {
    // We are Serializable, so we specify a version to allow changes to
    // method signatures without breaking serialization.  If you add or
    // remove fields, you should change this number to the current date.
    static final long serialVersionUID = 20230704L;

    // Variables starting with dkconfig_ should only be set via the
    // daikon.config.Configuration interface.
    /** Boolean. True iff Positive invariants should be considered. */
    public static boolean dkconfig_enabled = false;

    ///
    /// Required methods
    ///
    private IsDateDDMMYYYY(PptSlice ppt){ super(ppt); }

    private @Prototype
    IsDateDDMMYYYY() { super(); }

    private static @Prototype IsDateDDMMYYYY proto = new @Prototype IsDateDDMMYYYY();

    // Returns the prototype invariant
    public static @Prototype IsDateDDMMYYYY get_proto() { return proto; }

    @Override
    public boolean enabled() {
        return dkconfig_enabled;
    }

    @Override
    public IsDateDDMMYYYY instantiate_dyn(@Prototype IsDateDDMMYYYY this, PptSlice slice) {
        return new IsDateDDMMYYYY(slice);
    }

    // A printed representation for user output
    @SideEffectFree
    @Override
    public String format_using(@GuardSatisfied IsDateDDMMYYYY this, OutputFormat format) {
        return var().name() + " is a Date. Format: DD/MM/YYYY";
    }

    @Override
    public InvariantStatus check_modified(String v, int count) {
        /*
        *   The regex matches on a date with the DD/MM/YYYY format (Year min: 1900, Year max: 2050).
        *   For example:
        *       - 01/12/1900
        *       - 25.01.2019
        *       - 30-10-2050
        */
        // ^(?:0[1-9]|[12][0-9]|3[01])[-/.](?:0[1-9]|1[012])[-/.](?:19\d{2}|20[01234][0-9]|2050)$
        Pattern pattern = Pattern.compile("^(?:0[1-9]|[12][0-9]|3[01])[-/.](?:0[1-9]|1[012])[-/.](?:19\\d{2}|20[01234][0-9]|2050)$");

        Matcher matcher = pattern.matcher(v);

        if (matcher.matches()) {
            return InvariantStatus.NO_CHANGE;
        }
        return InvariantStatus.FALSIFIED;
    }

    @Override
    public InvariantStatus add_modified(String v, int count) { return check_modified(v, count); }

    @Override
    protected  double computeConfidence() {
        return 1 - Math.pow(.1, ppt.num_samples());
    }

    @Pure
    @Override
    public boolean isSameFormula(Invariant other) {
        assert other instanceof IsDateDDMMYYYY;
        return true;
    }

}
