#!/uns/bin/python1.5
# daikon.py -- detect patterns in collections of data
# Michael Ernst <mernst@cs.washington.edu>

# For some additional documentation, see daikon.py.doc.

# To run optimized, run Python with -O flag, and optionally do:
#   daikon.no_ternary_invariants = 1
#   daikon.no_invocation_counts = 1


###########################################################################
### Packages, constants
###

## Built-in Python modules
import glob, gzip, math, operator, os, pickle, posix, re, resource, string, sys, time, types, whrandom
# Do NOT use cPickle.  It is buggy!  (Or so it seems, as of 1.5.1.)
# As of 1.5.1, gzip's readline() result omits trailing "\n"; fixed in 1.5.2.

## Third-party Python modules
# In lieu of something built-in like gzip.py for "compress" files.
import TextFile

## User-defined Python modules
import util


true = (1==1)
false = (1==0)


###########################################################################
### Variables
###

# Annoyingly, these variables get wiped out when I reload this file unless
# I protect them.
# But if they're indented, maybe my future tools for variable decls won't work.
if not locals().has_key("fn_var_infos"):
    ### User configuration variables
    # Set these to true for speed, to false for completeness.
    no_ternary_invariants = true
    no_invocation_counts = true
    collect_stats = true                # performance statistics
    lackwit_type_format = "explicit"    # types list the comparable values
    # lackwit_type_format = "implicit"  # types name a symbol
    # lackwit_type_format = "none"      # no lackwit types provided

    ### Debugging
    debug_read = false                  # reading files
    debug_derive = false                # deriving new values
    debug_infer = false                 # inferring invariants

    # A negative invariant is not reported unless the chance that the invariant
    # only happens not to be true (and is not a true invariant) is at least this low.
    negative_invariant_confidence = .01     # .05 might also be reasonable

    # If true, list all the variables when printing a program point's invariants.
    display_all_vars = false

    ### Internal variables

    ## Program point variables
    # "fn" should really be "ppt" or some such.
    fn_var_infos = {}           # from program point name to list of var_infos
    fn_truevars = {}                    # from program point name to number of original var_infos
    fn_var_values = {}	    # from program point name to (tuple of values to occurrence counts)
    fn_samples = {}         # from program point name to number of samples
    ## What is the purpose of having this as a separate variable??  (It permits
    ## find_violations to report a file name and perhaps has no other purpose.)
    ## (It could also be used to regenerate fn_var_values after that has been
    ## modified by prune_database.)
    # Issue: this does not contain any derived variables, only original ones.
    file_fn_var_values = {} # from filename to (program point to (values-tuple to occurrence counts))
    # From function name to stats.  Used *only* if collect_stats = true
    fn_to_stats = {}

    fn_derived_from = {}    # functions that have had derived vars introduced

    ## Function variables
    # This is used to add invocation count variables for each program function.
    # Note the function name does not include the suffix ':::EXIT'
    functions = []
    fn_invocations = {}       # from function name to invocation count
    # From function name to stack of parameter values.
    fn_to_orig_param_vals = {}

    ## Diffing variables
    diff_to_ct = {}
    unary_diff_to_ct = {}
    bin_diff_to_ct = {}



##########################################
### jake's vars to keep track of numbers of differing invariants

inv_one_cons = 0
inv_diff_small_no_vals = 1
inv_one_none = 2
ssc_miss_min = 3
ssc_min_diff = 4
ssc_miss_max = 5
ssc_max_diff = 6
ssc_one_can_be_zero = 7
ssc_diff_mod = 8
ssc_diff_nonmod = 9
tsc_diff_lin_reln = 10
tsc_one_equal = 11
tsc_comparison_diff = 12
tsc_diff_num_diff = 13
tsc_diff_sum = 14
tsc_diff_ftn_reln = 15
tsc_diff_inv_ftn_reln = 16
sseq_diff_elem_equality = 17
sseq_diff_sortedness = 18
sseq_diff_inv_all_elem = 19
scseq_diff_membership = 20
two_seq_diff_lin_reln = 21
two_seq_one_equ = 22
two_seq_diff_comp = 23
two_seq_diff_subseq = 24
two_seq_diff_supseq = 25
two_seq_diff_revness = 26
g_unary_same = 27
g_unary_different = 28
g_pair_same = 29
g_pair_different = 30

def init_diff_globals():
    global diff_to_ct, unary_diff_to_ct, bin_diff_to_ct
    for ind in range(0, g_pair_different+1):
        diff_to_ct[ind] = 0
        unary_diff_to_ct[ind] = 0
        bin_diff_to_ct[ind] = 0

def clear_diff_to_ct():
    global diff_to_ct
    for ind in range(0, g_pair_different+1):
        diff_to_ct[ind] = 0

def add_to_unary():
    global diff_to_ct, unary_diff_to_ct, bin_diff_to_ct
    for ind in range(0, g_pair_different+1):
        unary_diff_to_ct[ind] = unary_diff_to_ct[ind] + diff_to_ct[ind]

def add_to_binary():
    global diff_to_ct, unary_diff_to_ct, bin_diff_to_ct
    for ind in range(0, g_pair_different+1):
        bin_diff_to_ct[ind] = bin_diff_to_ct[ind] + diff_to_ct[ind]

def print_inv_diff_tracking():
    print "UNARY DIFFS"
    print "General inv diffs:"
    print "  one constrained, one is not - ", unary_diff_to_ct[inv_one_cons]
    print "  different small num values - ", unary_diff_to_ct[inv_diff_small_no_vals]
    print "  one can be none, one can't - ", unary_diff_to_ct[inv_one_none]
    print "Single scalar diffs:"
    print "  missing minimum - ", unary_diff_to_ct[ssc_miss_min]
    print "  different min - ", unary_diff_to_ct[ssc_min_diff]
    print "  missing max - ", unary_diff_to_ct[ssc_miss_max]
    print "  different max - ", unary_diff_to_ct[ssc_max_diff]
    print "  one can be zero, one can't - ", unary_diff_to_ct[ssc_one_can_be_zero]
    print "  different modulus - ", unary_diff_to_ct[ssc_diff_mod]
    print "  different nonmodulus - ", unary_diff_to_ct[ssc_diff_nonmod]
    print "Two scalar diffs:"
    print "  different linear relation - ", unary_diff_to_ct[tsc_diff_lin_reln]
    print "  one is equal, other is not - ", unary_diff_to_ct[tsc_one_equal]
    print "  different comparison - ", unary_diff_to_ct[tsc_comparison_diff]
    print "  different numeric difference - ", unary_diff_to_ct[tsc_diff_num_diff]
    print "  different sum - ", unary_diff_to_ct[tsc_diff_sum]
    print "  different functional relation - ", unary_diff_to_ct[tsc_diff_ftn_reln]
    print "  different inv functional reln - ", unary_diff_to_ct[tsc_diff_inv_ftn_reln]
    print "Single sequence:"
    print "  different element equality - ", unary_diff_to_ct[sseq_diff_elem_equality]
    print "  different sortedness - ", unary_diff_to_ct[sseq_diff_sortedness]
    print "  different inv over all elem - ", unary_diff_to_ct[sseq_diff_inv_all_elem]
    print "Scalar sequence diffs:"
    print "  different membership - ", unary_diff_to_ct[scseq_diff_membership]
    print "Two sequence diffs:"
    print "  different linear relations - ", unary_diff_to_ct[two_seq_diff_lin_reln]
    print "  one equal, one is not - ", unary_diff_to_ct[two_seq_one_equ]
    print "  different comparision - ", unary_diff_to_ct[two_seq_diff_comp]
    print "  different subseq - ", unary_diff_to_ct[two_seq_diff_subseq]
    print "  different superseq - ", unary_diff_to_ct[two_seq_diff_supseq]
    print "  different reverseness - ", unary_diff_to_ct[two_seq_diff_revness]
    print
    print "BINARY DIFFS"
    print "General inv diffs:"
    print "  one constrained, one is not - ", bin_diff_to_ct[inv_one_cons]
    print "  different small num values - ", bin_diff_to_ct[inv_diff_small_no_vals]
    print "  one can be none, one can't - ", bin_diff_to_ct[inv_one_none]
    print "Single scalar diffs:"
    print "  missing minimum - ", bin_diff_to_ct[ssc_miss_min]
    print "  different min - ", bin_diff_to_ct[ssc_min_diff]
    print "  missing max - ", bin_diff_to_ct[ssc_miss_max]
    print "  different max - ", bin_diff_to_ct[ssc_max_diff]
    print "  one can be zero, one can't - ", bin_diff_to_ct[ssc_one_can_be_zero]
    print "  different modulus - ", bin_diff_to_ct[ssc_diff_mod]
    print "  different nonmodulus - ", bin_diff_to_ct[ssc_diff_nonmod]
    print "Two scalar diffs:"
    print "  different linear relation - ", bin_diff_to_ct[tsc_diff_lin_reln]
    print "  one is equal, other is not - ", bin_diff_to_ct[tsc_one_equal]
    print "  different comparison - ", bin_diff_to_ct[tsc_comparison_diff]
    print "  different numeric difference - ", bin_diff_to_ct[tsc_diff_num_diff]
    print "  different sum - ", bin_diff_to_ct[tsc_diff_sum]
    print "  different functional relation - ", bin_diff_to_ct[tsc_diff_ftn_reln]
    print "  different inv functional reln - ", bin_diff_to_ct[tsc_diff_inv_ftn_reln]
    print "Single sequence:"
    print "  different element equality - ", bin_diff_to_ct[sseq_diff_elem_equality]
    print "  different sortedness - ", bin_diff_to_ct[sseq_diff_sortedness]
    print "  different inv over all elem - ", bin_diff_to_ct[sseq_diff_inv_all_elem]
    print "Scalar sequence diffs:"
    print "  different membership - ", bin_diff_to_ct[scseq_diff_membership]
    print "Two sequence diffs:"
    print "  different linear relations - ", bin_diff_to_ct[two_seq_diff_lin_reln]
    print "  one equal, one is not - ", bin_diff_to_ct[two_seq_one_equ]
    print "  different comparision - ", bin_diff_to_ct[two_seq_diff_comp]
    print "  different subseq - ", bin_diff_to_ct[two_seq_diff_subseq]
    print "  different superseq - ", bin_diff_to_ct[two_seq_diff_supseq]
    print "  different reverseness - ", bin_diff_to_ct[two_seq_diff_revness]
    print
    print "SUMMARY"
    print "Identical unary invariants:", unary_diff_to_ct[g_unary_same]
    print "Differing unary invariants:", unary_diff_to_ct[g_unary_different]
    print "Identical binary invariants:", unary_diff_to_ct[g_pair_same]
    print "Differing binary invariants:", unary_diff_to_ct[g_pair_different]
######end Jakes inv diff tracking

def clear_variables():
    """Reset the values of some global variables."""

    global functions

    # Declaration-related variables
    fn_var_infos.clear()
    fn_truevars.clear()
    functions = []
    # Data-related variables
    clear_trace_variables()

def clear_trace_variables():
    """Reset the values of trace-related global variables."""
    fn_var_values.clear()
    fn_samples.clear()
    file_fn_var_values.clear()
    fn_invocations.clear()
    fn_to_orig_param_vals.clear()
    fn_to_stats.clear()
    fn_derived_from.clear()

def clear_invariants(fn_regexp=None):
    """Reset the values of invariants, globally."""
    for fn_name in fn_var_infos.keys():
        if fn_regexp and not fn_regexp.search(fn_name):
            continue
        for vi in fn_var_infos[fn_name]:
            vi.invariant = None
            vi.invariants = {}


def file_fn_var_values_invalid_modinkey(ffvv):
    return file_fn_var_values_invalid(ffvv)

def file_fn_var_values_invalid(ffvv):
    for file in ffvv.keys():
        subresult = fn_var_values_invalid_modinkey(ffvv[file])
        if subresult:
            return "%s in file %s" % (subresult, file)
    return false

def fn_var_values_invalid_modinkey(fvv):
    return fn_var_values_invalid(fvv, 0)

def fn_var_values_invalid_modincount(fvv):
    return fn_var_values_invalid(fvv, 1)

def fn_var_values_invalid(fvv, modincount):
    """MODINCOUNT is true if the value should be a length-2 tuple
    and the key should be a list of integers.
    MODINCOUNT is false if the value should be an integer
    and the key should be a list of length-2 tuples."""
    for ppt in fvv.keys():
        subresult = var_values_invalid(fvv[ppt], modincount)
        if subresult:
            return "%s at %s" % (subresult, ppt)
    return false

def var_values_invalid_modinkey(fvv):
    return var_values_invalid(fvv, 0)

def var_values_invalid_modincount(fvv):
    return var_values_invalid(fvv, 1)

def var_values_invalid(vv, modincount):
    for (values_tuple, counts) in vv.items():
        if modincount:
            ## This is a tricky test; skip it for now
            # for value_elt in values_tuple:
            #     if type(value_elt) != types.TupleType:
            #         return "value_elt %s should have len 2 in %s" % (value_elt, values_tuple)
            if (type(counts) != types.TupleType) or (len(counts) != 2):
                return "counts %s should have len 2 in %s" % (counts, values_tuple)
        else:
            # modinkey
            for value_elt in values_tuple:
                if (type(value_elt) != types.TupleType) or (len(value_elt) != 2):
                    return "value_elt %s should have len 2 in %s" % (value_elt, values_tuple)
            if type(counts) != types.IntType:
                return "counts %s should be an integer in %s" % (counts, values_tuple)


integer_re = re.compile(r'^-?[0-9]+$')
float_re = re.compile(r'^-?[0-9]*\.[0-9]+$|^-?[0-9]+\.[0-9]*$')
# variable name for a sequence; ends with "[]" or "[XXX..YYY]"
# This should only be used once per variable; after that, read out the
# type from the var_info object
sequence_re = re.compile(r'^[^	]+\[(|[^][]+\.\.[^][]+)\]$')
min_or_max_re = re.compile("^(min|max)\((.*)\)$")

# These differ: "+" vs. "*" for the parenthesis group.
java_type_re = re.compile(r'^(?:[a-zA-Z][a-zA-Z0-9]*(?:\.[a-zA-Z][a-zA-Z0-9]*)+'
                          + r'|[a-zA-Z][a-z0-9]*[A-Z][a-zA-Z0-9]*)$');
java_object_re = re.compile((r'\[*(?:' + r'^[a-zA-Z][a-zA-Z0-9_]*(?:\.[a-zA-Z][a-zA-Z0-9_]*)*'
                             + r'|' + r'[BCDFIJSZ]' + r')')
                            + r'@([0-9a-fA-F]+)$');

integral_types = ("int", "char", "float", "double", "integral", "boolean")
known_types = integral_types + ("pointer", "address")


class proglang_type:
    # base
    # dimensionality

    # With one argument, parse first argument.  Otherwise assume it's done.
    def __init__(self, base, dimensionality=None):
        assert type(base) == types.StringType
        if dimensionality == None:
            dimensionality = 0
            while base[-2:] == "[]":
                dimensionality = dimensionality+1
                base = base[:-2]
            while base[:9] == "array of ":
                dimensionality = dimensionality+1
                base = base[9:]
            if not (base in known_types):
                # hack for Java.  (I want to avoid the short names if possible.)
                # This isn't quite right because "String[]" gets converted
                # into "char[][]" which gets converted into Object[].  Yuck.
                # But it is working around some other shortcoming, I think.
                if (base == "java.lang.String") or (base == "String"):
                    base = "char"
                    dimensionality = dimensionality+1
                elif (base == "java.lang.Vector") or (base == "Vector"):
                    base = "java_object"
                    dimensionality = dimensionality+1
                else:
                    jtmatch = java_type_re.match(base)
                    if (jtmatch != None) or (base == "Object"):
                        base = "java_object"
                    # Terrible hack, just use java_object anyway!
                    else:
                        base = "java_object"
        else:
            assert type(dimensionality) == types.IntType

        # Deal with multidimensional arrays in a slightly hackish way.
        if dimensionality > 1:
            base = "java_object"
            dimensionality = 1

        self.base = base
        self.dimensionality = dimensionality

    def __repr__(self):
        return "<proglang_type %s %d>" % (self.base, self.dimensionality)

    def __cmp__(self, other):
        if (self.base == other.base):
            return self.dimensionality - other.dimensionality
        elif (self.base < other.base):
            return -1
        else:
            return 1

    def compatible(self, other):
        base1 = self.base
        base2 = other.base
        return ((self.dimensionality == other.dimensionality)
                and ((base1 == base2)
                     or ((base1 == "integral") and (base2 in integral_types))
                     or ((base2 == "integral") and (base1 in integral_types))))

    # Watch out for simple functions like this:  they hurt performance
    def is_array(self):
        return self.dimensionality > 0

    def element_type(self):
        assert self.is_array()
        return proglang_type(self.base, self.dimensionality-1)

int_proglang_type = proglang_type("int")

## Old implementation
# def valid_var_type(var_type):
#     return ((type(var_type) == types.StringType)
#             or ((type(var_type) == types.TupleType)
#                 and (len(var_type) == 2)
#                 and (type(var_type[0]) == types.StringType)
#                 and (type(var_type[1]) == types.IntType)))


ws_regexp = re.compile(r'[ \t]+')

# Lackwit types have two formats.
#  * An implicit lackwit type is an arbitrary string, and
#    comparisons succeed exactly if the two lackwit types are identical.
#  * An explicit lackwit type is a list of other variables, and comparisons
#    succeed if each variable is in the list of the other.

# A (explicit) Lackwit type is:
#  * for scalar types:  a tuple of comparable variables
#  * for array types:  a tuple of 'array', a list of variables comparable to
#    the element type, and some number of lists variables comparable to the
#    index types
#  * special:  a tuple of "alias", a name, and a Lackwit type.
#    This means to treat this variable as if it has the specified name.
#  * special:  the string "always" means always comparable

def is_lackwit_type(lt):
    if lackwit_type_format == "none":
        return true
    elif lackwit_type_format == "implicit":
        return type(lt) == types.StringType
    else:
        return ((lt == "always")
                or ((type(lt) == types.TupleType)
                    and (((lt[0] == "array")
                          and (len(lt) >= 3))
                         or ((lt[0] == "alias")
                             and (len(lt) == 3)
                             and (type(lt[1]) == types.StringType)
                             and is_lackwit_type(lt[2]))
                         or true)))


def lackwit_type_alias_name(lt):
    if (lt == "always"):
        return None
    elif (lt[0] == "alias"):
        return lackwit_type_alias_name(lt[2]) or lt[1]
    else:
        return None

def lackwit_make_alias(name, type):
    if lackwit_type_format == "none":
        return type                     # was "return None", but I think that's wrong
    elif lackwit_type_format == "implicit":
        return type
    else:
        return ("alias", name, type)


def lackwit_type_element_type(lt):
    head = lt[0]
    if (head == 'array'):
        return lt[1]
    elif (head == 'alias'):
        return ("alias", "%s-element" % lt[1], lackwit_type_element_type(lt[2]))
    else:
        raise "Not an array Lackwit type: " + `lt`

def lackwit_type_element_type_alias(vi):
    assert isinstance(vi, var_info)
    if lackwit_type_format == "none":
        return None
    elif lackwit_type_format == "implicit":
        return "always"
    else:
        lt = vi.lackwit_type
        seq_var_name = lackwit_type_alias_name(lt) or vi.name
        return ("alias", "%s-element" % seq_var_name,
                lackwit_type_element_type(lt))

# First index is numbered "1".
def lackwit_type_index_type(lt, dim):
    if lackwit_type_format == "none":
        return None
    head = lt[0]
    if (head == 'array'):
        return lt[dim+1]                # indices start at third elt (index 2)
    elif (head == 'alias'):
        return ("alias", "%s-index%d" % (lt[1], dim), lackwit_type_index_type(lt[2], dim))
    else:
        raise "Not an array Lackwit type: " + `lt`

def lackwit_type_index_type_alias(vi, dim):
    assert isinstance(vi, var_info)
    if lackwit_type_format == "none":
        return None
    if lackwit_type_format == "implicit":
        return "always"
    else:
        lt = vi.lackwit_type
        seq_var_name = lackwit_type_alias_name(lt) or vi.name
        return ("alias", "%s-index%d" % (seq_var_name, dim),
                lackwit_type_index_type(lt, dim))


def parse_lackwit_vartype(raw_str, vartype):
    if lackwit_type_format == "none":
        return None
    if lackwit_type_format == "implicit":
        return raw_str
    elif vartype.is_array():
        # The lackwit type is of the form
        #  (var1 var2 var3)[var1 var2 var3][var1 var2 var3]
        dims = vartype.dimensionality
        assert dims > 0
        # Permit "[]" to appear in variable names.
        match = re.compile("^\(([^)]*)\)" + ("\[\(?((?:[^\]\)]|\[\])*)\)?\]" * dims) + "$").match(raw_str)
        assert match != None
        assert len(match.groups()) == dims+1
        result = ("array",) + tuple(map(lambda substr: tuple(ws_regexp.split(substr)),
                                       match.groups()))
        return result
    else:
        # scalar variable
        if raw_str[0] == '(':
            raw_str = raw_str[1:]
            assert raw_str[-1] == ')'
            raw_str = raw_str[:-1]
        return tuple(ws_regexp.split(raw_str))

def array_derived_vars(array, indices):
    return ("%s-element" % array,) + tuple(map(lambda i,a=array: "%s-index%d" % (a,i), range(1,indices+1)))

def lackwit_types_compatible(name1, type1, name2, type2):
    # print "lackwit_types_compatible", name1, type1, name2, type2
    assert type(name1) == types.StringType
    assert type(name2) == types.StringType
    if lackwit_type_format == "none":
        return true
    if (type1 == "always") or (type2 == "always"):
        return true
    if lackwit_type_format == "implicit":
        return (type1 == type2)

    assert type(type1) == types.TupleType
    assert type(type2) == types.TupleType
    while type1[0] == "alias":
        name1 = type1[1]
        type1 = type1[2]
        assert type(name1) == types.StringType
        assert type(type1) == types.TupleType
    while type2[0] == "alias":
        name2 = type2[1]
        type2 = type2[2]
        assert type(name2) == types.StringType
        assert type(type2) == types.TupleType

    if (type1[0] == 'array') and (type2[0] == 'array'):
        # This isn't right.  I really want to loop over all the elements,
        # generating the appropriate variables and ensuring that the
        # subtypes are compatible.
        assert len(type1) == len(type2)

        dims = len(type1) - 2
    	subnames1 = array_derived_vars(name1, dims)
        subnames2 = array_derived_vars(name2, dims)
        # I really want to use "all" or some such here.  Is that not built in?
        results = map(lackwit_types_compatible, subnames1, type1[1:], subnames2, type2[1:])
        return not (0 in results)


    # Scalar type
    in12 = name1 in type2
    in21 = name2 in type1
    # print "lackwit_types_compatible = ", in12, "for:", name1, type1, name2, type2
    if (in12 != in21):
        print "variable comparability is non-symmetric:", name1, type1, name2, type2
    # now throw an error
    assert in12 == in21
    return in12


class var_info:

    # Instance variables:
    #  name                # string
    #  type                # type information:  a string or a tuple of
                                  # (string, dims), not an
                                  #   unpicklable object like types.IntType
    #  lackwit_type	   # lackwit type information, or var_info (meaning
                           #   to treat this variable like that one)
    #  index               # index in lists of variables

    # Info about derived variables: derivees from this and derivers of this.
    #  derived             # will be variables derived from this
                                  #   one; presently not used.
    #  derived_len         # index of len variable
    #  is_derived          # boolean (for now)

    # To find the invariant over a pair of variables, do a double-dispatch:
    # first look up the "invariants" field of one of the variables, then
    # look up the other variable in that map.
    #  invariant
    #  invariants	# map from indices to multiple-arity invariants.
			# The entry for a variable can sometimes be
                        # missing, which I think is equivalent to it being
                        # a trivial invariant satisfying no properties; but
                        # I'm not sure exactly when each is the case.
    #  equal_to         # list of indices of equal variables;
                        #   could be derived from invariants
                        #   by checking for inv.comparison == "=".
                        #   The variable itself is not on this list.


    def __init__(self, name, var_type, lackwit_type, index, is_derived=false):
        assert type(name) == types.StringType
        assert type(index) == types.IntType
        assert isinstance(var_type, proglang_type)
        assert is_lackwit_type(lackwit_type)

        self.name = name                # string
        self.type = var_type            # type information:  a string, not an
                                        #   unpicklable object like types.IntType
        self.lackwit_type = lackwit_type
        self.index = index              # index in lists of variables

        # info about derived variables: derivees from this and derivers of this
        self.derived = {}               # will be variables derived from this
                                        #   one; presently not used.
        self.derived_len = None         # index of len variable
        self.is_derived = is_derived    # boolean (for now)

        # To find the invariant over a pair of variables, do a double-dispatch:
        # first look up the "invariants" field of one of the variables, then
        # look up the other variable in that map.
        self.invariant = None
        self.invariants = {}	# map from indices to multiple-arity invariants
        self.equal_to = []        # list of indices of equal variables;
                                        #   could be derived from invariants
                                        #   by checking for inv.comparison == "="
                                        #   the variable itself is not on this list

    def __setstate__(self, state):
        for key in state.keys():
            setattr(self, key, state[key])

    def __repr__(self):
        return "<var %s %s>" % (self.name, self.type)

    def is_sequence(self):
        return self.type.is_array()

    def canonical_var(self):
        """Return index of the canonical variable that is always equal to this one.
        Return None if no such variable exists."""

        assert util.sorted(self.equal_to)
        if self.equal_to == []:
            return self.index
        else:
            return min(self.index, self.equal_to[0])

    def is_canonical(self):
        assert self.index != None
        assert self.equal_to == [] or self.canonical_var() != None
        return self.index == self.canonical_var()

def var_info_name_compare(vi1, vi2):
    return cmp(vi1.name, vi2.name)

def var_infos_compatible(vis1, vis2):
    """The arguments are lists of var_info objects."""
    # This just checks that the names are the same.
    return (map(lambda vi: vi.name, vis1) == map(lambda vi: vi.name, vis2))


def merge_var_infos(filename, sub_fn_var_infos):
    """Merge the values for the arguments into the corresponding global variables.
    See `read_data_trace_file' for a description of the argument types;
    argument 2 is a dictionary mapping from a function name to information
    about the function.
    """

    if debug_read:
        print "merge_var_infos", filename, sub_fn_var_infos.keys()

    for fname in sub_fn_var_infos.keys():
	if not(fn_var_values.has_key(fname)):
            var_infos = []
            sub_var_infos = sub_fn_var_infos[fname]
            for vi in sub_var_infos:
                var_infos.append(var_info(vi.name, vi.type, vi.lackwit_type, len(var_infos)))
	    fn_var_infos[fname] = var_infos
            fn_var_values[fname] = {}
	else:
            assert var_infos_compatible(fn_var_infos[fname], sub_fn_var_infos[fname])


def merge_var_values(filename, sub_fn_var_values, sub_fn_samples):
    """Merge the values for the arguments into the corresponding global variables.
    See `read_data_trace_file' for a description of the argument types;
    arguments 2-3 are dictionaries mapping from a function name to information
    about the function.
    """

    if debug_read:
        print "merge_var_values", filename, sub_fn_var_values.keys()

    assert not fn_var_values_invalid_modinkey(sub_fn_var_values)

    for fname in sub_fn_var_values.keys():
        sub_var_values = sub_fn_var_values[fname]
        var_infos = fn_var_infos[fname]
        if not fn_var_values.has_key(fname):
            fn_var_values[fname] = {}
        var_values = fn_var_values[fname]
        sub_var_values = sub_fn_var_values[fname]
        for (values, count) in sub_var_values.items():
            var_values[values] = var_values.get(values, 0) + count
        fn_samples[fname] = fn_samples.get(fname, 0) + sub_fn_samples[fname]
    file_fn_var_values[filename] = sub_fn_var_values


###
### Dictionary utilities
###

## These functions specify their inputs and outputs as "modinkey" or "modincount".

# def dict_of_tuples_modinkey_to_tuple_of_dicts_modinval(dot, tuple_len=None):
#     """Input: a dictionary mapping a tuple of elements to a count.
#     All the key tuples in the input have the same length unless optional argument
#     TUPLE_LEN is provided, in which case all tuples have at least that length.
#     If TUPLE_LEN is a tuple, then only those indices are extracted.
#     if TUPLE_LEN is an integer, indices up to it (non-inclusive) are extracted.
#     Output: a tuple of dictionaries, each mapping a single element to a count.
#     The first output dictionary concerns the first element of the original keys,
#     the second output the second element of the original keys, and so forth."""
# 
#     assert not var_values_invalid_modinkey(dot)
# 
#     if tuple_len == None:
#         tuple_len = len(dot.keys()[0])
#     if type(tuple_len) == types.IntType:
#         assert tuple_len <= len(dot.keys()[0])
#         if tuple_len == 0:
#             return ()
#         tuple_indices = range(0, tuple_len)
#     elif tuple_len == []:
#         return ()
#     else:
#         assert type(tuple_len) in [types.TupleType, types.ListType]
#         assert max(tuple_len) < len(dot.keys()[0])
#         assert min(tuple_len) >= 0
#         tuple_indices = tuple_len
#     # Next four lines accomplish "result = ({},) * tuple_len", but with
#     # distinct rather than identical dictionaries in the tuple.
#     result = []
#     for i in tuple_indices:
#         result.append({})
#     result = tuple(result)
#     for (key_tuple, count) in dot.items():
#         for i in range(0, len(tuple_indices)):
#             (this_key, this_modified) = key_tuple[tuple_indices[i]]
#             this_dict = result[i]
#             # Is this more efficient than the following?
#             #    this_dict_elt = this_dict.get(this_key, [0,0])
#             #    this_dict[this_key] = this_dict_elt
#             if this_dict.has_key(this_key):
#                 this_dict_elt = this_dict[this_key]
#             else:
#                 this_dict_elt = [0,0]
#                 this_dict[this_key] = this_dict_elt
#             this_dict_elt[0] = this_dict_elt[0] + count
#             if this_modified:
#                 this_dict_elt[1] = this_dict_elt[1] + count
#     return result
# # dict_of_tuples_modinkey_to_tuple_of_dicts(fn_var_values["PUSH-ACTION"])
# # dict_of_tuples_modinkey_to_tuple_of_dicts(daikon.fn_var_values['P180-15.1.1:::EXIT'])

## These appear not to be used, so comment them out in favor of versions
## later in the file that actually are used.
# def dict_of_sequences_to_element_dict(dot):
#     """Input: a dictionary mapping instances of a sequence (tuples) to a count.
#     Output: a dictionary, mapping elements of all of the sequence instances
#     to a count."""
#     result = {}
#     for (key_tuple, (count, modified)) in dot.items():
#         for this_key in key_tuple:
#             this_counts = result.get(this_key, [0,0])
#             result[this_key] = this_counts
#             this_counts[0] = this_counts[0] + count
#             this_counts[1] = this_counts[1] + modified
#     return result
# # dict_of_sequences_to_element_dict(dot)
# 
# 
# def dict_of_tuples_slice(dot, indices):
#     """Input: a dictionary mapping a tuple of elements to a count, and a
#     list of indices.
#     Output: a dictionary mapping a subset of the original elements to a count.
#     The subset is chosen according to the input indices.
# 
#     If the indices have length 2 or 3, you are better off using the
#     specialized functions dict_of_tuples_slice_2 and dict_of_tuples_slice_3;
#     this function can be very slow.
#     """
# 
#     if len(indices) == 2:
#         return dict_of_tuples_slice_2(dot, indices[0], indices[1])
#     if len(indices) == 3:
#         return dict_of_tuples_slice_3(dot, indices[0], indices[1], indices[2])
# 
#     result = {}
#     for (key_tuple, count) in dot.items():
#         sliced_tuple = util.slice_by_sequence(key_tuple, indices)
#         result[sliced_tuple] = result[sliced_tuple] + count
#     return result

# dict_of_tuples_slice(fn_var_values["PUSH-ACTION"], (0,))
# dict_of_tuples_slice(fn_var_values["PUSH-ACTION"], (1,))
# dict_of_tuples_slice(fn_var_values["VERIFY-CLEAN-PARALLEL"], (0,))
# dict_of_tuples_slice(fn_var_values["VERIFY-CLEAN-PARALLEL"], (1,))
# dict_of_tuples_slice(fn_var_values["VERIFY-CLEAN-PARALLEL"], (2,))
# dict_of_tuples_slice(fn_var_values["VERIFY-CLEAN-PARALLEL"], (0,1))
# dict_of_tuples_slice(fn_var_values["VERIFY-CLEAN-PARALLEL"], (0,2))
# dict_of_tuples_slice(fn_var_values["VERIFY-CLEAN-PARALLEL"], (1,2))


def dict_of_tuples_slice_2(dot, i1, i2):
    """Input: a dictionary mapping a tuple of elements to a count, and a
    list of indices.
    Output: a dictionary mapping a subset of the original elements to a count.
    The subset is chosen according to the input indices."""

    result = {}
    for (key_tuple, count) in dot.items():
        # sliced_tuple = util.slice_by_sequence(key_tuple, indices)
        sliced_tuple = (key_tuple[i1][0], key_tuple[i2][0])
        modified = key_tuple[i1][1] and key_tuple[i2][1]
        this_counts = result.get(sliced_tuple, [0, 0])
        result[sliced_tuple] = this_counts
        this_counts[0] = this_counts[0] + count
        if modified:
            this_counts[1] = this_counts[1] + count
    return result

def dict_of_tuples_slice_3(dot, i1, i2, i3):
    """Input: a dictionary mapping a tuple of elements to a count, and a
    list of indices.
    Output: a dictionary mapping a subset of the original elements to a count.
    The subset is chosen according to the input indices."""

    result = {}
    for (key_tuple, count) in dot.items():
        # sliced_tuple = util.slice_by_sequence(key_tuple, indices)
        sliced_tuple = (key_tuple[i1][0], key_tuple[i2][0], key_tuple[i3][0])
        modified = key_tuple[i1][1] and key_tuple[i2][1] and key_tuple[i3][1]
        this_counts = result.get(sliced_tuple, [0, 0])
        result[sliced_tuple] = this_counts
        this_counts[0] = this_counts[0] + count
        if modified:
            this_counts[1] = this_counts[1] + count
    return result


###
### Dictionary utilities -- new version
###

### Is this comment still correct?
## These take a dictionary with in-key modification information and
## produce a dictionary with in-value modification information.
## Their documentation needs to be updated to indicate this.

def dict_of_tuples_modinkey_to_tuple_of_dicts(dot, tuple_len=None):
    """Input: a dictionary mapping a tuple of elements to a count.
    All the key tuples in the input have the same length unless optional argument
    TUPLE_LEN is provided, in which case all tuples have at least that length.
    If TUPLE_LEN is a tuple, then only those indices are extracted.
    if TUPLE_LEN is an integer, indices up to it (non-inclusive) are extracted.
    Output: a tuple of dictionaries, each mapping a single element to a count.
    The first output dictionary concerns the first element of the original keys,
    the second output the second element of the original keys, and so forth."""

    assert not var_values_invalid_modinkey(dot)

    if tuple_len == None:
        tuple_len = len(dot.keys()[0])
    if type(tuple_len) == types.IntType:
        assert tuple_len <= len(dot.keys()[0])
        if tuple_len == 0:
            return ()
        tuple_indices = range(0, tuple_len)
    elif tuple_len == []:
        return ()
    else:
        assert type(tuple_len) in [types.TupleType, types.ListType]
        assert max(tuple_len) < len(dot.keys()[0])
        assert min(tuple_len) >= 0
        tuple_indices = tuple_len
    # Next four lines accomplish "result = ({},) * tuple_len", but with
    # distinct rather than identical dictionaries in the tuple.
    result = []
    for i in tuple_indices:
        result.append({})
    result = tuple(result)
    for (key_tuple, count) in dot.items():
        for i in range(0, len(tuple_indices)):
            (this_key, this_modified) = key_tuple[tuple_indices[i]]
            this_dict = result[i]
            # Is this more efficient than the following?
            #    this_dict_elt = this_dict.get(this_key, [0,0])
            #    this_dict[this_key] = this_dict_elt
            if this_dict.has_key(this_key):
                this_dict_elt = this_dict[this_key]
            else:
                this_dict_elt = [0,0]
                this_dict[this_key] = this_dict_elt
            this_dict_elt[0] = this_dict_elt[0] + count
            if this_modified:
                this_dict_elt[1] = this_dict_elt[1] + count
    return result
# dict_of_tuples_modinkey_to_tuple_of_dicts(fn_var_values["PUSH-ACTION"])
# dict_of_tuples_modinkey_to_tuple_of_dicts(daikon.fn_var_values['P180-15.1.1:::EXIT'])

def dict_of_sequences_to_element_dict(dot):
    """Input: a dictionary mapping instances of a sequence (tuples) to a count.
    Output: a dictionary, mapping elements of all of the sequence instances
    to a count."""
    result = {}
    for (key_tuple, (count, modified)) in dot.items():
        for this_key in key_tuple:
            this_counts = result.get(this_key, [0,0])
            result[this_key] = this_counts
            this_counts[0] = this_counts[0] + count
            this_counts[1] = this_counts[1] + modified
    return result
# dict_of_sequences_to_element_dict(dot)


def dict_of_tuples_slice(dot, indices):
    """Input: a dictionary mapping a tuple of elements to a count, and a
    list of indices.
    Output: a dictionary mapping a subset of the original elements to a count.
    The subset is chosen according to the input indices.

    If the indices have length 2 or 3, you are better off using the
    specialized functions dict_of_tuples_slice_2 and dict_of_tuples_slice_3;
    this function can be very slow.
    """

    if len(indices) == 2:
        return dict_of_tuples_slice_2(dot, indices[0], indices[1])
    if len(indices) == 3:
        return dict_of_tuples_slice_3(dot, indices[0], indices[1], indices[2])

    result = {}
    for (key_tuple, count) in dot.items():
        sliced_tuple = util.slice_by_sequence(key_tuple, indices)
        result[sliced_tuple] = result[sliced_tuple] + count
    return result

# dict_of_tuples_slice(fn_var_values["PUSH-ACTION"], (0,))
# dict_of_tuples_slice(fn_var_values["PUSH-ACTION"], (1,))
# dict_of_tuples_slice(fn_var_values["VERIFY-CLEAN-PARALLEL"], (0,))
# dict_of_tuples_slice(fn_var_values["VERIFY-CLEAN-PARALLEL"], (1,))
# dict_of_tuples_slice(fn_var_values["VERIFY-CLEAN-PARALLEL"], (2,))
# dict_of_tuples_slice(fn_var_values["VERIFY-CLEAN-PARALLEL"], (0,1))
# dict_of_tuples_slice(fn_var_values["VERIFY-CLEAN-PARALLEL"], (0,2))
# dict_of_tuples_slice(fn_var_values["VERIFY-CLEAN-PARALLEL"], (1,2))


# def dict_of_tuples_slice_2(dot, i1, i2):
#     """Input: a dictionary mapping a tuple of elements to a count, and a
#     list of indices.
#     Output: a dictionary mapping a subset of the original elements to a count.
#     The subset is chosen according to the input indices."""
# 
#     result = {}
#     for (key_tuple, (count, modified)) in dot.items():
#         # sliced_tuple = util.slice_by_sequence(key_tuple, indices)
#         sliced_tuple = (key_tuple[i1], key_tuple[i2])
#         this_counts = result.get(sliced_tuple, [0, 0])
#         result[sliced_tuple] = this_counts
#         this_counts[0] = this_counts[0] + count
#         this_counts[1] = this_counts[1] + modified
#     return result

# def dict_of_tuples_slice_3(dot, i1, i2, i3):
#     """Input: a dictionary mapping a tuple of elements to a count, and a
#     list of indices.
#     Output: a dictionary mapping a subset of the original elements to a count.
#     The subset is chosen according to the input indices."""
# 
#     result = {}
#     for (key_tuple, count) in dot.items():
#         # sliced_tuple = util.slice_by_sequence(key_tuple, indices)
#         sliced_tuple = (key_tuple[i1], key_tuple[i2], key_tuple[i3])
#         this_counts = result.get(sliced_tuple, [0, 0])
#         result[sliced_tuple] = this_counts
#         this_counts[0] = this_counts[0] + count
#         this_counts[1] = this_counts[1] + modified
#     return result


###########################################################################
### Derived variables
###


# I want to introduce some derived values and compute invariants over them
# before deciding whether to introduce other derived values.  Therefore,
# rather than having one function that introduces all the derived values,
# there are several passes.  (Actually, each pass is a combination of a
# number of highly-specialized introduction routines which are plugged into
# a harness which calls them.)

# all_numeric_invariants alternates between introducing new variables and
# computing new invariants (among the new variables, and between a new
# variable and old variables, but no recomputation is done among old old
# variables).  It repeats this cycle until fixpoint (or until we are done
# adding variables).



# In my documentation, I several lists of derived variables:
#  * by the type of derivees
#  * by the type of derived
#  * by what is required
#  * ordering over these things

# To simplify cross-reference, name derived variables according like so:
#  EE.D.n
# where the "E"s are the type of derivees,
#       the "D" is the type of the derived value, and
#       the "n" is a number added for uniqueness.
#  The types are "Q" for sequence and "C" for scalar.


# Derived variables:
#  sequence:
#   * scalars:
#      * Q.C.1:  size
#        restriction: not if derived subsequence of known (symbolic) length
#      * Q.C.2:  element at each index, counting from front
#	 must know: bounds on size of array (Q.C.1)
#        restriction: not sequences derived from prefix of another sequence
# 	 caveat: maybe not the element at *each* index, but just extremal few.)
#      * Q.C.3:  element at each index, counting from back
#	 must know: bounds on size of array (Q.C.1)
#	 restriction: not sequences of constant (literal) length
#        restriction: not sequences derived from suffix of another sequence
#		(that doesn't happen yet)
# 	 caveat: maybe not the element at *each* index, but just extremal few.)
#   * sequences:
#      * Q.Q.1:  subsequence up to zero element, exclusive
#        restriction: not sequences derived from prefix of another sequence
#      * Q.Q.2:  subsequence up to zero element, inclusive
#        restriction: not sequences derived from prefix of another sequence
#  sequence, scalar:
#   restriction: not sequences derived from prefix of another sequence
#   restriction: scalar not (equal to) size of this sequence (Q.C.1)
#   * scalars:
#      * QC.C.1:  elt at that index
#   * sequence:
#      * QC.Q.1:  subsequence up to that index, inclusive
#	 restriction: not if that index is greater than or equal to size (Q.C.1)
#      * QC.Q.2:  subsequence up to that index, exclusive
#		 (ie, prefix of that size)
#	 restriction: not if that index is greater than size (Q.C.1)
#      * QC.Q.3:  subsequence up to that element, inclusive (if elt in sequence)
#      * QC.Q.4:  subsequence up to that element, exclusive (if elt in sequence)

# I also want to avoid checking certain invariants which are vacuously true
# over derived values.
# Avoid obvious invariants:
#  * sequence membership:  an element of a sequence is a member of that
#    sequence, or of some other sequence derived from it (or from which it
#    was derived) which still covers that element.
#  * subsequence:  a derived sequence is a subsequence of its larger
#    containing sequence

# Need a mapping from a sequence variable to the variable representing its size.




# This routine does one "pass"; that is, it adds some set of derived
# variables, according to the functions that are passed in.
def introduce_new_variables_one_pass(var_infos, var_values, indices, functions):
    """Add new computed variables to the dictionaries, by side effect.
    The arguments are:
      VAR_INFOS: (complete) list of var_info objects
      INDICES: only values (partially) computed from these indices are candidates
      FUNCTIONS: (long) list of functions for adding new variables; see the code
    The first argument is typically an element of global variable fn_var_infos;
    this function operates over only one function, not all functions.
    """

    if debug_derive:
        print "introduce_new_variables_one_pass: indices %s (limit %d), functions %s" % (indices, len(var_infos), functions)

    if var_values == {}:
        # This program point was never encountered.
        return

    (intro_from_sequence, intro_from_scalar,
     intro_from_sequence_sequence, intro_from_sequence_scalar,
     intro_from_scalar_sequence, intro_from_scalar_scalar) = functions

    assert len(var_infos) == len(var_values.keys()[0])

    # We will modify this mutable list, then reinstall it as a tuple at the end.
    var_new_values = {}       # map from old values to new values
    for value in var_values.keys():
        var_new_values[value] = list(value)

    orig_len_var_infos = len(var_infos)
    all_indices = range(0, len(var_infos))

    for i in indices:
        this_var_info = var_infos[i]
        if not this_var_info.is_canonical():
            continue
        # It's a constant value, always None
        if this_var_info.invariant.one_of == [None]:
	    continue
        if this_var_info.is_sequence():
            intro_from_sequence(var_infos, var_new_values, i)
        else:
            intro_from_scalar(var_infos, var_new_values, i)

    # It's cleaner to do "for (i1,i2) in util.choose(2, all_indices):", but
    # also less efficient due to creation of long list of pairs.
    for i1 in range(0,orig_len_var_infos-1):
        vi1 = var_infos[i1]
        if not vi1.is_canonical():
            continue
        if vi1.invariant.one_of == [None]:
	    continue
        for i2 in range(i1+1,orig_len_var_infos):
            vi2 = var_infos[i2]
            if not vi2.is_canonical():
                continue
            if vi2.invariant.one_of == [None]:
                continue
            if not (i1 in indices or i2 in indices):
                # Do nothing if neither of these variables is under consideration.
                continue

            # (var1, vi2) = util.slice_by_sequence(var_infos, (i1, i2))
            if vi1.is_sequence() and vi2.is_sequence():
                intro_from_sequence_sequence(var_infos, var_new_values, i1, i2)
            elif vi1.is_sequence():
                intro_from_sequence_scalar(var_infos, var_new_values, i1, i2)
            elif vi2.is_sequence():
                intro_from_scalar_sequence(var_infos, var_new_values, i1, i2)
            else:
                intro_from_scalar_scalar(var_infos, var_new_values, i1, i2)

    # if len(var_infos) == orig_len_var_infos:
    #     print "introduce_new_variables_one_pass: added no variables"
    # if len(var_infos) > orig_len_var_infos:
    #     print "introduce_new_variables_one_pass: added variables", range(orig_len_var_infos, len(var_infos)), map(lambda i, vis=var_infos: vis[i].name, range(orig_len_var_infos, len(var_infos)))
    # Done adding; install the new, expanded list of variables
    for (vals, count) in var_values.items():
        del var_values[vals]
        var_values[tuple(var_new_values[vals])] = count


###
### Now, the specific functions for introducing new variables.
###

def introduce_from_sequence_pass1(var_infos, var_new_values, index):
    """Add size (only)."""
    seq_var_info = var_infos[index]

    assert len(var_infos) == len(var_new_values.values()[0])

    ## For now, add size unconditionally; fix later.
    # Add size only if this is an original
    # sequence, not a subsequence (sequence slice) we have added
    if (seq_var_info.derived_len == None and not seq_var_info.is_derived):
        name_size = "size(%s)" % (seq_var_info.name,)
        index_lackwit_type = lackwit_type_index_type_alias(seq_var_info, 1)
        seq_len_var_info = var_info(name_size, int_proglang_type, index_lackwit_type, len(var_infos), true)
        var_infos.append(seq_len_var_info)
        seq_var_info.derived_len = len(var_infos)-1
        if debug_derive:
            print "set derived_len for", seq_var_info, "to", len(var_infos)-1

        for new_values in var_new_values.values():
            (this_seq, this_seq_mod) = new_values[index]
            if this_seq == None:
                this_seq_len = None
            else:
                this_seq_len = len(this_seq)
            new_values.append((this_seq_len, this_seq_mod))


def introduce_from_scalar_pass1(var_infos, var_new_values, index):
    assert len(var_infos) == len(var_new_values.values()[0])
    pass

def introduce_from_sequence_sequence_pass1(var_infos, var_new_values, i1, i2):
    assert len(var_infos) == len(var_new_values.values()[0])
    pass

def introduce_from_sequence_scalar_pass1(var_infos, var_new_values, i1, i2):
    assert len(var_infos) == len(var_new_values.values()[0])
    pass

def introduce_from_scalar_sequence_pass1(var_infos, var_new_values, i1, i2):
    assert len(var_infos) == len(var_new_values.values()[0])
    pass

def introduce_from_scalar_scalar_pass1(var_infos, var_new_values, i1, i2):
    assert len(var_infos) == len(var_new_values.values()[0])
    pass

pass1_functions = (introduce_from_sequence_pass1,
                   introduce_from_scalar_pass1,
                   introduce_from_sequence_sequence_pass1,
                   introduce_from_sequence_scalar_pass1,
                   introduce_from_scalar_sequence_pass1,
                   introduce_from_scalar_scalar_pass1)


def introduce_from_sequence_pass2(var_infos, var_new_values, seqidx):
    """Add scalars from sequences (but don't examine any of the new scalars)."""
    assert len(var_infos) == len(var_new_values.values()[0])

    seq_var_info = var_infos[seqidx]
    seqvar = seq_var_info.name
    assert seq_var_info.type.is_array()

    # Add sum, min, and max (nearly unconditionally; maybe should be
    # unconditional once again)
    lackwit_elt_type = lackwit_type_element_type_alias(seq_var_info)
    proglang_elt_type = seq_var_info.type.element_type()

    # Could alternately test lackwit_elt_type.
    if ((seq_var_info.type.dimensionality > 1)
        or (seq_var_info.type.base != "int")):
        # Perhaps I should make an exception for char[][] which is String[].
        this_seq_sum = None
        this_seq_min = None
        this_seq_max = None
    else:
        var_infos.append(var_info("sum(%s)" % (seqvar,), int_proglang_type, lackwit_elt_type, len(var_infos), true))
        var_infos.append(var_info("min(%s)" % (seqvar,), int_proglang_type, lackwit_elt_type, len(var_infos), true))
        var_infos.append(var_info("max(%s)" % (seqvar,), int_proglang_type, lackwit_elt_type, len(var_infos), true))
        for new_values in var_new_values.values():
            (this_seq,this_seq_mod) = new_values[seqidx]
            if this_seq == None:
                this_seq_sum = None
                this_seq_min = None
                this_seq_max = None
            elif len(this_seq) == 0:
                this_seq_sum = 0
                this_seq_min = None
                this_seq_max = None
            else:
                try:
                    this_seq_sum = util.sum(this_seq)
                except OverflowError:
                    this_seq_sum = None
                this_seq_min = min(this_seq)
                this_seq_max = max(this_seq)
            new_values.append((this_seq_sum,this_seq_mod))
            new_values.append((this_seq_min,this_seq_mod))
            new_values.append((this_seq_max,this_seq_mod))

    # Add each individual element.
    ## For now, add if not a derived variable; a better test is if
    ## not a prefix subsequence (sequence slice) we have added.
    if not seq_var_info.is_derived:
        seq_len_inv = var_infos[seq_var_info.derived_len].invariant
        assert isinstance(seq_var_info, var_info)
        len_min = seq_len_inv.min or 0
        # The point of this is not to do checks over every last irrelevant
        # element; just look at the one or two at the beginning and the end.
        len_min = min(2, len_min)
        if len_min > 0:
            for i in range(0, len_min):
                var_infos.append(var_info("%s[%d]" % (seqvar, i), proglang_elt_type, lackwit_elt_type, len(var_infos), true))
            for new_values in var_new_values.values():
                for i in range(0, len_min):
                    (seq,seq_mod) = new_values[seqidx]
                    if seq == None:
                        elt_val = None
                    else:
                        elt_val = seq[i]
                    new_values.append((elt_val,seq_mod))
            if len_min != seq_len_inv.max:
                for i in range(-len_min, 0):
                    var_infos.append(var_info("%s[%d]" % (seqvar, i), proglang_elt_type, lackwit_elt_type, len(var_infos), true))
                for new_values in var_new_values.values():
                    for i in range(-len_min, 0):
                        (seq,seq_mod) = new_values[seqidx]
                        if seq == None:
                            elt_val = None
                        else:
                            elt_val = seq[i]
                        new_values.append((elt_val,seq_mod))


def introduce_from_scalar_pass2(var_infos, var_new_values, index):
    assert len(var_infos) == len(var_new_values.values()[0])
    pass

def introduce_from_sequence_sequence_pass2(var_infos, var_new_values, i1, i2):
    assert len(var_infos) == len(var_new_values.values()[0])
    pass

def introduce_from_sequence_scalar_pass2(var_infos, var_new_values, seqidx, sclidx):
    assert len(var_infos) == len(var_new_values.values()[0])

    seq_info = var_infos[seqidx]
    scl_info = var_infos[sclidx]
    seq_name = seq_info.name
    scl_name = scl_info.name
    scl_inv = scl_info.invariant
    seq_size_idx = seq_info.derived_len

    ## This makes absolutely no sense; I've left it commented out only
    ## so I don't get tempted to do something so silly again.
    # # Do nothing if the size is known (there's some other var practically
    # # equal to the size).
    # if seq_size_idx == 'known_var':
    #     return

    # For now, do nothing if the scalar is itself derived.
    if scl_info.is_derived:
        return
    # For now, do nothing if the sequence is itself derived.
    if seq_info.is_derived:
        return

    # Do nothing if this scalar is actually the size of this sequence
    if seq_size_idx == sclidx:
        return
    # Another check for scalar being the size of this sequence: sclidx may
    # not be canonical, but seq_size_idx certainly is, because
    # we don't call the introduction functions with non-canonical arguments.
    assert scl_info.is_canonical()
    if seq_size_idx != 'known_var':
        if sclidx == var_infos[seq_size_idx].canonical_var():
            return

    #     if seq_size_idx == 'no_var':
    #         print "sequence %s (size: no_var) and scalar %s (index: %d) unrelated" % (seq_name, scl_name, sclidx)
    #     else:
    #         print "sequence %s (size: %s, size index = %s) and scalar %s (index: %d) unrelated" % (seq_name, var_infos[seq_size_idx].name, seq_size_idx, scl_name, sclidx)

    if not lackwit_types_compatible(scl_info.name, scl_info.lackwit_type,
                                    "%s-index%d" % (seq_info.name, 1),
                                    lackwit_type_index_type(seq_info.lackwit_type, 1)):
        return

    # If the scalar is a known constant, record that.
    if scl_inv.is_exact():
        sclconst = scl_inv.min
    else:
        sclconst = None

    # If the scalar is the constant 0, do nothing (we already extract array[0],
    # and the subarrays array[0..-1] and array[0..0] are not interesting).
    # if sclconst == 0:
    if sclconst != None and sclconst < 1:
        return

    scalar_value_1 = not(sclconst == None or sclconst > 1)

    # Add subsequences
    if not seq_info.invariant.can_be_None and not seq_info.is_derived and not scl_info.invariant.can_be_None:
        lackwit_seq_type = lackwit_make_alias(seq_name, seq_info.lackwit_type)
        full_var_info = var_info("%s[0..%s]" % (seq_name, scl_name), seq_info.type, lackwit_seq_type, len(var_infos), true)
        # 'known_var' means there is a known value, but no variable
        # holds that particular value.
        full_var_info.derived_len = 'known_var' # length is 1 more than var[sclidx]
        var_infos.append(full_var_info)
        assert (not scalar_value_1) or sclconst == 1
        if not scalar_value_1:
            less_one_var_info = var_info("%s[0..%s-1]" % (seq_name, scl_name), seq_info.type, lackwit_seq_type, len(var_infos), true)
            less_one_var_info.derived_len = sclidx
            var_infos.append(less_one_var_info)
        for new_values in var_new_values.values():
            (seq,seq_mod) = new_values[seqidx]
            (scl,scl_mod) = new_values[sclidx]
            assert sclconst == None or scl == sclconst
            if (scl+1 <= len(seq)) and (scl+1 >= 0):
                new_value_full = seq[0:scl+1]
            else:
                new_value_full = None
            new_values.append(new_value_full,(seq_mod or scl_mod))
            if not scalar_value_1:
                if (scl <= len(seq)) and (scl >= 0):
                    new_value_less_one = seq[0:scl]
                else:
                    new_value_less_one = None
                new_values.append(new_value_less_one,(seq_mod or scl_mod))
            if debug_derive:
                print "seq %s = %s (len = %d), scl %s = %s, new_value_less_one = %s" % (seq_name, seq, len(seq), scl_name, scl, new_value_less_one)

    # Add scalars
    # Determine whether it is constant; if so, ignore.
    # Perhaps also check that it is within range at least once
    # (or even every time); if not, not very interesting.
    if ((not seq_info.is_derived)
        and (not scalar_value_1)
        and (seq_size_idx != 'known_var')
        and (scl_inv.max <= var_infos[seq_size_idx].invariant.max)):
        lackwit_elt_type = lackwit_type_element_type_alias(seq_info)
        proglang_elt_type = seq_info.type.element_type()
        var_infos.append(var_info("%s[%s]" % (seq_name, scl_name), proglang_elt_type, lackwit_elt_type, len(var_infos), true))
        for new_values in var_new_values.values():
            (this_seq,this_seq_mod) = new_values[seqidx]
            (this_scl,this_scl_mod) = new_values[sclidx]
            if ((this_seq != None) and (this_scl != None)
                and (this_scl < len(this_seq)) and (this_scl >= 0)):
                new_values.append(this_seq[this_scl],(this_seq_mod or this_scl_mod))
            else:
                new_values.append(None,(this_seq_mod or this_scl_mod))



def introduce_from_scalar_sequence_pass2(var_infos, var_new_values, sclidx, seqidx):
    assert len(var_infos) == len(var_new_values.values()[0])

    # Is there any subtlety here with respect to index ordering?  (Eg, do we
    # always expect the lesser index to appear first?)
    introduce_from_sequence_scalar_pass2(var_infos, var_new_values, seqidx, sclidx)


def introduce_from_scalar_scalar_pass2(var_infos, var_new_values, i1, i2):
    assert len(var_infos) == len(var_new_values.values()[0])
    pass

pass2_functions = (introduce_from_sequence_pass2,
                   introduce_from_scalar_pass2,
                   introduce_from_sequence_sequence_pass2,
                   introduce_from_sequence_scalar_pass2,
                   introduce_from_scalar_sequence_pass2,
                   introduce_from_scalar_scalar_pass2)


## Old definition
# def introduce_new_variables(sub_fn_var_names, sub_fn_var_values, sub_fn_var_derived, indices):
#     """Add new computed variables to the dictionaries, by side effect.
#     Only values (partially) computed from an index in INDICES are candidates.
#     The first three arguments are dictionaries mapping from a function name to
#     information about the function; see `read_data_trace_file' for a
#     description."""
#     #  * map from function name to tuple of variable names.
#     #  * map from function name to (map from tuple of values to occurrence count)
#     for fname in sub_fn_var_names.keys():
#         all_vars = sub_fn_var_names[fname]
#         sequence_vars = {}              # map from var name to index in all_vars
#         scalar_vars = {}                # map from var name to index in all_vars
#         for i in range(0, len(all_vars)):
#             var = all_vars[i]
#             if sequence_re.match(var):
#                 if var[-2:] == "[]":
#                     var = var[:-2]
#                 sequence_vars[var] = i
#             else:
#                 scalar_vars[var] = i
#         original_sequence_vars = sequence_vars.copy()
#         new_vars = list(all_vars)
# 
#         these_var_values = sub_fn_var_values[fname]
#         these_var_new_values = {}       # map from old values to new values
#         for value in these_var_values.keys():
#             these_var_new_values[value] = list(value)
# 
#         # Add subsequences (but don't examine any of the new subsequences)
#         for (seqvar, seqidx) in sequence_vars.items():
#             for (sclvar, sclidx) in scalar_vars.items():
#                 name_full = "%s[0..%s]" % (seqvar, sclvar)
#                 name_less_one = "%s[0..%s-1]" % (seqvar, sclvar)
#                 sequence_vars[name_full] = len(new_vars)
#                 new_vars.append(name_full)
#                 sequence_vars[name_less_one] = len(new_vars)
#                 new_vars.append(name_less_one)
#                 for new_values in these_var_new_values.values():
#                     seq = new_values[seqidx]
#                     scl = new_values[sclidx]
#                     if (scl+1 <= len(seq)) and (scl+1 >= 0):
#                         new_value_full = seq[0:scl+1]
#                     else:
#                         new_value_full = None
#                     if (scl <= len(seq)) and (scl >= 0):
#                         new_value_less_one = seq[0:scl]
#                     else:
#                         new_value_less_one = None
#                     # print "seq %s = %s (len = %d), scl %s = %s, new_value_less_one = %s" % (seqvar, seq, len(seq), sclvar, scl, new_value_less_one)
#                     new_values.append(new_value_full)
#                     new_values.append(new_value_less_one)
#         # Add scalars from sequences (but don't examine any of the new scalars)
#         scalar_vars_items = scalar_vars.items() # map may change in the loop
#         for (seqvar, seqidx) in sequence_vars.items():
#             # Add sum unconditionally
#             name_sum = "%s.sum" % (seqvar,)
#             sequence_vars[name_sum] = len(new_vars)
#             new_vars.append(name_sum)
#             for new_values in these_var_new_values.values():
#                 this_seq = new_values[seqidx]
#                 if this_seq == None:
#                     this_seq_sum = None
#                 else:
#                     this_seq_sum = util.sum(this_seq)
#                 new_values.append(this_seq_sum)
# 
#             # Add some values only if this is an original
#             # sequence, not a subsequence (sequence slice) we have added
#             if original_sequence_vars.has_key(seqvar):
# 
#                 # Add size (remembering min and max for future use), sum
#                 name_size = "%s.size_computed" % (seqvar,)
#                 sequence_vars[name_size] = len(new_vars)
#                 new_vars.append(name_size)
# 
#                 seq_len_min = 1000000
#                 seq_len_max = 0
#                 for new_values in these_var_new_values.values():
#                     this_seq = new_values[seqidx]
#                     if this_seq == None:
#                         this_seq_len = None
#                     else:
#                         this_seq_len = len(this_seq)
#                         seq_len_min = min(seq_len_min, this_seq_len)
#                         seq_len_max = max(seq_len_max, this_seq_len)
#                     new_values.append(this_seq_len)
#                 if seq_len_min == 1000000:
#                     seq_len_min = 0
# 
# 
#                 ## Add each individual element.
#                 if seq_len_min > 0:
#                     for i in range(0, seq_len_min):
#                         elt_name = "%s[%d]" % (seqvar, i)
#                         scalar_vars[elt_name] = len(new_vars)
#                         new_vars.append(elt_name)
#                     for new_values in these_var_new_values.values():
#                         for i in range(0, seq_len_min):
#                             seq = new_values[seqidx]
#                             if seq == None:
#                                 elt_val = None
#                             else:
#                                 elt_val = seq[i]
#                             new_values.append(elt_val)
#                     if seq_len_min != seq_len_max:
#                         for i in range(-seq_len_min, 0):
#                             elt_name = "%s[%d]" % (seqvar, i)
#                             scalar_vars[elt_name] = len(new_vars)
#                             new_vars.append(elt_name)
#                         for new_values in these_var_new_values.values():
#                             for i in range(-seq_len_min, 0):
#                                 seq = new_values[seqidx]
#                                 if seq == None:
#                                     elt_val = None
#                                 else:
#                                     elt_val = seq[i]
#                                 new_values.append(elt_val)
#                 ## Add element at each specific index, but not at constant indices
#                 for (sclvar, sclidx) in scalar_vars_items:
#                     # Determine whether it is constant; if so, ignore.
#                     # Perhaps also check that it is within range at least once
#                     # (or even every time) if not, not very interesting.
#                     prev_val = None
#                     for new_values in these_var_new_values.values():
#                         new_value = new_values[sclidx]
#                         if prev_val == None:
#                             prev_val = new_value
#                         elif prev_val != new_values:
#                             prev_val = None
#                             break
#                     if prev_val != None:
#                         break
#                     # Non-constant value
#                     elt_name = "%s[%s]" % (seqvar, sclvar)
#                     scalar_vars[elt_name] = len(new_vars)
#                     new_vars.append(elt_name)
#                     for new_values in these_var_new_values.values():
#                         this_seq = new_values[seqidx]
#                         this_scl = new_values[sclidx]
#                         if ((this_seq != None) and (this_scl != None)
#                             and (this_scl < len(this_seq)) and (this_scl >= 0)):
#                             new_values.append(this_seq[this_scl])
#                         else:
#                             new_values.append(None)
# 
#         # Done adding; install the new, expanded list of variables
#         sub_fn_var_names[fname] = tuple(new_vars)
#         for (vals, count) in these_var_values.items():
#             del these_var_values[vals]
#             these_var_values[tuple(these_var_new_values[vals])] = count


## I could just count on finding their values to be constant and thus
## eliminating them from further consideration...  For now, do that.
# def eliminate_vacuous_variables(sub_fn_var_names, sub_fn_var_values):
#     """Remove variables with constant value "None" from the dictionaries.
#     See `read_data_trace_file' for a description of the argument types;
#     arguments are dictionaries mapping from a function name to information
#     about the function."""
#     for fname in sub_fn_var_names.keys():
#         these_vars = sub_fn_var_names[fname]
#         for i in range(0,len(these_vars)):
#             for
#             
# 
# Could use dict_of_tuples_slice but since we know there will be no
# merging, it's more efficient to side-effect by hand (but still using
# slice_by_sequence).




###########################################################################
### Input/output
###

# An instrumented program produces a .dtrace file containing information about
# run-time values of expressions and variables.  The invariant detector tries
# to find patterns in the values recorded in one or more trace files.
# To detect invariants in a particular program, it is enough to insert code
# in the application which creates a trace file.  In Lisp, the
# `write-to-data-trace' macro and `instrument' function perform this task.
# For documentation of the data trace file format, see daikon.py.doc.



# This function makes a separate pass over the trace files because at each
# program point, we need to know the names of all the available variables,
# which includes all global variables (of which these are examples).
#  * Alternative:  declare all the functions before any data are seen
#     + more reasonable for on-line information (can't know when a
#       new declaration would be seen)
#     + obviates the need for this function and extra pass over the data
#     - forces declaration of all functions ahead of time
#     - perhaps not reasonable (at the least, complicated) for dynamic
#       loading of code
#  * Alternative:  go back and update info to insert new "zero" values
#    everywhere that the variables weren't yet known about
#     - not reasonable for online operation
#  * Alternative:  when a new variable is added, somehow treat the previous
#    tuples as having a default zero value.  I'm not sure exactly how this
#    would work, but it might be the best solution.

def read_data_trace_file_declarations(filename, fn_regexp=None):
    """Read data from .dtrace file; add to dictionary mapping file
    names to invocation counts.  The invocation counts are initialized
    to zero."""

    this_fn_var_infos = {}      # from function name to tuple of variable names

    fn_regexp = util.re_compile_maybe(fn_regexp, re.IGNORECASE)

    if debug_read:
        print "read_data_trace_file_declarations", filename, (fn_regexp and fn_regexp.pattern) or ""

    # if (filename[-3:] == ".gz"):
    #     file = gzip.open(filename, "r")
    # else:
    #     file = open(filename, "r")
    file = TextFile.TextFile(filename, "r")
    line = file.readline()
    if "\t" in line:
        raise "First line should be tag line; saw: " + line

    # Not using processed_lines dictionary appears to be marginally faster.
    # processed_lines = {}                # hashtable of tag lines seen so far
    while (line != ""):         # line == "" when we hit end of file
        if debug_read:
            print "rdtfd line", line
        if (line == "\n") or (line[0] == '#'):
            line = file.readline()
        elif (line == "DECLARE\n") or (line == "Declare\n"):
            process_declaration(file, this_fn_var_infos, fn_regexp)
            line = file.readline()
        else:
            while (line != "") and (line != "\n") and (line[0] != "#"):
                line = file.readline()

    return this_fn_var_infos


def init_ftn_call_ct():
    """Initialize function call counts to 0."""
    # Clear out the entire dictionaries in the name of paranoia
    global fn_to_orig_param_vals, fn_invocations
    fn_invocations = {}
    fn_to_orig_param_vals = {}

    for fn_name in functions:
        fn_invocations[fn_name] = 0
        fn_to_orig_param_vals[fn_name] = []


# This has certain parallels with read_data_trace file; but I think
# separating the implementations is clearer, even if there's a bit of
# duplication.
def process_declaration(file, this_fn_var_infos, fn_regexp=None):
    # We have just read the "DECLARE" line.
    if debug_read:
        print "process_declaration entered"
    line = file.readline()
    if (fn_regexp and not fn_regexp.search(line)):
        while (line != "\n") and (line != ""):
            line = file.readline()
        return

    assert line[-1] == "\n"
    program_point = line[:-1]
    if debug_read:
        print "process_declaration", program_point
    (tag_sans_suffix, tag_suffix) = (string.split(program_point, ":::", 1) + [""])[0:2]
    if tag_suffix == "ENTER":
        functions.append(tag_sans_suffix)

    assert not this_fn_var_infos.has_key(program_point)
    these_var_infos = []
    line = file.readline()
    while (line != "\n") and (line != ""):
        assert line[-1] == "\n"
        varname = line[:-1]
        line = file.readline()
        assert line[-1] == "\n"
        vartype = proglang_type(line[:-1])
        line = file.readline()
        assert line[-1] == "\n"
        var_comparable = parse_lackwit_vartype(line[:-1], vartype)
        if debug_read:
            print "var for", program_point, varname, vartype, var_comparable
        these_var_infos.append(var_info(varname, vartype, var_comparable, len(these_var_infos)))
        line = file.readline()

    assert not(this_fn_var_infos.has_key(program_point))
    this_fn_var_infos[program_point] = these_var_infos
    ## Is this done somewhere else?
    # this_fn_var_values[program_point] = {}



# I probably *do* want to have original values even for global variables,
# so I probably want to add original values after globals.
# What about original vs. final invocation counts?  That could give info
# about the dynamic call graph.  For now I'll do it, to avoid potential
# problems with number of orig vars not equaling number of final vars.
def after_processing_all_declarations():

    ## Take care not to call this multiple times.  If fn_truevars is set,
    ## it has been called.  But we need to set all the fn_truevars (for use
    ## adding _orig) before doing the rest of the work.

    fns_to_process = []

    for fn in fn_var_infos.keys():
        if not fn_truevars.has_key(fn):
            fn_truevars[fn] = len(fn_var_infos[fn])
            fns_to_process.append(fn)

    # Add invocation counts
    if not no_invocation_counts:
        for ppt in fns_to_process:
            these_var_infos = fn_var_infos[ppt]
            for callee in fn_invocations.keys():
                calls_var_name = "calls(%s)" % callee
                these_var_infos.append(var_info(calls_var_name, "integral", "always", len(these_var_infos)))
                these_values.append(fn_invocations[callee])
                current_var_index = current_var_index + 1

    # Add "_orig"inal values
    for ppt in fns_to_process:
        (ppt_sans_suffix, ppt_suffix) = (string.split(ppt, ":::", 1) + [""])[0:2]
        if ((ppt_suffix != "EXIT")
            and (ppt_suffix[0:4] != "EXIT")):
            continue
        these_var_infos = fn_var_infos[ppt]
        begin_ppt = ppt_sans_suffix + ":::ENTER"
        # Do not use the Lackwit type of the variable at the entry;
        # use the Lackwit type at exit.  (There may be more variables
        # at exit than at entry.)
        # for vi in fn_var_infos[begin_ppt][0:fn_truevars[begin_ppt]]:
        #     these_var_infos.append(var_info(vi.name + "_orig", vi.type, lackwit_make_alias(vi.name, vi.lackwit_type), len(these_var_infos)))
        for i in range(0, fn_truevars[begin_ppt]):
            begin_vi = fn_var_infos[begin_ppt][i]
            end_vi = fn_var_infos[ppt][i]
            assert begin_vi.name == end_vi.name
            assert begin_vi.type == end_vi.type
            these_var_infos.append(var_info(end_vi.name + "_orig", end_vi.type, lackwit_make_alias(end_vi.name, end_vi.lackwit_type), len(these_var_infos)))



def read_data_trace_file(filename, fn_regexp=None):
    """Read data from .dtrace file; return a tuple of three dictionaries.
     * map from function name to tuple of variable names (actually, var_infos)
     * map from function name to (map from tuple of values to occurrence count)
     * map from function name to number of samples
    """

    fn_regexp = util.re_compile_maybe(fn_regexp, re.IGNORECASE)

    if debug_read:
        print "read_data_trace_file", filename, fn_regexp and fn_regexp.pattern

    # if (filename[-3:] == ".gz"):
    #     file = gzip.open(filename, "r")
    # else:
    #     file = open(filename, "r")
    file = TextFile.TextFile(filename, "r")

    this_fn_var_values = {}	# from function name to (tuple of values to occurrence count)
    this_fn_samples = {}        # from function name to number of samples
    for ppt in fn_var_infos.keys():
        this_fn_var_values[ppt] = {}
        this_fn_samples[ppt] = 0

    init_ftn_call_ct()          # initialize function call counts to 0
    line = file.readline()
    if "\t" in line:
        raise "First line of " + filename + " should be tag line; saw: " + line

    # Use, or not, of this dictionary seems to have no measurable effect on
    # execution speed.
    #    seen_tags = {}

    while (line != ""):                 # line == "" when we hit end of file

        if (line == "\n") or (line[0] == '#'):
            line = file.readline()
            continue

        if (line == "DECLARE\n"):
            while (line != "\n"):
                line = file.readline()
            line = file.readline()
            continue

        tag = line[:-1]                 # remove trailing newline

        # Would it be faster to have a hashtable of lines that don't match
        # rather than calling a regexp routine?
        if fn_regexp and not fn_regexp.search(tag):
            # print "-", line, 	# comma because line ends in newline
            line = file.readline()
            while "\t" in line:
                line = file.readline()
            continue
        # print "+", line, 	# comma because line ends in newline

        assert fn_var_infos.has_key(tag), "No declaration for tag " + tag

        # I postulated that this hashtable check is faster than regexps.
        # It doesn't seem to be true, though; see above.
        #         if tag_seen:
        #             (tag_sans_suffix, tag_suffix) = seen_tags[tag]
        #         else:
        (tag_sans_suffix, tag_suffix) = (string.split(tag, ":::", 1) + [""])[0:2]

        # Increment function invocation count if ':::ENTER'
        if (not no_invocation_counts) and (tag_suffix == "ENTER"):
            fn_invocations[tag_sans_suffix] = fn_invocations[tag_sans_suffix] + 1

        ## Read the variable values
        these_var_infos = fn_var_infos[tag]
	these_values = []
        for this_var_info in these_var_infos[0:fn_truevars[tag]]:
            line = file.readline()
            assert line[-1] == "\n"
            assert line[:-1] == this_var_info.name
            line = file.readline()
            assert line[-1] == "\n"
            this_value = line[:-1]
            line = file.readline()
            assert (line == "1\n") or (line == "0\n") or (line == "2\n")
            this_var_modified = (line == "1\n")
            if (line == "2\n"):
                this_value = None

            this_var_type = this_var_info.type
            this_base_type = this_var_type.base


            if this_var_type.is_array():
                # variable is an array

                # Hack for null (missing) string:  turn it into empty string
                if ((this_base_type == 'char')
                    and (this_var_type.dimensionality == 1)
                    and (this_value == "null")):
                    this_value = "\"\""

                if ((this_base_type == 'char')
                    and (this_var_type.dimensionality == 1)
                    and (len(this_value) > 1)
                    and (this_value[0] == "\"") and (this_value[-1] == "\"")):
                    # variable is a string
                    # turn it into a tuple of *numbers* instead.
                    # (Probably I want to retain it as a string; it's obscure
                    # as a sequence of numbers.)
                    this_value = list(eval(this_value))
                    for seq_elem in range(0, len(this_value)):
                        this_value[seq_elem] = ord(this_value[seq_elem])
                    this_value = tuple(this_value)
                elif ((this_base_type == 'char')
                      and (this_var_type.dimensionality == 2)):
                    # Array of strings
                    this_value = tuple(eval(this_value))
                else:
                    # Deal with [] surrounding Java array output
                    if (len(this_value) > 1) and (this_value[0] == "[") and (this_value[-1] == "]"):
                        this_value = this_value[1:-1]

                    # This isn't right if a string contains embedded spaces.
                    this_value = string.split(this_value, " ")
                    if len(this_value) > 0 and this_value[-1] == "":
                        # Cope with trailing spaces on the line
                        this_value = this_value[0:-1]

                    # The regexp test and call to int do seem to be faster
                    # than a call to "eval", according to my timing tests.
                    # They also may be less error-prone (eg, not evaluating
                    # a string that happens to be a variable name).

                    # If sequence variable is uninit (as opposed to one
                    # element being uninit), mark as None
                    if len(this_value) > 0 and this_value[0] == "uninit":
                        this_value = None
                    else:
                        for seq_elem in range(0, len(this_value)):
                            if this_value[seq_elem] == "uninit":
                                this_value[seq_elem] = None
                            elif this_value[seq_elem] == "NIL":
                                # HACK
                                this_value[seq_elem] = 0
                            elif this_value[seq_elem] == "null":
                                # HACK
                                this_value[seq_elem] = 0
                            elif this_base_type == "java_object":
                                this_val = this_value[seq_elem]
                                if ((len(this_val) > 1)
                                    and (this_val[0] == "\"")
                                    and (this_val[-1] == "\"")):
                                    # Horrible, horrible hack, because I
                                    # don't want to deal with Java strings
                                    # that are in Object slots but were
                                    # printed out as strings (without a
                                    # hashcode).  The problem is that the
                                    # proglang_type initializer converts
                                    # "String[]" into "char[][]" and then
                                    # into "Object[]".
                                    this_value[seq_elem] = 2222
                                else:
                                    jomatch = java_object_re.match(this_val)
                                    assert jomatch != None
                                    this_value[seq_elem] = eval("0x" + jomatch.group(1))
                            elif this_base_type == "boolean":
                                assert (this_value[seq_elem] == "true") or (this_value[seq_elem] == "false")
                                this_value[seq_elem] = (this_value[seq_elem] == "true")
                            else:
                                assert integer_re.match(this_value[seq_elem])
                                this_value[seq_elem] = int(this_value[seq_elem])
                        this_value = tuple(this_value)
            else:
                # "pointer" is the deprecated name
                assert this_var_type.dimensionality == 0
                assert (this_base_type in integral_types) or (this_base_type == "pointer") or (this_base_type == "address") or (this_base_type == "java_object")
                if this_value == "uninit":
                    this_value = None
                elif (this_value == "NIL") or (this_value == "null"):
                    # HACK
                    this_value = 0
                elif (this_base_type == "pointer") or (this_base_type == "address"):
                    assert integer_re.match(this_value)
                    # Convert the number to signed.  This is gross, will be fixed.
                    this_value = eval(hex(long(this_value))[:-1])
                elif this_base_type == "java_object":
                    if this_value == None:
                        this_value = 0
                    else:
                        jomatch = java_object_re.match(this_value);
                        assert jomatch != None;
                        this_value = eval("0x" + jomatch.group(1));
                elif this_base_type == "char":
                    # Convert character or string rep of number to integer
                    if type(this_value) == types.IntType:
                        pass
                    elif type(this_value) == types.StringType:
                        try:
                            this_value = int(this_value)
                        except:
                            assert len(this_value) == 1
                            this_value = ord(this_value)
                    else:
                        raise "Bad character value in data trace file: " + this_value
                elif this_base_type == "boolean":
                    assert (this_value == "true") or (this_value == "false")
                    this_value = (this_value == "true")
                else:
                    assert integer_re.match(this_value)
                    this_value = int(this_value)
	    these_values.append((this_value,this_var_modified))

        line = file.readline()
        # Expecting the end of a block of values.
        assert (line == "\n") or (line == "")

        # Add invocation counts
        # Accessing global here-crappy
        if not no_invocation_counts:
            for ftn_tag in fn_invocations.keys():
                calls_var_name = "calls(%s)" % ftn_tag
                assert calls_var_name == these_var_infos[current_var_index].name
                these_values.append((fn_invocations[ftn_tag],1))
                current_var_index = current_var_index + 1

        ## Original values.
        # If beginning of function, store the original param val
        if tag_suffix == "ENTER":
            params_to_orig_val_stack = fn_to_orig_param_vals[tag_sans_suffix]
            params_to_orig_val_stack.append(these_values)
        # If end of function call, pop previous original param value and
        # add to current parameter values.
        if ((tag_suffix == "EXIT") or (tag_suffix[0:4] == "EXIT")):
            params_to_orig_val_stack = fn_to_orig_param_vals[tag_sans_suffix]
            # Equivalently, in Python 1.5.2:
            #   these_values = these_values + params_to_orig_val_stack.pop()
            old_values = params_to_orig_val_stack[-1]
            del params_to_orig_val_stack[-1]
            these_values = these_values + old_values

	these_values = tuple(these_values)
        assert len(these_values) == len(these_var_infos)
        # Effectively done above, by checking names and not reconstructing
        # these_var_infos from scratch
        # assert var_infos_compatible(fn_var_infos[tag], these_var_infos)
        assert type(this_fn_var_values[tag]) == types.DictType
        this_var_values = this_fn_var_values[tag]
        this_var_values[these_values] = this_var_values.get(these_values, 0) + 1
        this_fn_samples[tag] = this_fn_samples.get(tag, 0) + 1

    assert not fn_var_values_invalid_modinkey(this_fn_var_values)
    # print ""

    return (this_fn_var_values, this_fn_samples)


def print_hashtables():
    """Print the important global hashtables.  Principally for debugging."""
    function_names = fn_var_infos.keys()
    function_names.sort()
    for fn_name in function_names:
        print "==============================================================================="
	print fn_name, map(lambda vi: vi.name, fn_var_infos[fn_name]), fn_samples[fn_name], "samples"
	vals = fn_var_values[fn_name]
        # Also consider regular sorting, which orders the tuples rather
        # than their counts -- might make the patterns more evident.
        vals_items = vals.items()
        vals_items.sort(lambda a,b: cmp(a[1], b[1])) # sort by second item
        vals_items.reverse()
	for this_item in vals_items:
	    print "%5d: " % this_item[1], this_item[0]


###
### Cubist
###

## Don't delete this!  But don't run it without fixing it first.
## Not compatible with new data layout; bit rot must be corrected before
## this can actually be run.
# def print_cubist_files():
#     """Create files for Cubist experiments."""
#     function_names = fn_var_infos.keys()
#     function_names.sort()
#     for fn_name in function_names:
#         names = fn_var_infos[fn_name]
#         columns = len(names)
#         if columns > 1:
#             names = list(names)
#             for i in range(0, len(names)):
#                 names[i] = string.replace(names[i], ".", " ")
# 
#             dataname = fn_name + ".data"
#             fdata = open(dataname, "w")
#             vals = fn_var_values[fn_name]
#             # print "vals:", vals
#             for this_tuple in vals.keys():
#                 # print "this_tuple:", this_tuple
#                 fdata.write(string.join(map(repr, this_tuple), ", "))
#                 fdata.write("\n")
#             fdata.close()
# 
#             for col in range(0, columns):
#                 basename = "%s-%d" % (fn_name, col+1)
#                 fnames = open(basename + ".names", "w")
#                 fnames.write(names[col] + ".\n\n")
#                 assert len(names) == columns
#                 for name in names:
#                     fnames.write("%s: continuous.\n" % name)
#                 fnames.close()
# 
#                 this_dataname = basename + ".data"
#                 if os.path.exists(this_dataname):
#                     os.remove(this_dataname)
#                 os.symlink(dataname, this_dataname)
# 
# def run_cubist():
#     """Run cubist on the files created by `print_cubist_files'."""
#     function_names = fn_var_infos.keys()
#     function_names.sort()
#     for fn_name in function_names:
#         names = fn_var_infos[fn_name]
#         columns = len(names)
#         if columns > 1:
#             for col in range(0, columns):
#                 basename = "%s-%d" % (fn_name, col+1)
#                 # This path is now wrong, as I've moved the program.
#                 os.system("rm -f %s.out; /homes/gws/mernst/tmp/CubistR1/bin/cubistdemo -f %s > %s.out" % (basename, basename, basename))


###########################################################################
### Invariants -- numeric
###

## An invariant may be exact or approximate.  If an invariant is exact,
## then supersets of the variables in it are not supplied to higher-arity
## invariants.  For instance, if the variables are w,x,y,z, and an exact
## invariant is found over x, then only the three pairs (w,y), (w,z), and
## (y,z) are checked for two_scalar_numeric_invariant.

def all_numeric_invariants(fn_regexp=None):
    """Compute and print all the numeric invariants."""
    fn_regexp = util.re_compile_maybe(fn_regexp, re.IGNORECASE)

    assert not file_fn_var_values_invalid_modinkey(file_fn_var_values)
    assert not fn_var_values_invalid_modinkey(fn_var_values)

    clear_invariants(fn_regexp)

    # if collect_stats:
    #     collect_pre_derive_data()
    #     engine_begin_time = time.clock()
    #     engine_begin_time_wall = time.time()

    fn_names = fn_var_infos.keys()
    fn_names.sort()
    for fn_name in fn_names:
        if fn_regexp and not fn_regexp.search(fn_name):
            continue
        # if collect_stats:
        #     begin_fn_timing(fn_name)

        # If we discover that two values are equal, then there is no sense
        # in using both at any later stage; eliminate one of them and so
        # avoid redundant computation.
        # So don't derive anything from a variable until we've checked it
        # for equality with everything else (ie, until we have inferred
        # invariants over it).

        var_infos = fn_var_infos[fn_name]
        var_values = fn_var_values[fn_name]
        if var_values == {}:
            # print "No values for function", fn_name
            continue

        assert not var_values_invalid_modinkey(var_values)

        # Avoid repeated variable derivation:
        #  * it happens every time we call all_numeric_invariants; we don't
        #    want it to happen multiple times.
        #  * after derivation, do bad things happen if we try to read new
        #    values from files?
        # The solution here:
        #  * remember which program points have been derived from and don't
        #    re-derive them.  I can't conveniently
        #    read new data trace values (can I?).
        # Another approach:
        #  * add a function called in the same contexts as clear_invariants
        #    which eliminates all derived variables.

        if fn_derived_from.has_key(fn_name):
            # Don't do any variable derivation, only invariant inference
            numeric_invariants_over_index(
                range(0, len(var_infos)), var_infos, var_values)
        else:
            # Perform variable derivation as well as invariant inference
            fn_derived_from[fn_name] = true

            derivation_functions = (None, pass1_functions, pass2_functions)
            derivation_passes = len(derivation_functions)-1
            # First number: invariants are computed up to this index, non-inclusive
            # Remaining numbers: values have been derived from up to these indices
            derivation_index = (0,) * (derivation_passes+1)

            # invariant:  len(var_infos) >= invariants_index >= derivation_index[0]
            #   >= derivation_index[1] >= ...

            while derivation_index[-1] < len(var_infos):
                assert util.sorted(derivation_index, lambda x,y:-cmp(x,y))
                for i in range(0,derivation_index[1]):
                    vi = var_infos[i]
                    assert (not vi.type.is_array()) or vi.derived_len != None or not vi.is_canonical() or vi.invariant.can_be_None or vi.is_derived
                if debug_derive:
                    print "old derivation_index =", derivation_index, "num_vars =", len(var_infos)

                # If derivation_index == (a, b, c) and n = len(var_infos), then
                # the body of this loop:
                #     * computes invariants over a..n
                #     * does pass1 introduction for b..a
                #     * does pass2 introduction for c..b
                # and afterward, derivation_index == (n, a, b).

                # original number of vars; this body may well add more
                num_vars = len(var_infos)
                if derivation_index[0] != num_vars:
                    numeric_invariants_over_index(
                        range(derivation_index[0], num_vars), var_infos, var_values)

                for pass_no in range(1,derivation_passes+1):
                    if debug_derive:
                        print "pass", pass_no, "range", derivation_index[pass_no], derivation_index[pass_no-1]
                    if derivation_index[pass_no] == derivation_index[pass_no-1]:
                        continue
                    introduce_new_variables_one_pass(
                        var_infos, var_values,
                        range(derivation_index[pass_no], derivation_index[pass_no-1]),
                        derivation_functions[pass_no])

                derivation_index = (num_vars,) + derivation_index[:-1]
                if debug_derive:
                    print "new derivation_index =", derivation_index, "num_vars =", len(var_infos)


        assert len(var_infos) == len(var_values.keys()[0])

        print "==========================================================================="
        print_invariants_ppt(fn_name)

        # if collect_stats:
        #     end_fn_timing(fn_name)

    print "==========================================================================="

    # if collect_stats:
    #     engine_end_time = time.clock()
    #     engine_end_time_wall = time.time()
    #     collect_post_derive_data()
    #     print_stats(engine_begin_time, engine_end_time, engine_begin_time_wall, engine_end_time_wall)
    # print_invariants(fn_regexp)

## Testing:
# all_numeric_invariants()


def numeric_invariants_over_index(indices, var_infos, var_values):
    """Install invariants in VAR_INFOS.
    The installed invariants will all have at least one element in INDICES.
    VAR_INFOS and VAR_VALUES are elements of globals `fn_var_infos' and
    `fn_var_values'."""

    assert not var_values_invalid_modinkey(var_values)

    if indices == []:
        return
    if var_values == {}:
        # This function was never executed
        return

    if debug_infer:
        print "numeric_invariants_over_index", indices
        for i in indices:
            print var_infos[i]

    # (Intentionally) ignores anything added by body,
    # though probably nothing should be added by this body.
    index_limit = len(var_infos)
    # all_indices = range(0, len(var_infos))

    assert len(var_infos) == len(var_values.keys()[0])

    # Single invariants
    dicts = dict_of_tuples_modinkey_to_tuple_of_dicts(var_values, indices)
    non_exact_single_invs = []      # list of indices
    for j in range(0, len(indices)):
        i = indices[j]
        this_var_info = var_infos[i]
        this_dict = dicts[j]
        if (this_var_info.type.is_array()
            and (this_var_info.type.dimensionality > 1)):
            # One-dimensional non-numeric array
            this_inv = invariant(this_dict, (this_var_info,))
        elif this_var_info.type.is_array():
            this_inv = single_sequence_numeric_invariant(this_dict, (this_var_info,))
        else:
            this_inv = single_scalar_numeric_invariant(this_dict, (this_var_info,))
        assert this_var_info.invariant == None
        assert this_inv != None
        # print "Setting invariant for index", i, "to", this_inv
        this_var_info.invariant = this_inv

    if debug_infer:
        print "numeric_invariants_over_index: done with single invariants, starting pairs"
        for j in indices:
            print var_infos[i].invariant

    # Invariant pairs
    ## Don't do this; the large list of pairs can exhaust memory, though
    ## the code is cleaner in that case.
    # for (i1,i2) in util.choose(2, all_indices):
    for i1 in range(0,index_limit-1):
        vi1 = var_infos[i1]
        if not vi1.is_canonical():
            continue
        inv1 = vi1.invariant
        if inv1.can_be_None:
            continue
        ## Old for loop
        # for i2 in range(i1+1, index_limit):
        if i1 in indices:
            i2_range = range(i1+1, index_limit)
        else:
            i2_range = filter(lambda i2, lim=i1: i2>lim, indices)
        for i2 in i2_range:
            assert i1 != i2             # implied by the for loop
            assert (i1 in indices) or (i2 in indices) # implied by the for loop
            vi2 = var_infos[i2]
            if not vi2.is_canonical():
                continue
            if not vi1.type.compatible(vi2.type):
                continue
            if not lackwit_types_compatible(vi1.name, vi1.lackwit_type, vi2.name, vi2.lackwit_type):
                continue
            inv2 = vi2.invariant
            if inv2.can_be_None:
                # Do nothing if either of the variables can be missing.
                continue
            ## I guess I don't want to derive from exact variables,
            ## but why not infer invariants over them?  The justification
            ## for commenting this out is to make sure we recognize that
            ## two values, both always the constant zero, are equal (and
            ## we set one of them as non-canonical).
            # Do nothing if either of the variables is a constant.
            # if inv1.is_exact():
            #     continue
            # if inv2.is_exact():
            #     continue
            if inv1.is_exact() and inv2.is_exact():
                # assert inv1.min == inv1.max
                # assert inv2.min == inv2.max
                assert inv1.values == 1
                assert inv2.values == 1
                # if inv1.min == inv2.min:
                if inv1.one_of[0] == inv2.one_of[0]:
                    vi1.equal_to.append(i2)
                    vi2.equal_to.append(i1)
                continue
            if inv1.is_exact() or inv2.is_exact():
                continue

            values = dict_of_tuples_slice_2(var_values, i1, i2)
            # These are now set earlier on, so they can be reused more.
            # (vi1, vi2) = util.slice_by_sequence(var_infos, (i1,i2))
            if vi1.is_sequence() and vi2.is_sequence():
                this_inv = two_sequence_numeric_invariant(values, (vi1, vi2))
            elif vi1.is_sequence() or vi2.is_sequence():
                this_inv = scalar_sequence_numeric_invariant(values, (vi1, vi2))
            else:
                this_inv = two_scalar_numeric_invariant(values, (vi1, vi2))

            assert not vi1.invariants.has_key(i2)
            vi1.invariants[i2] = this_inv
            if ((isinstance(this_inv, two_scalar_numeric_invariant)
                 or isinstance(this_inv, two_sequence_numeric_invariant))
                and (this_inv.comparison == "=")):
                vi1.equal_to.append(i2)
                vi2.equal_to.append(i1)

    if debug_infer:
        print "numeric_invariants_over_index: done with pairs, starting triples"

    # Invariant triples
    if no_ternary_invariants == true:
        return

    ## Don't do this; the large list of triples can exhaust memory, though
    ## the code is cleaner in that case.
    # for (i1,i2,i3) in util.choose(3, all_indices):

    for i1 in range(0,index_limit-2):
        # print "triples: index1 =", i1
        vi1 = var_infos[i1]
        if vi1.invariant.is_exact():
            continue
        if vi1.invariant.can_be_None:
            continue
        if not vi1.is_canonical():
            continue
        for i2 in range(i1+1, index_limit-1):
            # # print "triples: index2 =", i2
            assert i1 != i2
            if (vi1.invariants.has_key(i2)
                and vi1.invariants[i2].is_exact()):
                continue
            vi2 = var_infos[i2]
            if not vi2.is_canonical():
                continue
            if not vi1.type.compatible(vi2.type):
                continue
            if vi2.invariant.is_exact():
                continue
            if vi2.invariant.can_be_None:
                continue
            ## Old for loop
            # for i3 in range(i2+1, index_limit):
            if i1 in indices or i2 in indices:
                i3_range = range(i2+1, index_limit)
            else:
                i3_range = filter(lambda i3, lim=i2: i3>lim, indices)
            for i3 in i3_range:
                assert (i1 in indices) or (i2 in indices) or (i3 in indices)
                assert i1 != i3 and i2 != i3

                vi3 = var_infos[i3]
                if not vi3.is_canonical():
                    continue
                if not (vi1.type.compatible(vi3.type)
                        and vi2.type.compatible(vi3.type)):
                    continue
                if vi3.invariant.is_exact():
                    # Do nothing if any of the variables is a constant.
                    continue
                if ((vi1.invariants.has_key(i3)
                     and vi1.invariants[i3].is_exact())
                    or (vi2.invariants.has_key(i3)
                        and vi2.invariants[i3].is_exact())):
                    # Do nothing if any of the pairs are exactly related.
                    continue
                if vi3.invariant.can_be_None:
                    # Do nothing if any of the variables can be missing.
                    continue

                values = dict_of_tuples_slice_3(var_values, i1, i2, i3)
                # These are now set earlier, above.
                # (vi1, vi2, vi3) = util.slice_by_sequence(var_infos, (i1,i2,i3))
                if (vi1.is_sequence()
                    or vi2.is_sequence()
                    or vi3.is_sequence()):
                    # print "got sequence in a triple"
                    this_inv = invariant(values, (vi1, vi2, vi3))
                else:
                    this_inv = three_scalar_numeric_invariant(values, (vi1, vi2, vi3))
                assert not vi1.invariants.has_key((i2,i3))
                vi1.invariants[(i2,i3)] = this_inv


### A lot of the logic here should be moved into invariant computation;
### then printing should just print all the existing invariants, or consult
### some other data structure regarding whether to print invariants.  (???)

def print_invariants(fn_regexp=None, print_unconstrained=0):
    """Print out non-unconstrained invariants.
    If optional second argument print_unconstrained is true,
    then print all invariants."""

    fn_regexp = util.re_compile_maybe(fn_regexp, re.IGNORECASE)

    # print "print_invariants", fn_regexp and fn_regexp.pattern

    function_names = fn_var_infos.keys()
    function_names.sort()
    for fn_name in function_names:
        if fn_regexp and not fn_regexp.search(fn_name):
            # if fn_regexp:  print fn_name, "does not match", fn_regexp.pattern
            continue
        print "==========================================================================="
        print_invariants_ppt(fn_name, print_unconstrained)
    print "==========================================================================="


def print_invariants_ppt(fn_name, print_unconstrained=0):
    """Print invariants for a single program point."""
    print fn_name, fn_samples[fn_name], "samples"

    var_infos = fn_var_infos[fn_name]

    if (display_all_vars):
        print "  variables: ",
        for vi in var_infos:
            print vi.name,
        print

    # Equality invariants
    for vi in var_infos:
        if not vi.is_canonical():
            continue
        if vi.equal_to == []:
            # Not equal to anything else, so not an equality invariant
            continue
        if vi.invariant.is_exact():
            # was vi.invariant.min
            value = "= %s" % (vi.invariant.one_of[0],) # this value may be a sequence
        else:
            value = ""
        print vi.name, "=", string.join(map(lambda idx, vis=var_infos: vis[idx].name, vi.equal_to), " = "), value,
        if vi.invariant.values == 1:
            print "\t(1 value)"
        else:
            print "\t(%d values)" % (vi.invariant.values,)

    # Single invariants
    for vi in var_infos:
        if not vi.is_canonical():
            continue
        this_inv = vi.invariant
        if (this_inv.is_exact() and vi.equal_to != []):
            # Already printed above in "equality invariants" section
            continue
        if print_unconstrained or not this_inv.is_unconstrained():
            print " ", this_inv.format((vi.name,))
    # Pairwise invariants
    nonequal_constraints = []
    # Maybe this is faster than calling "string.find"; I'm not sure.
    nonequal_re = re.compile(" != ")
    for vi in var_infos:
        if not vi.is_canonical():
            continue
        vname = vi.name
        for (index,inv) in vi.invariants.items():
            if type(index) != types.IntType:
                continue
            if not var_infos[index].is_canonical():
                continue
            if (not print_unconstrained) and inv.is_unconstrained():
                continue
            formatted = inv.format((vname, var_infos[index].name))
            # Not .match(...), which only checks at start of string!
            if nonequal_re.search(formatted):
                nonequal_constraints.append(formatted)
            else:
                print "   ", formatted
    for ne_constraint in nonequal_constraints:
        print "    ", ne_constraint
    # Three-way (and greater) invariants
    for vi in var_infos:
        if not vi.is_canonical():
            continue
        vname = vi.name
        for (index_pair,inv) in vi.invariants.items():
            if type(index_pair) == types.IntType:
                continue
            (i1, i2) = index_pair
            if not var_infos[i1].is_canonical():
                # Perhaps err; this shouldn't happen, right?
                continue
            if not var_infos[i2].is_canonical():
                # Perhaps err; this shouldn't happen, right?
                continue
            if print_unconstrained or not inv.is_unconstrained():
                print "     ", inv.format((vname, var_infos[i1].name, var_infos[i2].name))
    sys.stdout.flush()


###########################################################################
### Invariants -- single scalar
###            


class invariant:
    # Instance variables:
    #   one_of                   # sorted list of 5 or fewer distinct values
    #   values                   # number of distinct values; perhaps
                                 #   maintain this as a range rather
                                 #   than an exact number...  [Why?]
    #   samples                  # number of samples; >= values
    #   mod_samples                  # number of samples; >= values
    #   can_be_None              # only really sensible for single
                                 #   invariants, not those over pairs, etc. (?)
                                 #   Perhaps this should be a count.
    #   unconstrained_internal   # None, true, or false:  cached value for
                                 #   is_unconstrained(), computed by format()
    #   var_infos                # list of var_info objects, used only to
                                 #   provide var names for default formatting.
                                 #   Often not provided at all.

    def __init__(self, dict, var_infos):
        """DICT maps from values to number of occurrences."""
        vals = dict.keys()
        # if var_infos == None:
        #     # This is OK, but I need to be careful
        #     print "var_infos == None"

        self.var_infos = var_infos
        self.values = len(vals)
        samples = 0
        mod_samples = 0
        for (s, ms) in dict.values():
            samples = samples + s
            mod_samples = mod_samples + ms
        self.samples = samples
        self.mod_samples = mod_samples
        self.can_be_None = None in vals
        # if len(vals) < 5 and not self.can_be_None:
        if len(vals) < 5:
            vals.sort()
            self.one_of = vals
        else:
            self.one_of = None
        self.unconstrained_internal = None

    def __setstate__(self, state):
        for key in state.keys():
            setattr(self, key, state[key])

    def is_exact(self):
        return self.values == 1

    def is_unconstrained(self):
        if self.unconstrained_internal == None:
            self.format()
        return self.unconstrained_internal

    def format(self, args=None):
        """ARGS is uninterpreted.
        This function can return None:  it's intended to be used only as a helper.
        Any overriding implementation of this function should set the
        unconstrained_internal class-local variable.  Since this function sets it,
        too, callers of this function should be careful to do their manipulation
        after any call to this base method.
        The format function should be able to take no extra arguments, in which
        case it supplies default variable names.
        """

        if self.samples == 0:
            self.unconstrained_internal = false
            return None

        self.unconstrained_internal = false

        if args == None:
            args = map(lambda x: x.name, self.var_infos)
        if (type(args) in [types.ListType, types.TupleType]) and (len(args) == 1):
            args = args[0]

        if self.one_of:
            # If it can be None, print it only if it is always None and
            # is an invariant over non-derived variable.
            if self.can_be_None:
                if ((len(self.one_of) == 1)
                    and self.var_infos):
                    some_nonderived = false
                    for vi in self.var_infos:
                    	some_nonderived = some_nonderived or not vi.is_derived
                    if some_nonderived:
                        return "%s = uninit" % (args,)
            elif len(self.one_of) == 1:
                return "%s = %s" % (args, self.one_of[0])
            ## Perhaps I should unconditionally return this value;
            ## otherwise I end up printing ranges more often than small
            ## numbers of values (because when few values and many samples,
            ## the range always looks justified).
            # If few samples, don't try to infer a function over the values;
            # just return the list.
            elif (len(self.one_of) <= 3) or (self.samples < 100):
                return "%s in %s" % (args, util.format_as_set(self.one_of))
        self.unconstrained_internal = true
        return None

    # Possibly add an optional "args=None" argument, for formatting.
    def diff(self, other):
        """Returns None or a description of the difference."""
        # print "diff(invariant)"
        global inv_one_cons, inv_diff_small_no_vals, inv_one_none, ssc_miss_min, ssc_min_diff, ssc_miss_max, ssc_max_diff, ssc_one_can_be_zero, ssc_diff_mod, ssc_diff_nonmod, tsc_diff_lin_reln, tsc_one_equal, tsc_comparison_diff, tsc_diff_num_diff, tsc_diff_sum, tsc_diff_ftn_reln, tsc_diff_inv_ftn_reln, sseq_diff_elem_equality, sseq_diff_sortedness, sseq_diff_inv_all_elem, scseq_diff_membership, two_seq_diff_lin_reln, two_seq_one_equ, two_seq_diff_comp, two_seq_diff_subseq, two_seq_diff_supseq, two_seq_diff_revness, diff_to_ct
        inv1 = self
        inv2 = other
        assert inv1.__class__ == inv2.__class__
        if inv1.is_unconstrained() and inv2.is_unconstrained():
            return None
        if inv1.is_unconstrained() ^ inv2.is_unconstrained():
            diff_to_ct[inv_one_cons] = diff_to_ct[inv_one_cons] + 1
            return "One is unconstrained but the other is not"
        if inv1.one_of and inv2.one_of and inv1.one_of != inv2.one_of:
            diff_to_ct[inv_diff_small_no_vals] = diff_to_ct[inv_diff_small_no_vals] + 1
            return "Different small number of values"
        if inv1.can_be_None ^ inv2.can_be_None:
            diff_to_ct[inv_one_none] = diff_to_ct[inv_one_none] + 1
            return "One can be None but the other cannot"
        # return "invariant.diff: no differences"	# debugging
        return None


class single_scalar_numeric_invariant(invariant):
    # Instance variables:
    #   min
    #   max
    #   can_be_zero              # only interesting if range includes zero
    #   modulus
    #   nonmodulus
    #   min_justified
    #   max_justified
    #   nonnegative_obvious

    def __init__(self, dict, var_infos):
        """DICT maps from values to number of occurrences."""
        invariant.__init__(self, dict, var_infos)
        nums = dict.keys()
        nums.sort()
        if ((nums == [])
            or ((var_infos != None)
                and (var_infos[0].type.is_array()
                     or var_infos[0].type.base != "int"))
            or ((type(nums[0]) != types.IntType))):
            self.min = None
            self.max = None
        else:
            self.min = nums[0]
            self.max = nums[-1]
        # For when we didn't sort nums
        # self.min = min(nums)
        # self.max = max(nums)
        self.min_justified = false
        self.max_justified = false
        # Watch out: "None" sorts greater than any number
        # (but less than any string ?!).
        if self.max == None:
            self.min = None
        elif len(nums) < 3:
            self.min_justified = true
            self.max_justified = true
        else:
            # Accept a max/min if:
            #  * it contains more than twice as many elements as it ought to by
            #    chance alone, and that number is at least 3.
            #  * it and its predecessor/successor both contain more than half
            #    as many elements as they ought to by chance alone, and at
            #    least 3.
            (count_min,mod_min) = dict[self.min]
            (count_max,mod_max) = dict[self.max]
            try:
                range = self.max - self.min + 1
            except OverflowError:
                range = util.maxint
            twice_avg_num = 2.0*self.values/range
            half_avg_num = .5*self.values/range
            if ((mod_min >= 3)
                and ((mod_min > twice_avg_num)
                     or ((mod_min > half_avg_num) and (dict[nums[1]] > half_avg_num)))):
                self.min_justified = true
            if ((mod_max >= 3)
                and ((mod_max > twice_avg_num)
                     or ((mod_max > half_avg_num) and (dict[nums[-2]] > half_avg_num)))):
                self.max_justified = true
            # print "min (%d) justified=%d: %d min elts, %d adjacent" % (self.min, self.min_justified, mod_min, dict[nums[1]])
            # print "max (%d) justified=%d: %d max elts, %d adjacent" % (self.max, self.max_justified, mod_max, dict[nums[-2]])
        self.nonnegative_obvious = (self.var_infos != None) and ("size(" == self.var_infos[0].name[0:5])

        self.can_be_zero = (0 in nums)
        if self.min != None:
            self.modulus = util.common_modulus(nums)
            ## Too many false positives
            # self.nonmodulus = util.common_nonmodulus_nonstrict(nums)
            self.nonmodulus = util.common_nonmodulus_strict(nums)
        else:
            self.modulus = None
            self.nonmodulus = None


    def __setstate__(self, state):
        for key in state.keys():
            setattr(self, key, state[key])


    ## Can do no more than the parent can
    #     def is_exact(self):
    #         if invariant.is_exact(self):
    #             return true

    # In next three functions, use log in computations to avoid
    # overflow-(in this case, potentially very small numbers)
    def nonzero_justified(self):
        if (self.min == None) or (self.min >= 0) or (self.max <= 0) or not self.can_be_zero:
            return false
        probability = 1 - 1.0/(self.max - self.min + 1)
        #return probability**self.samples < negative_invariant_confidence
        return self.samples*math.log(probability) < math.log(negative_invariant_confidence)

    def modulus_justified(self):
        probability = 1.0/self.modulus[1]
        #return probability**self.samples < negative_invariant_confidence
        return self.samples*math.log(probability) < math.log(negative_invariant_confidence)

    def nonmodulus_justified(self):
        base = self.nonmodulus[1]
        probability = 1 - 1.0/base
        #return probability**self.samples * base < negative_invariant_confidence
        return self.samples*math.log(probability) + math.log(base) < math.log(negative_invariant_confidence)



    # This doesn't produce a readable expression as is the convention, but
    # neither is the default
    # "<invariants.single_scalar_numeric_invariant instance at 11bdf8>"
    def __repr__(self):
        result = "<invariant-1: "
        if self.one_of:
            result = result + "in %s, " % util.format_as_set(self.one_of)
        if self.min == None:
            min_rep = ""
        else:
            min_rep = `self.min`
        if self.max == None:
            max_rep = ""
        else:
            max_rep = `self.max`
        result = result + "[%s..%s], " % (min_rep, max_rep)
        result = result + "zero? %d, " % self.can_be_zero
        if self.modulus:
            result = result + "modulus: %s" % (self.modulus,)
        if self.nonmodulus:
            result = result + "nonmodulus: %s" % (self.nonmodulus,)
        result = result + "%s values, %s samples" % (self.values, self.samples)
        result = result + ">"
        return result

    def __str__(self):
        return self.format()

    def format(self, arg_tuple=None):
        if arg_tuple == None:
            if self.var_infos:
                arg = self.var_infos[0].name
            # not sure whether this is the right thing, but oh well
            else:
                arg = "var"
        else:
            (arg,) = arg_tuple

        as_base = invariant.format(self, arg)
        if as_base:
            return as_base
        self.unconstrained_internal = false

        suffix = " \t(%d values" % (self.values,)
        if self.can_be_None:
            suffix = suffix + ", can be None)"
        else:
            suffix = suffix + ")"

        if self.modulus and self.modulus_justified():
            return arg + " = %d (mod %d)" % self.modulus + suffix
        elif self.nonmodulus and self.nonmodulus_justified():
            return arg + " != %d (mod %d)" % self.nonmodulus + suffix

        nonzero = ((self.min < 0) and (self.max > 0)
                   and (not self.can_be_zero) and self.nonzero_justified())

        if self.min_justified and self.max_justified:
            result = " in [%s..%s]" % (self.min, self.max)
            if nonzero:
                result = " nonzero" + result
            return arg + result + suffix
        if self.min_justified and (self.min != 0 or not self.nonnegative_obvious):
            result = "%s >= %s" % (arg, self.min)
            if nonzero:
                result = result + " and nonzero"
            return result + suffix
        if self.max_justified:
            result = "%s <= %s" % (arg, self.max)
            if nonzero:
                result = result + " and nonzero"
            return result + suffix
        if nonzero:
            return arg + "!= 0" + suffix

        if self.one_of and not self.can_be_None:
            return "%s in %s" % (arg, util.format_as_set(self.one_of))

        self.unconstrained_internal = true
        return arg + " unconstrained" + suffix

    def diff(self, other):
        # print "diff(single_scalar_numeric_invariant)"
        global inv_one_cons, inv_diff_small_no_vals, inv_one_none, ssc_miss_min, ssc_min_diff, ssc_miss_max, ssc_max_diff, ssc_one_can_be_zero, ssc_diff_mod, ssc_diff_nonmod, tsc_diff_lin_reln, tsc_one_equal, tsc_comparison_diff, tsc_diff_num_diff, tsc_diff_sum, tsc_diff_ftn_reln, tsc_diff_inv_ftn_reln, sseq_diff_elem_equality, sseq_diff_sortedness, sseq_diff_inv_all_elem, scseq_diff_membership, two_seq_diff_lin_reln, two_seq_one_equ, two_seq_diff_comp, two_seq_diff_subseq, two_seq_diff_supseq, two_seq_diff_revness, diff_to_ct
        inv1 = self
        inv2 = other

        # If they print the same, then make them compare the same
        if diffs_same_format(inv1, inv2):
            return None

        as_base = invariant.diff(inv1, inv2)
        if as_base:
            return as_base

        min_missing = ((inv1.min_justified and not inv2.min_justified)
                       or (inv2.min_justified and not inv1.min_justified))
        min_different = (inv1.min_justified and inv2.min_justified
                         and inv1.min != inv2.min)
        max_missing = ((inv1.max_justified and not inv2.max_justified)
                       or (inv2.max_justified and not inv1.max_justified))
        max_different = (inv1.max_justified and inv2.max_justified
                         and (inv1.max != inv2.max))
        # print "max_different=%s" % (max_different,), inv1.max_justified, inv2.max_justified, inv1.max, inv2.max
        nzj1 = inv1.nonzero_justified()
        nzj2 = inv1.nonzero_justified()
        zero_different = (nzj1 and not nzj2) or (nzj2 and not nzj1)

        modulus_different = (inv1.modulus != inv2.modulus)
        nonmodulus_different = (inv1.nonmodulus != inv2.nonmodulus)

        result = []
        if min_missing:
            result.append("Missing minimum")
            diff_to_ct[ssc_miss_min] = diff_to_ct[ssc_miss_min] + 1
        if min_different:
            result.append("Different minimum")
            diff_to_ct[ssc_min_diff] = diff_to_ct[ssc_min_diff] + 1
        if max_missing:
            result.append("Missing maximum")
            diff_to_ct[ssc_miss_max] = diff_to_ct[ssc_miss_max] + 1
        if max_different:
            result.append("Different maximum")
            diff_to_ct[ssc_max_diff] = diff_to_ct[ssc_max_diff] + 1
        if zero_different:
            result.append("One can't be zero but other can")
            diff_to_ct[ssc_one_can_be_zero] = diff_to_ct[ssc_one_can_be_zero] + 1
        if modulus_different:
            result.append("Different modulus")
            diff_to_ct[ssc_diff_mod] = diff_to_ct[ssc_diff_mod] + 1
        if nonmodulus_different:
            result.append("Different nonmodulus")
            diff_to_ct[ssc_diff_nonmod] = diff_to_ct[ssc_diff_nonmod] + 1
        if result == []:
            return None
        return string.join(result, ", ")


# single_scalar_numeric_invariant(dict_of_tuples_modinkey_to_tuple_of_dicts(fn_var_values["PUSH-ACTION"])[0])




###########################################################################
### Invariants -- multiple scalars
###            

## Need to add code like this to __init__ of all invariants over multiple values.
#         if (not self.can_be_None) and (type(self.values[0]) == types.TupleType):
#             for val in vals:
#                 if None in val:
#                     self.can_be_None = true
#		      break

## For now, only look for perfectly-satisfied properties; deal with
## exceptions, disjunctions, and predicated properties later.


# Don't pass in a tuple plus two indices, because I have to aggregate the
# counts anyway.  Or maybe that isn't such a concern and it is more
# efficient to only aggregate the counts if everything looks good on other
# grounds.

class two_scalar_numeric_invariant(invariant):

    # Instance variables:
    #  linear                       # can be pair (a,b) such that y=ax+b
    #  comparison                   # can be "=", "<", "<=", ">", ">="
    #  comparison_obvious           # values like comparison slot
    #  can_be_equal
    #  difference_invariant
    #  sum_invariant
    #  functions                    # list of functions such that y=fun(x)
    #  inv_functions                # list of functions such that x=fun(y)

    # When there is a known invariant for one of the elements (eg, it's
    # constant), the pairwise invariant may not be interesting (though
    # equality can be).
    def __init__(self, dict_of_pairs, var_infos):
        """DICT maps from a pair of values to number of occurrences."""
        invariant.__init__(self, dict_of_pairs, var_infos)

        pairs = dict_of_pairs.keys()

        ## Now we use the single-scalar invariants
        ## instead of maintaining these separately here.
        # # Range
        # a_nums = map(lambda x: x[0], pairs)
        # self.a_min = min(a_nums)
        # self.a_max = max(a_nums)
        # b_nums = map(lambda x: x[1], pairs)
        # self.b_min = min(b_nums)
        # self.b_max = max(b_nums)

        (self.comparison, self.can_be_equal) = compare_pairs(pairs)

        (var1, var2) = (var_infos[0].name, var_infos[1].name)
        (type1, type2) = (var_infos[0].type, var_infos[1].type)

        if (type1 != int_proglang_type) or (type2 != int_proglang_type):
            self.linear = None
            self.difference_invariant = None
            self.sum_invariant = None
            self.comparison_obvious = None
            self.functions = None
            self.inv_functions = None
            return

        ## Linear relationship -- try to fit y = ax + b.
        # Should I also try x = ay + b?  I do not plan to call this with
        # the arguments reversed, so that is a reasonable idea.
        try:
            if len(pairs) > 1:
                (a,b) = bi_linear_relationship(pairs[0], pairs[1])
                for (x,y) in pairs:
                    if y != a*x+b:
                        self.linear = None
                        break
                else:
                    self.linear = (a,b)
        except OverflowError:
            self.linear = None

        ## Find invariant over x-y; this can be more exact than "x<y".
        diff_dict = {}
        sum_dict = {}
        for ((x,y),(count,modified)) in dict_of_pairs.items():
            if diff_dict:
                try:
                    x_y_diff = x-y
                    this_counts = diff_dict.get(x_y_diff, [0,0])
                    diff_dict[x_y_diff] = this_counts
                    this_counts[0] = this_counts[0] + count
                    this_counts[1] = this_counts[1] + modified
                except OverflowError:
                    diff_dict = None
            if sum_dict:
                try:
                    x_y_sum = x+y
                    this_counts = sum_dict.get(x_y_sum, [0,0])
                    sum_dict[x_y_sum] = this_counts
                    this_counts[0] = this_counts[0] + count
                    this_counts[1] = this_counts[1] + modified
                except OverflowError:
                    diff_dict = None
        if not diff_dict:
            diff_dict = {}
        if not sum_dict:
            sum_dict = {}
        self.difference_invariant = single_scalar_numeric_invariant(diff_dict, None)
        self.sum_invariant = single_scalar_numeric_invariant(sum_dict, None)

        self.comparison_obvious = None
        # These variables are set to the name of the sequence, or None.
        # Avoid regular expressions wherever possible.
        min1 = (var1[0:4] == "min(") and var1[4:-1]
        max1 = (var1[0:4] == "max(") and var1[4:-1]
        # I think "find" does a regexp operation, unfortunately
        aref1 = string.find(var1, "[")
        if aref1 == -1:
            aref1 = None
        else:
            aref1 = var1[0:aref1]
        if min1 or max1 or aref1:
            min2 = (var2[0:4] == "min(") and var2[4:-1]
            max2 = (var2[0:4] == "max(") and var2[4:-1]
            aref2 = string.find(var2, "[")
            if aref2 == -1:
                aref2 = None
            else:
                aref2 = var2[0:aref2]
            if min1 and max2 and min1 == max2:
                self.comparison_obvious = "<="
            elif min1 and aref2 and min1 == aref2:
                self.comparison_obvious = "<="
            elif max1 and min2 and max1 == min2:
                self.comparison_obvious = ">="
            elif max1 and aref2 and max1 == aref2:
                self.comparison_obvious = ">="
            elif aref1 and min2 and aref1 == min2:
                self.comparison_obvious = ">="
            elif aref1 and max2 and aref1 == max2:
                self.comparison_obvious = "<="

        if len(pairs) > 1:
            # Could add "int", but only interesting if it isn't always identity
            # "pos" certainly isn't interesting.
            functions = [abs, operator.neg, operator.inv]
            inv_functions = [abs, operator.neg, operator.inv]
            for (x,y) in pairs:
                for fn in functions:
                    try:
                        if y != apply(fn, (x,)):
                            # works by side effect, grrr.
                            functions.remove(fn)
                    except:
                        functions.remove(fn)
                for ifn in inv_functions:
                    try:
                        if x != apply(ifn, (y,)):
                            # works by side effect, grrr.
                            inv_functions.remove(ifn)
                    except:
                        inv_functions.remove(ifn)
                if (functions == []) and (inv_functions == []):
                    break
            self.functions = functions
            self.inv_functions = inv_functions
        else:
            self.functions = None
            self.inv_functions = None

    def __setstate__(self, state):
        for key in state.keys():
            setattr(self, key, state[key])

    def is_exact(self):
        return invariant.is_exact(self) or self.linear

    def nonequal_justified(self):
        inv1 = self.var_infos[0].invariant
        min1 = inv1.min
        max1 = inv1.max
        inv2 = self.var_infos[1].invariant
        min2 = inv2.min
        max2 = inv2.max

        if ((min1 == None) or (max1 == None) or (min2 == None) or (max2 == None)):
            # I'm not sure this is the right thing, but it's expedient
            probability = 1
        else:
            try:
                overlap = min(max1, max2) - max(min1, min2)
                if overlap < 0:
                    return false
                overlap = float(overlap + 1)
                probability = 1 - overlap/((max1 - min1 + 1) * (max2 - min2 + 1))
            except OverflowError:
                probability = 1
            # Equivalent and slower, albeit clearer
            # probability = 1 - (overlap/(max1 - min1 + 1)) * (overlap/(max2 - min2 + 1)) * (1/overlap)

        return probability**self.samples < negative_invariant_confidence

    def __repr__(self):
        result = "<invariant-2: "
        if self.linear:
            result = result + "linear: %s, " % (self.linear,) # self.linear is itself a tuple
        if self.comparison:
            result = result + "cmp: %s, " % (self.comparison,)
        result = result + "can be =: %s, " % (self.can_be_equal,)
        if self.functions:
            result = result + "functions: %s, " % (self.functions,)
        if self.inv_functions:
            result = result + "inv_functions: %s, " % (self.inv_functions,)
        result = result + ("sum: %s, diff: %s, "
                           % (self.sum_invariant, self.difference_invariant))
        result = result + "%s values, %s samples" % (self.values, self.samples)
        result = result + ">"
        return result

    def __str__(self):
        return self.format()

    def format(self, arg_tuple=None):
        if arg_tuple == None:
            arg_tuple = tuple(map(lambda x: x.name, self.var_infos))

        as_base = invariant.format(self, "(%s, %s)" % arg_tuple)
        if as_base:
            return as_base

        self.unconstrained_internal = false

        (x,y) = arg_tuple

        if self.values == 1:
            suffix = " \t(1 value)"
        else:
            suffix = " \t(%d values)" % (self.values,)

        if self.comparison == "=":
            return "%s = %s" % (x,y) + suffix
        if self.linear:
            (a,b) = self.linear
            if a == 1:
                if b < 0:
                    return "%s = %s - %s" % (y,x,abs(b)) + suffix
                else:
                    return "%s = %s + %s" % (y,x,b) + suffix
            elif b == 0:
                return "%s = %s %s" % (y,a,x) + suffix
            else:
                if b < 0:
                    return "%s = %s %s - %s" % (y,a,x,abs(b)) + suffix
                else:
                    return "%s = %s %s + %s" % (y,a,x,b) + suffix

        if self.functions or self.inv_functions:
            results = []
            if self.functions:
                for fn in self.functions:
                    results.append("%s = %s(%s)" % (y,util.function_rep(fn),x))
            if self.inv_functions:
                for fn in self.inv_functions:
                    results.append("%s = %s(%s)" % (x,util.function_rep(fn),y))
            return string.join(results, " and ") + suffix

        diff_inv = self.difference_invariant
        # Uninteresting differences:
        #  * >= 0 (x >= y), >= 1 (x > y)
        #  * <= 0 (x <= y), <= -1 (x < y)
        #  * nonzero (x != y)

        # invariant.format(diff_inv, ...) differs from diff_inv.format(...)!
        if diff_inv:
            diff_as_base = invariant.format(diff_inv, ("%s - %s" % (x,y),))

            if diff_as_base:
                return diff_as_base + suffix
            if diff_inv.modulus:
                (a,b) = diff_inv.modulus
                if a == 0:
                    return "%s = %s (mod %d)" % (x,y,b) + suffix
                else:
                    return "%s - %s = %d (mod %d)" % (x,y,a,b) + suffix
            if diff_inv.min_justified and diff_inv.max_justified:
                return "%d <= %s - %s <= %d \tjustified" % (diff_inv.min, x, y, diff_inv.max) + suffix
            if diff_inv.min_justified:
                if diff_inv.min == 0:
                    if not self.comparison_obvious == ">=":
                        return "%s <= %s \tjustified" % (y,x) + suffix
                elif diff_inv.min > 0:
                    return "%s <= %s - %d \tjustified" % (y,x,diff_inv.min) + suffix
                else:
                    assert diff_inv.min < 0
                    return "%s <= %s + %d \tjustified" % (y,x,-diff_inv.min) + suffix
            if diff_inv.max_justified:
                if diff_inv.max == 0:
                    if not self.comparison_obvious == "<=":
                        return "%s <= %s \tjustified" % (x,y) + suffix
                elif diff_inv.max > 0:
                    return "%s <= %s + %d \tjustified" % (x,y,diff_inv.max) + suffix
                else:
                    assert diff_inv.max < 0
                    return "%s <= %s - %d \tjustified" % (x,y,-diff_inv.max) + suffix

        # What can be interesting about a sum?  I'm not sure...
        sum_inv = self.sum_invariant
        if sum_inv:
            sum_as_base = invariant.format(sum_inv, ("%s + %s" % (x,y),))
            if sum_as_base:
                return sum_as_base
            if sum_inv.modulus:
                (a,b) = sum_inv.modulus
                return "%s + %s = %d (mod %d)" % (x,y,a,b) + suffix

        if self.comparison and self.comparison != self.comparison_obvious:
            if self.comparison in ["<", "<="]:
                if diff_inv and diff_inv.max and (diff_inv.max < -1):
                    suffix = " \t%s <= %s - %d" % (x, y, -diff_inv.max) + suffix
                return "%s %s %s" % (x, self.comparison, y) + suffix
            if diff_inv and diff_inv.min and (diff_inv.min > 1):
                suffix = " \t%s <= %s - %d" % (y, x, diff_inv.min) + suffix
            if self.comparison == ">":
                return "%s < %s" % (y, x) + suffix
            if self.comparison == ">=":
                return "%s <= %s" % (y, x) + suffix
            raise "Can't get here"

        if (not self.can_be_equal) and self.nonequal_justified():
            return "%s != %s" % (x,y) + suffix
        elif self.one_of:
            return "%s in %s" % ("(%s, %s)" % arg_tuple, util.format_as_set(self.one_of)) + suffix
        else:
            self.unconstrained_internal = true
            return "(%s, %s) unconstrained" % (x,y) + suffix

    def diff(self, other):
        # print "diff(two_scalar_numeric_invariant)"
        global inv_one_cons, inv_diff_small_no_vals, inv_one_none, ssc_miss_min, ssc_min_diff, ssc_miss_max, ssc_max_diff, ssc_one_can_be_zero, ssc_diff_mod, ssc_diff_nonmod, tsc_diff_lin_reln, tsc_one_equal, tsc_comparison_diff, tsc_diff_num_diff, tsc_diff_sum, tsc_diff_ftn_reln, tsc_diff_inv_ftn_reln, sseq_diff_elem_equality, sseq_diff_sortedness, sseq_diff_inv_all_elem, scseq_diff_membership, two_seq_diff_lin_reln, two_seq_one_equ, two_seq_diff_comp, two_seq_diff_subseq, two_seq_diff_supseq, two_seq_diff_revness, diff_to_ct
        inv1 = self
        inv2 = other

        # If they print the same, then make them compare the same
        if diffs_same_format(inv1, inv2):
            return None

        as_base = invariant.diff(inv1, inv2)
        if as_base:
            return as_base

        linear_different = (inv1.linear != inv2.linear)
        equal_different = (inv1.can_be_equal != inv2.can_be_equal)

        # Just the first character; ignore equality differences
        comp1 = inv1.comparison
        comp1 = comp1 and comp1[0]
        comp2 = inv2.comparison
        comp2 = comp2 and comp2[0]

        comparison_different = (comp1 != comp2)

        difference_difference = inv1.difference_invariant.diff(inv2.difference_invariant)
        sum_difference = inv1.sum_invariant.diff(inv2.sum_invariant)

        functions_different = (inv1.functions != inv2.functions)
        inv_functions_different = (inv1.inv_functions != inv2.inv_functions)

        result = []
        if linear_different:
            result.append("Different linear reln")
            diff_to_ct[tsc_diff_lin_reln] = diff_to_ct[tsc_diff_lin_reln] + 1
        if equal_different:
            result.append("One can be equal but other can't")
            diff_to_ct[tsc_one_equal] = diff_to_ct[tsc_one_equal] + 1
        if comparison_different:
            result.append("Different comparison")
            diff_to_ct[tsc_comparison_diff] = diff_to_ct[tsc_comparison_diff] + 1
        if difference_difference:
            result.append("Different numeric difference (subtraction) (" + difference_difference + ")")
            diff_to_ct[tsc_diff_num_diff] = diff_to_ct[tsc_diff_num_diff] + 1
        if sum_difference:
            result.append("Different sum (" + sum_difference + ")")
            diff_to_ct[tsc_diff_sum] = diff_to_ct[tsc_diff_sum] + 1
        if functions_different:
            result.append("Different functional relationship")
            diff_to_ct[tsc_diff_ftn_reln] = diff_to_ct[tsc_diff_ftn_reln] + 1
        if inv_functions_different:
            result.append("Different inverse functional relationship")
            diff_to_ct[tsc_diff_inv_ftn_reln] = diff_to_ct[tsc_diff_inv_ftn_reln] + 1
        if result == []:
            return None
        return string.join(result, ", ")


# No need for add, sub
symmetric_binary_functions = (min, max, operator.mul, operator.and_, operator.or_, util.gcd)
non_symmetric_binary_functions = (cmp, pow, round, operator.div, operator.mod, operator.lshift, operator.rshift)

class three_scalar_numeric_invariant(invariant):

    # Instance variables
    #  linear_z                  # can be pair (a,b,c) such that z=ax+by+c
    #  linear_y                  # can be pair (a,b,c) such that y=ax+bz+c
    #  linear_x                  # can be pair (a,b,c) such that x=ay+bz+c
    #  
    #  # In these lists, when the function is symmetric, the first variable is
    #  # preferred.
    #  functions_xyz                # list of functions such that z=fun(x,y)
    #  functions_yxz                # list of functions such that z=fun(y,x)
    #  functions_xzy                # list of functions such that y=fun(x,z)
    #  functions_zxy                # list of functions such that y=fun(z,x)
    #  functions_yzx                # list of functions such that x=fun(y,z)
    #  functions_zyx                # list of functions such that x=fun(z,y)


    def __init__(self, dict_of_triples, var_infos):
        """DICT maps from a triple of values to number of occurrences."""
        invariant.__init__(self, dict_of_triples, var_infos)

        triples = dict_of_triples.keys()

        # Can't be computed if len(triples) < 3, but
        # not meaningful (too few values) if len(triples) < 5;
        # instead of hard-coding a number here, could check for one_of.
        if len(triples) > 4:
            self.linear_z = checked_tri_linear_relationship(triples, (0,1,2))
            self.linear_y = checked_tri_linear_relationship(triples, (0,2,1))
            self.linear_x = checked_tri_linear_relationship(triples, (1,2,0))
        else:
            self.linear_z = None
            self.linear_y = None
            self.linear_x = None

        global symmetric_binary_functions, non_symmetric_binary_functions

        # if len(triples) > 1:
        if len(triples) > 4:
            functions_xyz = list(symmetric_binary_functions + non_symmetric_binary_functions)
            functions_yxz = list(non_symmetric_binary_functions)
            functions_xzy = list(symmetric_binary_functions + non_symmetric_binary_functions)
            functions_zxy = list(non_symmetric_binary_functions)
            functions_yzx = list(symmetric_binary_functions + non_symmetric_binary_functions)
            functions_zyx = list(non_symmetric_binary_functions)
            for (x,y,z) in triples:
                for fn in functions_xyz:
                    try:
                        if z != apply(fn, (x, y)):
                            functions_xyz.remove(fn)
                            # print "failed function: %d != %s (%d, %d)" % (z, fn, x, y)
                    except:
                        functions_xyz.remove(fn)
                for fn in functions_yxz:
                    try:
                        if z != apply(fn, (y, x)):
                            functions_yxz.remove(fn)
                    except:
                        functions_yxz.remove(fn)
                for fn in functions_xzy:
                    try:
                        if y != apply(fn, (x, z)):
                            functions_xzy.remove(fn)
                    except:
                        functions_xzy.remove(fn)
                for fn in functions_zxy:
                    try:
                        if y != apply(fn, (z, x)):
                            functions_zxy.remove(fn)
                    except:
                        functions_zxy.remove(fn)
                for fn in functions_yzx:
                    try:
                        if x != apply(fn, (y, z)):
                            functions_yzx.remove(fn)
                    except:
                        functions_yzx.remove(fn)
                for fn in functions_zyx:
                    try:
                        if x != apply(fn, (z, y)):
                            functions_zyx.remove(fn)
                    except:
                        functions_zyx.remove(fn)
                if (functions_xyz == []
                    and functions_yxz == []
                    and functions_xzy == []
                    and functions_zxy == []
                    and functions_yzx == []
                    and functions_zyx == []):
                    break
            self.functions_xyz = functions_xyz
            self.functions_yxz = functions_yxz
            self.functions_xzy = functions_xzy
            self.functions_zxy = functions_zxy
            self.functions_yzx = functions_yzx
            self.functions_zyx = functions_zyx
        else:
            self.functions_xyz = None
            self.functions_yxz = None
            self.functions_xzy = None
            self.functions_zxy = None
            self.functions_yzx = None
            self.functions_zyx = None


    def __setstate__(self, state):
        for key in state.keys():
            setattr(self, key, state[key])

    def is_exact(self):
        return invariant.is_exact(self) or self.linear_z or self.linear_y or self.linear_x

    def __repr__(self):
        result = "<invariant-3: "
        if self.linear_z:
            result = result + "linear_z: %s, " % (self.linear_z,)
        if self.linear_y:
            result = result + "linear_y: %s, " % (self.linear_y,)
        if self.linear_x:
            result = result + "linear_x: %s, " % (self.linear_x,)
        if self.functions_xyz:
            result = result + "functions_xyz: %s, " % (self.functions_xyz,)
        if self.functions_yxz:
            result = result + "functions_yxz: %s, " % (self.functions_yxz,)
        if self.functions_xzy:
            result = result + "functions_xzy: %s, " % (self.functions_xzy,)
        if self.functions_zxy:
            result = result + "functions_zxy: %s, " % (self.functions_zxy,)
        if self.functions_yzx:
            result = result + "functions_yzx: %s, " % (self.functions_yzx,)
        if self.functions_zyx:
            result = result + "functions_zyx: %s, " % (self.functions_zyx,)
        result = result + "%s values, %s samples" % (self.values, self.samples)
        result = result + ">"
        return result

    def __str__(self):
        return self.format()

    def format(self, arg_tuple=None):
        if arg_tuple == None:
            arg_tuple = tuple(map(lambda x: x.name, self.var_infos))

        as_base = invariant.format(self, "(%s, %s, %s)" % arg_tuple)
        if as_base:
            return as_base

        self.unconstrained_internal = false

        (x,y,z) = arg_tuple

        if self.values == 1:
            suffix = " \t(1 value)"
        else:
            suffix = " \t(%s values)" % (self.values,)

        if self.linear_z or self.linear_y or self.linear_x:
            results = []
            if self.linear_z:
                results.append(tri_linear_format(self.linear_z, (0,1,2)))
            if self.linear_y:
                results.append(tri_linear_format(self.linear_y, (0,2,1)))
            if self.linear_x:
                results.append(tri_linear_format(self.linear_x, (1,2,0)))
            if len(results) > 0:
                return string.join(results, " and ") + suffix

        if (self.functions_xyz or self.functions_yxz
            or self.functions_xzy or self.functions_zxy
            or self.functions_yzx or self.functions_zyx):

            fnrep = util.function_rep
            results = []
            if self.functions_xyz:
                for fn in self.functions_xyz:
                    results.append("%s = %s(%s, %s)" % (z,fnrep(fn),x,y))
            if self.functions_xyz:
                for fn in self.functions_yxz:
                    results.append("%s = %s(%s, %s)" % (z,fnrep(fn),y,x))
            if self.functions_xzy:
                for fn in self.functions_xzy:
                    results.append("%s = %s(%s, %s)" % (y,fnrep(fn),x,z))
            if self.functions_zxy:
                for fn in self.functions_zxy:
                    results.append("%s = %s(%s, %s)" % (y,fnrep(fn),z,x))
            if self.functions_yzx:
                for fn in self.functions_yzx:
                    results.append("%s = %s(%s, %s)" % (x,fnrep(fn),y,z))
            if self.functions_zyx:
                for fn in self.functions_zyx:
                    results.append("%s = %s(%s, %s)" % (x,fnrep(fn),z,y))
            if len(results) > 0:
                return string.join(results, " and ") + suffix

        self.unconstrained_internal = true
        return "(%s, %s, %s) unconstrained" % (x,y,z) + suffix

    def diff(self, other):
        # print "diff(three_scalar_numeric_invariant)"
        inv1 = self
        inv2 = other

        # If they print the same, then make them compare the same
        if diffs_same_format(inv1, inv2):
            return None

        as_base = invariant.diff(inv1, inv2)
        if as_base:
            return as_base

        result = []
        if (inv1.linear_z != inv2.linear_z): result.append("Different linear_z")
        if (inv1.linear_y != inv2.linear_y): result.append("Different linear_y")
        if (inv1.linear_x != inv2.linear_x): result.append("Different linear_x")
        if (inv1.functions_xyz != inv2.functions_xyz): result.append("Different functions_xyz")
        if (inv1.functions_yxz != inv2.functions_yxz): result.append("Different functions_yxz")
        if (inv1.functions_xzy != inv2.functions_xzy): result.append("Different functions_xzy")
        if (inv1.functions_zxy != inv2.functions_zxy): result.append("Different functions_zxy")
        if (inv1.functions_yxz != inv2.functions_yxz): result.append("Different functions_yxz")
        if (inv1.functions_zyx != inv2.functions_zyx): result.append("Different functions_zyx")
        if result == []:
            return None
        return string.join(result, ", ")




###
### Linear relationships
###


## Must check the output in case nonsense -- zeroes -- is returned.
def bi_linear_relationship(pair1, pair2):
    """Given ((x0,y0),(x1,y1)), return (a,b) such that y = ax + b.
    If no such (a,b) exists, then return (0,0)."""

    (x0, y0) = pair1
    (x1, y1) = pair2
    if (x0 == x1):
        return (0,0)
    # Assume that constants have already been found by a previous pass.
    a = float(y1-y0)/(x1-x0)
    b = float(y0*x1-x0*y1)/(x1-x0)

    # Convert from float to integer if appropriate
    if a == int(a):
        a = int(a)
    if b == int(b):
        b = int(b)

    # Should I check the results, as I do for tri_linear_relationship?
    # Yes, and rationalize if necessary.

    return (a,b)


def compare_pairs(pairs):
    """Given a sequence of PAIRS, return a tuple of (comparison, can_be_equal).
    COMPARISON is one of "=", "<", "<=", ">", ">=", or None.
    CAN_BE_EQUAL is boolean."""

    comparison = None
    ## Less-than or greater-than (or less-or-equal, greater-or-equal)
    maybe_eq = true
    maybe_lt = true
    maybe_le = true
    maybe_gt = true
    maybe_ge = true
    maybe_noneq = true
    for (x, y) in pairs:
        c = cmp(x,y)
        if c == 0:
            maybe_lt = maybe_gt = maybe_noneq = false
        elif c < 0:
            maybe_eq = maybe_gt = maybe_ge = false
        elif c > 0:
            maybe_eq = maybe_lt = maybe_le = false
        else:
            raise "no relationship -- impossible"
        if not(maybe_eq or maybe_lt or maybe_le or maybe_gt or maybe_ge or maybe_noneq):
            break
    else:
        if maybe_eq:
            comparison = "="
        elif maybe_lt:
            comparison = "<"
        elif maybe_le:
            comparison = "<="
        elif maybe_gt:
            comparison = ">"
        elif maybe_ge:
            comparison = ">="
    # Watch out: with few data points (say, even 100 data points when
    # values are in the range -100..100), we oughtn't conclude without
    # basis that the values are nonequal.
    return (comparison, not(maybe_noneq))


# May return None
def checked_tri_linear_relationship(triples, permutation):

    if len(triples) < 3:
        return None

    (xi, yi, zi) = permutation
    # t0 = util.slice_by_sequence(triples[0], permutation)
    t0 = (triples[0][xi], triples[0][yi], triples[0][zi])
    # t1 = util.slice_by_sequence(triples[1], permutation)
    t1 = (triples[1][xi], triples[1][yi], triples[1][zi])
    # t2 = util.slice_by_sequence(triples[2], permutation)
    t2 = (triples[2][xi], triples[2][yi], triples[2][zi])

    ## Linear relationship -- try to fit z = ax + by + c.
    (a,b,c) = tri_linear_relationship(t0, t1, t2)
    # needn't check first three, but it's a waste to create a new sequence
    for triple in triples:
        # (x,y,z) = util.slice_by_sequence(triple, permutation)
        x = triple[xi]
        y = triple[yi]
        z = triple[zi]
        try:
            if z != a*x+b*y+c:
                return None
        except OverflowError:
            return None
    else:
        return(a, b, c)


## Must check the output in case nonsense -- zeroes -- is returned.
def tri_linear_relationship(triple1, triple2, triple3):
    """Given ((x1,y1,z1),(x2,y2,z2),(x3,y3,z3)), return (a,b,c) such that z=ax+by+c.
    If no such (a,b,c) exists, then return (0,0,0)."""

    (x1, y1, z1) = triple1
    (x2, y2, z2) = triple2
    (x3, y3, z3) = triple3
    # Possibly reorder the triples to avoid division-by-zero problems.

    if (y2 == y3) or (x2 == x3):
        return (0,0,0)

    try:
        y1323 = float(y1-y3)/(y2-y3)
        a_numerator = z3-z1+(z2-z3)*y1323
        a_denominator = x3-x1+(x2-x3)*y1323

        x1323 = float(x1-x3)/(x2-x3)
        b_numerator = z3-z1+(z2-z3)*x1323
        b_denominator = y3-y1+(y2-y3)*x1323
    except OverflowError:
        return (0,0,0)

    if (a_denominator == 0) or (b_denominator == 0):
        return (0,0,0)

    a = a_numerator/a_denominator
    b = b_numerator/b_denominator
    c = z3-a*x3-b*y3

    # Convert from float to integer if appropriate
    try:
        if a == int(a):
            a = int(a)
    except OverflowError:
        pass
    try:
        if b == int(b):
            b = int(b)
    except OverflowError:
        pass
    try:
        if c == int(c):
            c = int(c)
    except OverflowError:
        pass

    # Check the results
    try:
        if (z1 != a*x1+b*y1+c) or (z2 != a*x2+b*y2+c) or (z3 != a*x3+b*y3+c):
            # print "rationalizing", (a,b,c)
            old = (a,b,c)
            ra = util.rationalize(a)
            if ra != None:
                a = ra
            rb = util.rationalize(b)
            if rb != None:
                b = rb
            rc = util.rationalize(c)
            if rc != None:
                c = rc
            # print "rationalized", (a,b,c)
            if (ra != None and ra != a) or (rb != None and rb != b) or (rc != None and rc != c) and ((z1 != a*x1+b*y1+c) or (z2 != a*x2+b*y2+c) or (z3 != a*x3+b*y3+c)):
                print "RATIONALIZATION DIDN'T HELP", old, (a,b,c)
    except OverflowError:
        return (0,0,0)

    return (a, b, c)

def tri_linear_format(abc, xyz):
    """Given ((a,b,c),(x,y,z)), format "z=ax+by+c".
    The result omits addition of zero, multiplication by one, etc."""
    (a,b,c) = abc
    (x,y,z) = xyz

    result = []
    if a == 1:
        result.append("%s" % (x,))
    elif a == -1:
        result.append("- %s" % (x,))
    elif a != 0:
        result.append("%s %s" % (a,x))
    if result != [] and b > 0:
        result.append(" + ")
    elif b < 0:
        result.append(" - ")
    if abs(b) == 1:
        result.append("%s" % (y,))
    elif b != 0:
        result.append("%s %s" % (abs(b),y))
    if c > 0:
        result.append("+ %s" % (c,))
    elif c < 0:
        result.append("- %s" % (c,))
    if result == []:
        result = "0"
    else:
        result = string.join(result, "")
    return ("%s = " % (z,)) + result


def _test_tri_linear_relationship():
    assert tri_linear_relationship((1,2,1),(2,1,7),(3,3,7)) == (4,-2,1)
    # like the above, but swap y and z; results in division-by-zero problem
    # tri_linear_relationship((1,1,2),(2,7,1),(3,7,3))
    assert tri_linear_relationship((1,2,6),(2,1,-4),(3,3,7)) == (-3,7,-5)



###########################################################################
### Invariants -- single sequence
###

class single_sequence_numeric_invariant(invariant):

    # Instance variables:
    #  # Invariants over sequence as a whole
    #  min              # min sequence of all instances
    #  max              # max sequence of all instances
    #  min_justified
    #  max_justified
    #  elts_equal            # per instance sorting data
    #  non_decreasing   #
    #  non_increasing   #
    #  
    #  # Invariants over elements of sequence
    #  all_index_sni    # single_scalar_numeric_invariant (sni) for all elements of the sequence
    #  #    per_index_sni    # tuple of element sni's for each index
    #  #                            #   across sequence instances
    #  #    reversed_per_index_sni

    def __init__(self, dict, var_infos):
        """DICT maps from tuples of values to number of occurrences."""
        invariant.__init__(self, dict, var_infos)
        seqs = dict.keys()
        seqs.sort()
        self.min = seqs[0]
        self.max = seqs[-1]

        # how to justify? i don't think the method used for single scalar
        #  invariants really makes sense here.
        self.min_justified = true
        self.max_justified = true

        # Check for sorted characteristics
        # how can we justify this as we do with min/max?
        self.elts_equal = true
        self.non_decreasing = true
        self.non_increasing = true
        for seq in seqs:
            if seq == None:
                # if any element is missing, infer nothing over anything
                self.elts_equal = self.non_decreasing = self.non_increasing = false
                self.all_index_sni = single_scalar_numeric_invariant({}, None)
                return
            for i in range(1, len(seq)):
                c = cmp(seq[i-1],seq[i])
                # should we have strictly ascending/descending?
                if c < 0:
                    self.elts_equal = self.non_increasing = false
                elif c > 0:
                    self.elts_equal = self.non_decreasing = false
                if not(self.elts_equal or self.non_decreasing \
                       or self.non_increasing):
                    break
            if not(self.elts_equal or self.non_decreasing \
                   or self.non_increasing):
                break
        # print "sequence %s: non_decreasing = %d; non_increasing = %d; elts_equal = %d" % (var_infos[0], self.non_decreasing, self.non_increasing, self.elts_equal)

        # Invariant check over elements of all sequence instances
        element_to_count = dict_of_sequences_to_element_dict(dict)
        self.all_index_sni = single_scalar_numeric_invariant(element_to_count, None)

        # Invariant check for each index over all sequence instances
        def per_index_invariants(dict, tuple_len):
            "Return a list of TUPLE_LEN single_scalar_numeric_invariant objects."
            # Use 'dict_of_tuples_to_tuple_of_dicts' in slightly different
            #  way than before.  Splits up sequence elements into dicts
            #  for each index.  Then do invariant check on per_index basis.
            per_index_elems_to_count = \
                    dict_of_tuples_to_tuple_of_dicts(dict, tuple_len)
            result = []
            for i in range(0, tuple_len):
                result.append(\
                    single_scalar_numeric_invariant(per_index_elems_to_count[i], None))
            return result

        ## The per_index_sni and reversed_per_index_sni aren't being used
        ## right now, so don't bother to compute them.  In any event, we
        ## only ever used the first element of each, so there is no point
        ## in computing them all.
        # tuple_len = min(map(len, dict.keys())) # min length of a tuple
        # self.per_index_sni = per_index_invariants(dict, tuple_len)
        # 
        ## This probably should not be done if the length is constant, because
        ## in the case of a 5-element list, we repeat work for a[-1] (== a[0]),
        ## for a[-2] (== a[1]), etc.
        # reversed_dict = {}
        # for (key, value) in dict.items():
        #     reversed_key = list(key)
        #     reversed_key.reverse()
        #     reversed_dict[tuple(reversed_key)] = value
        # self.reversed_per_index_sni = per_index_invariants(reversed_dict, tuple_len)

    def __setstate__(self, state):
        for key in state.keys():
            setattr(self, key, state[key])

    def __repr__(self):
        result = "<invariant-1 []>"
        # finish once get properties set
        # result = "<invariant-1 []: "
        return result

    def __str__(self):
        return self.format()

    def format(self, arg_tuple=None):
        if arg_tuple == None:
            arg = self.var_infos[0].name
        elif type(arg_tuple) in [types.ListType, types.TupleType]:
            (arg,) = arg_tuple
        elif type(arg_tuple) == types.StringType:
            arg = arg_tuple
        else:
            raise "Bad arg_tuple argument to single_sequence_numeric_invariant.format()"

        as_base = invariant.format(self, arg)
        if as_base:
            return as_base

        self.unconstrained_internal = false

        # Which is the strongest relationship (so we can ignore others)?
        # Do we care more that it is sorted, or that it is in given range?
        # How much of this do we want to print out?

        if self.values == 1:
            suffix = " \t(1 value)"
        else:
            suffix = " \t(%s values)" % (self.values,)
        result = []
        ## For now, comment this out; too much extraneous output.
        # if self.min_justified and self.max_justified:
        #     if self.min == self.max:
        #         result = result + "\t== %s" % (self.min,)
        #     else:
        #         result = result + "\tin [%s..%s]" % (self.min, self.max)
        # elif self.min_justified:
        #     result = result + "\t>= %s" % self.min
        # elif self.max_justified:
        #     result = result + "\t<= %s" % (self.max)

        if self.elts_equal:
            result.append("Per sequence elements equal")
        elif self.non_decreasing:
            result.append("Per sequence elements non-decreasing")
        elif self.non_increasing:
            result.append("Per sequence elements non-increasing")

        if self.all_index_sni != None:
            all_formatted = self.all_index_sni.format(("*every*element*",))
            if not self.all_index_sni.is_unconstrained():
                result.append("All sequence elements: " + all_formatted)
        #         if not (0 == len(self.per_index_sni)):
        #             first_formatted = self.per_index_sni[0].format(("*first*element*",))
        #             if not self.per_index_sni[0].is_unconstrained():
        #                 result = result + "\n" + "\tFirst sequence element: " + first_formatted
        #             last_formatted = self.reversed_per_index_sni[0].format(("*last*element*",))
        #             if not self.reversed_per_index_sni[0].is_unconstrained():
        #                 result = result + "\n" + "\tLast sequence element: " + last_formatted

        if result == []:
            self.unconstrained_internal = true
            return arg + " unconstrained" + suffix
        return arg + suffix + "\n\t" + string.join(result, "\n\t")

    def diff(self, other):
        # print "diff(single_sequence_numeric_invariant)"
        global inv_one_cons, inv_diff_small_no_vals, inv_one_none, ssc_miss_min, ssc_min_diff, ssc_miss_max, ssc_max_diff, ssc_one_can_be_zero, ssc_diff_mod, ssc_diff_nonmod, tsc_diff_lin_reln, tsc_one_equal, tsc_comparison_diff, tsc_diff_num_diff, tsc_diff_sum, tsc_diff_ftn_reln, tsc_diff_inv_ftn_reln, sseq_diff_elem_equality, sseq_diff_sortedness, sseq_diff_inv_all_elem, scseq_diff_membership, two_seq_diff_lin_reln, two_seq_one_equ, two_seq_diff_comp, two_seq_diff_subseq, two_seq_diff_supseq, two_seq_diff_revness, diff_to_ct
        inv1 = self
        inv2 = other

        # If they print the same, then make them compare the same
        if diffs_same_format(inv1, inv2):
            return None

        as_base = invariant.diff(inv1, inv2)
        if as_base:
            return as_base

        result = []

        if inv1.elts_equal != inv2.elts_equal:
            result.append("Different element equality")
            diff_to_ct[sseq_diff_elem_equality] = diff_to_ct[sseq_diff_elem_equality] + 1
        if (inv1.non_decreasing != inv2.non_decreasing) or (inv1.non_increasing != inv2.non_increasing):
            result.append("Different sortedness")
            diff_to_ct[sseq_diff_sortedness] = diff_to_ct[sseq_diff_sortedness] + 1
        # First two clauses here are for backward compatibility; remove once
        # the slot can no longer be None.
        elements_diff = inv1.all_index_sni and inv2.all_index_sni and inv1.all_index_sni.diff(inv2.all_index_sni)
        if elements_diff:
            result.append("Different invariants over all elements = (" + elements_diff + ")")
            diff_to_ct[sseq_diff_inv_all_elem] = diff_to_ct[sseq_diff_inv_all_elem] + 1
        if result == []:
            return None
        return string.join(result, ", ")



###########################################################################
### Invariants -- multiple sequence, or sequence plus scalar
###

class scalar_sequence_numeric_invariant(invariant):

    # Instance variables
    #  # I'm not entirely sure what to do with this one
    #  seq_first          # if true, the variables are (seq,scalar)
    #                     #  if false, the variables are (scalar,seq)
    #  member
    #  member_obvious
    #  # size
    #  # per_index_linear   # Array whose elements describe the linear
    #                       #  relationship between the number scalar and
    #                       #  the sequence element at that index.


    def __init__(self, dict_of_pairs, var_infos):

        invariant.__init__(self, dict_of_pairs, var_infos)
        pairs = dict_of_pairs.keys()

        if len(pairs) == 0:
            raise "empty dictionary supplied"
        self.seq_first = type(pairs[0][0]) == types.TupleType

        if self.seq_first:
            (seqvar,sclvar) = (var_infos[0].name, var_infos[1].name)
        else:
            (sclvar,seqvar) = (var_infos[0].name, var_infos[1].name)

        # For each (num, sequence), determine if num is a member of seq
        self.member_obvious = ((seqvar + "[" == sclvar[0:len(seqvar)+1])
                               or ("min(" + seqvar + ")" == sclvar)
                               or ("max(" + seqvar + ")" == sclvar))

        self.member = true
        if not self.member_obvious:
            for i in range(0, len(pairs)):
                if self.seq_first:
                    (seq,num) = pairs[i]
                else:
                    (num,seq) = pairs[i]
                if not(num in seq):
                    self.member = false
                    break


        ## This isn't necessary any longer:  we introduce a "size(SEQ)"
        ## variable for each sequence SEQ.
        # # Determine if the scalar is the size of the sequence.
        # # Only need to check sequence size once.
        # self.size = true
        # for i in range(0, len(pairs)):
        #     if self.seq_first:
        #         (seq,num) = pairs[i]
        #     else:
        #         (num,seq) = pairs[i]
        #     if num != len(seq):
        #         self.size = false
        #         break

        ## Linear relationship --
        # Find linear relationship between single scalar and each element
        # of the sequence.  Then determine if relationship at each index
        # holds  across all instances of the pairs (num,seq)
        #per_index_linear = []
        #(num,seq) = pairs[0]
        #for i in range(0, len(seq)):
        #    per_index_linear.append(None)
        #try:
        #    if len(pairs) > 1:
        #        for i in range(0, len(seq)):
        #            (num1,seq1) = pairs[0]
        #            (num2,seq2) = pairs[1]
        #            per_index_linear[i] = bi_linear_relationship((num1,num2), \
        #                                                   (seq1[0],seq2[0]))
        #        for (num,seq) in pairs:
        #            for i in range(0, len(seq)):
        #               (a,b) = per_index_linear[i]
        #                if seq[i] != a*num+b:
        #                    per_index_linear[i] = None
        #                    break
        #                if not(maybe_linear):
        #                    break
        #            else:
        #                self.linear = (a,b)
        #    except OverflowError:
        #        pass

    def __setstate__(self, state):
        for key in state.keys():
            setattr(self, key, state[key])

    def __repr__(self):
        result = "<invariant-2 (x,[])>"
        return result

    def __str__(self):
        return self.format()

    def format(self, arg_tuple=None):
        self.unconstrained_internal = false

        if arg_tuple == None:
            arg_tuple = arg_tuple or (self.var_infos[0].name, self.var_infos[1].name)
        if self.seq_first:
            (seqvar, sclvar) = arg_tuple
        else:
            (sclvar, seqvar) = arg_tuple

        result = []
        if self.member and not self.member_obvious:
            result.append("%s is a member of %s" % (sclvar,seqvar))
        # if self.size:
        #     result.append("%s is the size of %s" % (sclvar,seqvar))

        if self.values == 1:
            suffix = " \t(1 value)"
        else:
            suffix = " \t(%s values)" % (self.values,)
        if result != []:
            return string.join(result, " and ") + suffix

        self.unconstrained_internal = true
        return "(%s,%s) are unconstrained" % (arg_tuple) + suffix

    def diff(self, other):
        # print "diff(scalar_sequence_numeric_invariant)"
        global inv_one_cons, inv_diff_small_no_vals, inv_one_none, ssc_miss_min, ssc_min_diff, ssc_miss_max, ssc_max_diff, ssc_one_can_be_zero, ssc_diff_mod, ssc_diff_nonmod, tsc_diff_lin_reln, tsc_one_equal, tsc_comparison_diff, tsc_diff_num_diff, tsc_diff_sum, tsc_diff_ftn_reln, tsc_diff_inv_ftn_reln, sseq_diff_elem_equality, sseq_diff_sortedness, sseq_diff_inv_all_elem, scseq_diff_membership, two_seq_diff_lin_reln, two_seq_one_equ, two_seq_diff_comp, two_seq_diff_subseq, two_seq_diff_supseq, two_seq_diff_revness, diff_to_ct
        inv1 = self
        inv2 = other

        # If they print the same, then make them compare the same
        if diffs_same_format(inv1, inv2):
            return None

        as_base = invariant.diff(inv1, inv2)
        if as_base:
            return as_base

        result = []

        assert inv1.seq_first == inv2.seq_first
        if inv1.member != inv2.member:
            result.append("Different membership")
            diff_to_ct[scseq_diff_membership] = diff_to_ct[scseq_diff_membership] + 1

        if result == []:
            return None
        return string.join(result, ", ")


class two_sequence_numeric_invariant(invariant):

    # Instance variables:
    #  linear          # Relationship describing elements at same indices
    #                         # in the two sequences.  If not None, it is the same
    #                         # for each index.
    #  # per_index_linear # Array whose elements describe the linear
    #                            # relationship between the pair of sequence
    #                            # elements at that index.
    #  comparison      # can be "=", "<", "<=", ">", ">="
    #  can_be_equal
    #  sub_sequence
    #  super_sequence
    #  reverse                      # true if one is the reverse of the other
    #  subseq_obvious
    #  superseq_obvious

    def __init__(self, dict_of_pairs, var_infos):
        invariant.__init__(self, dict_of_pairs, var_infos)

        pairs = dict_of_pairs.keys()

        ## Linear relationship -- try to fit y[] = ax[] + b.
        self.linear = None              # default
        # Get one sample from the first elements of the first pair of
        #  sequences.  Then test all corresponding pairs in all other
        #  sequences.
        # How interesting is this?
        # only want if equal size, right?
        maybe_linear = true
        (seq1,seq2) = pairs[0]
        if (len(seq1) == len(seq2)) and (len(seq1) > 1):
            try:
                if len(pairs) > 1:
                    (a,b) = bi_linear_relationship((seq1[0],seq2[0]), \
                                                   (seq1[1],seq2[1]))
                    for (seq1,seq2) in pairs:
                        if len(seq1) != len(seq2) or len(seq1) < 2:
                            maybe_linear = false
                            break
                        for i in range(0, len(seq1)):
                            if seq2[i] != a*seq1[i]+b:
                                maybe_linear = false
                                break
                        if not(maybe_linear):
                            break
                    else:
                        self.linear = (a,b)
            except OverflowError:
                pass

        (var1, var2) = (self.var_infos[0].name, self.var_infos[1].name)
        self.subseq_obvious = (var2 + "[" == var1[0:len(var2)+1])
        self.superseq_obvious = (var1 + "[" == var2[0:len(var1)+1])

        if not (self.subseq_obvious or self.superseq_obvious):
            (self.comparison, self.can_be_equal) = compare_pairs(pairs)
        else:
            (self.comparison, self.can_be_equal) = (None, None)

        self.reverse = true
        for (x, y) in pairs:
            if len(x) != len(y):
                self.reverse = false
                break
            # Make shallow copy because reverse works in place.
            z = list(y)
            z.reverse()
            z = tuple(z)
            if x != z:
                self.reverse = false
                break
        self.sub_sequence = true
        if not self.subseq_obvious:
            for (x, y) in pairs:
                if not(util.sub_sequence_of(x, y)):
                    self.sub_sequence = false
                    break
        self.super_sequence = true
        if not self.superseq_obvious:
            for (x, y) in pairs:
                if not(util.sub_sequence_of(y, x)):
                    self.super_sequence = false
                    break

    def __setstate__(self, state):
        for key in state.keys():
            setattr(self, key, state[key])


    def __repr__(self):
        result = "<invariant-2 (%s,%s)>" % (self.var_infos[0].name, self.var_infos[1].name)
        return result

    def __str__(self):
        return self.format()

    def format(self, arg_tuple=None):
        if arg_tuple == None:
            arg_tuple = tuple(map(lambda x: x.name, self.var_infos))

        as_base = invariant.format(self, "(%s, %s)" % arg_tuple)
        if as_base:
            return as_base

        self.unconstrained_internal = false

        if self.values == 1:
            suffix = " \t(1 value)"
        else:
            suffix = " \t(%s values)" % (self.values,)

        (x, y) = arg_tuple
        if self.comparison == "=":
            return "%s = %s" % (x,y) + suffix
        if self.linear:
            (a,b) = self.linear
            if a == 1:
                if b < 0:
                    return "%s = %s - %s" % (y,x,abs(b)) + suffix
                else:
                    return "%s = %s + %s" % (y,x,b) + suffix
            elif b == 0:
                return "%s = %s %s" % (y,a,x) + suffix
            else:
                if b < 0:
                    return "%s = %s %s - %s" % (y,a,x,abs(b)) + suffix
                else:
                    return "%s = %s %s + %s" % (y,a,x,b) + suffix

        # The test of self.reverse needs to precede sub_sequence and
        # super_sequence, lest it never be printed.
        if self.reverse:
            return "%s is the reverse of %s" % (x,y) + suffix
        if self.sub_sequence and not self.subseq_obvious:
            return "%s is a subsequence of %s" % (x,y) + suffix
        if self.super_sequence and not self.superseq_obvious:
            if not (x + "[" == y[0:len(x)+1]):
                return "%s is a subsequence of %s" % (y,x) + suffix

        # I'm not sure how interesting these lexicographic comparisons are.
        if self.comparison:
            if self.comparison in ["<", "<="] and not self.subseq_obvious:
                    return "%s %s %s" % (x, self.comparison, y) + suffix
            if self.comparison == ">" and not self.superseq_obvious:
                    return "%s < %s" % (y, x) + suffix
            if self.comparison == ">=" and not self.superseq_obvious:
                    return "%s <= %s" % (y, x) + suffix
            raise "Can't get here"

        self.unconstrained_internal = true
        return "(%s,%s) unconstrained" % (x,y) + suffix


    def diff(self, other):
        # print "diff(two_sequence_numeric_invariant)"
        global inv_one_cons, inv_diff_small_no_vals, inv_one_none, ssc_miss_min, ssc_min_diff, ssc_miss_max, ssc_max_diff, ssc_one_can_be_zero, ssc_diff_mod, ssc_diff_nonmod, tsc_diff_lin_reln, tsc_one_equal, tsc_comparison_diff, tsc_diff_num_diff, tsc_diff_sum, tsc_diff_ftn_reln, tsc_diff_inv_ftn_reln, sseq_diff_elem_equality, sseq_diff_sortedness, sseq_diff_inv_all_elem, scseq_diff_membership, two_seq_diff_lin_reln, two_seq_one_equ, two_seq_diff_comp, two_seq_diff_subseq, two_seq_diff_supseq, two_seq_diff_revness, diff_to_ct
        inv1 = self
        inv2 = other

        # If they print the same, then make them compare the same
        if diffs_same_format(inv1, inv2):
            return None

        as_base = invariant.diff(inv1, inv2)
        if as_base:
            return as_base

        result = []

        linear_different = (inv1.linear != inv2.linear)
        equal_different = (inv1.can_be_equal != inv2.can_be_equal)

        # Just the first character; ignore equality differences
        comp1 = inv1.comparison
        comp1 = comp1 and comp1[0]
        comp2 = inv2.comparison
        comp2 = comp2 and comp2[0]

        comparison_different = (comp1 != comp2)

        result = []
        if linear_different:
            result.append("Different linear reln")
            diff_to_ct[two_seq_diff_lin_reln] = diff_to_ct[two_seq_diff_lin_reln] + 1
        if equal_different:
            result.append("One can be equal but other can't")
            diff_to_ct[two_seq_one_equ] = diff_to_ct[two_seq_one_equ] + 1
        if comparison_different:
            result.append("Different comparison")
            diff_to_ct[two_seq_diff_comp] = diff_to_ct[two_seq_diff_comp] + 1
        if inv1.sub_sequence != inv2.sub_sequence:
            result.append("Different subsequenceness")
            diff_to_ct[two_seq_diff_subseq] = diff_to_ct[two_seq_diff_subseq] + 1
        if inv1.super_sequence != inv2.super_sequence:
            result.append("Different supersequenceness")
            diff_to_ct[two_seq_diff_supseq] = diff_to_ct[two_seq_diff_supseq] + 1
        if inv1.reverse != inv2.reverse:
            result.append("Different reverseness")
            diff_to_ct[two_seq_diff_revness] = diff_to_ct[two_seq_diff_revness] + 1
        if result == []:
            return None
        return string.join(result, ", ")



###########################################################################
### Persistence
###

# Pickle files store the computational state of Python; in particular, they
# contain representations of the data structures that hold both the raw
# data and the computed invariants.  That means we can quickly load them
# and have the invariants all ready to print, diff, whatever rather than
# being forced to regenerate them from scratch (which is slow).


# Perhaps I don't really need fn_samples.
# Notice that at present this does not store fn_var_values, from which the
# invariants are computed -- it just stores the invariants themselves.

def write_state(filename):
    """Write global invariants variables to FILENAME."""
    filename = util.expand_file_name(filename)
    # if (filename[-3:] == ".gz"):
    #     file = gzip.open(filename, "w")
    # else:
    #     file = open(filename, "w")
    file = TextFile.TextFile(filename, "w")
    pickle.dump(fn_samples, file)
    pickle.dump(fn_var_infos, file)
    file.close()

def read_state(filename):
    """Read global invariants variables from FILENAME,
    and return the values rather than setting the globals."""
    filename = util.expand_file_name(filename)
    ## This doesn't work because TextFile doesn't support the read() operation.
    # file = TextFile.TextFile(filename, "r")
    if (filename[-3:] == ".gz"):
        file = gzip.open(filename, "r")
    elif (filename[-2:] == ".Z"):
        raise "Can't read from .Z state files; uncompress or convert to .gz"
    else:
        file = open(filename, "r")
    result_fn_samples = pickle.load(file)
    result_fn_var_infos = pickle.load(file)
    file.close()
    return (result_fn_samples, result_fn_var_infos)


## This version of write_state only writes fn_var_values (and related
## variables).
## I'm not sure that all of these are required, but just in case...
#     fn_var_infos
#     fn_truevars
#     fn_var_values
#     fn_samples
#     fn_derived_from
#     functions


def write_state_values(filename):
    filename = util.expand_file_name(filename)
    file = TextFile.TextFile(filename, "w")
    pickle.dump(fn_var_infos, file)
    pickle.dump(fn_truevars, file)
    pickle.dump(fn_var_values, file)
    pickle.dump(fn_samples, file)
    pickle.dump(fn_derived_from, file)
    pickle.dump(functions, file)
    file.close()

def read_state_values(filename):
    """Read global invariants variables from FILENAME,
    and return the values rather than setting the globals."""
    filename = util.expand_file_name(filename)
    ## This doesn't work because TextFile doesn't support the read() operation.
    # file = TextFile.TextFile(filename, "r")
    if (filename[-3:] == ".gz"):
        file = gzip.open(filename, "r")
    elif (filename[-2:] == ".Z"):
        raise "Can't read from .Z state files; uncompress or convert to .gz"
    else:
        file = open(filename, "r")
    result_fn_var_infos = pickle.load(file)
    result_fn_truevars = pickle.load(file)
    result_fn_var_values = pickle.load(file)
    result_fn_samples = pickle.load(file)
    result_fn_derived_from = pickle.load(file)
    result_functions = pickle.load(file)
    file.close()
    return (result_fn_var_infos, result_fn_truevars, result_fn_var_values, result_fn_samples, result_fn_derived_from, result_functions)

def read_set_state_values(filename):
    global fn_var_infos, fn_truevars, fn_var_values, fn_samples, fn_derived_from, functions
    (fn_var_infos, fn_truevars, fn_var_values, fn_samples, fn_derived_from, functions) = read_state_values(filename)


###########################################################################
### Differences between invariants
###

def diff_files(filename1, filename2):
    diff_to_ct[inv_one_cons] = diff_to_ct.get(inv_one_cons, 0) + 1
    (samples1, fn_var_infos1) = read_state(filename1)
    (samples2, fn_var_infos2) = read_state(filename2)
    diff_fn_var_infos(fn_var_infos1, fn_var_infos2)

## Perhaps have "strict" and "nonstrict" differences (of various sorts...)

# Results:
#  * missing invariant -- in one, but not the other
#  * missing bound
#     * one bound already there -- maybe note the other's unjustified bound
#     * no bound already there (subset of "missing invariant")
#  * different bound (quantify this with a percentage difference or an absolute difference?  or just report the two)



def diff_fn_var_infos(fn_var_infos1, fn_var_infos2):
    """Print differences between invariants in two sets of fn_var_infos."""
    # print "diff_fn_var_infos"
    function_names1 = fn_var_infos1.keys()
    function_names1.sort()
    function_names2 = fn_var_infos2.keys()
    function_names2.sort()
    function_names = util.sorted_list_intersection(function_names1, function_names2)
    extra_functions1 = util.sorted_list_difference(function_names, function_names1)
    extra_functions2 = util.sorted_list_difference(function_names, function_names2)
    if extra_functions1 != []:
        print "Functions in first list, not in second:", extra_functions1
    if extra_functions2 != []:
        print "Functions in second list, not in first:", extra_functions2

    for fn_name in function_names:
        print "==========================================================================="
        # print fn_name, fn_samples[fn_name], "samples"
        print fn_name
        diff_var_infos(fn_var_infos1[fn_name], fn_var_infos2[fn_name])


def diff_var_infos(var_infos1, var_infos2):
    """Print differences between invariants in two sets of var_infos."""
    global unary_diff_to_ct, g_unary_same, g_unary_different, g_pair_same, g_pair_different
    # var_infos1.sort(var_info_name_compare)
    # var_infos2.sort(var_info_name_compare)

    # It is not necessarily the case that the var_infos contain exactly the
    # same variables.  In particular, one of the data seets may have
    # resulted in derivation of variables not derived by the other data
    # set.  As an example, consider
    #   daikon.diff_files('/projects/se/people/mernst/replace_outputs/replace.main.4000.19981220.pkl','/projects/se/people/mernst/replace_outputs/replace.main.4500.19990103.pkl')
    # In that case, we must eliminate the extra var_infos.
    # There ae two basic strategies we could follow:
    #  * For each var_info, find the corresponding one in the other list.
    #  * Step through the lists together finding corresponding var_infos.
    #    If the lists aren't sorted, we can't know whether, in the case of
    #    a mismatch, we should look further forward in one list or in the
    #    other (or maybe neither of the current ones has a match).

    # This is quite inefficient.  Maybe it's good enough.
    indices1 = []
    indices2 = []
    for i1 in range(0, len(var_infos1)):
        vi1 = var_infos1[i1]
        i2 = None                 # index in var_infos2 corresponding to i2
        for i2 in range(0, len(var_infos2)):
            vi2 = var_infos2[i2]
            if not var_info_name_compare(vi1, vi2):
                indices1.append(i1)
                indices2.append(i2)
                break


    # indices1 = range(0, len(var_infos1))
    # indices2 = range(0, len(var_infos2))
    # if not var_infos_compatible(var_infos1, var_infos2):
    #     print "RATIONALIZING INCOMPATIBLE VAR_INFOS"
    #     ii = 0
    #     while ii < len(indices1) and ii < len(indices2):
    #         vi1 = var_infos1[indices1[ii]]
    #         vi2 = var_infos2[indices2[ii]]
    #         if not var_info_name_compare(vi1, vi2):
    #             # same name
    #             ii = ii + 1
    #             continue
    #         # different name
    #         if ii+1 < len(indices2) and not var_info_name_compare(vi1, var_infos2[indices2[ii+1]]):
    #             del indices2[ii]
    #             continue
    #         if ii+1 < len(indices1) and not var_info_name_compare(vi2, var_infos1[indices1[ii+1]]):
    #             del indices1[ii]
    #             continue
    #         raise "What is going on here?"

    assert len(indices1) == len(indices2)
    assert (map(lambda i, vis=var_infos1: vis[i].name, indices1)
            == map(lambda i, vis=var_infos2: vis[i].name, indices2))

    num_indices = len(indices1)
    print num_indices, "var_infos:"

    unary_same = 0
    unary_different = 0
    clear_diff_to_ct()
    for ii in range(0, num_indices):
        vi1 = var_infos1[indices1[ii]]
        vi2 = var_infos2[indices2[ii]]
        assert vi1.name == vi2.name
        inv1 = vi1.invariant
        inv2 = vi2.invariant
        if (inv1 == None) and (inv2 == None):
            # Function never called in either set of tests
            continue
        if (inv1 == None) or (inv2 == None):
            print "unary:", "Executions in only one run for", vi1.name
            print " ", inv1
            print " ", inv2
            continue
        assert inv1.__class__ == inv2.__class__
        can1 = vi1.is_canonical()
        can2 = vi2.is_canonical()
        if not can1 and not can2:
            continue
        if (can1 and not can2) or (can2 and not can1):
            # This is a pretty significant difference, actually...
            print "unary:", "Equality difference for", vi1.name
            print " ", inv1
            print " ", inv2
            continue
        assert can1 and can2
        # Both variables are canonical
        difference = inv1.diff(inv2)
        if difference:
            print "unary:", difference
            print " ", inv1.format((vi1.name,))
            print " ", inv2.format((vi2.name,))
            unary_different = unary_different + 1
        else:
            unary_same = unary_same + 1
        # This isn't guaranteed to be called due to "continue" statements
        # above.  Is that OK?  -MDE 4/22/1999
        add_to_unary()
        clear_diff_to_ct()

    pair_same = 0
    pair_different = 0

    for ii in range(0, num_indices):
        vi1 = var_infos1[indices1[ii]]
        vi2 = var_infos2[indices2[ii]]
        assert vi1.name == vi2.name
        name1 = vi1.name
        invs1 = vi1.invariants
        invs2 = vi2.invariants
        # keys1 = invs1.keys()
        # keys1.sort()
        # keys2 = invs2.keys()
        # keys2.sort()
        for ij in range(ii+1, num_indices):
            j1 = indices1[ij]
            j2 = indices2[ij]
            assert var_infos1[j1].name == var_infos2[j2].name
            name2 = var_infos1[j1].name
            in1 = invs1.has_key(j1) and not invs1[j1].is_unconstrained()
            in2 = invs2.has_key(j2) and not invs2[j2].is_unconstrained()
            if (not in1) and (not in2):
                continue
            if in1 and not in2:
                print "binary:", "First group contains invariant:", invs1[j1].format((name1, name2))
                pair_different = pair_different + 1
                continue
            if in2 and not in1:
                print "binary:", "Second group contains invariant:", invs2[j2].format((name1, name2))
                pair_different = pair_different + 1
                continue
            assert in1 and in2
            inv1 = invs1[j1]
            inv2 = invs2[j2]

            # Should I check for whether the variables are canonical?
            # I'm leaving them all in for now, lest it be too hard to
            # compare numbers of identical/different invariants.
            difference = inv1.diff(inv2)
            if difference:
                print "binary:", difference
                print " ", inv1.format((name1, name2))
                print " ", inv2.format((name1, name2))
                pair_different = pair_different + 1
            else:
                pair_same = pair_same + 1
            add_to_binary()
            clear_diff_to_ct()

    print "Identical unary invariants:", unary_same
    print "Differing unary invariants:", unary_different
    print "Identical binary invariants:", pair_same
    print "Differing binary invariants:", pair_different
    unary_diff_to_ct[g_unary_same] = unary_diff_to_ct[g_unary_same] + unary_same
    unary_diff_to_ct[g_unary_different] = unary_diff_to_ct[g_unary_different] + unary_different
    unary_diff_to_ct[g_pair_same] = unary_diff_to_ct[g_pair_same] + pair_same
    unary_diff_to_ct[g_pair_different] = unary_diff_to_ct[g_pair_different] + pair_different

#     # Equality invariants
#     for vi in var_infos:
#         if not vi.is_canonical():
#             continue
#         if vi.equal_to == []:
#             continue
#         if vi.invariant.is_exact():
#             value = "= %s" % (vi.invariant.min,)
#         else:
#             value = ""
#         print vi.name, "=", string.join(map(lambda idx, vis=var_infos: vis[idx].name, vi.equal_to), " = "), value
#     # Single invariants
#     for vi in var_infos:
#         if not vi.is_canonical():
#             continue
#         this_inv = vi.invariant
#         if (this_inv.is_exact() and vi.equal_to != []):
#             # Already printed above in "equality invariants" section
#             continue
#         if print_unconstrained or not this_inv.is_unconstrained():
#             print " ", this_inv.format((vi.name,))
#     # Pairwise invariants
#     nonequal_constraints = []
#     # Maybe this is faster than calling "string.find"; I'm not sure.
#     nonequal_re = re.compile(" != ")
#     for vi in var_infos:
#         if not vi.is_canonical():
#             continue
#         vname = vi.name
#         for (index,inv) in vi.invariants.items():
#             if type(index) != types.IntType:
#                 continue
#             if not var_infos[index].is_canonical():
#                 continue
#             if (not print_unconstrained) and inv.is_unconstrained():
#                 continue
#             formatted = inv.format((vname, var_infos[index].name))
#             # Not .match(...), which only checks at start of string!
#             if nonequal_re.search(formatted):
#                 nonequal_constraints.append(formatted)
#             else:
#                 print "   ", formatted
#     for ne_constraint in nonequal_constraints:
#         print "    ", ne_constraint
#     # Three-way (and greater) invariants
#     for vi in var_infos:
#         if not vi.is_canonical():
#             continue
#         vname = vi.name
#         for (index_pair,inv) in vi.invariants.items():
#             if type(index_pair) == types.IntType:
#                 continue
#             (i1, i2) = index_pair
#             if not var_infos[i1].is_canonical():
#                 # Perhaps err; this shouldn't happen, right?
#                 continue
#             if not var_infos[i2].is_canonical():
#                 # Perhaps err; this shouldn't happen, right?
#                 continue
#             if print_unconstrained or not inv.is_unconstrained():
#                 print "     ", inv.format((vname, var_infos[i1].name, var_infos[i2].name))

# If they print the same, then make them compare the same
def diffs_same_format(inv1, inv2):
    # This is a hack.  Better to do something more principled about whether
    # to put the number of values in the output.
    formatted1 = inv1.format()
    formatted2 = inv2.format()
    suffix_re = re.compile(r'\t\([0-9]+ values\)$')
    match1 = suffix_re.search(formatted1)
    match2 = suffix_re.search(formatted2)
    if match1: formatted1 = formatted1[0:match1.start(0)]
    if match2: formatted2 = formatted2[0:match2.start(0)]
    return formatted1 == formatted2


# Example calls:
#   daikon.all_fns_diff('/projects/se/people/mernst/replace_outputs/', '2500', '/projects/se/people/mernst/replace_outputs/', '3000')
#   daikon.all_fns_diff('/projects/se/people/jake/rollbk_for_plclose/', '1000', '/projects/se/people/jake/replace_plclose/', '1000')

# Or:
#   python -c "import daikon; daikon.all_fns_diff('/projects/se/people/mernst/replace_outputs/', '2500', '/projects/se/people/mernst/replace_outputs/', '3000')" > all_2500_3000.diff
#   python -c "import daikon; daikon.all_fns_diff('/projects/se/people/jake/rollbk_for_plclose/', '1000', '/projects/se/people/jake/replace_plclose/', '1000')" > all_plclose_1000.diff


def all_fns_diff(dir1, size1, dir2, size2):
    fns = ('addstr', 'amatch', 'change', 'dodash', 'esc', 'getccl',
           'getline', 'getpat', 'getsub', 'in_pat_set', 'in_set_2',
           'locate', 'main', 'makepat', 'makesub', 'omatch', 'patsize',
           'putsub', 'stclose', 'subline')

    init_diff_globals()
    for fn in fns:
        diff_files(dir1 + "replace." + fn + "." + size1 + ".pkl",
                   dir2 + "replace." + fn + "." + size2 + ".pkl")
    print_inv_diff_tracking()


###########################################################################
### Querying/pruning the database
###

def var_index(varname, fnname):
    try:
        return map(lambda vi:vi.name, fn_var_infos[fnname]).index(varname)
    except ValueError:
        return None
## Testing
# daikon.var_index("lj", 'makepat:::EXIT(arg_0[],start,delim,pat_100[])')
# daikon.var_index("lj", 'makepat:::EXIT(arg_0[],start,delim,pat_100[])')


def replace_vars_by_vals_indexed(condition, function):
    """For each expression in CONDITION (a Python expression) which is a
    variable in FUNCTION, replace it by the string "vals[i]" where i is
    the index of that variable in the function.

    Actually returns a tuple of the new condition and the max var index seen.
    The latter is useful for error-checking.
    """

    # print "Entered replace_vars_by_vals_indexed: ", condition

    # There will be trouble if anything matches "vals" (the replacement).

    max_var_idx = -1
    result = condition

    # Sort the names by length; we want the longest matches first, because
    # shorter names may be substrings of longer ones.
    var_names = []
    for i in range(0, len(fn_var_infos[function])):
        var_names.append((fn_var_infos[function][i].name), i)
    var_names.sort(lambda t1,t2:-cmp(len(t1[0]), len(t2[0])))

    for (var_name, var_idx) in var_names:
        var_regexp = re.compile(r'(\b|\W)(' + re.escape(var_name) + r')(\b|\W)', re.IGNORECASE)

        match = var_regexp.search(result)
        while match:
            # print "match for ", var_name, ": ", match
            ## Assumes modification info is in the key (modinkey is true)
            result = result[:match.start(2)] + "(vals[%s][0])" % (var_idx,) + result[match.end(2):]
            # print "new result:" , result
            max_var_idx = max(max_var_idx, var_idx)
            match = var_regexp.search(result)

    # print "Exiting replace_vars_by_vals_indexed: ", result
    return (result, max_var_idx)


## This implementation is no good because it doesn't deal with variables
## whose names contain word delimiters, such as "cursor.type".
# def replace_vars_by_vals_indexed(condition, function):
#     """For each word in condition (a Python expression), if it is a variable in
#     function, then replace it by the string "vals[i]" where i is the index of
#     that variable in the function.
# 
#     Actually returns a tuple of the new condition and the max var index seen.
#     The latter is useful for error-checking.
#     """
# 
#     ## It's a bit of a shame to repeatedly split the condition on
#     ## subsequent calls to this function.  Perhaps permit the condition to
#     ## already be a list, but probably not worth losing sleep over.
#     # I want to use re.split(r'\b', but that doesn't work.  Why??
#     # '(\W+)' does what I would think it would do...
#     ## This isn't quite right, because it splits "a*b == c" into "a" "*b" ...
#     ## Fix later.
#     # The capturing parentheses ensure that the punctuation does appear
#     # in the list returned by split.
#     condition_words = re.split('(\*?\w+)', condition)
# 
#     result_words = []
#     max_var_idx = -1
# 
#     for i in range(0,len(condition_words)):
#         var_idx = var_index(condition_words[i], function)
#         if var_idx != None:
#             ## Assumes modification info is in the key (modinkey is true)
#             result_words.append("(vals[%s][0])" % (var_idx,))
#             max_var_idx = max(max_var_idx, var_idx)
#         else:
#             result_words.append(condition_words[i])
#     return (string.join(result_words, ""), max_var_idx)



## Maybe this should be stated positively:  output when the condition is
## true.  (Easy enough to add...)
def find_violations(condition, fn_regexp=None):
    """Given a condition and a regular expression, output the values for all
    variables whenever that condition is violated in a function whose
    name matches the regular expression.
    The condition is a Python expression (a string) using symbolic
    variable names (that is, the variable names used in the program).
    Example calls:
      find_violations("lj <= j", "makepat:::EXIT")
      find_violations("*j_orig == *j - 1", "plclose:::EXIT")
    """

    fn_regexp = util.re_compile_maybe(fn_regexp, re.IGNORECASE)

    for fn in fn_var_infos.keys():
        if fn_regexp and not fn_regexp.search(fn):
            continue

        (cond, max_var_idx) = replace_vars_by_vals_indexed(condition, fn)

        vis = fn_var_infos[fn]
        # This implementation tells us the file in which the value occurs, but
        # file_fn_var_values does not contain derived variables.  So perhaps be
        # able to select between using file_fn_var_values and fn_var_values.
        for (file, this_fn_var_values) in file_fn_var_values.items():
            # print "checking file", file
            # This function might not appear in this file
            if not this_fn_var_values.has_key(fn):
                continue
            for (vals, count) in this_fn_var_values[fn].items():
                # I should probably catch errors here
                if not eval(cond, globals(), locals()):
                    # vals doesn't contain derived variables; vis does
                    # assert len(vals) == len(vis)
                    assert max_var_idx < len(vals)
                    print "==========================================================================="
                    print "function %s, file %s, %s samples" % (fn, file, count)
                    for i in range(0,len(vals)):
                        print vis[i].name, "\t", vals[i]


## Warning: this modifies fn_var_values, so it is no longer in perfect
## correspondence with file_fn_var_values.
def prune_database(condition, fn_regexp=None):
    """Given a condition and a regular expression, eliminate all samples
    from the database that violate that condition in a function whose
    name matches the regular expression.
    The condition is a Python expression (a string) using symbolic
    variable names (that is, the variable names used in the program).
    Example calls:
      prune_database("lj <= j", "makepat:::EXIT")
      prune_database("*j_orig == *j - 1", "plclose:::EXIT")
    """

    fn_regexp = util.re_compile_maybe(fn_regexp, re.IGNORECASE)

    for fn in fn_var_infos.keys():
        if fn_regexp and not fn_regexp.search(fn):
            continue

        (cond, max_var_idx) = replace_vars_by_vals_indexed(condition, fn)

        vis = fn_var_infos[fn]
        ## Can this happen?
        # # This function might not appear in this file
        # if not this_fn_var_values.has_key(fn):
        #     continue
        for (vals, count) in fn_var_values[fn].items():
            # I should probably catch errors here
            if not eval(cond, globals(), locals()):
                del fn_var_values[fn][vals]


def eliminate_ppt(fn_regexp, keep_matches=false):
    """Eliminate most mentions of program points matching the regular expression.
    If keep_matches is true, then program points matching the regular
    expression are retained rather than eliminated."""
    fn_regexp = util.re_compile_maybe(fn_regexp, re.IGNORECASE)
    if not fn_regexp:
        raise "Bad fn_regexp argument to eliminate_ppt"
    for fn in fn_var_infos.keys():
        match = fn_regexp.search(fn)
        if ((match and not keep_matches)
            or (keep_matches and not match)):
            del fn_var_infos[fn]
            del fn_truevars[fn]
            del fn_var_values[fn]
            del fn_samples[fn]
            if fn_derived_from.has_key(fn):
                del fn_derived_from[fn]
            # blech
            if functions.count(fn) > 0:
                functions.remove(fn)


###########################################################################
### Testing
###


# Run python from $inv/medic/data
# import daikon
# reload(daikon)

# daikon.clear_variables()
# daikon.read_invs('*.inv')
# daikon.read_invs('T*.inv', "clear first")
# daikon.read_invs('[TPD]*.inv', "clear first")
# daikon.all_numeric_invariants()

# As of 5/16/98, this took half an hour or more
# daikon.read_invs('*.inv', "clear first")

def read_merge_data_trace_file_declarations(filename, fn_regexp=None):
    if debug_read:
        print "read_merge_data_trace_file_declarations", filename, fn_regexp and fn_regexp.pattern
    this_fn_var_infos = read_data_trace_file_declarations(filename, fn_regexp)
    merge_var_infos(filename, this_fn_var_infos)

def read_merge_data_trace_file(filename, fn_regexp=None):
    if debug_read:
        print "read_merge_data_trace_file", filename, fn_regexp and fn_regexp.pattern
    (this_fn_var_values, this_fn_samples) = read_data_trace_file(filename, fn_regexp)
    merge_var_values(filename, this_fn_var_values, this_fn_samples)

# consider calling clear_variables() before calling this
def read_inv(filename="medic/invariants.raw"):
    read_merge_data_trace_file(filename)
    print_hashtables()

def read_decls_and_traces(files, clear=0, fn_regexp=None, num_files=None, random_seed=None):
    """Read declarations from the FILES, then read the traces in the FILES.
    When declarations and data traces are in separate files, it is more
    efficient to call read_declarations and then read_data_traces.

    FILES is either a sequence of file names or a single Unix file pattern.
    The remaining arguments are optional.
    CLEAR, if non-zero, means clear out global arrays first:  that is,
      replace old values with these, rather than appending these values.
    FN_REGEXP, if a string, is converted into a case-insensitive regular
      expression, and only program points matching it are considered.
    NUM_FILES indicates how many (randomly chosen) files are to be read;
      it defaults to all of the files.
    RANDOM_SEED is a triple of numbers, each in range(0,256), used to
      initialize the random number generator.
    """
    read_declarations(files, clear, fn_regexp)
    read_data_traces(files, clear, fn_regexp, num_files, random_seed)


def read_declarations(files, clear=0, fn_regexp=None):
    """Read function declarations from FILES.
    See read_decls_and_traces for more documentation.
    """
    if clear:
        clear_variables()
    fn_regexp = util.re_compile_maybe(fn_regexp, re.IGNORECASE)

    files_orig = files
    if type(files) == types.StringType:
        files = util.expand_file_name(files)
        files = glob.glob(files)
    if files == []:
        raise "No files specified"

    # Get all function names to add checks on ftn invocation counts
    for file in files:
        read_merge_data_trace_file_declarations(file, fn_regexp)
    after_processing_all_declarations()


def read_data_traces(files, clear=0, fn_regexp=None, num_files=None, random_seed=None):
    """Read data traces from FILES.
    See read_decls_and_traces for more documentation.
    """
    if clear:
        clear_trace_variables()
    fn_regexp = util.re_compile_maybe(fn_regexp, re.IGNORECASE)

    files_orig = files
    if type(files) == types.StringType:
        files = util.expand_file_name(files)
        files = glob.glob(files)
    if files == []:
        raise "No files specified"
    if num_files != None:
        if num_files == 0:
            raise "Requested 0 of %d files" % len(files)
        total_files = len(files)
        if num_files > total_files:
            raise "Requested %d files, but only %d supplied" % (num_files, total_files)
        if random_seed:
            (r1, r2, r3) = random_seed
            whrandom.seed(r1, r2, r3)
        files = util.random_subset(files, num_files)
        ## Skip this, too wordy.  We can always determine them later.
        # print num_files, "files randomly chosen:"
        # for file in files:
        #     print " ", file
    assert num_files == None or num_files == len(files)

    for file in files:
        read_merge_data_trace_file(file, fn_regexp)

    if __debug__:                       # for loop is outside assert, yuck
        for fname in fn_var_values.keys():
            assert ((len(fn_var_values[fname].keys()) == 0)
                    or (len(fn_var_infos[fname]) == len(fn_var_values[fname].keys()[0])))


def _test():
    _test_tri_linear_relationship()


# def foo():
#     fn_name = "READ-COMPACT-TRANSLATION"
#     fn_vars = fn_var_infos[fn_name]
#     num_vars = len(fn_vars)
#     for indices in util.choose(2, range(0,num_vars)):
#         this_dict = dict_of_tuples_slice(fn_var_values[fn_name], indices)
#         these_vars = util.slice_by_sequence(fn_vars, indices)
#         this_inv = two_scalar_numeric_invariant(this_dict)
#         # print fn_name, these_vars, this_inv, `this_inv`
#         print fn_name, these_vars
#         print "   ", `this_inv`
#         print "   ", this_inv

###########################################################################
### Gathering performance data
###

class stats:
    # Used to encapsulate all the stats we need to store for the invariant engine.
    # Used on a per function basis.

    # Instance variables:
    #  # Collected after reading in files and before deriving variables
    #  orig_num_scl_params = None
    #  orig_num_scl_locals = None
    #  orig_num_scl_globals = None
    #  orig_num_seq_params = None
    #  orig_num_seq_locals = None
    #  orig_num_seq_globals = None
    #  
    #  # Collected after deriving variables
    #  # Note that we aren't keeping track of derived parameters/locals/globals???
    #  total_num_scl = None
    #  total_num_seq = None
    #  
    #  samples = None
    #  
    #  # Collect the values separately for invariants on singles/pairs
    #  total_num_values_ind = None
    #  total_num_invs_pair = None
    #  total_num_values_pair = None
    #  
    #  # in secs...
    #  fn_begin_time = None
    #  fn_end_time = None
    #  #read_files_begin_time = None
    #  #read_files_end_time = None

    def __init__(self):
        self.orig_num_scl_params = 0
        self.orig_num_scl_locals = 0
        self.orig_num_scl_globals = 0
        self.orig_num_seq_params = 0
        self.orig_num_seq_locals = 0
        self.orig_num_seq_globals = 0

        self.total_num_scl = 0
        self.total_num_seq = 0

        self.samples = 0
        self.total_num_values_ind = 0
        self.total_num_invs_pair = 0
        self.total_num_values_pair = 0

        self.fn_begin_time = 0.0
        self.fn_end_time = 0.0
        self.fn_begin_time_wall = 0.0
        self.fn_end_time_wall = 0.0
        #self.read_files_begin_time = 0
        #self.read_files_end_time = 0

    def __setstate__(self, state):
        for key in state.keys():
            setattr(self, key, state[key])

    def format(self):
        total_secs_unrounded = self.fn_end_time - self.fn_begin_time
        total_secs = round(total_secs_unrounded, 3)

        hours = int(total_secs / 3600.0)
        minutes = int((total_secs % 3600)/60.0)
        seconds = float(total_secs % 60)
        rusage = resource.getrusage(resource.RUSAGE_SELF)
        print "    CPU time: %s hours, %s minutes, %s seconds" % (hours, minutes, seconds)
        print "    CPU time (secs):                       ", total_secs
        print "    Memory usage (kbytes):                 ", util.memory_usage()
        print "    Total number of scalars:               ", self.total_num_scl
        print "    Total number of sequences:             ", self.total_num_seq
        # next stmt not true if triples!?
        print "    Total number of invariants checked:    ", self.total_num_scl + self.total_num_seq + self.total_num_invs_pair
        print "    Total number of samples:               ", self.samples
        print "    Total number of individual values:     ", self.total_num_values_ind
        print "    Average number of individual values:   ", round(float(self.total_num_values_ind) / float(self.total_num_scl + self.total_num_seq), 2)
        print "    Total number of pairs of values:       ", self.total_num_values_pair
        if self.total_num_values_pair == 0:
            print "    Average number of pairs of values:     0.00"
        else:
            print "    Average number of pairs of values:     ", round(float(self.total_num_values_pair) / float(self.total_num_invs_pair), 2)
        print ""
        print "    Original number scalar parameters:     ", "%3i" % self.orig_num_scl_params
        print "    Original number scalar locals:         ", "%3i" % self.orig_num_scl_locals
        print "    Original number scalar globals:        ", "%3i" % self.orig_num_scl_globals
        print "    =================================="
        print "    Total original number scalars:         ", "%3i" % (self.orig_num_scl_params + self.orig_num_scl_locals + self.orig_num_scl_globals)
        print ""
        print "    Original number sequence parameters:   ", "%3i" % self.orig_num_seq_params
        print "    Original number sequence locals:       ", "%3i" % self.orig_num_seq_locals
        print "    Original number sequence globals:      ", "%3i" % self.orig_num_seq_globals
        print "    =================================="
        print "    Total original number sequences:       ", "%3i" % (self.orig_num_seq_params + self.orig_num_seq_locals + self.orig_num_seq_globals)
        print ""
        print "    Derived number of scalars:             ", "%3i" % (self.total_num_scl - (self.orig_num_scl_params + self.orig_num_scl_locals + self.orig_num_scl_globals))
        print "    Derived number of sequences:           ", "%3i" % (self.total_num_seq - (self.orig_num_seq_params + self.orig_num_seq_locals + self.orig_num_seq_globals))


# def init_collect_stats():
#     """ Initialize the function to stats dictionary."""
#     for fn_name in fn_var_infos.keys():
#         fn_to_stats[fn_name] = stats()

def get_global_var_list():
    """Compile a list of globals for use in stat collection."""

    # Grab variable list for first function as possible globals.
    # Eliminate elements that don't appear in other function var lists.
    function_names = fn_var_infos.keys()
    possible_global_var_infos = fn_var_infos[function_names[0]]
    globals = []
    for a_var_info in possible_global_var_infos:
        is_global_var = true
        for var_info_list in fn_var_infos.values():
            if a_var_info.name not in map(lambda vi: vi.name, var_info_list):
                is_global_var = false
                continue
        if is_global_var:
            globals.append(a_var_info.name)
    return globals

# def collect_pre_derive_data():
#     init_collect_stats()
#     globals = get_global_var_list()
# 
#     for (fn_name,var_info_list) in fn_var_infos.items():
#         fn_stats = fn_to_stats[fn_name]
#         (fn_name_sans_suffix, suffix) = (string.split(fn_name, ":::", 1) + [""])[0:2]
#         # The list of *original* paramters, as opposed to derived ones (???).
#         params = foobar["*****"]
#         for var in var_info_list:
#             # original values of parameters should be considered derived?
#             if string.find(var.name, "_orig") == -1:
#                 if var.type.is_array():
#                     if var.name in params:
#                         fn_stats.orig_num_seq_params = fn_stats.orig_num_seq_params + 1
#                     elif var.name in globals:
#                         fn_stats.orig_num_seq_globals = fn_stats.orig_num_seq_globals + 1
#                     else:
#                         fn_stats.orig_num_seq_locals = fn_stats.orig_num_seq_locals + 1
#                 else:
#                     if var.name in params:
#                         fn_stats.orig_num_scl_params = fn_stats.orig_num_scl_params + 1
#                     elif var.name in globals:
#                         fn_stats.orig_num_scl_globals = fn_stats.orig_num_scl_globals + 1
#                     else:
#                         fn_stats.orig_num_scl_locals = fn_stats.orig_num_scl_locals + 1
# 
# def collect_post_derive_data():
#     for (fn_name,var_info_list) in fn_var_infos.items():
#         fn_stats = fn_to_stats[fn_name]
#         fn_stats.samples = fn_samples[fn_name]
#         for var in var_info_list:
#             if var.invariant == None:
#                 # When can this happen?  (I know it can.)
#                 print "Warning: no invariant for variable", var, "in function", fn_name
#             else:
#                 fn_stats.total_num_values_ind = fn_stats.total_num_values_ind + var.invariant.values
# 
#             # Count all values for pairs of invariants.
#             # Be careful not to double count.  For example, if there is a
#             # pair invariant for indices (2,7), the invariant will appear
#             # in index 2's invariants[7] and in index 7's invariants[2].
#             # So we'll only include the invariants[j] in the list for index
#             # i if i < j.
#             # This doesn't work if include triple invariants???
# 
#             for i in var.invariants.keys():
#                 if i > var.index:
#                     fn_stats.total_num_values_pair = fn_stats.total_num_values_pair + var.invariants[i].values
#                     fn_stats.total_num_invs_pair = fn_stats.total_num_invs_pair + 1
# 
#             if var.type.is_array():
#                     fn_stats.total_num_seq = fn_stats.total_num_seq + 1
#             else:
#                     fn_stats.total_num_scl = fn_stats.total_num_scl + 1
# 
# def begin_fn_timing(fn):
#     fn_stats = fn_to_stats[fn]
#     fn_stats.fn_begin_time = time.clock()
#     fn_stats.fn_begin_time_wall = time.time()
# 
# 
# def end_fn_timing(fn):
#     fn_stats = fn_to_stats[fn]
#     fn_stats.fn_end_time = time.clock()
#     fn_stats.fn_end_time_wall = time.time()
# 
# def print_stats(engine_begin_time, engine_end_time, engine_begin_time_wall, engine_end_time_wall):
#     print "Invariant Engine Stats"
#     print "Configuration: no_invocation_counts: %s, no_ternary_invariants: %s, no_opts: %s" % (no_invocation_counts, no_ternary_invariants, __debug__)
# 
#     total_secs = engine_end_time - engine_begin_time
#     total_secs_wall = engine_end_time_wall - engine_begin_time_wall
#     # for (fn_name,fn_stats) in fn_to_stats.items():
#     #    total_secs = total_secs + (fn_stats.fn_end_time - fn_stats.fn_begin_time)
#     hours = int(total_secs / 3600.0)
#     minutes = int((total_secs % 3600)/60.0)
#     seconds = int(total_secs % 60)
#     print "CPU time: %s hours, %s minutes, %s seconds" % (hours, minutes, seconds)
#     print "CPU time in secs: ", total_secs
#     print "Wall time in secs: ", total_secs_wall
#     fn_names = fn_to_stats.keys()
#     fn_names.sort()
#     for fn_name in fn_names:
#          fn_stats = fn_to_stats[fn_name]
#          print "==============================================================================="
#          print fn_name
#          fn_stats.format()
