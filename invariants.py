#!/uns/bin/python1.5
# invariants.py -- detect patterns in collections of data
# Michael Ernst <mernst@cs.washington.edu>

# For some additional documentation, see invariants.py.doc.

import glob, operator, os, re, string, types, copy, posix

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
    no_ternary_invariants = false

    ### Internal variables
    fn_var_infos = {}           # from function name to list of var_infos
    fn_var_values = {}	    # from function name to (tuple of values to occurrence count)

    fn_samples = {}         # from function name to number of samples
    file_fn_var_infos = {}  # from filename to (infos, values, samples) tuple

    # From function name to invocation count.  Used to add invocation
    # count variables for each program function
    ftn_names_to_call_ct = {}

    # From function name to (parameter names to list of param values)
    # Used to store parameter list for functions.  Also used to store
    # the original value of parameters at the beginning of a ftn call
    # so we can use them in comparison at the end of a ftn call.
    # We have a list for the param values to provide for recursive
    # function calls.  As we pop recursive calls to the function off
    # the stack, we pop orig parameter vals off the list.
    ftn_to_orig_param_vals = {}

def clear_variables():
    """Reset the values of some global variables."""
    no_ternary_invariants = false

    fn_var_infos.clear()
    fn_var_values.clear()
    fn_samples.clear()
    file_fn_var_infos.clear()
    ftn_names_to_call_ct = {}
    ftn_to_orig_param_vals = {}

def clear_invariants(fn_regexp=None):
    """Reset the values of invariants, globally."""
    for fn_name in fn_var_infos.keys():
        if fn_regexp and fn_regexp.match(fn_name):
            continue
        for vi in fn_var_infos[fn_name]:
            vi.invariant = None
            vi.invariants = {}


integer_re = re.compile(r'^-?[0-9]+$')
float_re = re.compile(r'^-?[0-9]*\.[0-9]+$|^-?[0-9]+\.[0-9]*$')
# variable name for a sequence; ends with "[]" or "[XXX..YYY]"
# This should only be used once per variable; after that, read out the
# type from the var_info object
sequence_re = re.compile(r'^[^	]+\[(|[^][]+\.\.[^][]+)\]$')


class var_info:
    name = None                         # string
    type = None                         # type information
    index = None                        # index in lists of variables

    # info about derived variables: both derivees from this and derivers of this
    derived_len = None                  # index of len variable
    derived = None                      # will be variables derived from this
                                        #   one; presently not used.
    is_derived = None                   # boolean (for now)

    invariant = None                    # not sure
    # To find the invariant over a pair of variables, do a double-dispatch:
    # first look up the "invariants" field of one of the variables, then
    # look up the other variable in that map.

    invariants = None         # map from indices to multiple-arity invariants
    equal_to = None                     # list of indices of equal variables;
                                        #   could be derived from invariants
                                        #   by checking for inv.comparision == "="

    def __init__(self, name, var_type, index, is_derived=false):
        assert type(name) == types.StringType
        assert type(index) == types.IntType
        self.name = name
        self.type = var_type
        self.index = index
        self.derived = {}
        self.is_derived = is_derived
        self.invariant = None
        self.invariants = {}
        self.equal_to = []

    def __repr__(self):
        return "<var %s %s>" % (self.name, self.type)

    def is_sequence(self):
        return self.type == types.ListType

    def canonical_var(self):
        """Return index of the canonical variable that is always equal to this one.
        Return None if no such variable exists."""

        assert util.sorted(self.equal_to)
        if self.equal_to == []:
            return None
        else:
            return self.equal_to[0]

    def is_canonical(self):
        assert self.index != None
        assert self.equal_to == [] or self.canonical_var != None
        return self.equal_to == [] or self.canonical_var > self.index


def var_info_name_compare(vi1, vi2):
    return cmp(vi1.name, vi2.name)

def var_infos_compatible(vis1, vis2):
    """The arguments are lists of var_info objects."""
    # This just checks that the names are the same.
    return (map(lambda vi: vi.name, vis1) == map(lambda vi: vi.name, vis2))


def merge_variables(filename, sub_fn_var_infos, sub_fn_var_values, sub_fn_samples):
    """Merge the values for the arguments into the corresponding global variables.
    See `read_file' for a description of the argument types; arguments 2-4 are
    dictionaries mapping from a function name to information about the function.
    """

    assert not(file_fn_var_infos.has_key(filename))
    for fname in sub_fn_var_infos.keys():
        sub_var_infos = sub_fn_var_infos[fname]
	if not(fn_var_infos.has_key(fname)):
            var_infos = []
            for vi in sub_var_infos:
                var_infos.append(var_info(vi.name, vi.type, len(var_infos)))
	    fn_var_infos[fname] = var_infos
            fn_var_values[fname] = {}
	else:
            assert var_infos_compatible(fn_var_infos[fname], sub_fn_var_infos[fname])
        var_infos = fn_var_infos[fname]
        var_values = fn_var_values[fname]
        sub_var_values = sub_fn_var_values[fname]
        for (values, count) in sub_var_values.items():
            # var_values[value] += count
            util.mapping_increment(var_values, values, count)
        # fn_samples[fname] += sub_fn_samples[fname]
        util.mapping_increment(fn_samples, fname, sub_fn_samples[fname])
    file_fn_var_infos[filename] = sub_fn_var_infos


def dict_of_tuples_to_tuple_of_dicts(dot, tuple_len=None):
    """Input: a dictionary mapping a tuple of elements to a count.
    All the key tuples in the input have the same length unless optional argument
    TUPLE_LEN is provided, in which case all tuples have at least that length.
    If TUPLE_LEN is a tuple, then only those indices are extracted.
    if TUPLE_LEN is an integer, indices up to it (non-inclusive) are extracted.
    Output: a tuple of dictionaries, each mapping a single element to a count.
    The first output dictionary concerns the first element of the original keys,
    the second output the second element of the original keys, and so forth."""

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
            this_key = key_tuple[tuple_indices[i]]
            this_dict = result[i]
            util.mapping_increment(this_dict, this_key, count)
    return result
# dict_of_tuples_to_tuple_of_dicts(fn_var_values["PUSH-ACTION"])


def dict_of_sequences_to_element_dict(dot):
    """Input: a dictionary mapping instances of a sequence (tuples) to a count.
    Output: a dictionary, mapping elements of all of the sequence instances
    to a count."""
    result = {}
    for (key_tuple, count) in dot.items():
        for this_key in key_tuple:
            util.mapping_increment(result, this_key, count)
    return result
# dict_of_sequences_to_element_dict(dot)


def dict_of_tuples_slice(dot, indices):
    """Input: a dictionary mapping a tuple of elements to a count, and a
    list of indices.
    Output: a dictionary mapping a subset of the original elements to a count.
    The subset is chosen according to the input indices."""

    result = {}
    for (key_tuple, count) in dot.items():
        sliced_tuple = util.slice_by_sequence(key_tuple, indices)
        util.mapping_increment(result, sliced_tuple, count)
    return result
# dict_of_tuples_slice(fn_var_values["PUSH-ACTION"], (0,))
# dict_of_tuples_slice(fn_var_values["PUSH-ACTION"], (1,))
# dict_of_tuples_slice(fn_var_values["VERIFY-CLEAN-PARALLEL"], (0,))
# dict_of_tuples_slice(fn_var_values["VERIFY-CLEAN-PARALLEL"], (1,))
# dict_of_tuples_slice(fn_var_values["VERIFY-CLEAN-PARALLEL"], (2,))
# dict_of_tuples_slice(fn_var_values["VERIFY-CLEAN-PARALLEL"], (0,1))
# dict_of_tuples_slice(fn_var_values["VERIFY-CLEAN-PARALLEL"], (0,2))
# dict_of_tuples_slice(fn_var_values["VERIFY-CLEAN-PARALLEL"], (1,2))



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
# Record information about how derived values were derived from something,
# so as to avoid dumb invariants.
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
      VAR_INFOS: list of var_info objects
      INDICES: only values (partially) computed from these indices are candidates
      FUNCTIONS: (long) list of functions for adding new variables; see the code
    The first argument is typically an elements of global variable fn_var_infos;
    in this function, we operate over only one function, not all functions.
    """

    # print "introduce_new_variables_one_pass: indices %s (limit %s), functions %s" % (indices, len(var_infos), functions)

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
        if this_var_info.is_sequence():
            intro_from_sequence(var_infos, var_new_values, i)
        else:
            intro_from_scalar(var_infos, var_new_values, i)

    # It's cleaner to do "for (i1,i2) in util.choose(2, all_indices):", but
    # also less efficient due to creation of long list of pairs.
    for i1 in range(0,orig_len_var_infos-1):
        if not var_infos[i1].is_canonical():
            continue
        for i2 in range(i1+1,orig_len_var_infos):
            if not var_infos[i2].is_canonical():
                continue
            if not (i1 in indices or i2 in indices):
                # Do nothing if neither of these variables is under consideration.
                continue
            this_dict = dict_of_tuples_slice(var_values, (i1, i2))
            these_vars = util.slice_by_sequence(var_infos, (i1, i2))
            if these_vars[0].is_sequence() and these_vars[1].is_sequence():
                intro_from_sequence_sequence(var_infos, var_new_values, i1, i2)
            elif these_vars[0].is_sequence():
                intro_from_sequence_scalar(var_infos, var_new_values, i1, i2)
            elif these_vars[1].is_sequence():
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
        seq_len_var_info = var_info(name_size, types.IntType, len(var_infos), true)
        var_infos.append(seq_len_var_info)
        seq_var_info.derived_len = len(var_infos)-1
        # print "set derived_len for", seq_var_info, "to", len(var_infos)-1

        for new_values in var_new_values.values():
            this_seq = new_values[index]
            if this_seq == None:
                this_seq_len = None
            else:
                this_seq_len = len(this_seq)
            new_values.append(this_seq_len)


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

    # Add sum (unconditionally)
    var_infos.append(var_info("sum(%s)" % (seqvar,), types.IntType, len(var_infos), true))
    for new_values in var_new_values.values():
        this_seq = new_values[seqidx]
        if this_seq == None:
            this_seq_sum = None
        else:
            this_seq_sum = util.sum(this_seq)
        new_values.append(this_seq_sum)

    # Add each individual element.
    ## For now, add if not a derived variable; a better test is if
    ## not a prefix subsequence (sequence slice) we have added.
    if not seq_var_info.is_derived:
        seq_len_inv = var_infos[seq_var_info.derived_len].invariant
        assert isinstance(seq_var_info, var_info)
        len_min = seq_len_inv.min
        # The point of this is not to do checks over every last irrelevant
        # element; just look at the one or two at the beginning and the end.
        len_min = min(2, len_min)
        if len_min > 0:
            for i in range(0, len_min):
                var_infos.append(var_info("%s[%s]" % (seqvar, i), types.IntType, len(var_infos), true))
            for new_values in var_new_values.values():
                for i in range(0, len_min):
                    seq = new_values[seqidx]
                    if seq == None:
                        elt_val = None
                    else:
                        elt_val = seq[i]
                    new_values.append(elt_val)
            if len_min != seq_len_inv.max:
                for i in range(-len_min, 0):
                    var_infos.append(var_info("%s[%s]" % (seqvar, i), types.IntType, len(var_infos), true))
                for new_values in var_new_values.values():
                    for i in range(-len_min, 0):
                        seq = new_values[seqidx]
                        if seq == None:
                            elt_val = None
                        else:
                            elt_val = seq[i]
                        new_values.append(elt_val)


def introduce_from_scalar_pass2(var_infos, var_new_values, index):
    assert len(var_infos) == len(var_new_values.values()[0])
    pass

def introduce_from_sequence_sequence_pass2(var_infos, var_new_values, i1, i2):
    assert len(var_infos) == len(var_new_values.values()[0])
    pass

def introduce_from_sequence_scalar_pass2(var_infos, var_new_values, seqidx, sclidx):
    assert len(var_infos) == len(var_new_values.values()[0])

    seqvar = var_infos[seqidx].name
    sclvar = var_infos[sclidx].name
    scl_inv = var_infos[sclidx].invariant
    seq_size_idx = var_infos[seqidx].derived_len

    # Do nothing if this scalar is actually the size of this sequence
    if seq_size_idx == sclidx:
        return

    #     if seq_size_idx == 'no_var':
    #         print "sequence %s (size: no_var) and scalar %s (index: %s) unrelated" % (seqvar, sclvar, sclidx)
    #     else:
    #         print "sequence %s (size: %s, size index = %s) and scalar %s (index: %s) unrelated" % (seqvar, var_infos[seq_size_idx].name, seq_size_idx, sclvar, sclidx)

    # Add subsequences
    if not var_infos[seqidx].is_derived and not var_infos[sclidx].invariant.can_be_None:
        full_var_info = var_info("%s[0..%s]" % (seqvar, sclvar), types.ListType, len(var_infos), true)
        full_var_info.derived_len = sclidx
        var_infos.append(full_var_info)
        less_one_var_info = var_info("%s[0..%s-1]" % (seqvar, sclvar), types.ListType, len(var_infos), true)
        # 'no_var' means there is a known value, but no variable
        # holds that particular value.
        less_one_var_info.derived_len = 'no_var'
        var_infos.append(less_one_var_info)
        for new_values in var_new_values.values():
            seq = new_values[seqidx]
            scl = new_values[sclidx]
            if (scl+1 <= len(seq)) and (scl+1 >= 0):
                new_value_full = seq[0:scl+1]
            else:
                new_value_full = None
            if (scl <= len(seq)) and (scl >= 0):
                new_value_less_one = seq[0:scl]
            else:
                new_value_less_one = None
            # print "seq %s = %s (len = %s), scl %s = %s, new_value_less_one = %s" % (seqvar, seq, len(seq), sclvar, scl, new_value_less_one)
            new_values.append(new_value_full)
            new_values.append(new_value_less_one)

    # Add scalars
    # Determine whether it is constant; if so, ignore.
    # Perhaps also check that it is within range at least once
    # (or even every time) if not, not very interesting.
    if ((not var_infos[seqidx].is_derived)
        and (not scl_inv.is_exact()) and (scl_inv.min >= 0)
        and (seq_size_idx != 'no_var')
        and (scl_inv.max <= var_infos[seq_size_idx].invariant.max)):
        var_infos.append(var_info("%s[%s]" % (seqvar, sclvar), types.IntType, len(var_infos), true))
        for new_values in var_new_values.values():
            this_seq = new_values[seqidx]
            this_scl = new_values[sclidx]
            if ((this_seq != None) and (this_scl != None)
                and (this_scl < len(this_seq)) and (this_scl >= 0)):
                new_values.append(this_seq[this_scl])
            else:
                new_values.append(None)



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
#     information about the function; see `read_file' for a description."""
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
#                     # print "seq %s = %s (len = %s), scl %s = %s, new_value_less_one = %s" % (seqvar, seq, len(seq), sclvar, scl, new_value_less_one)
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
#                         elt_name = "%s[%s]" % (seqvar, i)
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
#                             elt_name = "%s[%s]" % (seqvar, i)
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
#     See `read_file' for a description of the argument types; arguments are
#     dictionaries mapping from a function name to information about the function."""
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

# An instrumented program produces a .inv file containing information about
# run-time values of expressions and variables.  The invariant detector tries
# to find patterns in the values recorded in one or more .inv files.
# 
# To detect invariants in a particular program, it is enough to insert code
# in the application which creates a .inv file.  In Lisp, the
# `check-for-invariants' macro performs this task.  Gries-style Lisp
# programs can be automatically instrumented -- the calls to
# `check-for-invariants' are inserted by the `instrument' function found in
# gries-helper.lisp.  Given a file of Gries-style Lisp functions,
# `instrument' produces a new file of instrumented Lisp code which can be
# compiled and run.
# 
# Each entry of a .inv file is of the form
# 
#   tag
#   varname1	value1
#   varname2	value2
#   ...
# 
# The tag is an arbitrary alphanumeric string indicating the program point
# at which this data was collected.  Varnames are separated by values by a
# tab (\t) character.  The varnames are uninterpreted strings (but for a
# given tag, the variable names must be consistent across different
# entries).  Tags and varnames may not contain the tab (\t) character.
# Currently the values are integers; this will be extended soon.

def read_file_ftns(filename, fn_regexp=None):
    """Read data from .inv file; add to dictionary mapping file
    names to invocation counts.  The invocation counts are initialized
    to zero.  Also initialize dict mapping files to parameters to
    original values."""

    if type(fn_regexp) == types.StringType:
        fn_regexp = re.compile(fn_regexp)

    file = open(filename, "r")
    line = file.readline()
    if "\t" in line:
        raise "First line should be tag line; saw: " + line

    while (line != ""):         # line == "" when we hit end of file
        if fn_regexp == None or fn_regexp.match(line):
            (tag, leftover) = string.split(line, ":::", 1)
            ftn_names_to_call_ct[tag] = 0

            # Get parameter list and initialize ftn to param vals dict
            label_and_params = string.split(leftover, "(", 1)
            if len(label_and_params) == 1:
                # Backward compatibility if no parameter list specified
                label = label_and_params
                param_list = ""
            else:
                (label, param_list) = label_and_params
            param_list = param_list[:-2] # remove trailing newline and ')'
            param_list = re.split("[, ]", param_list)
            params_to_orig_val_list = {}
            for param in param_list:
                if param != '': # handle empty parameter list???
                    params_to_orig_val_list[param] = []
            ftn_to_orig_param_vals[tag] = params_to_orig_val_list

        line = file.readline()
        while "\t" in line:
            # skip over (variable,value) line
            line = file.readline()

def init_ftn_call_ct():
    """Initialize ftn call counts to 0."""
    for ftn_tag in ftn_names_to_call_ct.keys():
        ftn_names_to_call_ct[ftn_tag] = 0

def read_file(filename, fn_regexp=None):
    """Read data from .inv file; return a tuple of three dictionaries.
     * map from function name to tuple of variable names.
     * map from function name to (map from tuple of values to occurrence count)
     * map from function name to number of samples
    """

    if type(fn_regexp) == types.StringType:
        fn_regexp = re.compile(fn_regexp)

    file = open(filename, "r")

    this_fn_var_infos = {}		# from function name to tuple of variable names
    this_fn_var_values = {}	# from function name to (tuple of values to occurrence count)
    this_fn_samples = {}           # from function name to number of samples
    this_ftn_names = {}         # from function name to current number
                                # of invocations

    this_ftn_names = read_file_ftns(filename)

    init_ftn_call_ct()          # initialize function call cts to 0
    line = file.readline()
    if "\t" in line:
        raise "First line should be tag line; saw: " + line

    while (line != ""):                 # line == "" when we hit end of file
        # line contains no tab character
        tag = line[:-1]                 # remove trailing newline

        if fn_regexp != None and not fn_regexp.match(line):
            while "\t" in line:
                line = file.readline()
            continue

        # Increment function invocation count if ':::BEGIN'
        (tag_sans_suffix, suffix) = string.split(tag, ":::", 1)
        label = string.split(suffix, "(", 1)[0]
        if label == "BEGIN":
            util.mapping_increment(ftn_names_to_call_ct, tag_sans_suffix, 1)

        # Get param to val list for the function
        # Accessing global here, crappy
        params_to_orig_val_list = {}
        params_to_orig_val_list = ftn_to_orig_param_vals[tag_sans_suffix]

        these_var_infos = []
	these_values = []
        line = file.readline()
        while "\t" in line:
            (this_var_name,this_value) = string.split(line, "\t", 1)
            this_var_name_orig = this_var_name
            if sequence_re.match(this_var_name) or re.match("^#\((.*)\)$", this_value):
                # variable is a sequence
                this_var_type = types.ListType
                if this_var_name[-2:] == "[]":
                    this_var_name = this_var_name[:-2]
                lisp_delimiters = re.match("^#\((.*)\)$", this_value)
                if lisp_delimiters:
                    this_value = lisp_delimiters.group(1)
                this_value = string.split(this_value, " ")
                # Can't do this unconditionally if we've eliminated the
                # lisp_delimiters (which also strips trailing newline?).
                if this_value[-1] == "\n":
                    this_value = this_value[:-1]   # remove trailing newline
                for seq_elem in range(0, len(this_value)):
                    # dumb to copy this: fix it
                    if integer_re.match(this_value[seq_elem]):
                        this_value[seq_elem] = int(this_value[seq_elem])
                    elif float_re.match(this_value[seq_elem]):
                        this_value[seq_elem] = float(this_value[seq_elem])
                    elif this_value[seq_elem] == "NIL":
                        # HACK
                        this_value[seq_elem] = 0
                    else:
                        raise "What value? " + `this_value[seq_elem]`
                this_value = tuple(this_value)
            else:
                this_var_type = types.IntType
                if integer_re.match(this_value):
                    this_value = int(this_value)
                elif float_re.match(this_value):
                    this_value = float(this_value)
                elif this_value == "NIL":
                    # HACK
                    this_value = 0
                else:
                    raise "What value?"
	    these_var_infos.append(var_info(this_var_name, this_var_type, len(these_var_infos)))
	    these_values.append(this_value)
            # print this_var_name, this_value

            # If beginning of function, store the original param val
            if label == "BEGIN":
                if this_var_name_orig in params_to_orig_val_list.keys():
                    param_list = []
                    param_list = params_to_orig_val_list[this_var_name_orig]
                    param_list.append(this_value)

            line = file.readline()
        # Add invocation counts
        # Accessing global here-crappy
        for ftn_tag in ftn_names_to_call_ct.keys():
            these_var_infos.append(var_info("calls(%s)" % (ftn_tag,), types.IntType, len(these_var_infos)))
            these_values.append(ftn_names_to_call_ct[ftn_tag])

        # Add original parameter values if end of function call
        # Then pop previous original param value off of param val list
        if label == "END":
            for (param, param_list) in params_to_orig_val_list.items():
                # Shouldn't need to do this regexp match; just look up the type
                if sequence_re.match(param):
                    if param[-2:] == "[]":
                        param = param[:-2]
                    these_var_infos.append(var_info(param + "_orig", types.ListType, len(these_var_infos)))
                else:
                    these_var_infos.append(var_info(param + "_orig", types.IntType, len(these_var_infos)))
                these_values.append(param_list[-1])
                del param_list[-1]

	these_values = tuple(these_values)
	if not(this_fn_var_infos.has_key(tag)):
	    this_fn_var_infos[tag] = these_var_infos
	    this_fn_var_values[tag] = {}
	else:
	    assert var_infos_compatible(this_fn_var_infos[tag], these_var_infos)
	    assert type(this_fn_var_values[tag]) == types.DictType
        util.mapping_increment(this_fn_var_values[tag], these_values, 1)
        util.mapping_increment(this_fn_samples, tag, 1)

    return (this_fn_var_infos, this_fn_var_values, this_fn_samples)


def print_hashtables():
    """Print the important global hashtables.  Principally for debugging."""
    function_names = fn_var_infos.keys()
    function_names.sort()
    for fn_name in function_names:
        print "==========================================================================="
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

# A negative invariant is not reported unless the chance that the invariant
# only happens not to be true (and is not a true invariant) is at least this low.
negative_invariant_confidence = .01     # .05 might also be reasonable

## An invariant may be exact or approximate.  If an invariant is exact,
## then supersets of the variables in it are not supplied to higher-arity
## invariants.  For instance, if the variables are w,x,y,z, and an exact
## invariant is found over x, then only the three pairs (w,y), (w,z), and
## (y,z) are checked for two_scalar_numeric_invariant.

def all_numeric_invariants(fn_regexp=None):
    """Compute and print all the numeric invariants."""
    if type(fn_regexp) == types.StringType:
        fn_regexp = re.compile(fn_regexp)

    clear_invariants(fn_regexp)

    for fn_name in fn_var_infos.keys():
        if fn_regexp and fn_regexp.match(fn_name):
            continue
        # If we discover that two values are equal, then there is no sense
        # in using both at any later stage; eliminate one of them and so
        # avoid redundant computation.
        # So don't derive anything from a variable until we've checked it
        # for equality with everything else (ie, until we have inferred
        # invariants over it).

        ## For the moment, don't do this recursive looping thing.
        # compute_invariants(via_calls_to, numeric_invariants_over_index,
        #                    and_to, introduce_new_variables)

        var_infos = fn_var_infos[fn_name]
        var_values = fn_var_values[fn_name]

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
                assert vi.type != types.ListType or vi.derived_len != None
            # print "old derivation_index =", derivation_index, "num_vars =", len(var_infos)

            # If derivation_index == (a, b, c) and n = len(var_infos), then
            # the body of this loop:
            #     * computes invariants over a..n
            #     * does pass1 introduction for b..a
            #     * does pass2 introduction for c..a
            # and afterward, derivation_index == (n, a, b).

            # original number of vars; this body may well add more
            num_vars = len(var_infos)
            numeric_invariants_over_index(
                range(derivation_index[0], num_vars), var_infos, var_values)

            for pass_no in range(1,derivation_passes+1):
                # print "pass", pass_no, "range", derivation_index[pass_no], derivation_index[pass_no-1]
                if derivation_index[pass_no] == derivation_index[pass_no-1]:
                    continue
                introduce_new_variables_one_pass(
                    var_infos, var_values,
                    range(derivation_index[pass_no], derivation_index[pass_no-1]),
                    derivation_functions[pass_no])

            derivation_index = (num_vars,) + derivation_index[:-1]
            # print "new derivation_index =", derivation_index, "num_vars =", len(var_infos)


        assert len(var_infos) == len(var_values.keys()[0])

    print_invariants(fn_regexp)
## Testing:
# all_numeric_invariants()


def numeric_invariants_over_index(indices, var_infos, var_values):
    """Install invariants in VAR_INFOS.
    The installed invariants all have at least one element in INDICES.
    VAR_INFOS and VAR_VALUES are elements of globals `fn_var_infos' and
    `fn_var_values'."""

    if indices == []:
        return

    # print "numeric_invariants_over_index", indices

    # (Intentionally) ignores anything added by body,
    # though probably nothing should be added by this body.
    index_limit = len(var_infos)
    # all_indices = range(0, len(var_infos))

    assert len(var_infos) == len(var_values.keys()[0])

    # Single invariants
    dicts = dict_of_tuples_to_tuple_of_dicts(var_values, indices)
    non_exact_single_invs = []      # list of indices
    for j in range(0, len(indices)):
        i = indices[j]
        this_var_info = var_infos[i]
        this_dict = dicts[j]
        if this_var_info.type == types.ListType:
            this_inv = single_sequence_numeric_invariant(this_dict)
        else:
            this_inv = single_scalar_numeric_invariant(this_dict)
        assert this_var_info.invariant == None
        assert this_inv != None
        # print "Setting invariant for index", i, "to", this_inv
        this_var_info.invariant = this_inv

    # print "numeric_invariants_over_index: done with single invariants, starting pairs"

    # Invariant pairs
    ## Don't do this; the large list of pairs can exhaust memory, though
    ## the code is cleaner in that case.
    # for (i1,i2) in util.choose(2, all_indices):
    for i1 in range(0,index_limit-1):
        inv1 = var_infos[i1].invariant
        if inv1.is_exact():
            continue
        if inv1.can_be_None:
            continue
        if not var_infos[i1].is_canonical():
            continue
        for i2 in range(i1+1, index_limit):
            # if i1 == i2:
            #     continue
            if (not i1 in indices) and (not i2 in indices):
                # Do nothing if neither variable is under consideration.
                continue
            inv2 = var_infos[i2].invariant
            if inv2.is_exact():
                # Do nothing if either of the variables is a constant.
                continue
            if inv2.can_be_None:
                # Do nothing if either of the variables can be missing.
                continue
            if not var_infos[i2].is_canonical():
                continue
            values = dict_of_tuples_slice(var_values, (i1,i2))
            these_var_infos = util.slice_by_sequence(var_infos, (i1,i2))
            if these_var_infos[0].is_sequence() and these_var_infos[1].is_sequence():
                this_inv = two_sequence_numeric_invariant(values)
            elif these_var_infos[0].is_sequence() or these_var_infos[1].is_sequence():
                this_inv = scalar_sequence_numeric_invariant(values)
            else:
                this_inv = two_scalar_numeric_invariant(values)

            #### NEED TO REIMPLEMENT THIS.
            # if this_inv.is_equality():
            #     # arbitrarily remove second one; fix this later
            #     non_exact_single_invs.remove(indices[1])
            # elif this_inv.is_exact():
            #     exact_pair_invs.append(indices)

            assert not var_infos[i1].invariants.has_key(i2)
            var_infos[i1].invariants[i2] = this_inv
            if ((isinstance(this_inv, two_scalar_numeric_invariant)
                 or isinstance(this_inv, two_sequence_numeric_invariant))
                and (this_inv.comparison == "=")):
                var_infos[i1].equal_to.append(i2)
                var_infos[i2].equal_to.append(i1)


    # print "numeric_invariants_over_index: done with pairs, starting triples"

    # Invariant triples
    if no_ternary_invariants == true:
        return

    ## Don't do this; the large list of triples can exhaust memory, though
    ## the code is cleaner in that case.
    # for (i1,i2,i3) in util.choose(3, all_indices):
    for i1 in range(0,index_limit-2):
        # print "triples: index1 =", i1
        if var_infos[i1].invariant.is_exact():
            continue
        if var_infos[i1].invariant.can_be_None:
            continue
        if not var_infos[i1].is_canonical():
            continue
        for i2 in range(i1+1, index_limit-1):
            # print "triples: index2 =", i2
            if i1 == i2:
                continue
            if var_infos[i2].invariant.is_exact():
                continue
            if (var_infos[i1].invariants.has_key(i2)
                and var_infos[i1].invariants[i2].is_exact()):
                continue
            if var_infos[i2].invariant.can_be_None:
                continue
            if not var_infos[i2].is_canonical():
                continue
            for i3 in range(i2+1, index_limit):
                if i1 == i3 or i2 == i3:
                    continue
                if (not i1 in indices) and (not i2 in indices) and (not i3 in indices):
                    # Do nothing if none of the variables is under consideration.
                    continue
                if var_infos[i3].invariant.is_exact():
                    # Do nothing if any of the variables is a constant.
                    continue
                if ((var_infos[i1].invariants.has_key(i3)
                     and var_infos[i1].invariants[i3].is_exact())
                    or (var_infos[i2].invariants.has_key(i3)
                        and var_infos[i2].invariants[i3].is_exact())):
                    # Do nothing if any of the pairs are exactly related.
                    continue
                if var_infos[i3].invariant.can_be_None:
                    # Do nothing if any of the variables can be missing.
                    continue
                if not var_infos[i3].is_canonical():
                    continue

                values = dict_of_tuples_slice(var_values, (i1,i2,i3))
                these_var_infos = util.slice_by_sequence(var_infos, (i1,i2,i3))
                if (these_var_infos[0].is_sequence()
                    or these_var_infos[1].is_sequence()
                    or these_var_infos[2].is_sequence()):
                    # print "got sequence in a triple"
                    this_inv = invariant(values)
                else:
                    this_inv = three_scalar_numeric_invariant(values)
                assert not var_infos[i1].invariants.has_key((i2,i3))
                var_infos[i1].invariants[(i2,i3)] = this_inv


### A lot of the logic here should be moved into invariant computation;
### then printing should just print all the existing invariants, or consult
### some other data structure regarding whether to print invariants.  (???)

# Ideas on better output in the future:
#  * put all the comparisons together and at the end, and order them cleverly
#  * when multiple things are equal to one another, put them together
#  * put info about a particular array together when possible

# Maybe this should (optionally?) print just for a given fn_name, so that
# I can provide output more interactively and quickly.
def print_invariants(fn_regexp=None, print_unconstrained=0):
    """Print out non-unconstrained invariants."""
    if type(fn_regexp) == types.StringType:
        fn_regexp = re.compile(fn_regexp)

    function_names = fn_var_infos.keys()
    function_names.sort()
    for fn_name in function_names:
        if fn_regexp and fn_regexp.match(fn_name):
            continue
        print "==========================================================================="
        print fn_name, fn_samples[fn_name], "samples"
        var_infos = fn_var_infos[fn_name]

        # Equality invariants
        for vi in var_infos:
            if not vi.is_canonical():
                continue
            if vi.equal_to == []:
                continue
            print vi.name,              # no newline if ends with comma
            for equal_var in vi.equal_to:
                print "=", equal_var.name,
            print ""                    # print newline
        # Single invariants
        for vi in var_infos:
            if not vi.is_canonical():
                continue
            this_inv = vi.invariant
            if print_unconstrained or not this_inv.is_unconstrained():
                print " ", this_inv.format((vi.name,))
        # Pairwise invariants
        for vi in var_infos:
            if not vi.is_canonical():
                continue
            vname = vi.name
            for (index,inv) in vi.invariants.items():
                if type(index) != types.IntType:
                    continue
                if not var_invs[index].is_canonical():
                    continue
                if print_unconstrained or not inv.is_unconstrained():
                    print "   ", inv.format((vname, var_infos[index].name))
        # Three-way (and greater) invariants
        for vi in var_infos:
            if not vi.is_canonical():
                continue
            vname = vi.name
            for (index_pair,inv) in vi.invariants.items():
                if type(index_pair) == types.IntType:
                    continue
                (i1, i2) = index_pair
                if not var_invs[i1].is_canonical():
                    # Perhaps err; this shouldn't happen, right?
                    continue
                if not var_invs[i2].is_canonical():
                    # Perhaps err; this shouldn't happen, right?
                    continue
                if print_unconstrained or not inv.is_unconstrained():
                    print "     ", inv.format((vname, var_infos[i1].name, var_infos[i2].name))


###########################################################################
### Invariants -- single scalar
###            


class invariant:
    one_of = None                   # list of 5 or fewer distinct values
    values = None                   # number of distinct values; perhaps
                                        # maintain this as a range rather
                                        # than an exact number...
    samples = None                  # number of samples; >= values
    # Perhaps this should be a count.
    can_be_None = None                  # only really sensible for single
                                        # invariants, not those over pairs, etc. (?)
    unconstrained_internal = None   # None, true, or false

    def __init__(self, dict):
        """DICT maps from values to number of occurrences."""
        vals = dict.keys()
        self.values = len(vals)
        self.samples = util.sum(dict.values())
        self.can_be_None = None in vals
        if len(vals) < 5 and not self.can_be_None:
            vals.sort()
            self.one_of = vals

    def is_exact(self):
        return self.values == 1

    def is_unconstrained(self):
        if self.unconstrained_internal == None:
            self.format()
        return self.unconstrained_internal

    def format(self, args="x"):
        """ARGS is uninterpreted.
        This function can return None:  it's intended to be used only as a helper.
        Any overriding implementation of this function should set the
        unconstrained_internal class-local variable.  Since this function sets it,
        too, callers of this function should be careful to do their manipulation
        after any call to this base method.
        The format function should be able to take no extra arguments, in which
        case it supplies default variable names.
        """

        self.unconstrained_internal = false
        if (type(args) in [types.ListType, types.TupleType]) and (len(args) == 1):
            args = args[0]
        if self.one_of:
            if len(self.one_of) == 1:
                return "%s = %s \t(%s samples)" % (args, self.one_of[0], self.samples)
            # If few samples, don't try to infer a function over the values
            elif self.samples < 100:
                return "%s in %s \t(%s samples)" % (args, util.format_as_set(self.one_of), self.samples)
        self.unconstrained_internal = true
        return None


class single_scalar_numeric_invariant(invariant):
    min = None
    max = None
    can_be_zero = None              # only interesting if range includes zero
    modulus = None
    nonmodulus = None
    min_justified = None
    max_justified = None

    def __init__(self, dict):
        """DICT maps from values to number of occurrences."""
        invariant.__init__(self, dict)
        nums = dict.keys()
        nums.sort()
        if nums == []:
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
        # Watch out: "None" sorts less than any number
        if self.min == None:
            self.max = None
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
            num_min = dict[self.min]
            num_max = dict[self.max]
            range = self.max - self.min + 1
            twice_avg_num = 2.0*self.values/range
            half_avg_num = .5*self.values/range
            if ((num_min >= 3)
                and ((num_min > twice_avg_num)
                     or ((num_min > half_avg_num) and (dict[nums[1]] > half_avg_num)))):
                self.min_justified = true
            if ((num_max >= 3)
                and ((num_max > twice_avg_num)
                     or ((num_max > half_avg_num) and (dict[nums[-2]] > half_avg_num)))):
                self.max_justified = true
            # print "min (%d) justified=%d: %d min elts, %d adjacent" % (self.min, self.min_justified, num_min, dict[nums[1]])
            # print "max (%d) justified=%d: %d max elts, %d adjacent" % (self.max, self.max_justified, num_max, dict[nums[-2]])

        self.can_be_zero = (0 in nums)
        if self.min != None:
            self.modulus = util.common_modulus(nums)
            ## Too many false positives
            # self.nonmodulus = util.common_nonmodulus_nonstrict(nums)
            self.nonmodulus = util.common_nonmodulus_strict(nums)

    ## Can do no more than the parent can
    #     def is_exact(self):
    #         if invariant.is_exact(self):
    #             return true

    def nonzero_justified(self):
        if self.min == None:
            return false
        probability = 1 - 1.0/(self.max - self.min + 1)
        return probability**self.samples < negative_invariant_confidence

    def modulus_justified(self):
        probability = 1.0/self.modulus[1]
        return probability**self.samples < negative_invariant_confidence

    def nonmodulus_justified(self):
        base = self.nonmodulus[1]
        probability = 1 - 1.0/base
        return probability**self.samples * base < negative_invariant_confidence


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

    def format(self, arg_tuple=("x",)):
        (arg,) = arg_tuple

        as_base = invariant.format(self, arg)
        if as_base:
            return as_base
        self.unconstrained_internal = false

        suffix = " \t(%s values, %s samples" % (self.values, self.samples)
        if self.can_be_None:
            suffix = suffix + ", can be None)"
        else:
            suffix = suffix + ")"

        if self.modulus and self.modulus_justified():
            return arg + " = %d (mod %d)" % self.modulus + suffix
        elif self.nonmodulus and self.nonmodulus_justified():
            return arg + " != %d (mod %d)" % self.nonmodulus + suffix

        nonzero = (not self.can_be_zero) and self.nonzero_justified()

        if self.min_justified and self.max_justified:
            result = " in [%s..%s]" % (self.min, self.max)
            if (self.min < 0 and self.max > 0 and nonzero):
                result = " nonzero" + result
            return arg + result + suffix
        if self.min_justified:
            result = "%s >= %s" % (arg, self.min)
            if self.min < 0 and nonzero:
                result = result + " and nonzero"
            return result + suffix
        if self.max_justified:
            result = "%s <= %s" % (arg, self.max)
            if self.max > 0 and nonzero:
                result = result + " and nonzero"
            return result + suffix
        if nonzero:
            return arg + "!= 0" + suffix

        if self.one_of:
            return "%s in %s" % (arg, util.format_as_set(self.one_of))

        self.unconstrained_internal = true
        return arg + " unconstrained" + suffix


# single_scalar_numeric_invariant(dict_of_tuples_to_tuple_of_dicts(fn_var_values["PUSH-ACTION"])[0])




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

    linear = None                       # can be pair (a,b) such that y=ax+b
    comparison = None                   # can be "=", "<", "<=", ">", ">="
    can_be_equal = None
    a_min = a_max = b_min = b_max = None
    difference_invariant = None
    sum_invariant = None
    functions = None                    # list of functions such that y=fun(x)
    inv_functions = None                # list of functions such that x=fun(y)

    # Note that Invariants produced for pairs such that there is a known
    # invariant for one of the elements (eg, it's constant) aren't interesting.
    def __init__(self, dict_of_pairs):
        """DICT maps from a pair of values to number of occurrences."""
        invariant.__init__(self, dict_of_pairs)

        pairs = dict_of_pairs.keys()

        # Range
        ## Perhaps someday have pointers to the single-scalar invariants
        ## instead of maintaining these separately here.
        a_nums = map(lambda x: x[0], pairs)
        self.a_min = min(a_nums)
        self.a_max = max(a_nums)
        b_nums = map(lambda x: x[1], pairs)
        self.b_min = min(b_nums)
        self.b_max = max(b_nums)

        ## Linear relationship -- try to fit y = ax + b.
        # Should I also try x = ax + b?  I do not plan to call this with
        # the arguments reversed, so that is a reasonable idea.
        try:
            if len(pairs) > 1:
                (a,b) = bi_linear_relationship(pairs[0], pairs[1])
                for (x,y) in pairs:
                    if y != a*x+b:
                        break
                else:
                    self.linear = (a,b)
        except OverflowError:
            pass

        ## Find invariant over x-y; this can be more exact than "x<y".
        diff_dict = {}
        sum_dict = {}
        for ((x,y),count) in dict_of_pairs.items():
            util.mapping_increment(diff_dict, x-y, count)
            util.mapping_increment(sum_dict, x+y, count)
        self.difference_invariant = single_scalar_numeric_invariant(diff_dict)
        self.sum_invariant = single_scalar_numeric_invariant(sum_dict)

        (self.comparison, self.can_be_equal) = compare_pairs(pairs)

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


    def is_exact(self):
        return invariant.is_exact(self) or self.linear

    def nonequal_justified(self):
        overlap = min(self.a_max, self.b_max) - max(self.a_min, self.b_min)
        if overlap < 0:
            return false
        overlap = float(overlap + 1)

        try:
            probability = 1 - overlap/((self.a_max - self.a_min + 1) * (self.b_max - self.b_min + 1))
        except OverflowError:
            probability = 1
        # Equivalent and slower, albeit clearer
        # probability = 1 - (overlap/(self.a_max - self.a_min + 1)) * (overlap/(self.b_max - self.b_min + 1)) * (1/overlap)

        return probability**self.samples < negative_invariant_confidence

    def __repr__(self):
        result = "<invariant-2: "
        if self.linear:
            result = result + "linear: %s, " % (self.linear,) # self.linear is itself a tuple
        if self.comparison:
            result = result + "cmp: %s, " % self.comparison
        result = result + "can be =: %s, " % self.can_be_equal
        if self.functions:
            result = result + "functions: %s, " % self.functions
        if self.inv_functions:
            result = result + "inv_functions: %s, " % self.inv_functions
        result = result + ("sum: %s, diff: %s, "
                           % (self.sum_invariant, self.difference_invariant))
        result = result + "%s values, %s samples" % (self.values, self.samples)
        result = result + ">"
        return result

    def __str__(self):
        return self.format()

    def format(self, arg_tuple=("x","y")):

        as_base = invariant.format(self, "(%s, %s)" % arg_tuple)
        if as_base:
            return as_base

        self.unconstrained_internal = false

        (x,y) = arg_tuple

        suffix = " \t(%s values, %s samples)" % (self.values, self.samples)

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

        if self.comparison:
            if self.comparison in ["<", "<="]:
                return "%s %s %s" % (x, self.comparison, y) + suffix
            if self.comparison == ">":
                return "%s < %s" % (y, x) + suffix
            if self.comparison == ">=":
                return "%s <= %s" % (y, x) + suffix
            raise "Can't get here"

        # Note that invariant.format(diff_inv, ...) is quite differerent from
        # diff_inv.format(...)!

        diff_inv = self.difference_invariant
        # Uninteresting differences:
        #  * >= 0 (x >= y), >= 1 (x > y)
        #  * <= 0 (x <= y), <= -1 (x < y)
        #  * nonzero (x != y)
        diff_as_base = invariant.format(diff_inv, ("%s - %s" % (x,y),))

        if diff_as_base:
            return diff_as_base + suffix
        if diff_inv.modulus:
            (a,b) = diff_inv.modulus
            if a == 0:
                return "%s = %s (mod %d)" % (x,y,b) + suffix
            else:
                return "%s - %s = %d (mod %d)" % (x,y,a,b) + suffix
        if diff_inv.min > 1:
            return "%s >= %s + %d" % (x,y,diff_inv.min) + suffix
        if diff_inv.max < -1:
            return "%s >= %s + %d" % (x,y,diff_inv.min) + suffix

        # What can be interesting about a sum?  I'm not sure...
        sum_inv = self.sum_invariant
        sum_as_base = invariant.format(sum_inv, ("%s + %s" % (x,y),))
        if sum_as_base:
            return sum_as_base
        if sum_inv.modulus:
            (a,b) = sum_inv.modulus
            return "%s + %s = %d (mod %d)" % (x,y,a,b) + suffix

        if (not self.can_be_equal) and self.nonequal_justified():
            return "%s != %s" % (x,y) + suffix
        elif self.one_of:
            return "%s in %s" % ("(%s, %s)" % arg_tuple, util.format_as_set(self.one_of)) + suffix
        else:
            self.unconstrained_internal = true
            return "(%s, %s) unconstrained" % (x,y) + suffix




# No need for add, sub
symmetric_binary_functions = (min, max, operator.mul, operator.and_, operator.or_)
non_symmetric_binary_functions = (cmp, pow, round, operator.div, operator.mod, operator.lshift, operator.rshift)

class three_scalar_numeric_invariant(invariant):

    linear_z = None                  # can be pair (a,b,c) such that z=ax+by+c
    linear_y = None                  # can be pair (a,b,c) such that y=ax+bz+c
    linear_x = None                  # can be pair (a,b,c) such that x=ay+bz+c

    # In these lists, when the function is symmetric, the first variable is
    # preferred.
    functions_xyz = None                # list of functions such that z=fun(x,y)
    functions_yxz = None                # list of functions such that z=fun(y,x)
    functions_xzy = None                # list of functions such that y=fun(x,z)
    functions_zxy = None                # list of functions such that y=fun(z,x)
    functions_yzx = None                # list of functions such that x=fun(y,z)
    functions_zyx = None                # list of functions such that x=fun(z,y)


    def __init__(self, dict_of_triples):
        """DICT maps from a triple of values to number of occurrences."""
        invariant.__init__(self, dict_of_triples)

        triples = dict_of_triples.keys()

        if len(triples) > 2:
            linear_z = checked_tri_linear_relationship(triples, (0,1,2))
            linear_y = checked_tri_linear_relationship(triples, (0,2,1))
            linear_x = checked_tri_linear_relationship(triples, (1,2,0))


        global symmetric_binary_functions, non_symmetric_binary_functions

        if len(triples) > 1:
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

    def is_exact(self):
        return invariant.is_exact(self) or self.linear_z or self.linear_y or self.linear_x

    def __repr__(self):
        result = "<invariant-3: "
        if self.linear_z:
            result = result + "linear_z: %s, " % self.linear_z
        if self.linear_y:
            result = result + "linear_y: %s, " % self.linear_y
        if self.linear_x:
            result = result + "linear_x: %s, " % self.linear_x
        if self.functions_xyz:
            result = result + "functions_xyz: %s, " % self.functions_xyz
        if self.functions_yxz:
            result = result + "functions_yxz: %s, " % self.functions_yxz
        if self.functions_xzy:
            result = result + "functions_xzy: %s, " % self.functions_xzy
        if self.functions_zxy:
            result = result + "functions_zxy: %s, " % self.functions_zxy
        if self.functions_yzx:
            result = result + "functions_yzx: %s, " % self.functions_yzx
        if self.functions_zyx:
            result = result + "functions_zyx: %s, " % self.functions_zyx
        result = result + "%s values, %s samples" % (self.values, self.samples)
        result = result + ">"
        return result

    def __str__(self):
        return self.format()

    def format(self, arg_tuple=("x","y","z")):

        as_base = invariant.format(self, "(%s, %s, %s)" % arg_tuple)
        if as_base:
            return as_base

        self.unconstrained_internal = false

        (x,y,z) = arg_tuple

        suffix = " \t(%s values, %s samples)" % (self.values, self.samples)

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

    (x, y, z) = permutation
    t0 = util.slice_by_sequence(triples[0], permutation)
    t1 = util.slice_by_sequence(triples[1], permutation)
    t2 = util.slice_by_sequence(triples[2], permutation)

    ## Linear relationship -- try to fit z = ax + by + c.
    (a,b,c) = tri_linear_relationship(t0, t1, t2)
    # needn't check first three, but it's a waste to create a new sequence
    for triple in triples:
        (x,y,z) = util.slice_by_sequence(triple, permutation)
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

    y1323 = float(y1-y3)/(y2-y3)
    a_numerator = z3-z1+(z2-z3)*y1323
    a_denominator = x3-x1+(x2-x3)*y1323

    x1323 = float(x1-x3)/(x2-x3)
    b_numerator = z3-z1+(z2-z3)*x1323
    b_denominator = y3-y1+(y2-y3)*x1323

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
        result.append("%s" % x)
    elif a == -1:
        result.append("- %s" % x)
    elif a != 0:
        result.append("%s %s" % (a,x))
    if result != [] and b > 0:
        result.append(" + ")
    elif b < 0:
        result.append(" - ")
    if abs(b) == 1:
        result.append("%s" % y)
    elif b != 0:
        result.append("%s %s" % (abs(b),y))
    if c > 0:
        result.append("+ %s" % c)
    elif c < 0:
        result.append("- %s" % c)
    if result == []:
        result = "0"
    else:
        result = string.join(result, "")
    return ("%s = " % z) + result


def _test_tri_linear_relationship():
    assert tri_linear_relationship((1,2,1),(2,1,7),(3,3,7)) == (4,-2,1)
    # like the above, but swap y and z; results in division-by-zero problem
    # tri_linear_relationship((1,1,2),(2,7,1),(3,7,3))
    assert tri_linear_relationship((1,2,6),(2,1,-4),(3,3,7)) == (-3,7,-5)



###########################################################################
### Invariants -- single sequence
###

class single_sequence_numeric_invariant(invariant):
    # Invariants over sequence as a whole
    min = None              # min sequence of all instances
    max = None              # max sequence of all instances
    min_justified = None
    max_justified = None
    equal = None            # per instance sorting data
    non_decreasing = None   #
    non_increasing = None   #

    # Invariants over elements of sequence
    all_index_sni = None    # sni for all elements of the sequence
    per_index_sni = None    # tuple of element sni's for each index
                            #   across sequence instances
    reversed_per_index_sni = None

    def __init__(self, dict):
        """DICT maps from tuples of values to number of occurrences."""
        invariant.__init__(self,dict)
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
        self.equal = true
        self.non_decreasing = true
        self.non_increasing = true
        for seq in seqs:
            if seq == None:
                self.equal = self.non_decreasing = self.non_increasing = false
                # if any element is missing, infer nothing over anything
                return
            for i in range(1, len(seq)):
                c = cmp(seq[i-1],seq[i])
                # should we have strictly ascending/descending?
                if c < 0:
                    self.equal = self.non_increasing = false
                elif c > 0:
                    self.equal = self.non_decreasing = false
                if not(self.equal or self.non_decreasing \
                       or self.non_increasing):
                    break
            if not(self.equal or self.non_decreasing \
                   or self.non_increasing):
                break

        # Invariant check over elements of all sequence instances
        element_to_count = dict_of_sequences_to_element_dict(dict)
        self.all_index_sni = single_scalar_numeric_invariant(element_to_count)

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
                    single_scalar_numeric_invariant(per_index_elems_to_count[i]))
            return result

        tuple_len = min(map(len, dict.keys())) # min length of a tuple
        self.per_index_sni = per_index_invariants(dict, tuple_len)

        reversed_dict = {}
        for (key, value) in dict.items():
            reversed_key = list(key)
            reversed_key.reverse()
            reversed_dict[tuple(reversed_key)] = value
        self.reversed_per_index_sni = per_index_invariants(reversed_dict, tuple_len)


    def __repr__(self):
        result = "<invariant-1 []>"
        # finish once get properties set
        # result = "<invariant-1 []: "
        return result

    def __str__(self):
        return self.format()

    def format(self, arg_tuple=("x[]",)):
        (arg,) = arg_tuple

        # as_base = invariant.format(self, arg)
        # if as_base:
        #     return as_base

        self.unconstrained_internal = false

        # Which is the strongest relationship (so we can ignore others)?
        # Do we care more that it is sorted, or that it is in given range?
        # How much of this do we want to print out?

        suffix = " \t(%s values, %s samples)" % (self.values, self.samples)
        result = ""
        if self.min_justified and self.max_justified:
            if self.min == self.max:
                result = result + "\t== %s" % (self.min,)
            else:
                result = result + "\tin [%s..%s]" % (self.min, self.max)
        elif self.min_justified:
            result = result + "\t>= %s" % self.min
        elif self.max_justified:
            result = result + "\t<= %s" % (self.max)

        if self.equal:
            result = result + "\n" + "\tPer sequence elements equal"
        elif self.non_decreasing:
            result = result + "\n" + "\tPer sequence elements non-decreasing"
        elif self.non_increasing:
            result = result + "\n" + "\tPer sequence elements non-increasing"

        if self.all_index_sni != None:
            all_formatted = self.all_index_sni.format(("*every*element*",))
            if not self.all_index_sni.is_unconstrained():
                result = result + "\n" + "\tAll sequence elements: " + all_formatted
        #         if not (0 == len(self.per_index_sni)):
        #             first_formatted = self.per_index_sni[0].format(("*first*element*",))
        #             if not self.per_index_sni[0].is_unconstrained():
        #                 result = result + "\n" + "\tFirst sequence element: " + first_formatted
        #             last_formatted = self.reversed_per_index_sni[0].format(("*last*element*",))
        #             if not self.reversed_per_index_sni[0].is_unconstrained():
        #                 result = result + "\n" + "\tLast sequence element: " + last_formatted

        if result == "":
            self.unconstrained_internal = true
            return arg + " unconstrained" + suffix
        return arg + suffix + "\n" + result


###########################################################################
### Invariants -- multiple sequence, or sequence plus scalar
###

class scalar_sequence_numeric_invariant(invariant):
    # I'm not entirely sure what to do with this one
    seq_first = None          # if true, the variables are (seq,scalar)
                              # if false, the variables are (scalar,seq)
    member = None
    size = None
    per_index_linear = None   # Array whose elements describe the linear
                              # relationship between the number scalar and
                              # the sequence element at that index.


    def __init__(self, dict_of_pairs):

        invariant.__init__(self, dict_of_pairs)
        pairs = dict_of_pairs.keys()

        if len(pairs) == 0:
            raise "empty dictionary supplied"
        self.seq_first = type(pairs[0][0]) == types.TupleType


        # For each (num, sequence), determine if num is a member of seq
        self.member = true
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

    def __repr__(self):
        result = "<invariant-2 (x,[])>"
        return result

    def __str__(self):
        return self.format()

    def format(self, arg_tuple=None):
        if arg_tuple == None:
            if self.seq_first:
                arg_tuple = ("seq","num")
            else:
                arg_tuple = ("num","seq")

        self.unconstrained_internal = false

        suffix = " \t(%s values, %s samples)" % (self.values, self.samples)

        # arg_tuple is a pair of names; it contains no info about types
        if self.seq_first:
            (seqvar, sclvar) = arg_tuple
        else:
            (sclvar, seqvar) = arg_tuple

        if self.member:
            return "%s is a member of %s" % (sclvar,seqvar) + suffix
        if self.size:
            return "%s is the size of %s" % (sclvar,seqvar) + suffix

        self.unconstrained_internal = true
        return "(%s,%s) are unconstrained" % (arg_tuple) + suffix


class two_sequence_numeric_invariant(invariant):

    linear = None          # Relationship describing elements at same indices
                           # in the two sequences.  If not None, it is the same
                           # for each index.
    # per_index_linear = None # Array whose elements describe the linear
                              # relationship between the pair of sequence
                              # elements at that index.
    comparison = None      # can be "=", "<", "<=", ">", ">="
    can_be_equal = None
    sub_sequence = None
    super_sequence = None
    reverse = None

    def __init__(self, dict_of_pairs):
        invariant.__init__(self, dict_of_pairs)

        pairs = dict_of_pairs.keys()

        ## Linear relationship -- try to fit y[] = ax[] + b.
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

        (self.comparison, self.can_be_equal) = compare_pairs(pairs)

        self.reverse = true
        for (x, y) in pairs:
            # Make shallow copy because reverse works in place.
            # Must do some nasty casting in the process?!
            z = list(copy.copy(y))
            z.reverse()
            z = tuple(z)
            if x != z:
                self.reverse = false
                break
        self.sub_sequence = true
        self.super_sequence = true
        for (x, y) in pairs:
            if not(util.sub_sequence_of(x, y)):
                self.sub_sequence = false
                break
        for (x, y) in pairs:
            if not(util.sub_sequence_of(y, x)):
                self.super_sequence = false
                break

    def __repr__(self):
        result = "<invariant-2 ([],[])>"
        return result

    def __str__(self):
        return self.format()

    def format(self, arg_tuple=("x[]","y[]")):

        # as_base = invariant.format(self, "(%s, %s)" % arg_tuple)
        # if as_base:
        #     return as_base

        self.unconstrained_internal = false

        suffix = " \t(%s values, %s samples" % (self.values, self.samples)

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

        if self.sub_sequence:
            return "%s is a subsequence of %s" % (x,y) + suffix
        if self.super_sequence:
            return "%s is a subsequence of %s" % (y,x) + suffix
        if self.reverse:
            return "%s is the reverse of %s" % (x,y) + suffix

        if self.comparison:
            if self.comparison in ["<", "<="]:
                return "%s %s %s" % (x, self.comparison, y) + suffix
            if self.comparison == ">":
                return "%s < %s" % (y, x) + suffix
            if self.comparison == ">=":
                return "%s <= %s" % (y, x) + suffix
            raise "Can't get here"

        self.unconstrained_internal = true
        return "(%s,%s) unconstrained" % (x,y) + suffix






###########################################################################
### Testing
###


# Run python from $inv/medic/data
# import invariants
# reload(invariants)

# invariants.clear_variables()
# invariants.read_invs('*.inv')
# invariants.read_invs('T*.inv', "clear first")
# invariants.read_invs('[TPD]*.inv', "clear first")
# invariants.all_numeric_invariants()

# As of 5/16/98, this took half an hour or more
# invariants.read_invs('*.inv', "clear first")

def read_merge_file(filename, fn_regexp=None):
    (this_fn_var_infos, this_fn_var_values, this_fn_samples) = read_file(filename, fn_regexp)
    merge_variables(filename, this_fn_var_infos, this_fn_var_values, this_fn_samples)

# consider calling clear_variables() before calling this
def read_inv(filename="medic/invariants.raw"):
    read_merge_file(filename)
    print_hashtables()

def read_invs(files, clear=0, fn_regexp=None):
    """FILES is either a sequence of file names or a single Unix file pattern."""
    if clear:
        clear_variables()
    if type(fn_regexp) == types.StringType:
        fn_regexp = re.compile(fn_regexp)

    def env_repl(matchobj):
        return posix.environ[matchobj.group(0)[1:]]
    if type(files) == types.StringType:
        files = re.sub(r'^~/', '$HOME/', files)
        files = re.sub(r'^~', '/homes/fish/', files)
        files = re.sub(r'\$[a-zA-Z_]+', env_repl, files)
        files = glob.glob(files)
    if files == []:
        raise "No files specified"
    # Get all function names to add checks on ftn invocation counts
    for file in files:
        read_file_ftns(file, fn_regexp)
    for file in files:
        read_merge_file(file, fn_regexp)

    # For loop is outside assert, yuck
    for fname in fn_var_values.keys():
        assert len(fn_var_infos[fname]) == len(fn_var_values[fname].keys()[0])

    # # This comes late, because the number of variables introduced depends
    # # on the values (eg, sequence lengths).  It can't be done before
    # # merging different files, because different variables would be
    # # introduced in the different files.
    # introduce_new_variables(fn_var_infos, fn_var_values)


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
