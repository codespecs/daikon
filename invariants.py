#!/uns/bin/python1.5
# invariants.py -- detect patterns in collections of data
# Michael Ernst <mernst@cs.washington.edu>

import glob, operator, os, re, string, types

import util

true = (1==1)
false = (1==0)

###########################################################################
### Variables
###

# Annoyingly, these variables get wiped out when I reload this file unless
# I protect them.
# But if they're indented, maybe my future tools for variable decls won't work.
if not locals().has_key("var_names"):
    var_names = {}          # from function name to tuple of variable names
    var_values = {}	    # from function name to (tuple of values to occurrence count)
    samples = {}            # from function name to number of samples
    file_vars = {}          # from filename to (names, values, samples) tuple


integer_re = re.compile(r'^-?[0-9]+$')
float_re = re.compile(r'^-?[0-9]*\.[0-9]+$|^-?[0-9]+\.[0-9]*$')

def clear_variables():
    """Reset the values of some global variables."""
    var_names.clear()
    var_values.clear()
    samples.clear()
    file_vars.clear()

def merge_variables(filename, sub_var_names, sub_var_values, sub_samples):
    """Merge the values for the arguments into the corresponding global variables.
See `read_file' for a description of the argument types; arguments 2-4 are
dictionaries mapping from a function name to information about the function."""
    # Yuck, loop is outside assert
    for fname in var_names.keys():
        assert not(sub_var_values.has_key(fname)) or (var_names[fname] == sub_var_names[fname])
    assert util.same_elements(sub_var_names.keys(), sub_var_values.keys())
    assert util.same_elements(sub_var_names.keys(), sub_samples.keys())
    assert not(file_vars.has_key(filename))
    for fname in sub_var_names.keys():
	if not(var_names.has_key(fname)):
	    var_names[fname] = sub_var_names[fname]
	    var_values[fname] = {}
	else:
            assert var_names[fname] == sub_var_names[fname]
	    assert type(var_values[fname]) == types.DictType
        for (values, count) in sub_var_values[fname].items():
            # var_values[fname][value] += count
            util.mapping_increment(var_values[fname], values, count)
        # samples[fname] += sub_samples[fname]
        util.mapping_increment(samples, fname, sub_samples[fname])
    file_vars[filename] = (sub_var_names, sub_var_values, sub_samples)

def dict_of_tuples_to_tuple_of_dicts(dot):
    """Input: a dictionary mapping a tuple of elements to a count.
Output: a tuple of dictionaries, each mapping a single element to a count.
The first output dictionary concerns the first element of the original keys,
the second output the second element of the original keys, and so forth."""
    tuple_len = len(dot.keys()[0])
    tuple_indices = range(0, tuple_len)
    # Next four lines accomplish "result = ({},) * tuple_len", but with
    # distinct rather than identical dictionaries in the tuple.
    result = []
    for i in tuple_indices:
        result.append({})
    result = tuple(result)
    for (key_tuple, count) in dot.items():
        for i in tuple_indices:
            this_key = key_tuple[i]
            this_dict = result[i]
            util.mapping_increment(this_dict, this_key, count)
    return result
# dict_of_tuples_to_tuple_of_dicts(var_values["PUSH-ACTION"])


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
# dict_of_tuples_slice(var_values["PUSH-ACTION"], (0,))
# dict_of_tuples_slice(var_values["PUSH-ACTION"], (1,))
# dict_of_tuples_slice(var_values["VERIFY-CLEAN-PARALLEL"], (0,))
# dict_of_tuples_slice(var_values["VERIFY-CLEAN-PARALLEL"], (1,))
# dict_of_tuples_slice(var_values["VERIFY-CLEAN-PARALLEL"], (2,))
# dict_of_tuples_slice(var_values["VERIFY-CLEAN-PARALLEL"], (0,1))
# dict_of_tuples_slice(var_values["VERIFY-CLEAN-PARALLEL"], (0,2))
# dict_of_tuples_slice(var_values["VERIFY-CLEAN-PARALLEL"], (1,2))


###########################################################################
### Input/output
###

# An instrumented program produces a .inv file containing information about
# run-time values of expressions and variables.  The invariant detector tries
# to find patterns in the values recorded in one or more .inv files.
# 
# To detect invariants in a particular program, it is enough to insert code
# in the application which creates a .inv file.  In Lisp, the
# check-for-invariants macro performs this task.  Gries-style Lisp programs
# can be automatically instrumented -- the calls to the check-for-invariants
# macro are inserted by the instrument function found in gries-helper.lisp.
# Given a file of Gries-style Lisp functions, instrument produces a new file
# of instrumented Lisp code which can be compiled and run.
# 
# Each line of a .inv file is of the form
# 
#   tag varname1 value1 varname2 value2 ...
# 
# The tag is an arbitrary alphanumeric string indicating the program point at
# which this data was collected.  The varnames are also alphanumeric.
# Currently the values are integers.


def read_file(filename):
    """Read data from .inv file; return a tuple of three dictionaries.
 * map from function name to tuple of variable names.
 * map from function name to (map from tuple of values to occurrence count)
 * map from function name to number of samples"""
    file = open(filename, "r")
    this_var_names = {}		# from function name to tuple of variable names
    this_var_values = {}	# from function name to (tuple of values to occurrence count)
    this_samples = {}           # from function name to number of samples

    for line in file.readlines():
	line_elts = string.split(line)
	function_name = line_elts[0]
        # # Remove trailing colon
        # if function_name[-1] == ":":
        #     function_name = function_name[:-1]
	these_names = []
	these_values = []
	for i in range(1, len(line_elts), 2):
	    this_name = line_elts[i]
	    this_value = line_elts[i+1]
	    if integer_re.match(this_value):
		this_value = int(this_value)
            elif float_re.match(this_value):
                this_value = float(this_value)
            elif this_value == "NIL":
                # HACK
                this_value = 0
            else:
                raise "What value?"
	    these_names.append(this_name)
	    these_values.append(this_value)
	these_names = tuple(these_names)
	these_values = tuple(these_values)
	if not(this_var_names.has_key(function_name)):
	    this_var_names[function_name] = these_names
	    this_var_values[function_name] = {}
	else:
	    assert this_var_names[function_name] == these_names
	    assert type(this_var_values[function_name]) == types.DictType
        util.mapping_increment(this_var_values[function_name], these_values, 1)
        util.mapping_increment(this_samples, function_name, 1)
    return (this_var_names, this_var_values, this_samples)


def print_hashtables():
    """Print the important global hashtables.  Principally for debugging."""
    sorted_keys = var_names.keys()
    sorted_keys.sort()
    for fn_name in sorted_keys:
	print fn_name, var_names[fn_name], samples[fn_name], "samples"
	vals = var_values[fn_name]
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

def print_cubist_files():
    """Create files for Cubist experiments."""
    sorted_keys = var_names.keys()
    sorted_keys.sort()
    for fn_name in sorted_keys:
        names = var_names[fn_name]
        columns = len(names)
        if columns > 1:
            names = list(names)
            for i in range(0, len(names)):
                names[i] = string.replace(names[i], ".", " ")

            dataname = fn_name + ".data"
            fdata = open(dataname, "w")
            vals = var_values[fn_name]
            # print "vals:", vals
            for this_tuple in vals.keys():
                # print "this_tuple:", this_tuple
                fdata.write(string.join(map(repr, this_tuple), ", "))
                fdata.write("\n")
            fdata.close()

            for col in range(0, columns):
                basename = "%s-%d" % (fn_name, col+1)
                fnames = open(basename + ".names", "w")
                fnames.write(names[col] + ".\n\n")
                assert len(names) == columns
                for name in names:
                    fnames.write("%s: continuous.\n" % name)
                fnames.close()

                this_dataname = basename + ".data"
                if os.path.exists(this_dataname):
                    os.remove(this_dataname)
                os.symlink(dataname, this_dataname)

def run_cubist():
    """Run cubist on the files created by `print_cubist_files'."""
    sorted_keys = var_names.keys()
    sorted_keys.sort()
    for fn_name in sorted_keys:
        names = var_names[fn_name]
        columns = len(names)
        if columns > 1:
            for col in range(0, columns):
                basename = "%s-%d" % (fn_name, col+1)
                # This path is now wrong, as I've moved the program.
                os.system("rm -f %s.out; /homes/gws/mernst/tmp/CubistR1/bin/cubistdemo -f %s > %s.out" % (basename, basename, basename))


###########################################################################
### Invariants -- numeric
###

# A negative invariant is not reported unless the chance that the invariant
# only happens not to be true (and isn't a true invariant) is at least this low.
negative_invariant_confidence = .01     # .05 might also be reasonable

## An invariant may be exact or approximate.

def all_numeric_invariants():
    sorted_keys = var_names.keys()
    sorted_keys.sort()
    for fn_name in sorted_keys:
        print fn_name, samples[fn_name], "samples"
        fn_vars = var_names[fn_name]
        dicts = dict_of_tuples_to_tuple_of_dicts(var_values[fn_name])
        assert len(fn_vars) == len(dicts)
        non_exact_single_invs = []
        for i in range(0, len(fn_vars)):
            this_var = fn_vars[i]
            this_dict = dicts[i]
            this_inv = single_field_numeric_invariant(this_dict)
            print " ", this_inv.format((this_var,))
            # print " ", this_var, this_inv
            # print "   ", `this_inv`
            if not this_inv.is_exact():
                non_exact_single_invs.append(i)
        exact_pair_invs = []
        if len(non_exact_single_invs) > 1:
            for indices in util.choose(2, non_exact_single_invs):
                this_dict = dict_of_tuples_slice(var_values[fn_name], indices)
                these_vars = util.slice_by_sequence(fn_vars, indices)
                this_inv = two_field_numeric_invariant(this_dict)
                if (this_inv.is_exact()):
                    exact_pair_invs.append(indices)
                print "   ", this_inv.format(these_vars)
                # print "   ", these_vars, this_inv
                # print "     ", `this_inv`
        if len(non_exact_single_invs) > 2:
            for indices in util.choose(3, non_exact_single_invs):
                if (([indices[0],indices[1]] in exact_pair_invs) or
                    ([indices[0],indices[2]] in exact_pair_invs) or
                    ([indices[1],indices[2]] in exact_pair_invs)):
                    continue
                # print "didn't find two of", indices, "in", exact_pair_invs, ";", util.slice_by_sequence(fn_vars, indices), fn_vars
                this_dict = dict_of_tuples_slice(var_values[fn_name], indices)
                these_vars = util.slice_by_sequence(fn_vars, indices)
                this_inv = three_field_numeric_invariant(this_dict)
                if this_inv != None:
                    # if this_inv[0] == "linear":
                    #     print "found tri_linear: %s = %s %s + %s %s + %s" % (these_vars[2], this_inv[1], these_vars[0], this_inv[2], these_vars[1], this_inv[3])
                    # print "     ", these_vars, this_inv
                    # print "       ", `this_inv`
                    print "     ", this_inv.format(these_vars)
## Testing:
# all_numeric_invariants()



###########################################################################
### Invariants -- single field
###            


class invariant:
    one_of = None                   # list of 5 or fewer distinct values
    values = None                   # number of distinct values; perhaps
                                        # maintain this as a range rather
                                        # than an exact number...
    samples = None                  # number of samples; >= values

    def __init__(self, dict):
        """DICT maps from values to number of occurrences."""
        vals = dict.keys()
        self.values = len(vals)
        self.samples = util.sum(dict.values())
        if len(vals) < 5:
            vals.sort()
            self.one_of = vals

    def is_exact(self):
        return self.values == 1

    def format(self, args):
        """ARGS is uninterpreted.
This function can return None:  it's intended to be used only as a helper."""
        if (type(args) in [types.ListType, types.TupleType]) and (len(args) == 1):
            args = args[0]
        if self.one_of:
            if len(self.one_of) == 1:
                return "%s = %s" % (args, self.one_of[0])
            else:
                return "%s in %s" % (args, self.one_of)
        return None



class single_field_numeric_invariant(invariant):
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
        # For when we didn't sort nums
        # self.min = min(nums)
        # self.max = max(nums)
        self.min = nums[0]
        self.max = nums[-1]
        self.min_justified = false
        self.max_justified = false
        if len(nums) < 3:
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
            print "min (%d) %d justified? %d %d" % (self.min, self.min_justified, num_min, dict[nums[1]])
            print "max (%d) %d justified? %d %d" % (self.max, self.max_justified, num_max, dict[nums[-2]])

        self.can_be_zero = (0 in nums)
        self.modulus = util.common_modulus(nums)
        ## Too many false positives
        # self.nonmodulus = util.common_nonmodulus_nonstrict(nums)
        self.nonmodulus = util.common_nonmodulus_strict(nums)

    ## Can do no more than the parent can
    #     def is_exact(self):
    #         if invariant.is_exact(self):
    #             return true

    def nonzero_justified(self):
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
    # "<invariants.single_field_numeric_invariant instance at 11bdf8>"
    def __repr__(self):
        result = "<invariant-1: "
        if self.one_of:
            result = result + "one of %s, " % self.one_of
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
        self.format(("x",))

    def format(self, arg_tuple):
        (arg,) = arg_tuple

        as_base = invariant.format(self, arg)
        if as_base:
            return as_base

        if self.modulus and self.modulus_justified():
            return arg + " = %d (mod %d)" % self.modulus
        elif self.nonmodulus and self.nonmodulus_justified():
            return arg + " != %d (mod %d)" % self.nonmodulus

        nonzero = (not self.can_be_zero) and self.nonzero_justified()

        if self.min_justified and self.max_justified:
            result = " in [%s..%s]" % (self.min, self.max)
            if (self.min < 0 and self.max > 0 and nonzero):
                result = " nonzero" + result
            return arg + result
        if self.min_justified:
            result = "%s >= %s" % (arg, self.min)
            if min < 0 and nonzero:
                result = result + " and nonzero"
            return result
        if self.max_justified:
            result = "%s <= %s" % (arg, self.max)
            if max > 0 and nonzero:
                result = result + " and nonzero"
            return result
        if nonzero:
            return arg + "!= 0"

        return arg + " unconstrained"


# single_field_numeric_invariant(dict_of_tuples_to_tuple_of_dicts(var_values["PUSH-ACTION"])[0])


def all_single_field_numeric_invariants():
    sorted_keys = var_names.keys()
    sorted_keys.sort()
    for fn_name in sorted_keys:
        fn_vars = var_names[fn_name]
        dicts = dict_of_tuples_to_tuple_of_dicts(var_values[fn_name])
        assert len(fn_vars) == len(dicts)
        for i in range(0, len(fn_vars)):
            this_var = fn_vars[i]
            this_dict = dicts[i]
            this_inv = single_field_numeric_invariant(this_dict)
            print fn_name, this_var, samples[fn_name], "samples", this_inv
## Testing:
# all_single_field_numeric_invariants()


###########################################################################
### Invariants -- multiple fields
###            

## For now, only look for perfectly-satisfied properties; deal with
## exceptions, disjunctions, and predicated properties later.


# Don't pass in a tuple plus two indices, because I have to aggregate the
# counts anyway.  Or maybe that isn't such a concern and it is more
# efficient to only aggregate the counts if everything looks good on other
# grounds.

class two_field_numeric_invariant(invariant):

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
        ## Perhaps someday have pointers to the single-field invariants
        ## instead of maintaining these separately here.
        a_nums = map(lambda x: x[0], pairs)
        self.a_min = min(a_nums)
        self.a_max = max(a_nums)
        b_nums = map(lambda x: x[1], pairs)
        self.b_min = min(b_nums)
        self.b_max = max(b_nums)

        ## Linear relationship -- try to fit y = ax + b.
        # Should I also try x = ax + b?
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
        self.difference_invariant = single_field_numeric_invariant(diff_dict)
        self.sum_invariant = single_field_numeric_invariant(sum_dict)

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
            if not(maybe_eq or maybe_lt or maybe_le or maybe_gt or maybe_ge):
                break
        else:
            if maybe_eq:
                self.comparison = "="
            elif maybe_lt:
                self.comparison = "<"
            elif maybe_le:
                self.comparison = "<="
            elif maybe_gt:
                self.comparison = ">"
            elif maybe_ge:
                self.comparison = ">="
        # Watch out: with few data points (say, even 100 data points when
        # values are in the range -100..100), we oughtn't conclude without
        # basis that the values are nonequal.
        self.can_be_equal = not(maybe_noneq)

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

        probability = 1 - overlap/((self.a_max - self.a_min + 1) * (self.b_max - self.b_min + 1))
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
        self.format(("x","y"))

    def format(self, arg_tuple):
        as_base = invariant.format(self, arg_tuple)
        if as_base:
            return as_base

        (x,y) = arg_tuple

        if self.comparison == "=":
            return "%s = %s" % (x,y)
        if self.linear:
            (a,b) = self.linear
            if a == 1:
                if b < 0:
                    return "%s = %s - %s" % (y,x,abs(b))
                else:
                    return "%s = %s + %s" % (y,x,b)
            elif b == 0:
                return "%s = %s %s" % (y,a,x)
            else:
                if b < 0:
                    return "%s = %s %s - %s" % (y,a,x,abs(b))
                else:
                    return "%s = %s %s + %s" % (y,a,x,b)

        if self.functions or self.inv_functions:
            results = []
            if self.functions:
                for fn in self.functions:
                    results.append("%s = %s(%s)" % (y,util.function_rep(fn),x))
            if self.inv_functions:
                for fn in self.inv_functions:
                    results.append("%s = %s(%s)" % (x,util.function_rep(fn),y))
            return string.join(results, " and ")

        if self.comparison:
            if self.comparison in ["<", "<="]:
                return "%s %s %s" % (x, self.comparison, y)
            if self.comparison == ">":
                return "%s < %s" % (y, x)
            if self.comparison == ">=":
                return "%s <= %s" % (y, x)
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
            return diff_as_base
        if diff_inv.modulus:
            (a,b) = diff_inv.modulus
            if a == 0:
                return "%s = %s (mod %d)" % (x,y,b)
            else:
                return "%s - %s = %d (mod %d)" % (x,y,a,b)
        if diff_inv.min > 1:
            return "%s >= %s + %d" % (x,y,diff_inv.min)
        if diff_inv.max < -1:
            return "%s >= %s + %d" % (x,y,diff_inv.min)

        # What can be interesting about a sum?  I'm not sure...
        sum_inv = self.sum_invariant
        sum_as_base = invariant.format(sum_inv, ("%s - %s" % (x,y),))
        if sum_as_base:
            return sum_as_base
        if sum_inv.modulus:
            (a,b) = sum_inv.modulus
            return "%s + %s = %d (mod %d)" % (x,y,a,b)

        if (not self.can_be_equal) and self.nonequal_justified():
            return "%s != %s" % (x,y)
        else:
            return "(%s, %s) unconstrained" % (x,y)


def all_two_field_numeric_invariants():
    sorted_keys = var_names.keys()
    sorted_keys.sort()
    for fn_name in sorted_keys:
        fn_vars = var_names[fn_name]
        num_vars = len(fn_vars)
        if num_vars < 2:
            continue
        for indices in util.choose(2, range(0,num_vars)):
            this_dict = dict_of_tuples_slice(var_values[fn_name], indices)
            these_vars = util.slice_by_sequence(fn_vars, indices)
            this_inv = two_field_numeric_invariant(this_dict)
            # print fn_name, these_vars, this_inv, `this_inv`
            print fn_name, these_vars
            print "   ", `this_inv`
            print "   ", this_inv

## Testing
# all_two_field_numeric_invariants()


# No need for add, sub
symmetric_binary_functions = (min, max, operator.mul, operator.and_, operator.or_)
non_symmetric_binary_functions = (cmp, pow, round, operator.div, operator.mod, operator.lshift, operator.rshift)

class three_field_numeric_invariant(invariant):

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
        self.format(("x","y","z"))

    def format(self, arg_tuple):
        as_base = invariant.format(self, arg_tuple)
        if as_base:
            return as_base

        (x,y,z) = arg_tuple

        if self.linear_z or self.linear_y or self.linear_x:
            results = []
            if self.linear_z:
                results.append(tri_linear_format(self.linear_z, (0,1,2)))
            if self.linear_y:
                results.append(tri_linear_format(self.linear_y, (0,2,1)))
            if self.linear_x:
                results.append(tri_linear_format(self.linear_x, (1,2,0)))
            return string.join(results, " and ")

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
            return string.join(results, " and ")

        return "(%s, %s, %s) unconstrained" % (x,y,z)


def all_three_field_numeric_invariants():
    sorted_keys = var_names.keys()
    sorted_keys.sort()
    for fn_name in sorted_keys:
        fn_vars = var_names[fn_name]
        num_vars = len(fn_vars)
        if num_vars < 3:
            continue
        for indices in util.choose(3, range(0,num_vars)):
            this_dict = dict_of_tuples_slice(var_values[fn_name], indices)
            these_vars = util.slice_by_sequence(fn_vars, indices)
            this_inv = three_field_numeric_invariant(this_dict)
            # print fn_name, these_vars, this_inv
            print fn_name, these_vars
            print "   ", `this_inv`
            print "   ", this_inv



# all_three_field_numeric_invariants()



###
### Linear relationships
###


## Must check the output in case nonsense -- zeroes -- is returned.
def bi_linear_relationship(pair1, pair2):
    """Given ((x0,y0),(x1,y1)), return (a,b) such that y = ax + b.
If no such (a,b) exists, then return (0,0)."

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
        if z != a*x+b*y+c:
            return None
    else:
        return(a, b, c)


## Must check the output in case nonsense -- zeroes -- is returned.
def tri_linear_relationship(triple1, triple2, triple3):
    """Given ((x1,y1,z1),(x2,y2,z2),(x3,y3,z3)), return (a,b,c) such that z=ax+by+c.
If no such (a,b,c) exists, then return (0,0,0)."

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

    return (a, b, c)

def tri_linear_format(abc, xyz):
    """Given ((a,b,c),(x,y,z)), format "z=ax+by+c".
The result omits addition of zero, multiplication by one, etc."
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

def read_merge_file(filename):
    (this_var_names, this_var_values, this_samples) = read_file(filename)
    merge_variables(filename, this_var_names, this_var_values, this_samples)

# consider calling clear_variables() before calling this
def read_inv(filename="medic/invariants.raw"):
    read_merge_file(filename)
    print_hashtables()

def read_invs(files, clear=0):
    """FILES is either a sequence of file names or a single Unix file pattern."""
    if clear:
        clear_variables()
    if type(files) == types.StringType:
        files = glob.glob(files)
    if files == []:
        raise "No files specified"
    for file in files:
        read_merge_file(file)

def _test():
    _test_tri_linear_relationship()


def foo():
    fn_name = "READ-COMPACT-TRANSLATION"
    fn_vars = var_names[fn_name]
    num_vars = len(fn_vars)
    for indices in util.choose(2, range(0,num_vars)):
        this_dict = dict_of_tuples_slice(var_values[fn_name], indices)
        these_vars = util.slice_by_sequence(fn_vars, indices)
        this_inv = two_field_numeric_invariant(this_dict)
        # print fn_name, these_vars, this_inv, `this_inv`
        print fn_name, these_vars
        print "   ", `this_inv`
        print "   ", this_inv
