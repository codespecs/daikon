### util.py -- Michael Ernst's Python utilities

import math, operator, posix, string, tempfile, time, types, re, whrandom

true = (1==1)
false = (1==0)


###########################################################################
### Numbers
###

maxint = 2147483647      # largest positive 32-bit 2s-complement int

## Roundoff errors can make floating-point values not quite what they ought
## to be.  This is a fairly unpleasant hack around the problem.

def rationalize(num, epsilon=1e-8):
    """Attempt to return a rational value near NUM (within EPSILON of it).
The value is returned as an integer or (more likely) floating-point number.
Return None if the value cannot be rationalized.
*Do not* use the return value as a boolean, because 0 and 0.0 test false.
Instead, explicitly test the result against None."""

    def rationalize_simple(num, eps=epsilon):
        floor = math.floor(num)
        diff = num-floor
        if diff < eps:
            return floor
        if 1-diff < eps:
            return floor+1
        return None

    if type(num) == types.IntType:
        return num

    result = rationalize_simple(num)
    if result != None:
        return result

    for denominator in range(2,21):
        result = rationalize_simple(num*denominator)
        if result != None:
            return result/denominator
        result = rationalize_simple(denominator/num)
        if (result != None) and (result != 0):
            return denominator/result
    return None

# Euclid's algorithm
def gcd(a,b):
    """Return the greatest common divisor of the two arguments."""
    while b != 0:
        (a, b) = (b, a % b)
    return a

def _test_gcd():
    assert gcd(2, 50) == 2
    assert gcd(12, 144) == 12
    assert gcd(96, 144) == 48
    assert gcd(10, 25) == 5


def gcd_list(nums):
    """Return the greatest common divisor of the elements of NUMS."""
    if len(nums) == 0:
        raise "No numbers passed to gcd()"
    result = nums[0]
    for n in nums:
        # First call is a bit of a waste, since the two arguments are identical
        result = gcd(result, n)
        if result == 1:
            return result
    return result

def _test_gcd_list():
    assert gcd_list([2, 50]) == 2
    assert gcd_list([12, 144]) == 12
    assert gcd_list([96, 144]) == 48
    assert gcd_list([10, 25]) == 5
    assert gcd_list([100, 10, 25]) == 5
    assert gcd_list([768, 324]) == 12
    assert gcd_list([2400, 48, 36]) == 12
    assert gcd_list([2400, 72, 36]) == 12


###########################################################################
### Random numbers
###

def random_subset(set, size):
    """Return a randomly-chosen subset of list or tuple SET of size SIZE."""
    total_size = len(set)

    if size < 0 or size > total_size:
        raise "Bad goal size %d passed to random_subset; original set has size %d" % (size, total_size)

    if size < total_size/2:
        chosen = {}
        while len(chosen) < size:
            chosen[set[whrandom.randint(0, total_size-1)]] = 1
        result = filter(lambda f, c=chosen: c.has_key(f), set)
    else:
        # If we ask for (say) 4999 of 5000 items, the above could iterate
        # a long time while inserting the last one.
        num_omitted = total_size - size
        omitted = {}
        while len(omitted) < num_omitted:
            omitted[set[whrandom.randint(0, total_size-1)]] = 1
        result = filter(lambda f, o=omitted: not(o.has_key(f)), set)

    return result

# I wish there was a way to save away the random state so I could restore
# it after calling this.
# def _test_random_subset():
#     whrandom.seed(3, 14, 67)
#     a_to_j = ('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j')
#     assert random_subset(a_to_j, 5) == ('a', 'b', 'c', 'h', 'i')
#     assert random_subset(a_to_j, 5) == ('a', 'g', 'h', 'i', 'j')
#     assert random_subset(a_to_j, 5) == ('a', 'b', 'c', 'g', 'j')
#     assert random_subset(a_to_j, 0) == ()
#     assert random_subset(a_to_j, 1) == ('c',)
#     assert random_subset(a_to_j, 2) == ('i', 'j')
#     assert random_subset(a_to_j, 8) == ('b', 'c', 'd', 'f', 'g', 'h', 'i', 'j')
#     assert random_subset(a_to_j, 9) == ('a', 'b', 'c', 'd', 'e', 'f', 'g', 'i', 'j')
#     assert random_subset(a_to_j, 10) == a_to_j


###########################################################################
### Strings
###

def remove_trailing_newline(str):
    """Return a copy of the input string STR, sans trailing newline.
If the input string STR has no trailing newline, it is returned unmodified."""
    if str[-1] == "\n":
        return str[:-1]
    else:
        return str

def re_compile_maybe(str, flags):
    """If first argument is a string, return it as a compiled regular expression.
Otherwise, return it unchanged."""
    if type(str) == types.StringType:
        return re.compile(str, flags)
    else:
        return str


###########################################################################
### Lists
###

def sub_sequence_of(seq1, seq2):
    """Return true if seq1 is a subsequence of seq2."""
    if len(seq1) > len(seq2):
        return false
    # For each slice of seq2 with length equal to seq1, test for subset
    subsequence_range = range(0, len(seq2) - len(seq1) + 1)
    for i in subsequence_range:
        if seq1 == seq2[i:len(seq1)+i]:
            return true
    return false


###########################################################################
### Sets (implemented as lists)
###

def sorted_list_difference(minuend, subtrahend):
    """Return a list of elements in sequence MINUEND but not in SUBTRAHEND.
Both input sequences are sorted, as is the output sequence."""
    # It's more efficient to do this by hand than to use "filter" and index.
    mmax = len(minuend)
    smax = len(subtrahend)
    if mmax == 0 or smax == 0:
        return minuend
    mindex = 0; mcurr = minuend[mindex]
    sindex = 0; scurr = subtrahend[sindex]
    result = []
    while (mindex < mmax) or (sindex < smax):
	while (mindex < mmax) and ((sindex == smax) or (mcurr < scurr)):
	    result.append(mcurr)
	    mindex = mindex+1
	    if (mindex < mmax): mcurr = minuend[mindex]
	while (sindex < smax) and ((mindex == mmax) or (scurr < mcurr)):
	    sindex = sindex+1
	    if (sindex < smax): scurr = subtrahend[sindex]
	if (mindex < mmax) and (sindex < smax) and (mcurr == scurr):
	    mindex = mindex+1
	    if (mindex < mmax): mcurr = minuend[mindex]
	    sindex = sindex+1
	    if (sindex < smax): scurr = subtrahend[sindex]
    return result

def _test_sorted_list_difference():
    assert sorted_list_difference([1,2,3,4,5,6], [2,4,6]) == [1, 3, 5]
    assert sorted_list_difference([1,2,3,4,5,6], [1,2,3]) == [4, 5, 6]
    assert sorted_list_difference([1,5,6], [1,2,3,4,5,6]) == []
    assert sorted_list_difference([1,5,6], [0,1,2,3,4,6,7]) == [5]
    assert sorted_list_difference([1,2,5,6], [2,3,4,5]) == [1, 6]

# I have a feeling this implementation could be substantially improved
# (both performance and style).
def sorted_list_intersection(a, b):
    amax = len(a)
    bmax = len(b)
    if amax == 0 or bmax == 0:
        return []
    aindex = 0; bindex = 0
    result = []
    while (aindex < amax) and (bindex < bmax):
        acurr = a[aindex]; bcurr = b[bindex]
        comparison = cmp(acurr,bcurr)
        if comparison == 0:
            result.append(acurr)
	    aindex = aindex+1
            bindex = bindex+1
        elif comparison < 0:
            aindex = aindex+1
        else:
            assert comparison > 0
            bindex = bindex+1
    return result

def _test_sorted_list_intersection():
    assert sorted_list_intersection([1,2,3,4,5,6], [2,4,6]) == [2, 4, 6]
    assert sorted_list_intersection([1,2,3,4,5,6], [1,2,3]) == [1, 2, 3]
    assert sorted_list_intersection([1,5,6], [1,2,3,4,5,6]) == [1, 5, 6]
    assert sorted_list_intersection([1,5,6], [0,1,2,3,4,6,7]) == [1, 6]
    assert sorted_list_intersection([1,2,5,6], [2,3,4,5]) == [2, 5]


# There MUST be a better way to do this!
def same_elements_setwise(seq1, seq2):
    """Return true if sequences seq1 and seq2 contain the same elements
(in any order)."""
    s1 = list(seq1)
    s2 = list(seq2)
    s1.sort()
    s2.sort()
    return s1 == s2

def format_as_set(seq1):
    result = `seq1`
    return "{" + result[1:-1] + "}"

## Could be useful, I guess.
# def intersects(seq1, seq2):
#     """Return true if sequences seq1 and seq2 contain any element in common."""


###########################################################################
### Mappings
###

# This is too slow; inline it.
def mapping_increment(mapping, key, incr=1):
    """Given a MAPPING from keys to numbers, increment the value associated
with KEY by INCR.  If KEY does not appear in MAPPING, then add it, with
initial value INCR.
It's a bad idea to use this function due to poor performance;
inline its (simple) definition instead."""
    mapping[key] = mapping.get(key, 0) + incr

# This is probably too slow (but I am not currently using it).
def mapping_append(mapping, key, element):
    """Given a MAPPING from keys to arrays, append, to the end of the value
associated with KEY, ELEMENT.  If KEY does not appear in MAPPING, then add
it, with initial value a list containing only ELEMENT."""
    mapping[key] = mapping.get(key, []) + [element]


## I think that none of this is used by Daikon; so comment it out to avoid
## confusion.
# ###########################################################################
# ### Slicing
# ###
# 
# ## This function is TOO SLOW!  To speed it up, perhaps inline all calls:
# ##   foo = util.slice_by_sequence(seq, (i1, i2))
# ## ==>
# ##   foo = (seq[i1], seq[i2])
# def slice_by_sequence(seq, indices):
#     """Given a sequence SEQ and a sequence of INDICES, return a new sequence
# whose length is the same as that of INDICES and whose elements are the values
# of SEQ indexed by the respective elements of INDICES.
# This function is so slow that you probably shouldn't use it; inline the calls
# instead."""
#     result = []
#     for i in indices:
#         result.append(seq[i])
#     ## This is probably completely extraneous.
#     # if type(seq) == types.TupleType:
#     #     result = tuple(result)
#     return result
# 
# def _test_slice_by_sequence():
#     def _test_slice_by_sequence_helper(indices):
#         assert slice_by_sequence(range(0,7), indices) == list(indices)
#         # No longer return a tuple if the argument was a tuple.
#         # assert slice_by_sequence(tuple(range(0,7)), indices) == tuple(indices)
#     _test_slice_by_sequence_helper(range(0,7))
#     _test_slice_by_sequence_helper(range(1,4))
#     _test_slice_by_sequence_helper((1,2,4,5))
# 
# 
# ###
# ### Dictionary slicing
# ###
# 
# def dict_of_tuples_to_tuple_of_dicts(dot, tuple_len=None):
#     """Input: a dictionary mapping a tuple of elements to a count.
#     All the key tuples in the input have the same length unless optional argument
#     TUPLE_LEN is provided, in which case all tuples have at least that length.
#     If TUPLE_LEN is a tuple, then only those indices are extracted.
#     if TUPLE_LEN is an integer, indices up to it (non-inclusive) are extracted.
#     Output: a tuple of dictionaries, each mapping a single element to a count.
#     The first output dictionary concerns the first element of the original keys,
#     the second output the second element of the original keys, and so forth."""
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
#             this_key = key_tuple[tuple_indices[i]]
#             this_dict = result[i]
#             this_dict[this_key] = this_dict.get(this_key, 0) + count
#     return result
# # dict_of_tuples_to_tuple_of_dicts(fn_var_values["PUSH-ACTION"])
# 
# 
# def dict_of_sequences_to_element_dict(dot):
#     """Input: a dictionary mapping instances of a sequence (tuples) to a count.
#     Output: a dictionary, mapping elements of all of the sequence instances
#     to a count."""
#     result = {}
#     for (key_tuple, count) in dot.items():
#         for this_key in key_tuple:
#             result[this_key] = result.get(this_key, 0) + count
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
#     if len(indices) == 2:
#         return dict_of_tuples_slice_3(dot, indices[0], indices[1], indices[2])
# 
#     result = {}
#     for (key_tuple, count) in dot.items():
#         sliced_tuple = util.slice_by_sequence(key_tuple, indices)
#         result[sliced_tuple] = result[sliced_tuple] + count
#     return result
# 
# # dict_of_tuples_slice(fn_var_values["PUSH-ACTION"], (0,))
# # dict_of_tuples_slice(fn_var_values["PUSH-ACTION"], (1,))
# # dict_of_tuples_slice(fn_var_values["VERIFY-CLEAN-PARALLEL"], (0,))
# # dict_of_tuples_slice(fn_var_values["VERIFY-CLEAN-PARALLEL"], (1,))
# # dict_of_tuples_slice(fn_var_values["VERIFY-CLEAN-PARALLEL"], (2,))
# # dict_of_tuples_slice(fn_var_values["VERIFY-CLEAN-PARALLEL"], (0,1))
# # dict_of_tuples_slice(fn_var_values["VERIFY-CLEAN-PARALLEL"], (0,2))
# # dict_of_tuples_slice(fn_var_values["VERIFY-CLEAN-PARALLEL"], (1,2))
# 
# 
# def dict_of_tuples_slice_2(dot, i1, i2):
#     """Input: a dictionary mapping a tuple of elements to a count, and a
#     list of indices.
#     Output: a dictionary mapping a subset of the original elements to a count.
#     The subset is chosen according to the input indices."""
# 
#     result = {}
#     for (key_tuple, count) in dot.items():
#         # sliced_tuple = util.slice_by_sequence(key_tuple, indices)
#         sliced_tuple = (key_tuple[i1], key_tuple[i2])
#         result[sliced_tuple] = result.get(sliced_tuple, 0) + count
#     return result
# 
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
#         result[sliced_tuple] = result.get(sliced_tuple, 0) + count
#     return result



###########################################################################
### Lists of numbers
###

# This implementation appears to be the best I can easily write; see
# timings below.
def sum(nums):
    result = 0
    for num in nums:
        result = result + num
    return result

# Timing data indicates sum3 is fastest by quite a bit:
#   sum1 2.36
#   sum2 1.91
#   sum3 1.35
# Moral: avoid function calls!
def sum1(nums):
    return reduce(lambda x,y: x+y, nums, 0)
def sum2(nums):
    return reduce(operator.add, nums, 0)
def sum3(nums):
    result = 0
    for num in nums:
        result = result + num
    return result
def time_sums():
    testdata = range(256)
    print `testdata`
    testfuncs = sum1, sum2, sum3
    for f in testfuncs: print f.func_name, f(testdata)
    for f in testfuncs: timing(f, 100, testdata)


## I can't imagine what this would be good for.  It used to be erroneously
## used in common_modulus, but now I use gcd_list and list_differences
## instead.
def sorted_list_min_gap(sorted_nums):
    """Return the smallest difference between adjacent elements of the sorted list."""
    if len(sorted_nums) < 2:
        raise IndexError("List too short")
    min_gap = sorted_nums[1]-sorted_nums[0]
    for i in range(1, len(sorted_nums)-1):
        this_gap = sorted_nums[i+1]-sorted_nums[i]
        if this_gap < min_gap:
            min_gap = this_gap
    return min_gap

def _test_sorted_list_min_gap():
    assert sorted_list_min_gap([2,4,6,8]) == 2
    assert sorted_list_min_gap([2,4,60,80]) == 2
    assert sorted_list_min_gap([20,40,600,608]) == 8
    assert sorted_list_min_gap([2,40,46,80]) == 6
    assert sorted_list_min_gap([2,4]) == 2
    # Errors
    # sorted_list_min_gap([2])
    # sorted_list_min_gap([])


def list_differences(nums):
    """Return the list of differences between adjacent elements of NUMS."""
    result = []
    if len(nums) < 2:
        raise IndexError("List too short")
    for i in range(0, len(nums)-1):
        ## Just let the OverflowError occur and be propagated to the caller
        # try:
        #     difference = nums[i+1]-nums[i]
        # except OverflowError:
        #     difference = maxint
        difference = nums[i+1]-nums[i]
        result.append(difference)
    return result

def _test_list_differences():
    assert list_differences([1,2,3,4,5]) == [1,1,1,1]
    assert list_differences([1,2,3,5,8,13]) == [1,1,2,3,5]
    assert list_differences([22,0,15,-12,33]) == [-22,15,-27,45]


def common_modulus(nums):
    """Return a tuple of (r,m) where each number in NUMS is equal to r (mod m).
The largest possible modulus is used, and the trivial constraint that all
integers are equal to 0 mod 1 is not returned (None is returned instead)."""
    if len(nums) < 3:
        return None
    ## I think I don't need to sort
    # nums = list(nums)			# convert arg if it's a tuple
    # nums.sort()

    ## Rather than computing the entire list_differences, I could inline
    ## and bail out early.  Perhaps not worth it for now.
    try:
        modulus = abs(gcd_list(list_differences(nums)))
    except OverflowError:
        return None
    if modulus == 1:
        return None

    remainder = nums[0] % modulus

    def check_modulus(m, r, nums=nums):
        for elt in nums:
            if r != elt % m:
                return false
        return true
    assert check_modulus(modulus, remainder)

    return (remainder, modulus)


def _test_common_modulus():
    assert common_modulus([3,7,47,51]) == (3,4)
    assert common_modulus([3,11,43,51]) == (3,8)
    assert common_modulus([3,11,47,55]) == (3,4)
    assert common_modulus([2383,4015,-81,463,-689]) == (15,32)



## This implementation is particularly inefficient; find a better way to
## compute this.
def common_nonmodulus_strict(nums):
    """Return a tuple of (r,m) where no number in NUMS is equal to r (mod m)
but all missing numbers in their range are."""
    nums = list(nums)
    nums.sort()
    try:
        if nums[-1] - nums[0] > 65536:
            return None
    except OverflowError:
        return None
    return common_modulus(sorted_list_difference(range(nums[0]+1, nums[-1]-1), nums))

def _test_common_nonmodulus_strict():
    # Doesn't work because given only two items, common_modulus doesn't
    # go out on a limb and suggest they're equal mod their difference
    assert common_nonmodulus_strict([1,2,3,5,6,7,9]) == None
    assert common_nonmodulus_strict([-1,1,2,3,5,6,7,9]) == (0,4)
    ## This is an error!  The missing numbers ==0 mod 2, but so do
    ## some of the present numbers.
    # assert common_nonmodulus_strict([1,2,3,5,6,7,9,11]) == (0,2)
    assert common_nonmodulus_strict([1,2,3,5,6,7,11]) == None

# This seems to give too many false positives.
def common_nonmodulus_nonstrict(nums):
    """Return a tuple of (r,m) where no number in NUMS is equal to r (mod m)
but for every number in NUMS, at least one is equal to every non-r remainder.
The modulus is chosen as small as possible, but no greater than half the
range of the input numbers (else None is returned)."""
    nums = list(nums)
    nums.sort()
    max_modulus = (nums[-1] - nums[0]) / 2
    # no real sense checking 2, as common_modulus would have found it, but
    # include it to make this function stand on its own
    for m in range(2, max_modulus+1):
        remainders = list((0,) * m)
        # It might be more efficient to quit as soon as we see every remainder
        # instead of processing each element of the input list.
        for num in nums:
            remainders[num % m] = 1
        if remainders.count(0) == 1:
            return (remainders.index(0), m)
    return None

def _test_common_nonmodulus_nonstrict():
    assert common_nonmodulus_nonstrict([1,2,3,5,6,7,9]) == (0,4)
    assert common_nonmodulus_nonstrict([1,2,3,5,6,7,9,11]) == (0,4)
    assert common_nonmodulus_nonstrict([1,2,3,5,6,7,9,11,12,13]) == (4,6)
    assert common_nonmodulus_nonstrict([1,2,3,5,6,7,9,11,12,13,16]) == None


###########################################################################
### Sorting
###

def sorted(l, comparator=cmp):
    for i in range(1, len(l)):
        if apply(comparator, (l[i-1], l[i])) == 1:
            return false
    return true

# This doesn't test the optional argument; probably ought to.
def _test_sorted():
    assert sorted([1,2,3,4,5])
    assert sorted([])
    assert sorted([22])
    assert sorted([1,2,2,2,17,17])
    assert not sorted([22,17])
    assert not sorted([1,2,4,8,16,5,8,13,21])
    assert sorted([1,2,3,4,5], cmp)
    assert sorted([], cmp)
    assert sorted([22], cmp)
    assert sorted([1,2,2,2,17,17], cmp)
    assert not sorted([22,17], cmp)
    assert not sorted([1,2,4,8,16,5,8,13,21], cmp)


# Not worth defining -- too trivial
# def cmp_second_element(a, b):
#     # index 1 is second element
#     return cmp(a[1],  b[1])


###########################################################################
### Combinatorics
###

def choose(num, objs):
    """Choose NUM of the obs, without repetition.  Order is irrelevant.
Returns a list of the results, which are themselves lists."""
    if num == 0:
        return []
    if num == 1:
        return map(lambda x: [x], objs)
    if num == len(objs):
        return [list(objs)]
    if num > len(objs):
        raise IndexError("Can't choose %d objects from list of length %d" % (num, len(objs)))
    def make_conser(first):
        return lambda x, f=first: [f] + x
    rest = objs[1:]
    return choose(num, rest) + map(make_conser(objs[0]), choose(num-1, rest))

def _test_choose():
    assert choose(3,[1,2,3]) == [[1,2,3]]
    assert choose(3,[1,2,3,4]) == [[2,3,4], [1,3,4], [1,2,3], [1,2,4]]


# There must be a better way of doing this!
def permutations(objs):
    """Return a list containing all permutations of OBJS; each permutation
is itself a list.
Assumes OBJS contains no two elements eql to one another (they are
considered to appear just once)."""
    if len(objs) == 0:
        return [[]]
    result = []
    for obj in objs:
        subobjs = list(objs)            # makes a copy
        subobjs.remove(obj)
        for subperm in permutations(subobjs):
            subperm.append(obj)
            result.append(subperm)
    return result

def _test_permutations():
    assert permutations(()) == [[]]
    assert permutations((1,)) == [[1]]
    assert permutations((1,2)) == [[2,1], [1,2]]
    assert permutations((1,2,3)) == [[3,2,1], [2,3,1], [3,1,2], [1,3,2], [2,1,3], [1,2,3]]


###########################################################################
### Functions
###

builtin_fn_re = re.compile(r'^<built-in function ([a-zA-Z_]+)>$')
user_fn_re = re.compile(r'^<function ([a-zA-Z_]+) at [0-9a-fA-F]+>$')

def function_rep(fn):
    """Return an abbreviated printed representation for function FN."""
    fnrep = `fn`
    match = builtin_fn_re.match(fnrep)
    if match:
        return match.group(1)
    match = user_fn_re.match(fnrep)
    if match:
        return match.group(1)
    return fnrep


###########################################################################
### Timing (from http://www.python.org/doc/essays/f.py)
###

def timing(f, n, a):
    print f.__name__,
    r = range(n)
    t1 = time.clock()
    for i in r:
        f(a); f(a); f(a); f(a); f(a); f(a); f(a); f(a); f(a); f(a)
    t2 = time.clock()
    print round(t2-t1, 3)


###########################################################################
### Memory usage
###

# On both Linux and Solaris, "getrusage" reports zero pages in use, when
# called from Python, C, or the command line, even if I have just allocated
# tens of megabytes of memory.  The "ps" and "top" programs do work, however;
# this implementation uses "ps".

def memory_usage(pid = posix.getpid()):
    """Return the number of kilobytes of virtual memory used by process PID
    (by default, the current process)."""
    os = posix.uname()[0]
    if os == 'SunOS':
        # On Solaris, do
        #   % /usr/bin/ps -p 12162 -o vsz,osz,rss,pmem
        #    VSZ   SZ  RSS %MEM
        #   37360 4670 35272 18.5
        # where the options mean the following (and VSZ is what I want):
        #      vsz         The size of the process in (virtual)  memory  in
        #                  kilobytes as a decimal integer.
        #      osz         The size (in pages) of the  swappable  process's
        #                  image in main memory.
        #      rss         The resident set size of the process,  in  kilo-
        #                  bytes as a decimal integer.
        #      pmem        The ratio of the process's resident set size  to
        #                  the physical memory on the machine, expressed as
        #                  a percentage.

        tfilename = tempfile.mktemp()
        # Ought to check return status of command
        posix.system("/usr/bin/ps -p %d -o vsz > %s" % (pid, tfilename))
        tfile = open(tfilename, "r")
        line = tfile.readline()
        assert line == " VSZ\n"
        vsize = int(string.strip(tfile.readline()))
        tfile.close()
        posix.unlink(tfilename)
    elif os == 'Linux':
        # On Linux, do
        # % /bin/ps -mp 24335
        #   PID TTY MAJFLT MINFLT   TRS   DRS  SIZE  SWAP   RSS  SHRD   LIB  DT COMMAND
        # 24335  qd  13965  13667     6 11482 13672  2184 11488     9     0 11476 /uns/bin/p
        # and the SIZE field is what I want (it is code+data+stack).

        tfilename = tempfile.mktemp()
        # Ought to check return status of command
        posix.system("/bin/ps -mp %d > %s" % (pid, tfilename))
        tfile = open(tfilename, "r")
        line = tfile.readline()
        assert line == "  PID TTY MAJFLT MINFLT   TRS   DRS  SIZE  SWAP   RSS  SHRD   LIB  DT COMMAND\n"
        vsize = int(string.split(string.strip(tfile.readline()))[6])
        tfile.close()
        posix.unlink(tfilename)
    else:
        raise "memory_usage does not understand operating system %s; please teach it" % os

    return vsize


###########################################################################
### File names
###

def expand_file_name(filename):
    def env_repl(matchobj):
        return posix.environ[matchobj.group(0)[1:]]
    filename = re.sub(r'^~/', '$HOME/', filename)
    filename = re.sub(r'^~', '/homes/fish/', filename)
    filename = re.sub(r'\$[a-zA-Z_]+', env_repl, filename)
    return filename


###########################################################################
### Forms
###

re_class = re.compile("a").__class__

def read_form(file, form):
    """Reads input from the file, parse the input according to the form,
    and returns a list of values.
    The form is an array of the form
      ((REGEXP-OR-STRING [, STRING] [, group1, group2, group3, ...[, (default1, default2, ...)]])
       [, ...])
    The form may be modified to insert a regexp before the string, if no
    regexp is provided.  If both of the first two elements are strings, the
    second is ignored and the first is treated literally (not as a regexp).
    Not yet implemented:
      If defaults are provided, it is not an error for a line not to match
      (the next pattern is tried).  The last pattern should not be optional,
      lest this code gobble a line without processing it.
    """

    # First, convert any strings to regular expressions
    for elt in form:
        re_or_str = elt[0]
        if ((type(re_or_str) == types.StringType)
            and ((len(elt) == 1) or (type(elt[1]) != types.StringType))):
            elt.insert(0, re.compile(elt[0], re.IGNORECASE))
        else:
            assert (((type(re_or_str) == types.StringType) and (type(elt[1]) == types.StringType))
                    or (re_or_str.__class__ == re_class))

    result = []
    for elt in form:
        line = file.readline()
        this_re = elt[0]
        if type(this_re) == types.StringType:
            if line != this_re:
                raise "Expected line to be " + this_re + "\nbut found " + line
        else:
            match = this_re.search(line)
            if not match:
                raise "Expected match for " + elt[1] + "\nbut found " + line
            if len(elt) > 2:
                # This seems to require calling apply; yuck.
                # result.fromlist(apply(MatchObject.group(match, (elt[range(2, len(elt)))))))
                for i in range(2, len(elt)):
                    result.append(match.group(elt[i]))

    return result


###########################################################################
### Tests
###

def _test():
    if not __debug__:
        raise "__debug__ is not set, so _test() will have no effect!"
    _test_gcd()
    _test_gcd_list()
    _test_sorted_list_difference()
    _test_sorted_list_intersection()
    _test_slice_by_sequence()
    _test_sorted_list_min_gap()
    _test_list_differences()
    _test_common_modulus()
    _test_common_nonmodulus_strict()
    _test_common_nonmodulus_nonstrict()
    _test_sorted()
    _test_choose()
    _test_permutations()

# It's slightly annoying that these run every time that I load this directly.
# But I shouldn't load it directly except to test anyway.
if __name__=='__main__':
    _test()
    print "util.py tests completed successfully"
