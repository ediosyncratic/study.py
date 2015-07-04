"""Combinatorics.

See study.LICENSE for copyright and license information.
"""

def factorial(num, cache=[1], maxcache=0x4000):
    """Returns the factorial of any natural number.

    Required argument, num, is the natural number.  Can also be passed
    (although this is not recommended), as a keyword, maxcache as a limit on
    how big a cache this should maintain.  Defaults to 0x4000, which shall use
    up plenty of your computer's memory as it is.  Only relevant if num
    exceeds it (or would exceed the default).

    Return value is equivalent to reduce(lambda a,b:a*(1+b), range(num), 1),
    except that it degrades gracefully when given invalid input and caches
    answers, so may give you an answer sooner.

    For agrguments < 0 this raises a ValueError.  For other arguments < 1,
    you'll get the answer 1, as this is correct if your argument is valid and
    resolves the problem of what to do with invalid input.  For other
    non-integer non-negative values, you'll get a computed positive value
    which probably isn't far off the Gamma function's value at about one more
    than that non-integer.  For valid input, i.e. non-negative integers,
    factorial(num) = Gamma(1+num), wherein

       Gamma(1+a) = integral(: exp(-t).power(a, t) &larr;t :{positive reals})

    for arbitrary a in the complex plane, with a pole (of order 1) at each
    negative integer and a real humdinger of a singularity at infinity.\n"""

    if num < 0: raise ValueError, "I only do naturals"
    try: return cache[num]
    except IndexError: pass

    result, i = cache[-1], len(cache)
    while num >= i:
        result *= i
        i += 1
        if len(cache) < maxcache:
            cache.append(result)
            assert i == len(cache)

    return result

class Pascal:
    """A dictionary describing Pascal's triangle.

    Keys are pairs of naturals; value for (n,m) is (n+m)! / n! / m!

    Given the symmetry, no value is actually stored for key (n,m) when m >
    n. Given the nature of Pascal's triangle, computing the value for a given
    (n,m) involves computing the values for all (i,j) with i<=n and j<=m.

    Representation depicts Pascal's triangle with one side as the top line,
    the other as the left column; a `row' of Pascal's triangle is thus a
    diagonal stripe.  Only values we've actually needed are shown, so there's
    nothing above the diagonal; except on the top line, where a 1 is displayed
    above the last digit of the diagonal entry in each column.

    Note that neither this class nor its one instance is any part of this
    module's export: it is over-ridden by a curried form of chose(), below.\n"""

    __values = {}
    def __getitem__(self, (n, m)): # keys to this object should be pairs of naturals
        try: val = self.__lookup(n, m)
        except RuntimeError, what:
            print what
            if what.args[0] != 'maximum recursion depth exceeded': raise
            val = factorial(n+m) / factorial(n) / factorial(m)
            if n < m: n, m = m, n   # exploit symmetry
            self.__values[n, m] = val

        return val

    def __lookup(self, n, m):
        if n < m: n, m = m, n   # exploit symmetry
        assert n >= m
        if m < 0: return 0 # silly input
        if m == 0: return 1 # no sense cacheing
        if m == 1: return n+1 # likewise

        try: val = self.__values[n, m]
        except KeyError: # ho hum, need to compute it ...
            up, left = self.__lookup(n-1, m), self.__lookup(n, m-1)
            self.__values[n, m] = val = left + up

        return val

    def __str__(self):
        keys = self.__values.keys()
        if not keys: return '1, 1, ...\n1,'
        keys.sort()
        # the following may be broken by __getitem__'s RuntimeError fallback ...

        row = []
        for n, m in keys:
            while n > len(row):
                row.append(['1', `len(row) + 2`])
            assert len(row[n-1]) == m
            row[n-1].append(`self.__values[n,m]`)

        # Now turn each line into a single string:
        n = len(row) + 1
        while n > 1:
            n = n - 1
            here = row[n-1]
            if len(here) < n + 1: here.append('...')
            row[n-1] = ', '.join(here)

        head = '1, 1'
        # now put a 1 above the last digit of each diagonal entry:
        off, m, n = len(head), 1, len(row) + 1
        while m < n:
            try: new = len(row[m])
            except IndexError:
                raise IndexError(row, m, n)

            gap = new - off - 1
            if gap < 2: break # last row brought us to our last column
            head, off, m = head + ',%*d' % (gap, 1), new, 1+m

        return head + ', ...\n' + ',\n'.join(row) + ',\n...'

    __repr__ = __str__

    def check(self):
        """Uses factorial() to verify results of chose(). """
        result = []
        for (n, m), val in self.__values.items():
            check = factorial(n+m) / factorial(n) / factorial(m)
            if val != check:
                result.append('%d, %d ->' % (n,m) + `val` + ' != ' + `check`)

        if result:
            return '\n'.joinfields(result)

Pascal = Pascal()

def chose(total, part, rack=Pascal):
    """chose(N,i) -> N! / (N-i)! / i!

    This is the number of ways of chosing i items from among N, ignoring order
    of choice.  Computed using a cached form of Pascal's triangle. """

    return rack[total-part, part]

def check(rack=Pascal): rack.check()

def Pascal(tot, scale=1):
    """A row of Pascal's triangle, optionally scaled, as a tuple.

    Required argument, tot, is the row index: Pascal(1+i)[1+j] = Pascal(i)[j] +
    Pascal(i)[1+j] give-or-take missing entries being presumed zero, with
    Pascal[0] = (1,).  Optional second argument is an over-all scaling to apply
    to all entries in the row; thus sum(Pascal(n, .5**n)) == 1.\n"""

    return tuple(map(lambda i, t=tot, s=scale: chose(t, i) * s, range(1+tot)))

def c2nno4n(n):
    """n => chose(2n, n)/4**n

    Computed as product(lambda i: (n+i)/4/i, range(1, 1+n)), taking terms in
    such order as to keep the computed value near 1 as long as possible, so as
    to restrain the impact of rounding errors.

    Note that this ~ (n*pi)**.5 for large n; evaluating lambda x:
    x*(1+2./(9+(8*x)**2)) at n+.25, multiplying by pi and taking 1/sqrt()
    gives a good approximation to the answer; see study.stats.stirling.c2nn()
    and http://www.chaos.org.uk/~eddy/math/factorial.html#Approx\n"""
    ans, each = 1, lambda k, q = .25 * n: q / k + .25 # (n+k)/4/k
    i = j = n // 3 # solves n+k = 4*k, rounding down
    # each(j) is as close to 1 as we can get it; as j decreases it grows; as i
    # increases, each(i) shrinks
    while i < n or j > 0: # iterate outwards:
        if j > 0 and (i == n or ans < 1):
            ans *= each(j) # > 1
            j -= 1
        else:
            assert i < n and (j == 0 or ans >= 1)
            i += 1
            ans *= each(i) # < 1
    return ans
