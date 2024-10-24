"""Computing digits of exponentials

Compare edigit.py, which only does this for exp(1) = e, rather than
general exp(x), so can do some optimisations based on that.  It can
also do some pretty-printing.

See study.LICENSE for copyright and license information.
"""
import math

def _fracSplit(v):
    u = int(v)
    if u > v: u += 1
    assert 0 <= v -u < 1
    return u, v -u

class Digex (object):
    """Generator for digits of an exponential.

    The basic idea is that we have a power series sum(: x**n/n!
    &larr;n :{naturals}) that we can represent as s[1] +(1/2).(s[2]
    +(1/3).(s[3] +...)) in which, in the first instance (with s[1] = 1
    +x and each subsequent s[i] being x**i) we can, for each i from 1
    onwards, take the fractional part of s[i], multiply it by i +1,
    add that into s[i +1] and increment i, thereby reducing each entry
    in s to a whole number.  There shall be an error term right at the
    end, to which I'll return.

    We can now start with the highest-index s[i] that's >= i > 1,
    divide it by i, leave the remainder as s[i] and add the quotient
    into s[i-1]; working doown through decreasing i, we then get our
    sequence into a form where, aside from s[1], each s[i] < i.

    Once we have this initial representation of our sum, s[1] is the
    whole number part of the value we're computing.  That might take
    several digits to represent in our number base, of course; do so,
    emitting the digits in decreasing order of significance, and emit
    the fractional part separator before proceeding further.

    Once we've dealt with the whole-number part of 10**n times our
    value, and cleared s[1], we have a sum with each s[i] < i and 1 >
    (1/2).(s[2] +(1/3).(s[3] +(1/4).(s[4] +...))).  If we now scale
    each entry in s by 10 and repeat the sweep from highest index s[i]
    >= i > 1, subsequently decreasing i to 1, as we q, s[i] =
    divmod(s[i], i); s[i-1] += q, the resulting s[1] shall be < 10
    (because the sum, before we scaled it by 10, was < 1) and the
    whole number part of 10**(n+1) times our value, i.e. its next
    digit.  We then emit s[1], clear it and repeat this operation to
    get each successive digit.

    Inevitably we have only a finite sub-sum, truncating the power
    series at some index.  We can use logarithms and Stirling's
    formula to compute the scale of the error resulting from that
    truncation and, as a result, how many digits we can rely on from
    this process.

    As the process progresses towards the last digit we can rely on,
    we can indeed truncate our sequence s, as its later terms become
    moot due to the error term effectively creeping up the chain, to
    emerge at s[1] when we can no longer trust the digits we get out
    of it.  So we can terminate our sequence with an error-term
    tracker object that keeps track of the spread of values that the
    next entry in s would have, were it still live; once that gets to
    be more than its index in s, the tracker converts it to an error
    in the last active entry in s and replaces that entry.

    During initial set-up we can, indeed, also use a terminal object,
    that rides outwards in similar manner, to initialise the entries
    in s.  This lets us manage our error estimate with better
    precision, by not actually computing some values until they are
    O(1) or bigger.  As we multiply by 10 after emitting a digit, this
    set-up object rides further out along s, filling in entries and
    keeping track of the residual error term.  Once that leads to it
    hitting the end of s, it replaces itself with the error-term
    tracker that trims entries instead.

    The crucial fact this all relies on is that s[i] in i for all i
    ensures sum(: s[i]/i! &larr;i :) < 1.  To see this, observe

      sum(: (i -1)/i! &larr;i; i>1 :)
        = sum(: 1/(i-1)! -1/i! &larr;i; i>1 :)
        = sum(: 1/j! &larr;j; j>0 :) -sum(: 1/i! &larr;i; i>1 :)
        = 1,

    since every term but j = 1 is the same between the two sums.  This
    is the maximal value that sum(: s[i]/i! &larr;i :) can have, as
    long as 0 <= s[i] < i for each entry in s.

    Note, of course, that this computation is only as accurate as the
    input it's given.  If you ask it for exp(x) for some
    floating-point number x (such as math.log(2), to take an example
    I've tested), it's only accurate to about one part in 2**53 or
    about 17 decimal places, so the value of exp(x) that gets reported
    be suject to the same level of imprecision.
    """
    class __Stepper (object):
        """Virtual base for __Filler and __Trimmer.

        Defines the common behaviour exercised by Digex.
        Data members:

          index -- the object represents s[self.index]

        Methods:

          step(s) -- scale by 10 and, as needed, advance or retreat.

        A call to step() may change index.  Its caller should add
        step()'s return to s[index-1] and perform the divmod(s[i], i)
        iteration from there downwards to i = 1 that generates the
        next digit.
        """

    class __Filler (__Stepper):
        carry = 0
        def __init__(self, s, x, base):
            self.x, self.base, self.ticks = x, base, 0
            i = 1
            if isinstance(x, int) or isinstance(x, long):
                s[i] = 1 +x
                while i +1 < len(s) and s[i] +1 >= i:
                    i += 1
                    s[i] = x**i
                r = 0
            else:
                s[i], r = _fracSplit(1 +x)
                while i +1 < len(s) and s[i] +1 >= i:
                    i += 1
                    s[i], r = _fracSplit(x**i +i * r)
            self.error = r
            self.index = i +1

        def step(self, s):
            self.ticks += 1
            scale = self.base ** self.ticks
            i, x = self.index, self.x
            if isinstance(x, int) or isinstance(x, long):
                assert self.error == 0
                s[i] = x**i * scale
                while i +1 < len(s) and s[i] +1 >= i:
                    i += 1
                    s[i] = x**i * scale
            else:
                s[i], r = _fracSplit((x**i +i * self.error) * scale)
                while i +1 < len(s) and s[i] +1 >= i:
                    i += 1
                    s[i], r = _fracSplit((x**i +i * r) * scale)
                self.error = r
            self.index = i +1

    class __Trimmer (__Stepper):
        def __init__(self, s, filler):
            i = filler.index
            assert i == len(s)
            assert i > abs(filler.x) # Computation will be lousy otherwise
            self.index, self.base = i, filler.base
            r = filler.error * i
            # Our s[i] is somewhere between abs(x)**i and the result
            # of dividing that by (1 -abs(x)/i); both get to be scaled
            # by base**ticks and we add r to each.
            tail = abs(filler.x) ** i * filler.base ** filler.ticks
            self.__high = tail / (1 -abs(filler.x) / i) +r
            self.__tail = tail +r
            self.__trim(s)

        def __trim(self, s):
            tail, high, i = self.__tail, self.__high, self.index
            self.carry = _fracSplit(tail)[0] // i
            # If high would imply carrying more, wind left until it doesn't:
            while i > 2 and high > (self.carry +1) * i -1:
                high /= i
                tail /= i
                i -= 1
                self.carry = _fracSplit(tail)[0] // i
            carried = self.carry * i
            self.__tail, self.__high = tail - carried, high - carried
            self.index = i

        def step(self, s):
            tail, high = self.__tail * self.base, self.__high * self.base
            self.__trim(s)

    def __init__(self, size, x = 1, base = 10):
        if not size > 1:
            raise ValueError("Way too small", size)
        self.__s = [0] * size
        self.__step = Digex.__Filler(self.__s, x, base)
        self.__gather()

    def __gather(self):
        s = self.__s
        if isinstance(self.__step, Digex.__Filler) and self.__step.index >= len(self.__s):
            self.__step = Digex.__Trimmer(s, self.__step)
        i = self.__step.index
        assert i <= len(s)
        carry = self.__step.carry
        while i > 2:
            i -= 1
            carry, s[i] = divmod(s[i] +carry, i)
        s[1] += carry

    def __iter__(self): return self
    def next(self):
        if self.__step.index < 2:
            raise StopIteration

        result, self.__s[1] = self.__s[1], 0
        for i in range(2, self.__step.index):
            self.__s[i] *= self.__step.base

        self.__step.step(self.__s)
        self.__gather()

        return result

    @staticmethod
    def __enough(digits, x, base, ln = math.log, offset=math.log(2/math.pi)/2):
        """Compute number of digits for enough().

        To have any hope of bounding the error size, we need x < n so
        that each subsequent x/i factor makes later terms smaller.
        For x/n <= q < 1, our error is then smaller than x**n/n! times
        sum(: q**i &larr;i :{naturals}) = 1/(1 -q), so we want
        x**n/n!/(1 -x/n) < .5/base**digits.  Taking logarithms, that
        becomes, n.ln(x) -log(n!) -ln(1 -x/n) < -digits*ln(base)
        -ln(2).  Using Stirling's formula, we get

            n.ln(x) +digits*ln(base) -ln(1 -x/n) +ln(2)
              < log(n!) = (n + .5) * ln(n) -n +ln(2*pi)/2 -1/12./n

        which can be re-written as

            digits*ln(base) +ln(2/pi)/2
              < n.ln(n/x) +ln(n)/2 -n +ln(1 -x/n) -1/12./n
        """
        assert x >= 0, "You should pass abs(x), not the actual x"
        goal = digits * ln(base) +offset
        func = lambda n, u=float(x): n * ln(n / u) +ln(n)/2 -n +ln(1 -u/n) -1/12./n
        fdash = lambda n, u=float(x): ln(n / u) +(0.5 +1/(n/u -1) +.25/3/n) / n
        n = max((goal, 1, x))

        # Newton-Raphson, with a tweaked termination condition:
        err, fix = -1, 0
        while err <= 0 or fix >= base:
            ans -= fix
            err = func(ans) -goal
            fix = err / fdash(ans)

        return int(ans) +1

    @classmethod
    def enough(cls, digits, x = 1, base = 10):
        """Pseudo-constructor to get a target number of digits

        Works out how long s must be to make the error term sum(: x**i
        / i! &larr;i; i > len(s) :{naturals}) < base**digits, forwards
        that size to the standard constructor.  The size estimate may
        be conservative; the resulting iterator may produce more
        digits than you asked for.
        """
        return cls(cls.__enough(digits, abs(x), base), x, base)
