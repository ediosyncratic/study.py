"""Computing the digits of e

Compare digex.py which does similar for general exp(x).

See study.LICENSE for copyright and license information.
"""
import math

class EDigitGenerator (object):
    """A generator for the digits of e.

    The basic idea is to start with

       e = 1/0! +1/1! +1/2! +1/3! +1/4! +1/5! +1/6! +...
         = 2 +1/2! +1/3! +1/4! +1/5! +1/6! +...

    giving us 2 as first digit, then multiply the rest by ten to get

      10/2! +10/3! +10/4! +10/5! +10/6! +10/7! +10/8! +10/9! +10/10! +10/11! +...
      = 10/2! +10/3! +10/4! +10/5! +10/6! +10/7! +10/8! +11/9! +0/10! +10/11! +...
      = 10/2! +10/3! +10/4! +10/5! +10/6! +10/7! +11/8! +2/9! +0/10! +10/11! +...
      = 10/2! +10/3! +12/4! +1/5! +5/6! +4/7! +3/8! +2/9! +0/10! +10/11! +...
      = 14/2! +1/3! +0/4! +1/5! +5/6! +4/7! +3/8! +2/9! +0/10! +10/11! +...
      = 7 +0/2! +1/3! +0/4! +1/5! +5/6! +4/7! +3/8! +2/9! +0/10! +10/11! +...

    giving us 7 as next digit; then multiply again by ten to get

      0/2! +10/3! +0/4! +10/5! +50/6! +40/7! +30/8! +20/9! +0/10! +100/11! +...
      = 3/2! +1/3! +3/4! +4/5! +2/6! +2/7! +0/8! +2/9! +9/10! +10/11! +...
      = 1 +1/2! +1/3! +3/4! +4/5! +2/6! +2/7! +0/8! +2/9! +9/10! +10/11! +...

    so the next digit is 1; then, by repeating this process, we get
    successive digits of e.  This relies on the fact that sum(i/n! for
    n, i in enumerate(f)) < 1 whenever f satisfies all(i in range(n)
    for n, i in enumerate(f)).  This can be proved by observing that
    the highest that such a sum can be is sum(n/(n+1)! for n in
    naturals) and:

      1/i! -sum(n/(n+1)! for n in naturals if n > i)
      = 1/i! -i/(i+1)! -sum(n/(n+1)! for n in naturals if n > i+1)
      = (i +1 -i)/(i+1)! -sum(n/(n+1)! for n in naturals if n > i+1)
      = 1/(i+1)! -sum(n/(n+1)! for n in naturals if n > i+1)

    is inductively the same for all natural i, hence also for i = 0;
    as its value manifestly tends to zero for large i, it must in fact
    be zero, giving

      1 = sum(n/(n+1)! for n in naturals)

    Alternatively, we can write this sum as the derivative, at 1, of
    the function sum(x**n/(n+1)! for n in naturals) = (exp(x) -1)/x,
    whose derivative is exp(x)/x -exp(x)/x/x +1/x/x = (exp(x)*(x -1)
    +1)/x/x, which is indeed 1 at x = 1.
    """
    def __init__(self, count, digits="0123456789", decsep='.'):
        """Construct the generator.

        Required first argument, count, is the number of digits we're
        to produce.  Optional second argument is a string, digits, for
        which digits[i] shall be used to represent the digit i to base
        len(digits).  The calculations shall be done to this number
        base.  The default is the usual decimal digits, 0 through 9,
        to get the usual decimal representaiton.  Optional third
        argument is the separator to supply between the whole-number
        part and the fractional part of the number.

        The first yield will be the whole number part, plus this
        separator, after which each yield shall be a single digit of
        the fractional part.

        Note that the iterator may produce somewhat more than count
        digits in practice: it uses a more conservative estimate of
        the error in deciding how big an array to use than in actually
        computing using that array.  (Furthermore, the way errors are
        estimated leads to getting more extra digits as the number
        asked for approaches the base from below than when asking for
        just more than the base.)  The final digit is not rounded, so
        if the next digit is half the base or more it is not a valid
        truncation of e's representation to this base.
        """
        assert count >= 0, "Don't be silly"
        assert len(digits) > 1, "Don't be silly"
        self.__digits = digits
        self.__base = len(digits)
        self.count = count
        n = self.__array_size()
        # f(2) through f(n) representing sum(f(i)/i!) as
        # sum(e / (i +2)! for i, e in enumerate(self.__array)).
        self.__array = [1] * (n - 1)
        # At index i, error <= m/i is represented by __error = i, m
        self.__error = n, 1
        initial, whole = decsep, 2 # The whole-number part
        while whole:
            whole, r = divmod(whole, self.__base)
            initial = digits[r] + initial
        self.__initial = initial

    def __array_size(self, ln=math.log, offset = math.log(2 / math.pi) / 2):
        """Number of terms needed to get self.count digits.

        When computing e as sum(1./factorial(i) for i in range(n+1)),
        the error is 1/(n+1)! +1/(n+2)! +1/(n +3)! +... < 1/(n+1)!
        +1/(n+2)! +2/(n +3)!, which is (1 +(1 +2/(n+3))/(n+2))/(n+1)!
        = (1 +(n+5)/(n+3)/(n+2))/(n+1)! < (1 +1/n)/(n +1)! = 1/n/n!,
        as (n +5)*n = n*n +5*n < n*n +5*n +6 = (n +3)*(n +2) so (n
        +5)/(n +3)/(n +2) < 1/n.  So dividing the final 1/n! term we
        did include by n gives an upper bound on the error, and we
        want that to be less than (b**-d)/2, where b = self.__base and
        d is self.count, to make the error less than half the
        difference that a change of one in the last digit would make.
        So we want n for which ln(n!) +ln(n/2) > ln(b)*d.

        Stirling gives us ln(n!) = (n + .5) * ln(n) -n +ln(2*pi)/2
        -1./12/n, so we want some n for which

          (n + 1.5) * ln(n) -n -1./12/n > ln(b)*d +ln(2/pi)/2

        Returns an n satisfying this condition; tries to be tolerably
        close to minimal.
        """
        goal = ln(self.__base) * self.count +offset
        func = lambda n: (n +1.5) * ln(n) -n -1/12./n
        fdash = lambda n: ln(n) +(1.5 +1/12./n)/n
        ans = max(goal, 1)

        # Newton-Raphson, with a tweaked termination condition:
        err, fix = -1, 0
        while err <= 0 or fix >= self.__base:
            ans -= fix
            err = func(ans) - goal
            fix = err / fdash(ans)

        return int(ans) + 1 # Round up

    def __iter__(self):
        return self

    def next(self):
        if self.__initial is not None: # First iteration
            result = self.__initial
            self.__initial = None
        else:
            b = self.__base
            last, numer = self.__error
            numer *= b
            # We can tolerate moderately large errors in later entries.
            while last > 1 and numer > (last if last <= b + 2 else last * b):
                # We have error <= numer / last > 1 in the 1/last! term;
                # turn it into an upper bound on error in the 1/(last-1)! term:
                q, r = divmod(numer, last)
                last -= 1
                numer = q + 1 if r else q

            if last <= 2:
                raise StopIteration # errors undermine current digit.
            self.__error = last, numer

            i, res = len(self.__array) + 1, 0
            while i >= 2:
                u = self.__array[i -2] * b +res
                res, self.__array[i -2] = divmod(u, i)
                i -= 1

            assert 0 <= res < b
            result = self.__digits[res]

        return result

    @classmethod
    def brutal(cls, count, digits="0123456789", decsep='.'):
        """Emit a string representing e.

        Parameters are as for the constructor.  They are used to
        construct an instance which is promptly iterated to
        completion, returning the string it produces.
        """
        return ''.join(cls(count, digits, decsep))

    @classmethod
    def pretty(cls, margin, count, digits="0123456789", decsep='.'):
        """As for brutal, but formatted neatly with a given margin."""
        inst = cls(count, digits, decsep)
        lead = next(inst)
        glue = '\n' + ' ' * len(lead)
        # Number of digits of fractional part per line:
        line = margin - len(lead)
        return lead + ''.join(d if i % line else d +glue
                              for i, d in enumerate(inst, 1))
