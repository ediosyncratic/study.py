"""Bits of python having to do with searching.

Note on Newton-Raphson (from HAKMEM note,
http://www.inwap.com/pdp10/hbaker/hakmem/hakmem.html):

 Regarding convergence of Newton's method for quadratic equations:
 Draw the perpendicular bisector of the line connecting the two
 roots. Points on either side converge to the closest root.

 On the line: 
  * they do not converge
  * there is a dense set of points which involve division by zero
  * there is a dense set of points which loop, but roundoff error
    propagates so all loops are unstable
  * being on the line is also unstable (if the roots are imaginary and
    you are on the real axis, you may be doing exact computation of
    the imaginary part (0), hence stay on the line. Example: X^2 + 1 =
    0, X0 = random real floating point number.)

Regarding cubics (same source):
 Solutions to
   F(X) = X**3  - 3 * B * X**2  + C * X + D = 0
 with derivative DF are
   B - K * (F(B)/2 + math.hypot(F(B)/2, DF(B)/2))**(1./3)
     - K * (F(B)/2 - math.hypot(F(B)/2, DF(B)/2))**(1./3)
 where K is each cube root of 1; 1, (-1 +/- sqrt(-3))/2.

Same source has endless more, e.g. on quartics, quintics ...

Note that there is an elegant way to compute square roots in binary.  Any number
exactly representable in binary is a diadic rational hence s = sum(: 2**p
&larr;p |d) for some finite subset, d, of {integers}.  We want the matching r =
sum(: 2**p &larr;p |b) for which r*r differs from s by less than some given 2**q
with q sufficiently negative.  We can construct b as the last B(i) which
contains no integer <= q with B(0) empty and each B(1+i) obtained from B(i) as
follows: let S(i) = s -sum(: 2**p &larr;p |B(i))**2 and select the highest
integer n for which

See study.LICENSE for copyright and license information.
"""

# I'll use complex numbers as a handy model of two dimensions
def dot(x, y): return x.real * y.real + x.imag * y.imag
def cross(x, y): return x.real * y.imag - x.imag * y.real
# aka dot(1j*x, y)
# Note that cross returns a real, though you can think of it as the z component
# of a 3-D vector cross product.
from cmath import exp
radix = exp(2j)
if False:
    # not used, can't remember what I was playing with these for ...
    from cmath import log, exp
    def polar(x):   # needs: from cmath import log
        """Express a vector as radius and orientation."""
        r = abs(x)
        if r > 0: return r, log(x/r).imag
        return r, 0
    def ampliphase(r, theta):       # needs: from cmath import exp
        return r * exp(1j * theta)
    # ampliphase(polar(x)) yields x

def debase(v, a, b):
    """Express a vector in terms of a basis.

    Required arguments:
       v -- a vector
       a, b -- two linearly independent vectors

    Returns reals A, B for which v = A*a + B*b, raising a ZeroDivisionError if A
    and B are parallel (even if v is also parallel to them). """

    d = cross(a,b)
    return cross(v,b) / d, cross(a,v) / d

def triangulate(f, v, s):
    """Estimates a root of a function by linear interpolation.

    Required arguments:
      f -- a function taking one argument.
      v -- a valid input to f
      s -- an estimate at the difference between v and a root of f

    Evaluates f at v, v+s and v + 1j*s, then uses triangulation from these three
    to estimate a position at which f might plausibly be zero: returns this
    position. """

    F, E, G = f(v), f(v + s), f(v + s * 1j)
    e, g = debase(F, E-F, G-F)
    return v - s * (e + 1j * g)

# Now for Newton-Raphson refinement ...
def logrange(a, b=None, c=None):
    # if given c but not b, use b = 1.
    if   c: start, stop, step = a, abs(b or 1), c
    elif b: start, stop, step = a, abs(b), .1
    else: start, stop, step = 1, abs(a), .1

    result, direction = [], abs(step) - 1
    while (stop - abs(start)) * direction > 0:
        result.append(start)
        start = start * step

    return result

def median(seq, fn=None):
    """Find the median of a sequence.

    Required first argument, seq, is an iterable of values.  Optional second
    argument, fn, is a comparison function or None (the default) to use the
    built-in comparison, cmp().  Doesn't go to all the trouble of sorting (a
    copy of) the sequence, just moves the middle element or two to the
    middle.\n"""
    row = list(seq) # always gets a copy; and iterate only once
    if not row: raise IndexError, 'empty sequence has no median'

    # Adapted from Wirth's "Data Structures + Algorithms = Programs"
    def split(L, R, m, s=row, cmp=fn or cmp):
        """Move the m-th highest entry in row to index m.

        This actually works for any m; it requires that we know the m-th
        highest is actually no earlier than index L and no later than
        index R, with m lying between L and R.  In O(R-L) steps, it
        shuffles the m-th entry to position s[m], then returns.\n"""

        while L < R:
            x, i, j = s[m], L, R
            while True:
                while cmp(s[i], x) < 0: i += 1
                while cmp(s[j], x) > 0: j -= 1

                if i > j: break
                elif i < j:
                    s[i], s[j] = s[j], s[i]
                i += 1
                j -= 1
                if i > j: break

            if j < m: L = i
            if m < i: R = j

    mid, bit = divmod(len(row), 2)
    split(0, len(row) - 1, mid)
    if bit: return row[mid]
    split(0, mid, mid-1)
    q, r = divmod(row[mid] + row[mid-1], 2)
    if r: return (row[mid] + row[mid-1]) * .5
    return q

def gradients(fn, arg, *deltas):
    if len(deltas) == 1 and isinstance(deltas[0], (tuple, list)):
        deltas = deltas[0]

    result = []
    for e in deltas:
        try: r = (fn(arg + e) - fn(arg - e)) * .5 / e
        except (OverflowError, ZeroDivisionError, ValueError): pass
        else: result.append(r)

    # However, if the last few deltas are so small df is lost in rounding, throw
    # them away !
    if any(result):	# i.e. there are some non-zeros in the list ...
        while result and result[-1] == 0:
            result = result[:-1]

    return result

def gradient(fn, arg):
    """Brute force differentiation."""

    deltas = logrange(1, 1e-16, -.1)
    # i.e. [ 1, -.1, .01, -.001, ... ]
    grads = gradients(fn, arg, deltas)
    try: # does fn cope with complex values ?
        more = gradients(fn, arg, [1j * x for x in deltas])
        grads = [x + 0j for x in grads] + more # add 0j to coerce complex
        # return meadian of real parts + 1j * median of imaginary parts:
        return median(x.real for x in grads) \
        + 1j * median(x.imag for x in grads)

    except (AttributeError, TypeError): return median(grads)

def Raphson(f, v):
    """Implements Newton-Raphson refinement.

    Arguments:

      f -- a function taking one argument

      v -- a valid input to this function

    Returns a value at which f seems likely to be close to 0, computed according
    to the Newton-Raphson method - compute error and gradient, divide error by
    gradient, perturb from v by this ratio. """

    try: return v - f(v) / gradient(f, v)
    except ZeroDivisionError:
        raise ValueError('stationary point', v)

class Search:
    """Administration of successive searching for values of a complex function.

    Initialisation data:

      func -- function being explored.  In typical use, we'll be looking for
      inputs to this function which give zero as output: such inputs are called
      roots.  However, more general goals can be realised by judicious choice
      of goal (see below).

      start -- an initial suggested value.  In typical use, this'll be some
      input to func which you suspect will be near a root.

      goal -- default: abs().  A function from complex (or, at least, func's
      outputs) to reals.  This is used to test whether one output from func is
      `better' than another: F is `better' than G iff goal(F) < goal(G).  The
      default, abs(), is suitable for use when looking for roots of func().

      stride -- default: 1.  A displacement in the complex plane, neither large
      nor very small by comparison to the anticipated error in start.

    Public data:

      goal -- initialisation datum recorded for later use.

      best -- a pair, (v, f) with f=func(v) and goal(f) no greater than any
      other value of goal(func(input)) yet seen.  Initialised using v=start.

      score -- the value of goal(best[1]), for convenience.

      next -- the input the Search object thinks is worth trying next.

      stride -- a step-size the Search object considers worth using: this is
      initialised from the eponymous initialisation datum.  The Search object
      will sometimes halve this and sometimes set it to the difference between
      next and best[0].

    Public methods:

      goal() -- this public datum behaves as a method.

      func() -- packages the eponymous initialisation datum (recorded
      privately): provides for the `yet seen' clause in .best's specification.

      rummage() -- applies refinements (see Raphson and triangulate, above),
      using a mixture of triangulation and Newton-Raphson, optionally stopping
      early if .score drops below some threshold.

      reset() -- forgets all old values of func(), optionally supplying new
      values for any or all of the initialisation data.

    To look for solutions to func(z) = F for a succession of values of F, it
    would be sensible to use a search object with the given func and set its
    goal to lambda x,f=F: abs(x-f).  Once a sufficiently refine()d z is found, a
    reset() with a similar goal for the new F value will begin the search; this
    should work particularly well when each z is expected to be near to the
    previous one (e.g. F is only slightly changed).  It may make sense to revise
    stride at each reset(), also. """

    # it might be nice to add some optional verbosity !

    def __init__(self, func, start, goal=abs, stride=1.):
        self.goal, self.__func = goal, func
        self.stride, self.next = stride, start + stride
        self.__initscore(start)

    def reset(self, func=None, start=None, goal=None, stride=None):
        if func: self.__func = func
        if start: self.next = start
        if goal: self.goal = goal
        if stride: self.stride = stride
        self.__initscore(self.__best[0])

    def __initscore(self, start):
        # Contrast with .func(), below.
        F = self.__func(start)
        self.__best = start, F
        self.__score = self.goal(F)

    def __getattr__(self, key):
        if key == 'best': return self.__best
        if key == 'score': return self.__score

        raise AttributeError, key

    def func(self, v):
        # Evaluate requested value
        F = self.__func(v)

        # pay attention to what's passing through our hands
        score = self.goal(F)
        if score < self.__score:
            # v takes the title for best root seen thus far
            self.__best = v, F
            self.__score = score

        # Return requested value
        return F

    def Newton(self):
        before, best = self.next, self.__score
        self.next = Raphson(self.func, self.next)
        if self.__score < best: return self.__best[0] - self.next
        return self.next - before

    def triangulate(self, step):
        before, best = self.next, self.__score
        self.next = triangulate(self.func, self.next, step)
        new = self.next - before

        # Begin weird heuristic munging for next time's step:
        rat = new / step
        art = abs(rat)
        if rat.imag > rat.real: turn = 1j+1
        else: turn = 1j-1

        if self.__score < best:
            # we've seen an improvement
            if art < .5: return new
            if art < 2: return new / turn

            if rat.real + rat.imag > 0:
                return new / (art - 1)

        return turn * new / art

    def rummage(self, tries=42, threshold=-1):
        """Does a search for a better answer.

        Arguments are optional:

          tries -- 42 by default: the number of attempts to make at refine().
          Uses a mixture of Newton-Raphson and triangulation refinements.

          threshold -- -1: if a refine()ment produces an answer whose goal()
          value is < threshold, rummage() will return the new best estimate and
          its answer, as a tuple, even if it hasn't yet done refine() tries
          times.  The default renders this impossible if goal() is never
          negative; however, the default also makes ZeroDivisionError quite
          likely to arise (in which case self.best is probably quite good).

        Returns the new best value if it beats the threshold, otherwise
        None. """

        # Admin variables
        best, step = self.__score, self.stride

        while tries > 0:
            tries = tries - 1
            try:
                if tries % 3:
                    if tries % 2: step = -1j * step
                    try: step = self.triangulate(step)
                    except (ZeroDivisionError, OverflowError):
                        step = self.Newton()
                else:
                    step = self.Newton()
            except (ZeroDivisionError, OverflowError, ValueError):
                step = radix * step
                continue

            gain = best - self.__score
            if gain > 0:
                if self.__score < threshold:
                    return self.best

                # Record details of this improvement:
                best = self.__score

# Passing thought: given a search for sought in text, we can do the following:
def text_search(sought, text, skip = 0):
    total = len(text)
    length = len(sought)
    if total < length: return -1

    end = 0     # A match, if found, will be text[end-length:end].
    last = {}   # lookup table to say how far to advance given mis-match.

    # last will hold, indexed by character, how far to the right of a
    # character is the first position at which a match of sought can end,
    # given that the match does not end before the given character.

    # First: find least possible value of end (while initialising last)
    for i in range(length):
        here = sought[i]
        last[here] = length - i
        # Loop invariant:
        # a match of sought[:i] cannot arise earlier than text[end-i:end]
        while text[end] != here:
            end = 1 + end
            if end >= total: return -1

    # Hereafter, advance in search of a match
    while end <= total:
        match = (0==0)
        for i in range(length):
            if text[end-1-i] != sought[-1-i]:
                fro = end - i + length
                if last.has_key(text[end-i]):
                    fro = end - i + last[text[end-i]]
                if fro > end:
                    end = fro
                else: match = (0!=0)
        else:
            if match:
                if skip > 0: skip = skip - 1
                else: return end - length
            end = end + 1
    else: return -1
