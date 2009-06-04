"""Analysis of musical theory.

See http://www.chaos.org/~eddy/math/music.xhtml

$Id: music.py,v 1.3 2009-06-04 11:50:00 eddy Exp $
"""
import math
def log2(val, ln=math.log, ln2=math.log(2)): return ln(val) / ln2
del math
from ratio import Rational
from study.cache.property import lazyattr

class Rational (Rational):
    """Extended rationals.
    """

    @lazyattr
    def log(self, ig=None, log2=log2):
        return log2(self.real)

    __ps = (2,)
    def prefer(self, ps):
        self.__ps = ps
        del self.clean, self.complexity

    from primes import factorise
    @lazyattr
    def __factors(self, ig=None, crack=factorise):
        return crack(self.numerator, crack(self.denominator))
    del factorise

    @lazyattr
    def complexity(self, ig=None):
        bok = self.__factors
        ans = sum(bok.keys()) + sum(bok.values()) - len(bok)
        if self.clean: return ans
        return ans * (self.numerator + self.denominator)

    @lazyattr
    def clean(self, ig=None):
        return not filter(lambda k, fs=self.__ps: k not in fs, self.__factors.keys())

from study.snake.sequence import Ordered
class LeastBad (Ordered):
    __upinit = Ordered.__init__
    def __init__(self, target, scale):
        self.__upinit(attr='bad', unique=True)
        self.__scale, self.__target = scale, target

    def prefer(self, ps):
        self.__ps = ps
        all = tuple(self)
        del self[:]
        for it in all: self.append(it)

    __upapp = Ordered.append
    def append(self, ind, value=None):
        if value is None: value = ind
        try: value.prefer(self.__ps)
        except AttributeError: pass
        value.error = value.real - self.__target
        value.bad = value.complexity * abs(value.error)
        if value in self: return False
        if len(self) < 1 or value.bad < self.__scale * self[0].bad:
            self.__upapp(value)
            if value is self[0]:
                # New best entry; may make some of tail redundant.
                cut, i = value.bad * self.__scale, 1
                while i < len(self):
                    if self[i].bad < cut: i += 1
                    else: del self[i:]
            return True
        return False

    insert = append

class ArithList (Ordered):
    __upinit = Ordered.__init__
    def __init__(self, vals, attr=None):
        if attr is None:
            self.__upinit(vals, unique=True)
        else:
            self.__upinit(vals, attr=attr)

    def __mulit(self, other):
        for a in self:
            for b in other:
                yield a * b

    def __mul__(self, other):
        return self.__class__(self.__mulit(other))
    __rmul__ = __mul__

    def __divit(self, other):
        for a in self:
            for b in other:
                yield a / b

    def __div__(self, other):
        return self.__class__(self.__divit(other))

    def __rdivit(self, other):
        for a in self:
            for b in other:
                yield b / a

    def __rdiv__(self, other):
        return self.__class__(self.__rdivit(other))

class MultiIter (object):
    def __iter__(self): return self
    def __init__(self, seqs):
        self.__srcs = seqs
        self.__its = map(iter, seqs)
        try: self.__vals = map(lambda i: i.next(), self.__its[:-1]) + [None]
        except StopIteration: self.next = self.__empty

    def __empty(self): raise StopIteration
    def next(self):
        i = len(self.__srcs)
        while i > 0:
            i -= 1
            try: self.__vals[i] = self.__its[i].next()
            except StopIteration:
                self.__its[i] = iter(self.__srcs[i])
                self.__vals[i] = self.__its[i].next()
            else: return tuple(self.__vals)

        self.next = self.__empty
        raise StopIteration

class Scale (object):
    def __init__(self, count):
        """Initialize a musical scale.

        Single argument, count, is the number of equal intervals into which the
        octave is to be subdivided.\n"""
        assert count > 0, "don't be ridiculous !"
        self.__count = count

    from study.maths.natural import sqrt
    @lazyattr
    def __complex(self, ig=None, root=sqrt):
        return self.__count * root(self.__count) * 2
    del sqrt

    @lazyattr
    def __rough(self, ig=None, Row=LeastBad, Frac=Rational):
        seq, count, big = (), self.__count, self.__complex
        i, step = count + 1, 1. / count
        while i > 0:
            i -= 1
            v = 2**(i * step)
            seq = (Row(v, count),) + seq
            rs = Frac.approach(v)
            for r in rs:
                if abs(r.log * count - i) > .5: continue
                if len(seq[0]) > 0 and r.complexity > big: break
                seq[0].append(r)

        return seq

    def __refine(self, limit,
                 Row=ArithList, Seq=Ordered, Multi=MultiIter, one=Rational(1, 1)):
        assert limit > 0, "don't be ridiculous !"
        count = self.__count
        def mess(seq, bad=limit+1):
            if seq: return seq[0].complexity
            return bad
        i, seq = count + 1, Seq(key=mess)
        while i > 0:
            i -= 1
            good = Row(self.__rough[i].filter(
                    lambda x, h=limit: x.clean and x.complexity <= h),
                       attr='complexity')
            good.index = i
            seq.append(good)

        gaps = map(lambda r: r.index, seq.filter(lambda r: len(r) < 1))
        work, take, neg = len(gaps) > 0, 1, 0
        seq = seq.filter(None)
        while work:
            neg += 1
            if neg > take: take, neg = take + 1, 0
            nice = False
            for rs in Multi(take * (seq,)):
                b, n, vs = take, 0, Row((one,))
                while b > 0:
                    b -= 1
                    r = rs[b]
                    if b < neg: n, vs = n - r.index, vs / r
                    else: n, vs = n + r.index, vs * r
                q, n = divmod(n, count)
                assert 0 <= n < count
                if q: vs /= Row((one * 2**q,))
                tgt = self.__rough[n]
                for v in vs:
                    if abs(v.log * count - n) > .5: continue
                    if v.complexity <= limit: nice = True
                    elif len(tgt) > 0 and v.complexity > self.__complex: continue
                    if tgt.append(v): work = False

            if not nice: break

        if work: return False
        del self.best
        return True

    @lazyattr
    def best(self, ig=None, unlack=(None,)):
        return map(lambda s, u=unlack: (s or u)[0], self.__rough)

    def prefer(self, *primes):
        for it in self.__rough: it.prefer(primes)
        del self.best

    def refine(self, limit):
        return self.__refine(limit)

    def taste(self, ind, num, den, Frac=Rational):
        rat = Frac(num, den)
        if abs(rat.log * self.__count - ind) > .5: return False
        tgt = self.__rough[ind]
        if len(tgt) > 0 and rat.complexity > self.__complex: return False
        return tgt.append(rat)

del LeastBad, ArithList, Ordered, Rational
