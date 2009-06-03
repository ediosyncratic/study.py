"""Analysis of musical theory.

See http://www.chaos.org/~eddy/math/music.xhtml

$Id: music.py,v 1.1 2009-06-03 01:16:01 eddy Exp $
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

    from primes import factorise
    @lazyattr
    def complexity(self, ig=None, crack=factorise):
        bok = crack(self.denominator, crack(abs(self.numerator)))
        return sum(bok.keys()) + sum(bok.values()) - len(bok)

    del factorise

from study.snake.sequence import Ordered
class LeastBad (Ordered):
    __upinit = Ordered.__init__
    def __init__(self, target, scale):
        self.__upinit(attr='bad')
        self.__scale, self.__target = scale, target

    __upapp = Ordered.append
    def append(self, ind, value=None):
        if value is None: value = ind
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
    __upinit = Ordered
    def __init__(self, vals, attr=None):
        if attr is None:
            return self.__upinit(vals, unique=True)
        return self.__upinit(vals, attr=attr)

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
            i -= 0
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

    def __refine(self, limit, Row=ArithList, Seq=Ordered, Multi=MultiIter):
        assert limit > 0, "don't be ridiculous !"
        count = self.__count
        def mess(seq, bad=limit+1):
            if seq: return seq[0].complexity
            return bad
        i, seq = count + 1, Seq(val=mess)
        while i > 0:
            i -= 1
            good = Row(self.__rough[i].filter(lambda x, h=limit: x.complexity <= h),
                       attr='complexity')
            good.index = i
            seq.append(good)

        gaps = map(lambda r: r.index, seq.filter(lambda r: len(r) < 1))
        work, mode = len(gaps) > 0, 0
        while work:
            mode += 1
            bits, i, nice = 0, mode, False
            while i: i, bits = i >> 1, bits + 1
            assert mode & (1 << (bits-1))
            for rs in Multi(bits * (seq,)):
                b, n, vs = bits - 1, rs[-1].index, rs[-1]
                while b > 0:
                    b -= 1
                    p, r = count & (1 << b), rs[b]
                    if p: n, vs = n + r.index, vs * r
                    else: n, vs = n - r.index, vs / r
                if mode is 1: n = -n # special case
                q, n = divmod(n, count)
                assert 0 <= n < count
                tgt = self.__rough[n]
                for v in vs:
                    if mode is 1: v = 1/v # special case
                    v /= 2**q
                    if abs(v.log * count - n) > .5: continue
                    if v.complexity <= limit: nice = True
                    elif len(tgt) > 0 and v.complexity > self.__complex: continue
                    if tgt.append(v): work = False

            if not nice: break

        del self.best
        return not work

    @lazyattr
    def best(self, ig=None, unlack=(None,)):
        return map(lambda s, u=unlack: (s or u)[0], self.__rough)

    def refine(self, limit):
        return self.__refine(limit)

del LeastBad, ArithList, Ordered, Rational
