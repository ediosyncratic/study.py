"""Solving an equation in one dimension.

See search.Search for 2 dimensions (represented by complex).
"""
from study.cache.property import Cached, lazyprop

class Search (Cached):
    """Searching for roots of a real function.
    """
    def __init__(self, func, guess, goal=abs, stride=1):
        self.goal, self.stride = goal, stride
        self.__func = self.__wrap(func, self.__notice)

        # Initialise best
        self.__best = (None, 1e1024)
        self.func(guess)
        assert self.__best[0] is not None, "Whacky goal function you've got there ..."

    from study.cache.mapping import LazyFunc
    @staticmethod
    def __wrap(func, notice, w=LazyFunc.wrap):
        def check(val, f=func, n=notice):
            return n(val, f(val))
        return w(check)
    del LazyFunc

    def __notice(self, val, ans):
        score = self.goal(ans)
        if score < self.__best[1]: self.__best = val, score
        return ans

    def __known(self):
        return self.__func.known().map(lambda (k, v): (k[0][0], v))

    from study.maths.differentiate import Single
    @lazyprop
    def gradient(self, cls=None, S=Single):
        assert cls is None
        return S(self.__func)
    del Single

    def __getattr__(self, key):
        if key == 'best': return self.__best[0]
        if key == 'score': return self.__best[1]

        raise AttributeError, key

    def func(self, val):
        try: return val.evaluate(self.__func)
        except AttributeError: return self.__func(val)

    def __like(self, (k, v), (h, u)):
        g = self.goal
        k, h = k[0][0], h[0][0]
        return cmp(g(v), g(u)) or cmp(abs(k), abs(h)) or cmp(h, k)

    def __flush(self, keep=10):
        self.__func.flush(keep, self.__like)

    def exact(val): # local function, not method.
        try: zero = val.copy(lambda x: 0)
        except AttributeError: zero = val * 0
        try:
            while val.width > zero: val = val.best
        except AttributeError: pass
        return val

    def Raphson(self, val=None, step=None, hit=exact):
        if val is None: val = self.best
        move = - hit(self.func(val)) / hit(self.gradient(val, step))
        if move: self.stride = move
        ans = hit(val + move)
        return ans, self.goal(self.func(ans))

    def chord(self, val=None, step=None, hit=exact):
        if val is None: val = self.best
        if not step: step = self.stride
        self.func(val), self.func(val + step)

        data = list(self.__known())
        data.sort()
        (x, y), cut = data[0], []
        for (k,v) in data[1:]:
            if v * y < 0: cut.append((min(self.goal(y), self.goal(v)), (x,y), (k,v)))
            x, y = k, v

        if not cut:
            data.sort(lambda (k,v), (x,y), g=self.goal: cmp(g(v), g(y)))
            x, y = data[0]
            data = filter(lambda (k, v), x=x, y=y: k!=x and v!=y, data)
            data.sort(lambda (k,v), (w, z), x=x, y=y: cmp((v-y)/(k-x), (z-y)/(w-x)))
            k, v = data[-1] # we're doomed if that hits IndexError
            cut.append((self.goal(y), (x,y), (k,v)))

        ig, (x,y), (k,v) = min(cut)
        rate = hit(v-y)/hit(k-x)
        if rate:
            move = - hit(y / rate)
            if move: self.stride = move
            ans = hit(x + move)
        else: ans = hit(k + x) * .5

        return ans, self.goal(self.func(ans))

    del exact

    def __broaden(self):
        while 1:
            self.func(self.best + self.stride)
            vals = tuple(self.__known().map(lambda (k,v): (v,k)))
            hi, lo = max(vals), min(vals)
            if hi[0] != lo[0]: break
            gap = hi[1] - lo[1]
            self.stride = 10 * gap

    def rummage(self, tries=42, threshold=-1):
        self.__broaden()
        good = self.score
        while tries > 0:
            tries = tries - 1
            if tries % 3:
                try: self.Raphson()
                except ZeroDivisionError: tries = 3 * (tries / 3)
            else:
                try: self.chord()
                except IndexError: continue

            if good < self.score: # improvement
                if self.score < threshold: # victory
                    self.__flush()
                    return self.__best

                good = self.score

        self.__flush(16)

del Cached, lazyprop
