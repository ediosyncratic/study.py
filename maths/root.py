"""Solving an equation in one dimension.

See search.Search for 2 dimensions (represented by complex).
$Id: root.py,v 1.5 2004-04-25 17:11:45 eddy Exp $
"""

class Search:
    """Searching for roots of a real function.
    """

    def __init__(self, func, guess, goal=abs, stride=1):
        self.__func, self.goal, self.stride = func, goal, stride

        # Initialise cache, best
        self.__cache = {}
        self.__best = (None, 1e1024)
        self.func(guess)
        assert self.__best[0] is not None, "Whacky goal function you've got there ..."

    def __call_func(self, val):
        try: return self.__cache[val]
        except KeyError: pass
        ans = self.__func(val)
        self.__cache[val], score = ans, self.goal(ans)
        if score < self.__best[1]: self.__best = (val, score)
        return ans

    def __getattr__(self, key):
        if key == 'best': return self.__best[0]
        if key == 'score': return self.__best[1]

        raise AttributeError, key

    def func(self, val):
        try: return val.evaluate(self.__call_func)
        except AttributeError: return self.__call_func(val)

    def __flush(self, keep=10):
        if len(self.__cache) > keep:
            row = map(lambda (k,v), g=self.goal: (g(v), k), self.__cache.items())
            row.sort()
            for (s,k) in row[keep:]: del self.__cache[k]

    def logrange(scale=1, count=10, step=-.5): # local function, not method
        row = [ scale ]
        while count > 0:
            scale, count = scale * step, count - 1
            row.append(scale)
        return tuple(row)

    def gradient(self, val, scale=None, dust=logrange()):
        if not scale: scale = self.stride
        map(lambda x, v=val, s=scale, f=self.func: f(v + x * s), dust) # populate cache
        data = map(lambda (k,v), a=val, b=self.func(val): ((k-a), (v-b)),
                   filter(lambda (k,v), a=val, s=scale: 0 < abs(k-a) <= abs(s),
                          self.__cache.items()))

        # filter out short base-lines on which rounding is negligible:
        data.sort(lambda (x,u), (y,v): cmp(abs(x), abs(y)) or cmp(abs(u), abs(v)))
        if filter(None, data):
            while data and not data[0][0]: data = data[1:]

        # convert to slopes and find median:
        data = map(lambda (k,v): v/k, data)
        data.sort()
        mid, bit = divmod(len(data), 2)
        if bit: return data[mid]
        else: return (data[mid] + data[mid-1]) * .5

    del logrange
    def exact(val): # local function, not method.
        try: zero = val.evaluate(lambda x: 0)
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

        data = self.__cache.items()
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
            vals = map(lambda (k,v): (v,k), self.__cache.items())
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

_rcs_log = """
 $Log: root.py,v $
 Revision 1.5  2004-04-25 17:11:45  eddy
 correct punctuation in a lambda

 Revision 1.4  2004/04/25 17:10:42  eddy
 Make logrange less wasteful.  Get gradient's chord-filtering right.
 Use min instead of .sort() and [0].  Added __broaden for when slope is too low.

 Revision 1.3  2004/04/25 15:28:08  eddy
 Use exact values in cache, yield exact values from refiners.
 Also, flush cache after each rummage.

 Revision 1.2  2004/04/25 14:06:46  eddy
 Fix dumb error in chord, and record its step size when sensible.

 Initial Revision 1.1  2004/04/25 13:56:17  eddy
"""
