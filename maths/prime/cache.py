"""Cache files, directories and their managment.

A factor cache object knows the range of integer values it spans.  A prime cache
object also knows the range of prime indices it spans.  Maybe a factor cache
object shall, too.

Caches:

 * Built on top of the study.cache.whole infrastructure, using types 'F'
   (factor) and 'P' (prime) in its data-type naming scheme.  Internal data
   indicate format and whether complete (rather than a partial sieve).  Each
   type of file may also know, when complete, the range of prime indices that
   fall in its interval.

 * Cache files describing incompletely sieved chunks of the naturals may also
   exist; these are apt (but not sure) to know their index start-point and
   perhaps even an upper bound on their number of primes.

 * Custom compression of the data in cache files is managed by the client.
   However the cache shall base64-encode long strings whose repr would thereby
   be shortened.  (Non-printable characters (and a few others) get expanded by
   repr expressing them as escape sequences; this can easilly expand a string by
   more than the factor of 4/3 incurred by base 64.)

 * Keywords passed in to _save_ methods with names ending 'bz2' or 'b64' shall,
   if the value is a string, when read back by _load_, have had these suffixes
   stripped and their values bz2.decompress()ed or base64-decoded (both if the
   name ended 'bz2b64'); generally, it is expected that any bz2.compress()ion
   shall be done by the application and indicated in this way; whereas
   base64-encoding is done by _save_ when suitable.

 * The actual range of naturals described by any cache entity always starts and
   ends at a multiple of the modulus of the cache's extended octet type; so the
   numeric parts of names, and hence the .span attributes of objects describing
   files and directories, are all reduced by an implicit factor of this modulus.
   The .interval attribute provides .span times this modulus, for the actual
   range of naturals described.

 * Some cache files may record sporadics, in so far as we've discovered any;
   that is, a list of primes beyond the end of the completed range and, in a
   factor cache, a mapping from integers to one proper factor each.  The latter
   is not worth recording, however, if the factor is one of the primes used for
   the octet type of this cache.  The client, when traversing its caches in
   search of data, should check the object it gets back and be ready to continue
   its search if the first match it finds is of this type.

 * The cache root __init__.py may eventually record a cache format version, for
   future-prooofing purposes !  However, until the need for that is realised, we
   can leave it out and have it default to 0 if not found :-)

See study.LICENSE for copyright and license information.
"""

from study.cache import whole
from study.snake.regular import Interval
from study.cache.property import lazyattr, lazyprop
from base64 import standard_b64encode, standard_b64decode
from bz2 import decompress

class Node (whole.Node):
    @lazyattr
    def indices(self, ig=None):
        """Range of prime indices"""
        try: ind = self.__indices
        except AttributeError:
            if not self.prime: raise
            self.content # evaluate to force self.load()ing
            ind = self.__indices # raises AttributeError if really not available

        if self.parent is not None:
            ind += self.parent.indices.start

        return ind

    # FIXME: [PQ] -> P, [FG] -> F: distinguish internally instead.

    @lazyprop
    def prime(self, ig=None):
        if self.parent is None: return len(self.primes) > 0
        return 'P' in self.types or 'Q' in self.types
    @lazyprop
    def factor(self, ig=None):
        if self.parent is None: return len(self.factors) > 0
        return 'F' in self.types or 'G' in self.types

    @lazyprop
    def interval(self, ig=None, Range=Interval):
        lo, sz, mo = self.span.start, len(self.span), self.root.octet.modulus
        lo *= mo
        if sz is not None: sz *= mo
        return Range(lo, sz)

    __upload = whole.Node._load_
    def _load_(self, bok=None,
               Range=Interval, dec=standard_b64decode, unz=decompress):
        bok = self.__upload(bok)
        try: gap = bok.pop('indices') # twople format in file
        except KeyError: pass
        else: self.__indices = Range(gap[0], ga[1])

        for (k, v) in bok.items():
            if k[-3:] == 'b64' and isinstance(v, basestring):
                del bok[k]
                # Helpfully, standard_b64decode knows to ignore '\n'
                k, v = k[:-3], dec(v)
                bok[k] = v

            if k[-3:] == 'bz2' and isinstance(v, basestring):
                del bok[k]
                bok[k[:-3]] = unz(v)

        return bok

del lazyattr, lazyprop, decompress

class CacheSubNode (Node, whole.CacheSubNode):
    __upinit = whole.CacheSubNode.__init__
    def __init__(self, parent, types, start, reach, sign=None, replaces=None):
        self.__upinit(parent, types, start, reach, sign, replaces)
        if replaces is not None:
            pass # TODO: sort out other attributes from replaces

import re

class WriteNode (Node, whole.WriteNode):
    __upsave = whole.WriteNode._save_

    def repgen(rep):
        # Lexical scoping lets myrepr call itself :-)
        def myrepr(value, repr=rep):
            # Represent sequences as normal but: use ',\n' instead of ', ' to
            # join entry representations; and apply our custom representation
            # recursively to sequence entries.

            if isinstance(value, tuple):
                return '(\n' + ',\n'.join(map(myrepr, value)) + '\n)'
            if isinstance(value, list):
                return '[\n' + ',\n'.join(map(myrepr, value)) + '\n]'

            if isinstance(v, basestring) and len(v) > 80 and '\n' in v:
                txt = repr(v).replace('\\n', '\n')
                if txt[0] == 'u': head, txt = txt[0], txt[1:]
                else: head = ''
                assert text[1] != txt[0] == txt[-1]
                assert txt[-1] != txt[-2] or txt[-3] == '\\'
                txt, tail = txt[1:-1], 3 * txt[-1] + '\n'
                head += tail
                return head + txt + tail

            return repr(v)
        return myrepr

    def _save_(self, formatter=None, genrep=repgen,
               cut=re.compile('.{,80}').findall,
               b64enc=standard_b64encode, **what):
        """Saves data to file.

        See study.cache.whole.WriteNode._save_ for general documentation.  This
        derived class over-rides the handling of keyword 'indices', substituting
        self's index range relative to that of parent, encoded as a simple
        (start, span) tuple.  It also over-rides the formatting of all values of
        the following kinds:
          * lists or tuples of length > 20: newline is used in place of space,
            after the comma, in the separator between entries;
          * strings (unicode or not) over 80 bytes in length, that include at
            least one newline: doc-string format is used.
        """

        try:
            gap = self.indices
            if self.parent is None: off = 0
            else: off = self.parent.indices.start
        except AttributeError: pass
        else: what['indices'] = (gap.start - off, len(gap))

        def reformat(k, v, repr=repr, given=formatter,
                     repgen=genrep, e=b64enc, chop=cut):

            if isinstance(v, basestring) and len(v) > 40:
                r = repr(v)
                if (len(r) - 2) * 3 > len(v) * 4: # i.e. len(r) > len(repr(e(v))
                    b64 = e(v)
                    lines = chop(b64)
                    if len(repr(b64)) + len(lines) - 1 < len(r):
                        k, v = k + 'b64', '\n'.join(lines)
                    del b64, lines # apt to be large objects in memory !
                del r # likewise.

            repr = repgen(repr)
            if given is None: return '%s = %s\n' % (k, repr(v))
            return given(k, v, repr)

        self.__upsave(reformat, **what)

    del repgen

del standard_b64encode, standard_b64decode, re

class CacheFile (CacheSubNode, whole.CacheFile):
    __load = Node._load_
    def _load_(self, bok=None):
        bok = self.__load(bok)
        assert not self.prime  or filter(lambda k: k[:5] == 'prime',  bok.keys())
        assert not self.factor or filter(lambda k: k[:6] == 'factor', bok.keys())
        return bok

class WriteFile (WriteNode, CacheFile, whole.WriteFile):
    __save = WriteNode._save_
    def _save_(self, formatter, **what):
        assert not self.prime  or filter(lambda k: k[:5] == 'prime',  what.keys())
        assert not self.factor or filter(lambda k: k[:6] == 'factor', what.keys())
        return self.__save(formatter, **what)


weaklisting = whole.CacheDir.weaklisting
class CacheDir (Node, whole.CacheDir):
    @staticmethod
    def _child_class_(isfile, mode):
        # possibly complicate further for mode
        if isfile: return CacheFile
        return CacheSubDir

    __upontidy = whole.CacheDir._ontidy_
    def _ontidy_(self):
        self.__upontidy()
        del self.primes, self.factors

    __upgap = whole.CacheDir._gap_
    def _gap_(self, before, after, limit,
              Range=Interval, Stub=whole.CacheFile):
        if before is None: before = Stub('', self.root, '', +1, -1, 1)
        gap = self.__upgap(before, after, limit)

        # Find data on range of indices:
        try: start = before.indices.stop
        except AttributeError: start = None
        try: stop = after.indices.start
        except AttributeError: stop = None
        try: ind = limit.indices
        except AttributeError: pass
        else:
            assert ind.step == 1 # positive
            if stop is None or stop > ind: stop = ind.stop
            if start is None or start < ind: start = ind.start

        if start is None: start = 0
        if stop is None: gap.indices = Range(start, None)
        else: gap.indices = Range(start, stop - start)

        return gap

    @weaklisting
    def primes(self, mode): return 'P' in mode or 'Q' in mode
    @weaklisting
    def factors(self, mode): return 'F' in mode or 'G' in mode

    # optionally extend _load_ some more

del weaklisting

class WriteDir (WriteNode, CacheDir, whole.WriteDir):
    # optionally extend _save_ some more

    @staticmethod
    def _child_class_(isfile, mode):
        # possibly complicate further for mode
        if isfile: return WriteFile
        return WriteSubDir

class CacheSubDir (CacheDir, CacheSubNode, whole.CacheSubDir): pass
class WriteSubDir (WriteDir, CacheSubDir, whole.WriteSubDir): pass

class CacheRoot (CacheDir, whole.CacheRoot):
    span = Interval(0, None)

    def get_factors(self, value, gap=None):
        """Find a chunk or gap enclosing a designated integer.

        Arguments:
          value -- integer to be found in cache (required)
          gap -- optional Gap around value else (default) None

        Required first argument, value, is a natural number.  The return shall
        describe an interval in which that natural falls; if the return is obj,
        then obj.start <= value < obj.stop; if this cache has least proper
        factor data for the given natural, obj is an instance of CacheFile;
        otherwise, it is an instance of Gap. (recognizable as such by its lack
        of a .content).  In the latter case, if the optional second argument gap
        is supplied, the returned Gap is its intersection with the widest range
        of naturals about value in which self has no data.  When gap is None, it
        is treated as if it were Gap(self, 0, None).

        The return has, when available, a .indices attribute describing the
        range of prime indices it spans; however, the returned index interval
        may (e.g. in a factor-only cache, or when describing a gap) be wider
        than the actual interval described by the return, if this is the best
        available information.

        See also: get_primes().\n"""

        if gap is None: gap = self._gap_(None, None, None)
        if not self.factor: return gap
        return self.locate(value / self.octet.modulus, 'span', gap, 'factors')

    def get_primes(self, value, gap=None, index=None):
        """Find a cache file or gap enclosing a designated integer.

        Like get_factors (q.v.) except that it returns data on primes, and
        allows value to be None, in which case its (otherwise ignored and
        optional) third argument, index, must be supplied: in this case, index
        must be a natural and the effect is as if primes[index] had been passed
        as value - no-one can be expected to know primes[index] without calling
        this function to get the cache object that tells us !\n"""

        if gap is None: gap = self._gap_(None, None, None)
        if not self.prime: return gap
        if value is None: return self.locate(index, 'indices', gap, 'primes')
        else: return self.locate(value / self.octet.modulus, 'span', gap, 'primes')

    __load = Node._load_
    from octet import OctetType
    def _load_(self, bok=None, mode=OctetType, Range=Interval):
        bok = self.__load(bok)
        self.span = Range(0, bok.pop('top'))
        self.octet = mode(bok.pop('octet'))
        return bok
    del OctetType

class WriteRoot (WriteDir, whole.WriteRoot):
    __save = WriteDir._save_
    def _save_(self, formatter, **what):
        if self.span.stop is None:
            what['top'] = max(filter(None, map(lambda x: x.span.stop,
                                               self.listing)))
        else: what['top'] = self.span.stop
        what['octet'] = self.octet.primes
        return self.__save(formatter, **what)


del Node, CacheSubNode, WriteNode, Interval, whole
from study.snake.sequence import Ordered
import os

class oldCache (object):
    """Iterator over an old-style cache.

    Earlier versions of this study package included a cruder study.maths.primes
    module, without factor information, with its own cache directory; this class
    makes it possible to digest one of those into a form which can be saved
    (with the help of octet.Chunker) into a new-style prime cache.  An instance
    only provides prime data; you'll still need to sieve using the new system in
    order to build up factor data, but this makes as much use of old data as
    possible.

    An instance is an iterator over primes found in the old cache, possibly
    yielding None after it has completed the contiguous range of the old cache;
    thereafter, it may leave out some primes.  The range of primes for which
    data is yielded may be limited by supplying start and stop parameters to the
    constructor (q.v.).\n"""

    def __iter__(self): return self
    def __init__(self, cdir, start=0, stop=None, os=os, List=Ordered):
        """Digest an old-style prime-only cache directory.

        Required arguments:
          cdir -- path name of an old-style cache directory

        Optional arguments:
          start -- inclusive lower bound on naturals (default: 0)
          stop -- exclusive upper bound on naturals or (default) None, meaning
                  unbounded\n"""

        row, self.__sparse = [], List(unique=True)
        for name in os.listdir(cdir):
            if name[:1] == 'c' and name[-3:] == '.py' and '-' in name[1:-3]:
                try: lo, hi = map(int, name[1:-3].split('-'))
                except ValueError:
                    print 'ignored malformed name', name, 'in old cache', cdir
                else: row.append((lo, hi, os.path.join(cdir, name)))
        row.sort()

        self.__src = self.__primes(row, start, stop)

    def next(self): return self.__src.next()

    def __file(self, lo, hi, fd):
        quote, num = '', 0
        while True:
            if quote:
                off, bs = line.find(quote), 0
                while off > bs:
                    if line[off-1-bs] == '\\': bs += 1
                    elif bs % 2: off, bs = line.find(quote, off+1), 0
                    else: break

                if off < 0:
                    line = fd.readline()
                    num += 1
                    continue
                else:
                    off += len(quote)
                    line, quote = line[off:], ''
                    if not line.strip():
                        line = fd.readline()
                        num += 1
            else:
                line = fd.readline()
                num += 1

            if line.strip() == 'block = [':
                line = fd.readline()
                num += 1
                break
            elif line[:9] == 'sparse = ':
                self.__sparse += eval(line[8:])
            elif line[:5] == 'to = ': assert hi == int(line[4:]), (line, num)
            elif line[:5] == 'at = ': assert lo == int(line[4:]), (line, num)
            elif line[:3] in ('"""', "'''"):
                quote, line = line[:3], line[3:]
            elif line[:1] in ('"', "'"):
                quote, line = line[:1], line[1:]
            else:
                assert False, ('Unexpected line in old cache file', line, num)
                off, bs = line.find('"'), 0
                if off < 0 or "'" in line[:off]: off = line.find("'")
                while off > bs:
                    if line[off-1-bs] == '\\': bs += 1
                    elif bs % 2:
                        old = off+1
                        off, bs = line.find('"', old), 0
                        if off < 0 or "'" in line[old:off]:
                            off = line.find("'", old)
                    else: break

                if off >= 0:
                    # ignore line[:off]
                    if line[off] == line[off+1] == line[off+2]:
                        quote, line = line[off:off+3], line[off+3:]
                    else: quote, line = line[off], line[off+1:]
                # else simply ignore line

        while line.strip() != ']':
            if line:
                yield eval(line) # it should eval to a tuple
                line = fd.readline()
                num += 1
            else:
                assert False, ('Premature end of old cache file', num)
                break

        while line:
            line = fd.readline()
            num += 1
            assert not line.strip(), ('Stray content at end of old cache file',
                                      line, num)
        raise StopIteration

    def __primes(self, row, start, stop):
        it = 0
        for (lo, hi, name) in row:
            fd = open(name)
            # print "Processing", name
            try:
                for seq in self.__file(lo, hi, fd):
                    if seq[-1] < start: pass
                    elif stop is None or seq[-1] < stop:
                        for it in seq: yield it
                    else:
                        for it in seq:
                            if it < start: pass
                            elif stop is None or it < stop: yield it
                            else: raise StopIteration # discarding __sparse

            finally: fd.close()
            # print "Finished", name

        # Now deal with sparse.  First trim it:
        if stop is not None:
            try: ind = self.__sparse.index(stop)
            except ValueError, what: ind = what.args[0]
            if what >= 0: del self.__sparse[what:]

        if it < start: it = start # we never yielded anything !
        # Find how much, if any, of self.__sparse belongs after it:
        try: ind = self.__sparse.index(it)
        except ValueError, what:
            ind = what.args[0] # where Ordered.append would insert it
        else:
            if ind >= 0: ind += 1 # don't repeat it

        if ind >= 0: # -1 means "after last entry"
            if ind > 0: del self.__sparse[:ind]
            if self.__sparse:
                yield None
                for it in self.__sparse: yield it

        raise StopIteration

del os, Ordered
