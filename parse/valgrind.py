"""Parsing valgrind memcheck logs.

When run on a multi-threaded program, output from different threads
gets interleaved, which can make the logs hard to read. In any case,
the logs are not optimised for the human reader.  The MemCheck class
here provides a way to de-interleave the multi-threaded output and
digest the logs (for each thread), identifying duplicates tying the
different kinds of report together. Start by running

  valgrind --trace-children=yes --log-file=vgr.%p.%n.log yourProg

in a directory where the resulting vgr.{pid}.{seq}.log files are easy
to see.  Once that's finished,

>>> from study.parse.valgrind import MemCheck
>>> from glob import glob
>>> vgr = MemCheck()
>>> reps = vgr.ingest(*glob("vgr.*.*.log"))

will give you a tuple of Report objects that you can explore.

See study.LICENSE for copyright and license information.
"""
import re

class ParseError (SyntaxError):
    """Error during parsing of a valgrind memcheck log-file."""
    __upinit = SyntaxError.__init__
    def __init__(self, message, *details):
        while len(details) < 2: details += (None,)
        self.__upinit(message, *details)

def readint(text): return int(''.join(text.split(',')))

class Source (object):
    def __init__(self, sfile, line=None):
        self.source = sfile
        if line is not None: self.line = line
        self.frames = set() # TODO: need a weakset()

    def __hash__(self):
        ans = hash(self.source)
        try: line = self.line
        except AttributeError: return ans
        return ans ^ hash(line)

    def __cmp__(self, other):
        return (cmp(self.source, other.source) or
                cmp(getattr(self, 'line', None), getattr(other, 'line', None)))

    __known = {}
    @classmethod
    def get(cls, sfile, line=None):
        try: ans = cls.__known[sfile, line]
        except KeyError:
            ans = cls.__known[sfile, line] = cls(sfile, line)

        return ans

class Frame (object):
    @staticmethod
    def __parse(text,
                borked=re.compile(r'\bat 0x([0-9a-fA-F]+): \?\?\?').search,
                locat=re.compile(r'\b(by|at)\s+0x([0-9a-fA-F]+):\s+((?:in )?\S+)\s*').search,
                inlib=re.compile(r'\s*\(in ([^)]*)\)').match,
                sause=re.compile(r'\s*\(([^)]*):(\d+)\)').match):
        place = locat(text)
        if place:
            leaf = place.group(1) == 'at'
            addr, func = place.group(2, 3)
            if func == '???': func = None
            tail = text[place.end():].strip()
        else:
            place = borked(text)
            if not place: raise ParseError('No (at|by) stack-frame data', text)
            return True, place.group(1), None, None

        src = inlib(tail)
        if src is None:
            src = sause(tail)
            if src is not None:
                src = Source.get(src.group(1), int(src.group(2)))
        else:
            src = Source.get(src.group(1))

        return leaf, addr, func, src

    def __init__(self, text, leaf, addr, func, source):
        self.text, self.leaf = text, leaf
        self.addr, self.func, self.source = addr, func, source
        self.stacks = set()
        if source is not None: source.frames.add(self)

    def __hash__(self):
        return hash(self.leaf) ^ hash(self.addr) ^ hash(self.func) ^ hash(self.source)

    def __cmp__(self, other):
        return (cmp(self.addr, other.addr) or
                cmp(self.func, other.func) or
                cmp(self.source, other.source) or
                cmp(self.leaf, other.leaf))

    def __repr__(self): return self.text

    # TODO: frames from different binaries (reported in different log files) may
    # have different addresses for the same file and line, that we would ideally
    # identify; but different addresses within a given binary should not be
    # conflated, even if they come from the same line.  Not easy to resolve
    # this: probably needs an iteration over Source.__known()'s values whose
    # .frames has more than two entries; requires interactive decision about
    # which ones to conflate.  For now, ignoring command passed to .get(), but
    # it's the hook by which I hope to be able to make this possible.

    __known = {}
    @classmethod
    def get(cls, text, command): # ignoring command; see TODO above.
        key = cls.__parse(text)
        try: ans = cls.__known[key]
        except KeyError: ans = cls.__known[key] = cls(text, *key)
        return ans

class Stack (object):
    def __init__(self, *frames):
        assert frames and frames[0].leaf and all(not x.leaf for x in frames[1:]), frames
        self.__frames = frames
        for f in frames: f.stacks.add(self)
        self.issues = set()

    def __repr__(self): return '\n'.join([repr(f) for f in self.__frames])
    def __hash__(self):
        return reduce(lambda x, y: x ^ hash(y), self.__frames, id(self))
    def __cmp__(self, other): return cmp(self.__frames, other.__frames)
    def __contains__(self, frame): return frame in self.__frames
    def __getitem__(self, ind): return self.__frames[ind]

    __known = {}
    @classmethod
    def get(cls, frames):
        frames = tuple(frames)
        try: ans = cls.__known[frames]
        except KeyError: ans = cls.__known[frames] = cls(*frames)
        return ans

class Issue (object):
    def __init__(self, stack, grumble):
        self.stack, self.problem = stack, grumble
        if stack is not None:
            stack.issues.add(self)
        self.fixed = False

    def __repr__(self): return self.problem

    # Derived classes may want to do more than this:
    def clear(self): self.fixed = True

    __subs = []
    @classmethod
    def register(cls, sub):
        assert issubclass(sub, cls)
        for key in sub.matchers():
            cls.__subs.insert(0, (sub, key))
    # prepend, so that later (i.e. more specific) classes can easily take precedence

    __known = {}
    @classmethod
    def get(cls, text, stack, address):
        for sub, key in cls.__subs:
            if key(text):
                ans = sub.get(text, stack, address)
                if isinstance(ans, tuple): return ans
                return ans, 0

        #assert address is None, (text, stack, address)
        return cls._cache_(cls.__known, stack, text), 0

    @classmethod
    def _cache_(cls, bok, stack, *rest):
        try: ans = bok[stack]
        except KeyError: ans = bok[stack] = cls(stack, *rest)
        return ans

class UMR (Issue):
    """Uninitialised memory read."""
    @classmethod
    def matchers(cls):
        yield lambda x: x.startswith('Invalid read of size')
        yield lambda x: 0 <= x.find('uninitialized value')
        yield re.compile(r'Syscall param\b.*\bpoints to uninitialised byte\(s\)').match

    __upclear = Issue.clear
    def clear(self):
        self.__upclear()
        MemoryChunk.disuse(self)

    __known = {}
    @classmethod
    def get(cls, text, stack, address):
        ans = cls._cache_(cls.__known, stack, text)
        if address is not None: address.usedby(ans)
        return ans

Issue.register(UMR)

class FDM (Issue):
    "Mismatched free / delete / delete[]"
    @classmethod
    def matchers(cls):
        yield lambda x, d=cls.__doc__: x.strip() == d

    __known = {}
    @classmethod
    def get(cls, text, stack, address):
        pass

Issue.register(FDM)

class MemoryChunk (Issue):
    @classmethod
    def matchers(cls):
        yield lambda x: 0 <= x.find('bytes inside a block of size')

    @staticmethod
    def __parse(text, asint=readint, match = re.compile(
            r"Address 0x([0-9a-fA-F]+) is ([0-9,]+) "
            r"bytes inside a block of size ([0-9,]+) (\w+)'d").match):
        it = match(text)
        if not it: raise ParseError('Malformed block description line', text)
        addr, func = it.group(1, 4)
        offset, size = [asint(x) for x in it.group(2, 3)]
        return func, size, offset, addr

    __upinit = Issue.__init__
    def __init__(self, stack, text, func):
        self.__upinit(stack, text)
        self.author, self.__eg, self.__users = func, set(), set()

    def example(self, *what): self.__eg.add(what)
    @property
    def samples(self): return tuple(self.__eg)

    def usedby(self, other): self.__users.add(other)
    @property
    def users(self): return tuple(self.__users)

    @classmethod
    def disuse(cls, umr):
        """Notice that a given abuse has been resolved.

        If this is the last in some chunk's list of known abusers, that chunk
        can be considered fixed (no-one is abusing it, after all).\n"""

        for it in cls._active_():
            if umr in it.__users:
                if all(x.fixed for x in it.__users):
                    it.clear()

    @classmethod
    def _active_(cls):
        for sub in set(cls.__subs.itervalues()):
            for it in sub._active_(): yield it
        for it in cls.__known.values(): yield it

    __subs = {}
    @classmethod
    def register(cls, sub, func): cls.__subs[func] = sub

    __known = {}
    @classmethod
    def get(cls, text, stack, address=None):
        func, size, off, addr = cls.__parse(text)
        try: sub = cls.__subs[func]
        except KeyError:
            print 'Encountered unexpected MemoryChunk function:', func
            ans = cls._cache_(cls.__known, stack, text, func)
        else: ans = sub.get(text, stack)
        ans.example(addr, off, size)
        return ans

Issue.register(MemoryChunk)

class FMA (MemoryChunk):
    "Free memory access"
    @classmethod
    def _active_(cls):
        for it in cls.__known.values(): yield it

    __known = {}
    @classmethod
    def get(cls, text, stack):
        return cls._cache_(cls.__known, stack, text, 'free')
MemoryChunk.register(FMA, 'free')

class UHR (MemoryChunk, UMR):
    "Uninitialized heap read"
    __init__ = MemoryChunk.__init__ # not UMR's

    @classmethod
    def _active_(cls):
        for it in cls.__known.values(): yield it

    __known = {}
    @classmethod
    def get(cls, text, stack):
        return cls._cache_(cls.__known, stack, text, 'alloc')
MemoryChunk.register(UHR, 'alloc')

class Leak (Issue):
    @classmethod
    def matchers(cls):
        yield lambda x: 0 <= x.find('lost in loss record')

    @staticmethod
    def __parse(text, asint=readint,
                direct=re.compile(r'([0-9,]+) bytes\s*').match,
                burden=re.compile(
            r'([0-9,]+) \(([0-9,]*) direct, ([0-9,]+) indirect\) bytes\s*').match,
                blocks=re.compile(r'in ([0-9,]+) blocks are\s*').match,
                record=re.compile(r'lost in loss record ([0-9,]+) of ([0-9,]+)').search):
        it = direct(text)
        if not it:
            it = burden(text)
            if not it: raise ParseError('No byte-total on leak-line', text)
            routes = [asint(x) for x in it.groups()]
            total, routes = routes[0], tuple(routes[1:])
            assert total == sum(routes)
        else: routes = (asint(it.group(1)), 0)
        text = text[it.end():].strip()

        it = blocks(text)
        if not it: raise ParseError('No block-count on leak-line', text)
        count = asint(it.group(1))
        text = text[it.end():].strip()

        sure = text.startswith('definitely')
        if not (sure or text.startswith('possibly')):
            raise ParseError('Neither "possibly" nor "definitely" on leak-line', text)

        it = record(text)
        if not it: raise ParseError('No loss record details in leak-line', text)
        index, total = [asint(x) for x in it.groups()]

        return sure, routes, count, index, total

    __upinit = Issue.__init__
    def __init__(self, stack, text, sure, routes, count, index):
        self.__upinit(stack, 'Leak: ' + text)
        self.size = self.__size(sure, routes, count)
        self.index = index

    from study.maths.vector import Namely
    class LeakSize (Namely):
        _component_names_ = ('blocks', 'maybe', 'direct', 'indirect')
        @classmethod
        def fromParsed(cls, sure, blocks, direct, indirect):
            if sure: return cls(blocks, 0, direct, indirect)
            return (blocks, direct + indirect, 0, 0)
    del Namely

    @staticmethod
    def __size(sure, routes, blocks, gen=LeakSize.fromParsed):
        return gen(sure, blocks, *routes)
    del LeakSize

    @property
    def sure(self): return sum(self.size[2:]) > 0

    __upclear = Issue.clear
    def clear(self): # returns (maybe-leaked, direct-leaked, indirect-leaked, blocks)
        self.__upclear()
        return self.size

    __known = {}
    @classmethod
    def get(cls, text, stack, address):
        assert address is None
        sure, routes, count, index, total = cls.__parse(text)
        return cls._cache_(cls.__known, stack, text, sure, routes, count, index), total

Issue.register(Leak)

# Placeholders: no actual use for them yet
class Traffic (object):
    def __init__(self, lost, block, grab, free, churn):
        self.lost, self.block = lost, block
        self.allocate, self.free, self.total = grab, free, churn

class LeakSummary (object):
    def __init__(self, sure, more, maybe, reach, skip):
        self.sure, self.more, self.maybe, self.reach, self.skip = sure, more, maybe, reach, skip

class FinalSummary (object):
    def __init__(self, err, ctx, serr=0, sctx=0):
        self.reports = err, ctx
        self.suppressed = serr, sctx

class Report (object):
    def __init__(self, command, ppid, issues, traffic, leaks, leaksum, overall):
        self.command, self.ppid = command, ppid
        self.issues, self.traffic = issues, traffic
        self.leaks, self.leaksum = leaks, leaksum
        self.overall = overall

class MemCheck (object):
    def __init__(self):
        self.leaks, self.issues, self.fixed, self.dull = set(), set(), set(), set()

    def __len__(self): return len(tuple(iter(self)))
    def __iter__(self):
        for it in self.__iter():
            if it.fixed: self.fixed.add(it)
            elif it in self.fixed or it in self.dull: pass
            else: yield it

    def __iter(self):
        for it in self.leaks:
            if it.sure: yield it

        for it in self.issues: yield it

        for it in self.leaks:
            if not it.sure: yield it

    @staticmethod
    def __frame_out(source, frame, dump, each=None):
        saved = None
        for it in source:
            if frame in it.stack:
                leak = None if each is None else each(it)
                if saved is None: saved = leak
                elif leak is not None: saved += leak
                dump.add(it)

        return saved

    def __ditch(self, frame, dump, leak=True, each=None):
        return self.__frame_out(self.leaks if leak else self.issues, frame, dump, each)

    def repair(self, frame, leak=True):
        return self.__ditch(frame, self.fixed, leak, lambda x: x.clear())

    def ignore(self, frame, leak=True):
        return self.__ditch(frame, self.dull, leak)

    # The (hairy spitball of an ad hoc) parser:
    @staticmethod
    def __parseheader(src,
                      head=(re.compile(r'Copyright (C) \d+-\d+.*'),
                            re.compile(r'Using Valgrind.*')),
                      cmd=re.compile(r'Command: (.*)'),
                      parent=re.compile(r'Parent PID: (\d+)')):
        for it in head:
            n, line = src.next()
            it = it.match(line)
            if it is None: break
        else: n, line = src.next()

        it = cmd.match(line)
        if it:
            command = it.group(1)
            n, line = src.next()
        else: command = None

        it = parent.match(line)
        if it:
            ppid = int(it.group(1))
            n, line = src.next()
        else: ppid = None

        # Look for blank line at end of header
        while line: n, line = src.next()
        return command, ppid

    @staticmethod
    def __parseblocks(src, stopper, mode, command,
                      cruft=(
            re.compile(r'More than (100|1000 different) errors detected\.').match,
            re.compile(r'Warning: set address range perms: large range '
                       r'\[0x[0-9a-fA-F]+, 0x[0-9a-fA-F]+\) \(noaccess\)').match,
            # TODO: parse these, too
            lambda x: 'Process terminating with default action of signal' in x,
            lambda x: x == 'Jump to the invalid address stated on the next line')):
        items, stanza, addr, count = [], None, None, None
        for n, line in src:
            if line in (stopper, 'All heap blocks were freed -- no leaks are possible'):
                break

            if not line: # end of stanza
                if addr or stanza:
                    if stack:
                        try: stack = Stack.get(stack)
                        except AssertionError as what:
                            what.args += (n, line, stack)
                            raise
                    else:
                        stack = None

                if addr:
                    addr = MemoryChunk.get(addr, stack)
                    items.append(addr)
                    stack, mode = prior # restore normal parsing
                    del prior

                if stanza:
                    block, n = Issue.get(stanza, stack, addr)
                    if count is None: count = n
                    else: assert count == n

                    items.append(block)
                    addr = stanza = None
                    del stack
                # else: more than one blank line
            elif any(c(line) for c in cruft):
                # Skip cruft (TODO: and following block):
                while line: n, line = src.next()
                # bug: this skips the first report after the cruft line.
                # tolerating this (it's often a duplicate) rather than uglify code.
            elif line.startswith('Address'):
                assert addr is None
                prior = Stack.get(stack), mode # stash while parsing allocation block
                addr, stack, mode = line, [], 'Address block for ' + mode
            elif stanza or addr: # read stack-frame line:
                try: frame = Frame.get(line, command)
                except ParseError, what:
                    # TODO: is there nothing else we can do here ?
                    args = what.args
                    while args and args[-1] is None: args = args[:-1]
                    what.args = ('Failed to parse line', mode, n) + args
                    raise
                stack.append(frame)

            else: stack, stanza = [], line # first line of new block

        try: stack
        except NameError: pass
        else: raise ParseError('Missing blank line before', line, n)
        assert stanza is None

        return tuple(items), n

    @staticmethod
    def __parsetraffic(src, asint=readint,
                       inuse=re.compile(r'in use at exit: ([0-9,]+) '
                                        r'bytes in ([0-9,]+) blocks').match,
                       total=re.compile(r'total heap usage: ([0-9,]+) allocs, ([0-9,]+) '
                                        r'frees, ([0-9,]+) bytes allocated').match):
        n, line = src.next()
        it = inuse(line)
        if it: lost, block = [asint(x) for x in it.groups()]
        else: raise ParseError('Failed to parse heap-in-use summary', line, n)

        n, line = src.next()
        it = total(line)
        if it: grab, free, churn = [asint(x) for x in it.groups()]
        else: raise ParseError('Failed to parse heap traffic totals', line, n)

        return Traffic(lost, block, grab, free, churn), n

    def leaksum(text, n, prefix, asint=readint, # tool function for __parseleaksummary
                chunk=re.compile(r'([0-9,]+) bytes in ([0-9,]+) blocks')):
        if text.startswith(prefix + ': '):
            it = chunk.search(text)
            if not it: raise ParseError('Malformed "%s" line' % prefix, text, n)
            return tuple(asint(x) for x in it.groups())
        return None

    junk = ('Reachable blocks (those to which a pointer was found) are not shown.',
            'To see them, rerun with: --leak-check=full --show-reachable=yes',
            'Rerun with --leak-check=full to see details of leaked memory')

    @staticmethod
    def __parseleaksummary(src, getsum=leaksum,
                           prefixes=('definitely lost', 'indirectly lost',
                                     'possibly lost', 'still reachable', 'suppressed'),
                           cruft=junk):
        n, line = src.next()
        data = []
        for it in prefixes:
            data.append(getsum(line, n, it))
            if data[-1]:
                n, line = src.next()
                if 'reachable' in it and line.endswith('of which reachable via heuristic:'):
                    n, line = src.next()
                    heurist = getsum(line, n, 'newarray           ')
                    if heurist:
                        n, line = src.next()

        while line in cruft: n, line = src.next()
        assert not line, line

        return LeakSummary(*data), n

    del leaksum
    junk = ('For counts of detected and suppressed errors, rerun with: -v',
            'Use --track-origins=yes to see where uninitialised values come from')

    @staticmethod
    def __parsetail(src,
                    errs=re.compile(r'ERROR SUMMARY: (\d+) errors from (\d+) contexts\s*'),
                    skip=re.compile(r'\(suppressed: (\d+) from (\d+)\)'),
                    cruft=junk):
        n, line = src.next()
        while not line or line in cruft: n, line = src.next()
        it = errs.match(line)
        if not it: raise ParseError('Failed to parse final error summary', line, n)
        data = it.groups()
        line = line[it.end():].strip()
        it = skip.match(line)
        if line and not it: raise ParseError('Unrecognised tail for final summary', line, n)
        data = [int(x) for x in data + it.groups()]
        return FinalSummary(*data)

    del junk

    def byline(fd, stem): # tool function for __ingest
        n, off = 1, len(stem) # we got stem off the first line.
        while True:
            line = fd.readline()
            if not line: break
            n += 1
            if line.startswith(stem): yield n, line[off:].strip()
            else:
                raise ParseError("Line doesn't start with expected prefix",
                                 line, n, stem)

    @classmethod
    def __ingest(cls, fd,
                 reader=byline,
                 first=re.compile(r'(.* )Memcheck, a memory error detector')):
        line = fd.readline()
        stem = first.match(line)
        if not stem: raise ParseError('Unfamiliar first line', line)

        src = reader(fd, stem.group(1))
        try: command, ppid = cls.__parseheader(src)
        except StopIteration:
            raise ParseError('Incomplete or unterminated header')

        issues, n = cls.__parseblocks(src, 'HEAP SUMMARY:', 'issue stack-frame', command)
        try: traffic, n = cls.__parsetraffic(src)
        except StopIteration:
            raise ParseError('Incomplete, missing or unterminated heap summary', n)

        leaks, n = cls.__parseblocks(src, 'LEAK SUMMARY:', 'leak stack-frame', command)
        try: leaksum, n = cls.__parseleaksummary(src)
        except StopIteration:
            raise ParseError('Incomplete, missing or unterminated leak summary', n)
        try: overall = cls.__parsetail(src)
        except StopIteration:
            raise ParseError('Incomplete or missing tail-piece', n)

        return command, ppid, issues, traffic, leaks, leaksum, overall

    del byline

    def ingest(self, *logs):
        """Make sense of a bunch of valgrind memcheck log files.

        Each argument should name a log file.  Returns a tuple of Report
        objects, one per log file supplied.  Updates self with knowledge of the
        issues (including leaks) found in all such reports.\n"""
        ans = []
        for log in logs:
            with open(log) as fd:
                try:
                    (command, ppid, issues, traffic, leaks, leaksum, overall
                     ) = self.__ingest(fd)
                except ParseError as what:
                    what.args += (log,)
                    raise
            for it in issues: self.issues.add(it)
            for it in leaks: self.leaks.add(it)
            ans.append(Report(command, ppid, issues, traffic, leaks, leaksum, overall))
        return tuple(ans)

del re, readint
