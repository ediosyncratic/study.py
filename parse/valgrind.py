"""Parsing valgrind memcheck logs.
"""
import re

class ParseError (SyntaxError):
    """Error during parsing of a valgrind memcheck log-file."""
    __upinit = SyntaxError.__init__
    def __init__(self, message, *details, **what):
        while len(details) < 2: details += (None,) # SyntaxError is fussy
        self.__upinit(message, *details)
        while self.args[-1] is None: self.args = self.args[:-1]
        for k, v in what.items(): setattr(self, k, v)

def readint(text): return int(''.join(text.split(',')))

class Source (object):
    def __init__(self, sfile, line=None):
        assert sfile
        self.source = sfile
        if line is not None: self.line = line
        self.frames = set() # TODO: need a weakset()

    def __repr__(self):
        try: n = self.line
        except AttributeError:
            return self.source
        return ':'.join([self.source, str(n)])

    def __hash__(self):
        ans = hash(self.source)
        try: line = self.line
        except AttributeError: return ans
        return ans ^ hash(line)

    def __rcmp__(self, other):
        if other is None: return -1
        return (cmp(other.source, self.source) or
                cmp(getattr(other, 'line', None), getattr(self, 'line', None)))

    def __cmp__(self, other):
        if other is None: return +1
        return (cmp(self.source, other.source) or
                cmp(getattr(self, 'line', None), getattr(other, 'line', None)))

    __known = {}
    @classmethod
    def get(cls, sfile, line=None):
        try: ans = cls.__known[sfile, line]
        except KeyError:
            ans = cls.__known[sfile, line] = cls(sfile, line)

        return ans

    @classmethod
    def mungibles(cls):
        """Iterator over potentially aliasing frames.

        If a Source has a .line and distinct entries in .frames from
        distinct binaries, it's possible some of the distinct frames
        are really aliases for one another.  This iterates over the
        candidates.\n"""
        for src in cls.__known.values():
            try: src.line
            except AttributeError: pass
            else:
                addrs = set(frm.addr for frm in src.frames)
                if len(addr) > 1:
                    bag = set()
                    while addrs:
                        it = addrs.pop()
                        bag = bag.union(it.mayalias(oth) for oth in addrs)
                    if None not in bag: yield src, addrs, bag

class Program (object):
    def __init__(self, name): self.name = name
    def __hash__(self): return hash(self.name)
    def __cmp__(self, other): return cmp(self.name, other.name)
    __known = {}
    @classmethod
    def get(cls, name):
        try: ans = cls.__known[name]
        except KeyError:
            ans = cls.__known[name] = cls(name)

        return ans

class Address (object):
    def __init__(self, addr, prog):
        self.where = { prog: addr }
        self.frames = set() # TODO: weekset

    def __len__(self): return len(self.where)
    def __hash__(self): return id(self.where)
    def __cmp__(self, other):
        return cmp(id(self.where), id(other.where))

    def mayalias(self, other):
        count = 0
        for p in set(self.where.keys()).intesection(other.where.keys()):
            if self.where[p] != other.where[p]:
                return None
            count += 1
        return count

    def conflate(self, *rest):
        for addr in rest:
            if addr is self: continue

            fs = tuple(addr.frames)
            addr.frames.clear()
            for frame in fs:
                frame.addr = self
                self.frames.add(frame)

            for p, a in addr.where.items():
                try: was = self.where[p]
                except KeyError: self.where[p] = a
                else:
                    if was != a:
                        raise ValueError('Conflating two addresses in one program',
                                         p, was, a, self, addr)
                assert self.__known[a, p] is addr
                self.__known[a, p] = self

    __known = {}
    @classmethod
    def get(cls, addr, prog):
        try: ans = cls.__known[addr, prog]
        except KeyError:
            ans = cls.__known[addr, prog] = cls(addr, prog)
        return ans

from study.snake.sequence import Tuple
class Tuple (Tuple):
    """Tuple variant for which Frame entries are natural.

    Its display format is just one entry per line, with no further decoration,
    which works nicely when each entry is a Frame.\n"""
    def __repr__(self): return '\n'.join([repr(f) for f in self])

class Frame (object):
    @staticmethod
    def __parse(text, prog,
                locat=re.compile(r'\b(by|at)\s+0x([0-9a-fA-F]+):\s+(\S+)\s*'),
                inlib=re.compile(r'\s*\(in ([^)]*)\)'),
                sause=re.compile(r'\s*\(([^)]*):(\d+)\)')):
        place = locat.search(text)
        if not place: raise ParseError('No (at|by) stack-frame data', text)
        leaf = place.group(1) == 'at'
        addr, func = place.group(2, 3)
        if func == '???': func = None
        addr = Address.get(addr, prog)
        tail = text[place.end():].strip()

        src = inlib.match(tail)
        if src is None:
            src = sause.match(tail)
            if src is None: assert not tail and func is None, text
            else: src = Source.get(src.group(1), int(src.group(2)))
        else: src = Source.get(src.group(1))

        return leaf, addr, func, src

    def __init__(self, text, leaf, addr, func=None, source=None):
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

    def callers(self, n=1):
        """Find other entries in self's stacks, at offset n.

        Optional argument n (default: 1) is the number of steps down the stack
        (i.e. towards main) to go; a negative value means up (i.e. to the
        functions self called).  Stacks are ignored if the given offset from
        self isn't recorded.  Returns a Tuple of a set of frames thus
        found.  With the default n = 1, you get self's (known) callers.\n"""

        ans = set()
        for it in self.stacks:
            ind = it.index(self) # shouldn't ValueError !
            try:
                if n + ind < 0: raise IndexError
                set.add(it[n + ind]) # IndexError if out of bounds
            except IndexError: pass

        return Tuple(ans)

    def conflate(self, *rest):
        """Treat each parameter as an alias for self.

        Pass arbitrarily many frames as parameters, each of which is
        just a different binary's way of referring to the same
        instruction as self.  These frames are then conflated.  This
        changes the hashes of the others, which requires some
        juggling.\n"""
        allstacks = self.stacks
        for f in rest:
            if f is self: continue
            if f.source is not None:
                f.source.frames.remove(f)
            allstacks = allstacks.union(f.stacks)

        self.addr.conflate(*[f.addr for f in rest])

        for f in rest:
            if f is self: continue
            if f.source is not None:
                f.source.frames.add(f)
            f.stacks = allstacks
        self.stacks = allstacks

    __known = {}
    @classmethod
    def get(cls, text, command):
        key = cls.__parse(text, Program.get(command))
        try: ans = cls.__known[key]
        except KeyError: ans = cls.__known[key] = cls(text, *key)
        return ans

class Stack (Tuple):
    def __init__(self, frames):
        assert frames and frames[0].leaf and all(not x.leaf for x in frames[1:])
        for f in frames: f.stacks.add(self)
        self.issues = set()

    __known = {}
    @classmethod
    def get(cls, frames):
        frames = tuple(frames)
        try: ans = cls.__known[frames]
        except KeyError: ans = cls.__known[frames] = cls(frames)
        return ans

class Issue (object):
    def __init__(self, stack, grumble):
        self.stack, self.problem = stack, grumble
        if stack: stack.issues.add(self)
        self.fixed, self.victim = False, set() # TODO: weakset

    def __repr__(self): return self.problem

    # Derived classes may want to do more than this:
    def clear(self): self.fixed = True

    __subs = []
    @classmethod
    def register(cls, sub, key): cls.__subs.insert(0, (sub, key))
    # prepend, so that later (i.e. more specific) classes can easily take precedence

    __known = {}
    @classmethod
    def get(cls, text, stack, address):
        for sub, key in cls.__subs:
            if key(text):
                ans = sub.get(text, stack, address)
                if isinstance(ans, tuple): return ans
                return ans, 0

        assert address is None, text
        return cls._cache_(cls.__known, stack, text), 0

    @classmethod
    def _cache_(cls, bok, stack, *rest):
        try: ans = bok[stack]
        except KeyError: ans = bok[stack] = cls(stack, *rest)
        return ans

class UMR (Issue):
    """Uninitialised memory read."""
    __known = {}
    @classmethod
    def get(cls, text, stack, address):
        ans = cls._cache_(cls.__known, stack, text)
        if address is not None: address.usedby(ans)
        return ans

    __upclear = Issue.clear
    def clear(self):
        self.__upclear()
        MemoryChunk.disuse(self)

Issue.register(UMR, lambda x: x.startswith('Invalid read of size'))
Issue.register(UMR, lambda x: 0 <= x.find('uninitialised value'))
Issue.register(UMR, re.compile(
        r'Syscall param\b.*\bpoints to uninitialised byte\(s\)').match)

class BadFree (Issue):
    """Releasing unallocated memory."""
    __known = {}
    @classmethod
    def get(cls, text, stack, address):
        ans = cls._cache_(cls.__known, stack, text)
        if address is not None: address.usedby(ans)
        return ans

    __upclear = Issue.clear
    def clear(self):
        self.__upclear()
        MemoryChunk.disuse(self)

Issue.register(BadFree, lambda x: x == 'Invalid free() / delete / delete[] / realloc()')

class UnMapped (Issue):
    """Access to unmapped memory"""
    @staticmethod
    def __parse(text,
                grab=re.compile('Access not within mapped region at address ' +
                                r'0x([0-9a-fA-F]+)').match):
        it = grab(text)
        if not it: raise ParseError('Unfamiliar non-mapped access message', text)
        address = it.group(1)
        return address

    __upinit = Issue.__init__
    def __init__(self, stack, text, addr):
        self.__upinit(stack, text)
        self.address = addr

    __known = {}
    @classmethod
    def get(cls, text, stack, address):
        addr = cls.__parse(text)
        ans = cls._cache_(cls.__known, stack, text, addr)
        if address is not None: address.usedby(ans)
        return ans

    __upclear = Issue.clear
    def clear(self):
        self.__upclear()
        MemoryChunk.disuse(self)

Issue.register(UnMapped, lambda x: x.startswith('Access not within mapped region'))

class MemoryChunk (Issue):
    @staticmethod
    def __parse(text, asint=readint,
                stray=re.compile(r"Address 0x([0-9a-fA-F]+) is not stack'd, " +
                                 r"malloc'd or \(recently\) free'd").match,
                line=re.compile(r"Address 0x([0-9a-fA-F]+) is ([0-9,]+) " +
                                r"bytes (inside|after) a block " +
                                r"of size ([0-9,]+) (\w+)'d").match):
        it = line(text)
        if it:
            addr, func = it.group(1, 5)
            offset, size = [asint(x) for x in it.group(2, 4)]
            if it.group(3) == 'after': offset += size
        else:
            it = stray(text)
            if not it: raise ParseError('Malformed block description line', text)
            addr = it.group(1)
            func = offset = size = None
        return func, size, offset, addr

    __upinit = Issue.__init__
    def __init__(self, stack, text, func):
        self.__upinit(stack or (), text)
        self.author, self.__eg, self.__users = func, set(), set() # TODO: weakset

    def example(self, *what): self.__eg.add(what)
    @property
    def samples(self): return tuple(self.__eg)

    def usedby(self, other):
        self.__users.add(other)
        other.victim.add(self)
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

Issue.register(MemoryChunk, lambda x: 0 <= x.find('bytes inside a block of size'))
Issue.register(MemoryChunk, lambda x: 0 <= x.find('bytes after a block of size'))
Issue.register(MemoryChunk, lambda x: 0 <= x.find("is not stack'd, malloc'd or (recently) free'd"))

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

class SMA (MemoryChunk):
    "Stray memory access"

    @classmethod
    def _active_(cls):
        for it in cls.__known.values(): yield it

    __known = {}
    @classmethod
    def get(cls, text, stack):
        return cls._cache_(cls.__known, stack, text, 'alloc')
MemoryChunk.register(SMA, None)

class Leak (Issue):
    @staticmethod
    def __parse(text, asint=readint,
                direct=re.compile(r'([0-9,]+) bytes\s*'),
                burden=re.compile(r'([0-9,]+) \(([0-9,]*) direct, ([0-9,]+) indirect\) bytes\s*'),
                blocks=re.compile(r'in ([0-9,]+) blocks are\s*'),
                record=re.compile(r'lost in loss record ([0-9,]+) of ([0-9,]+)')):
        it = direct.match(text)
        if not it:
            it = burden.match(text)
            if not it: raise ParseError('No byte-total on leak-line', text)
            routes = [asint(x) for x in it.groups()]
            total, routes = routes[0], tuple(routes[1:])
            assert total == sum(routes), text
        else: routes = (asint(it.group(1)), 0)
        text = text[it.end():].strip()

        it = blocks.match(text)
        if not it: raise ParseError('No block-count on leak-line', text)
        count = asint(it.group(1))
        text = text[it.end():].strip()

        sure = text.startswith('definitely')
        if not (sure or text.startswith('possibly')):
            raise ParseError('Neither "possibly" nor "definitely" on leak-line', text)

        it = record.search(text)
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
        assert address is None, text
        sure, routes, count, index, total = cls.__parse(text)
        return cls._cache_(cls.__known, stack, text, sure, routes, count, index), total

Issue.register(Leak, lambda x: 0 <= x.find('lost in loss record'))

# Placeholders: no actual use for them yet
class Terminal (object):
    def __init__(self, text, signal, err, addr):
        self.fatal = text
        self.signal, self.error, self.address = signal, err, addr

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

class Thread (object):
    def __init__(self, issues, traffic, leaks, leaksum, overall):
        self.issues, self.traffic = issues, traffic
        self.leaks, self.leaksum = leaks, leaksum
        self.overall = overall

class Report (object):
    def __init__(self, command, ppid, dead, threads):
        self.command, self.ppid, self.dead = command, ppid, dead
        self.threads = threads

class MemCheck (object):
    def __init__(self):
        self.leaks, self.issues, self.fatal = set(), set(), set()
        self.fixed, self.dull = set(), set()

    def __len__(self): return len(tuple(iter(self)))
    def __iter__(self):
        for it in self.__iter():
            if it.fixed: self.fixed.add(it)
            elif it in self.fixed or it in self.dull: pass
            else: yield it

    def __iter(self):
        for it in self.fatal: yield it
        # The following might repeat entries in fatal; but they should be fixed
        # by the time we do so, ensuring they get filtered out in __iter__().

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
        if isinstance(leak, Issue): leak = isinstance(leak, Leak)
        return self.__frame_out(self.leaks if leak else self.issues, frame, dump, each)

    def repair(self, frame, leak=True):
        return self.__ditch(frame, self.fixed, leak, lambda x: x.clear())

    def ignore(self, frame, leak=True):
        return self.__ditch(frame, self.dull, leak)

    def liberate(self, source):
        """Ignore all UMRs from a given source.

        For use with libssl and libcrypto."""
        for item in self.issues:
            if isinstance(item, UMR) and any(f.source is source for f in item.stack):
                self.dull.add(item)

    # The (hairy spitball of an ad hoc) parser:
    @staticmethod
    def __parseheader(dest, pid,
                      head=(re.compile(r'Copyright (C) \d+-\d+.*'),
                            re.compile(r'Using Valgrind.*')),
                      cmd=re.compile(r'Command: (.*)'),
                      parent=re.compile(r'Parent PID: (\d+)')):
        try:
            for it in head:
                p, n, line = yield
                assert p == pid
                it = it.match(line)
                if it is None: break
            else:
                p, n, line = yield
                assert p == pid

            it = cmd.match(line)
            if it:
                command = it.group(1)
                p, n, line = yield
                assert p == pid
            else: command = None

            it = parent.match(line)
            if it:
                ppid = int(it.group(1))
                p, n, line = yield
                assert p == pid
            else: ppid = None

            # Look for blank line at end of header
            while line:
                p, n, line = yield
                assert p == pid
        except StopIteration:
            raise ParseError('Incomplete or unterminated header')

        dest[:] = [ command, ppid ]

    @staticmethod
    def __parseblocks(dest, stopper, mode, command,
                      cruft=re.compile(r'More than (100|1000 different) errors detected\.').match,
                      died=re.compile('Process terminating with default action of '
                                      r'signal ([0-9]+) \(([A-Z0-9]+)\)').match,
                      thread=re.compile(r'Thread \d+:$').match):
        items = []
        stanza = addr = count = terminal = signal = None

        while True:
            n, line = yield
            if line == stopper: break

            if not line: # end of stanza
                if addr or stanza:
                    if stack: stack = Stack.get(stack)
                    else: stack = None

                if addr:
                    assert stanza, line
                    addr = MemoryChunk.get(addr, stack)
                    items.append(addr)
                    stack, mode = prior # restore normal parsing
                    del prior

                if stanza:
                    block, m = Issue.get(stanza, stack, addr)
                    if count is None: count = m
                    else: assert count == m, line
                    items.append(block)

                    if terminal:
                        assert isinstance(terminal, basestring), line
                        terminal, signal = Terminal(terminal, signal, block, addr), None

                    addr = stanza = None
                    del stack

                # else: more than one blank line
            elif cruft(line):
                # Skip notice about reduced reporting:
                while line: n, line = yield
                # bug: this skips the first report after the 100 errors report.
                # tolerating this rather than uglify code.
            elif thread(line): pass # ignore Thread lines.
            elif line.startswith('Process terminating'):
                assert terminal is None, line
                terminal = line
                it = died(line)
                if not it: raise ParseError('Unfamiliar termination', line, lineno=n)
                signal = int(it.group(1)), it.group(2)
            elif line.startswith('Address'):
                assert addr is None and stanza, line
                prior = Stack.get(stack), mode # stash while parsing allocation block
                addr, stack, mode = line, [], 'Address block for ' + mode
            elif stanza or addr: # read stack-frame line:
                try: frame = Frame.get(line, command)
                except ParseError, what:
                    if terminal is None: # ignore cruft after terminal block
                        # TODO: is there nothing else we can do here ?
                        what = what.args
                        while what[-1] is None: what = what[:-1]
                        raise ParseError('Failed to parse line', mode, what, lineno=n)
                stack.append(frame)

            else: stack, stanza = [], line # first line of new block

        try: stack
        except NameError: pass
        else: raise ParseError('Missing blank line before', line, lineno=n)
        assert stanza is None

        dest[:] = [ tuple(items), terminal ]

    @staticmethod
    def __parsetraffic(dest, asint=readint,
                       inuse=re.compile(r'in use at exit: ([0-9,]+) bytes in ([0-9,]+) blocks'),
                       total=re.compile(r'total heap usage: ([0-9,]+) allocs, ([0-9,]+) frees, ([0-9,]+) bytes allocated')):
        try:
            n, line = yield
            it = inuse.match(line)
            if it: lost, block = [asint(x) for x in it.groups()]
            else: raise ParseError('Failed to parse heap-in-use summary', line, lineno=n)

            n, line = yield
        except GeneratorExit:
            raise ParseError('Incomplete, missing or unterminated heap summary',
                             lineno=n)

        it = total.match(line)
        if it: grab, free, churn = [asint(x) for x in it.groups()]
        else: raise ParseError('Failed to parse heap traffic totals', line, lineno=n)

        dest[:] = [ Traffic(lost, block, grab, free, churn) ]

    def leaksum(text, n, prefix, asint=readint, # tool function for __parseleaksummary
                chunk=re.compile(r'([0-9,]+) bytes in ([0-9,]+) blocks')):
        if text.startswith(prefix + ': '):
            it = chunk.search(text)
            if not it: raise ParseError('Malformed "%s" line' % prefix, text, lineno=n)
            return tuple(asint(x) for x in it.groups())
        return None

    @staticmethod
    def __parseleaksummary(dest, getsum=leaksum,
                           prefixes=('definitely lost', 'indirectly lost',
                                     'possibly lost', 'still reachable', 'suppressed'),
                           cruft=('Reachable blocks (those to which a pointer was found) are not shown.',
                                  'To see them, rerun with: --leak-check=full --show-reachable=yes')):
        try:
            n, line = yield
            data = []
            for it in prefixes:
                data.append(getsum(line, n, it))
                if data[-1]: n, line = yield

            while line in cruft: n, line = yield
        except GeneratorExit:
            raise ParseError('Incomplete, missing or unterminated leak summary',
                             lineno = n)
        assert not line, line
        dest[:] = [ LeakSummary(*data) ]

    del leaksum

    @staticmethod
    def __parsetail(dest,
                    errs=re.compile(r'ERROR SUMMARY: (\d+) errors from (\d+) contexts\s*'),
                    skip=re.compile(r'\(suppressed: (\d+) from (\d+)\)'),
                    cruft=('For counts of detected and suppressed errors, rerun with: -v',
                           'Use --track-origins=yes to see where uninitialised values come from')):
        try:
            n, line = yield
            while not line or line in cruft: n, line = yield
        except GeneratorExit:
            raise ParseError('Incomplete or missing tail-piece', lineno=n)

        it = errs.match(line)
        if not it:
            raise ParseError('Failed to parse final error summary', line, lineno=n)
        data = it.groups()
        line = line[it.end():].strip()
        it = skip.match(line)
        if line and not it:
            raise ParseError('Unrecognised tail for final summary', line, lineno=n)
        data = [int(x) for x in data + it.groups()]

        dest[:] = [ FinalSummary(*data) ]

    @classmethod
    def __muncher(cls, bok, fatal, pid, command):
        dest = []
        munch = cls.__parseblocks(dest, 'HEAP SUMMARY:',
                                  'issue stack-frame', command)
        munch.next()
        try:
            while True: munch.send((yield))
        except GeneratorExit:
            munch.close()
            raise
        except StopIteration: pass
        issues, dead = dest
        if dead is not None: fatal.add(dead)

        dest = []
        munch = cls.__parsetraffic(dest)
        munch.next()
        try:
            while True: munch.send((yield))
        except GeneratorExit:
            munch.close()
            raise
        except StopIteration: pass
        traffic = dest

        dest = []
        munch = cls.__parseblocks(dest, 'LEAK SUMMARY:',
                                  'leak stack-frame', command)
        munch.next()
        try:
            while True: munch.send((yield))
        except GeneratorExit:
            munch.close()
            raise
        except StopIteration: pass
        leaks, dead = dest
        assert dead is None

        dest = []
        munch = cls.__parseleaksummary(dest)
        munch.next()
        try:
            while True: munch.send((yield))
        except GeneratorExit:
            munch.close()
            raise
        except StopIteration: pass
        leaksum = dest

        dest = []
        munch = cls.__parsetail(dest)
        munch.next()
        try:
            while True: munch.send((yield))
        except GeneratorExit: munch.close()
        except StopIteration: pass
        overall = dest[0]

        bok[pid] = issues, traffic, leaks, leaksum, overall

    def numbered(fd): # tool function for byline
        n = 0 # line number
        while True:
            line = fd.readline()
            if not line: break # end of file
            n += 1
            yield n, line

    def byline(fd, # tool function for ingest
               each=numbered,
               front=re.compile(r'==([0-9]+)==\s+').match):
        lines, stem = each(fd), None
        for n, line in lines:
            if stem and line.startswith(stem): pass
            else:
                it = front(line)
                if not it:
                    raise ParseError("Line doesn't start with expected prefix",
                                     line, lineno=n)
                stem, proc = it.group(0), int(it.group(1))
                off = len(stem)
            yield proc, n, line[off:].strip()

    @classmethod
    def __ingest(cls, fd,
                 reader=byline,
                 first='Memcheck, a memory error detector'):
        src = reader(fd)
        pid, n, line = src.next()
        if line != first:
            raise ParseError('Unfamiliar first line', line, lineno=1)

        dest = []
        munch = cls.__parseheader(dest, pid)
        munch.next()
        try:
            while True:
                p, n, line = src.next()
                munch.send((p, n, line))
        except GeneratorExit: munch.close()
        except StopIteration: pass
        command, ppid = dest

        partial, final, fatal = {}, {}, set()
        partial[pid] = munch = cls.__muncher(final, fatal, pid, command)
        munch.next()
        for p, n, line in src:
            if p != pid:
                try: munch = partial[p]
                except KeyError:
                    assert not final.has_key(p), line
                    munch = partial[p] = cls.__muncher(final, fatal, p, command)
                    munch.next()
                pid = p

            try: munch.send((n, line))
            except StopIteration:
                assert final.has_key(pid), line
                try: del partial[pid]
                except KeyError: assert munch is None, line
                else: munch = None

        n += 1
        for pid, munch in partial.items():
            try: munch.send(n, '')
            except StopIteration: pass
            else: munch.close()

        return command, ppid, fatal, final

    del byline

    def ingest(self, *logs):
        """Make sense of a bunch of valgrind memcheck log files.

        Each argument should name a log file.  Returns a tuple of Report
        objects, one per log file supplied.  Updates self with knowledge of the
        issues (including leaks) found in all such reports.\n"""
        ans = []
        for log in logs:
            fd = open(log)
            try:
                try: cmd, ppid, dead, final = self.__ingest(fd)
                except ParseError, what:
                    what.filename = log
                    raise what
            finally: fd.close()

            threads = []
            for pid, vs in final.items():
                issues, traffic, leaks, leaksum, overall = vs
                for it in issues: self.issues.add(it)
                for it in leaks: self.leaks.add(it)
                threads.append(Thread(*vs))
            ans.append(Report(cmd, ppid, dead, tuple(threads)))
            for term in dead:
                self.fatal.add(term.error)
        return tuple(ans)

del re, readint
