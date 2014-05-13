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

    def conflate(self, *rest): # TODO: make this work
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
                locat=re.compile(r'\b(by|at)\s+0x([0-9a-fA-F]+):\s+').search,
                funky=re.compile(r'(\(below main\)|[^(]\S+)\s*').match,
                inlib=re.compile(r'\s*\(in ([^)]*)\)').match,
                sause=re.compile(r'\s*\(([^)]*):(\d+)\)').match):
        it = locat(text)
        if not it: raise ParseError('No (at|by) stack-frame data', text)
        leaf = it.group(1) == 'at'
        addr = Address.get(it.group(2), prog)
        tail = text[it.end():].strip()

        it = funky(tail)
        if not it: raise ParseError('No function identification', text)
        func = it.group(1)
        if func == '???': func = None
        tail = tail[it.end():].strip()

        src = inlib(tail)
        if src is None:
            src = sause(tail)
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
        if text == '<inherited from parent>': key = (text, True, None)
        else: key = cls.__parse(text, Program.get(command))
        try: ans = cls.__known[key]
        except KeyError: ans = cls.__known[key] = cls(text, *key)
        return ans

class Stack (Tuple):
    def __init__(self, frames):
        if frames and frames[0].leaf and all(not x.leaf for x in frames[1:]):
            for f in frames: f.stacks.add(self)
            self.issues = set()
        else:
            raise ParseError('Expected one leaf, first', frames)

    __known = {}
    @classmethod
    def get(cls, frames):
        if frames is None: return None
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
    def get(cls, text, stack, address, value):
        for sub, key in cls.__subs:
            if key(text):
                ans = sub.get(text, stack)
                if isinstance(ans, Issue): count = 0
                else: ans, count = ans
                if address is not None: address.usedby(ans)
                if value is not None: value.usedby(ans)
                return ans, count

        assert address is None and value is None, text
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
    def get(cls, text, stack):
        ans = cls._cache_(cls.__known, stack, text)
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
    def get(cls, text, stack):
        ans = cls._cache_(cls.__known, stack, text)
        return ans

    __upclear = Issue.clear
    def clear(self):
        self.__upclear()
        MemoryChunk.disuse(self)

Issue.register(BadFree, lambda x: x == 'Invalid free() / delete / delete[] / realloc()')

class UnMapped (Issue):
    """Access to unmapped memory"""
    preamble = 'Access not within mapped region at address '
    @staticmethod
    def __parse(text,
                grab=re.compile(preamble + r'0x([0-9a-fA-F]+)').match):
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
    def get(cls, text, stack):
        addr = cls.__parse(text)
        ans = cls._cache_(cls.__known, stack, text, addr)
        return ans

    __upclear = Issue.clear
    def clear(self):
        self.__upclear()
        MemoryChunk.disuse(self)

Issue.register(UnMapped, lambda x: x.startswith(UnMapped.preamble))

class Origin (Issue):
    __upinit = Issue.__init__
    def __init__(self, stack, text):
        self.__upinit(stack, text)
        self.__users = set() # TODO: weakset

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

class NoValue (Origin):
    preamble = 'Uninitialised value was created by'
    @staticmethod
    def __parse(text,
                form=re.compile(preamble + r' a (heap|stack) allocation').match):
        it = form(text)
        if not it: raise ParseError('Malformed value provenance line', text, form)
        return it.group(1)

    __upinit = Origin.__init__
    def __init__(self, stack, text, source):
        self.__upinit(stack, text)
        self.source = source

    @classmethod
    def _active_(cls):
        for it in cls.__known.values(): yield it

    __known = {}
    @classmethod
    def get(cls, text, stack):
        source = cls.__parse(text)
        return cls._cache_(cls.__known, stack, text, source)

class MemoryChunk (Origin):
    preamble = 'Address'
    @staticmethod
    def __parse(text, asint=readint,
                intro=re.compile(preamble + r" 0x([0-9a-fA-F]+) is").match,
                stray=re.compile("not stack'd, malloc'd or " +
                                 r"\(recently\) free'd").match,
                line=re.compile(r"([0-9,]+) bytes (inside|after) a " +
                                r"block of size ([0-9,]+) (\w+)'d").match,
                thread=re.compile(r"on thread (\d+)'s stack").match):
        it = intro(text)
        if not it: raise ParseError('Malformed address description line', text)
        addr = it.group(1)
        text = text[it.end():].strip()

        it = line(text)
        if it:
            func = it.group(4)
            offset, size = [asint(x) for x in it.group(1, 3)]
            if it.group(2) == 'after': offset += size
            return func, size, offset, None, addr

        it = stray(text)
        if it: return None, None, None, None, addr

        it = thread(text)
        if it: return 'stack', None, None, asint(it.group(1)), addr

        raise ParseError('Malformed block description line', text)

    __upinit = Origin.__init__
    def __init__(self, stack, text, func=None):
        self.__upinit(stack or (), text)
        self.__eg = set() # TODO: weakset
        if func is not None: self.author = func

    def example(self, *what): self.__eg.add(what)
    @property
    def samples(self): return tuple(self.__eg)

    @classmethod
    def _active_(cls):
        for sub in set(cls.__subs.itervalues()):
            for it in sub._active_(): yield it
        for it in cls.__known.values(): yield it

    __subs = {}
    @classmethod
    def register(cls, sub, func):
        cls.__subs[func] = sub
        sub.author = func

    __known = {}
    @classmethod
    def get(cls, text, stack):
        func, size, off, tid, addr = cls.__parse(text)
        try: sub = cls.__subs[func]
        except KeyError:
            print 'Encountered unexpected MemoryChunk function:', func
            ans = cls._cache_(cls.__known, stack, text, func)
        else: ans = sub.get(text, stack)
        ans.example(addr, off, size, tid)
        return ans

Issue.register(MemoryChunk, lambda x: 0 <= x.find('bytes inside a block of size'))
Issue.register(MemoryChunk, lambda x: 0 <= x.find('bytes after a block of size'))
Issue.register(MemoryChunk, lambda x: 0 <= x.find("is not stack'd, malloc'd or (recently) free'd"))
Issue.register(MemoryChunk, lambda x: x.endswith("'s stack"))

class FMA (MemoryChunk):
    "Free memory access"
    @classmethod
    def _active_(cls):
        for it in cls.__known.values(): yield it

    __known = {}
    @classmethod
    def get(cls, text, stack):
        return cls._cache_(cls.__known, stack, text)
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
        return cls._cache_(cls.__known, stack, text)
MemoryChunk.register(UHR, 'alloc')

class USR (MemoryChunk):
    "Uninitialized stack access"
    @classmethod
    def _active_(cls):
        for it in cls.__known.values(): yield it

    __known = {}
    @classmethod
    def get(cls, text, stack):
        return cls._cache_(cls.__known, stack, text)
MemoryChunk.register(UHR, 'stack')

class SMA (MemoryChunk):
    "Stray memory access"

    @classmethod
    def _active_(cls):
        for it in cls.__known.values(): yield it

    __known = {}
    @classmethod
    def get(cls, text, stack):
        return cls._cache_(cls.__known, stack, text)
MemoryChunk.register(SMA, None)

class LeakBase (Issue): pass # Common base-class

class FDLeak (LeakBase):
    @staticmethod
    def __parse(text, asint=readint,
                front=re.compile(r'Open file descriptor (\d+):').match):
        it = front(text)
        if not it: raise ParseError('Unrecognised file descriptor block', text)
        return text[it.end():].strip(), asint(it.group(1))

    __upinit = LeakBase.__init__
    def __init__(self, stack, text, name, fd):
        self.__upinit(stack, text)
        self.fd, self.name = fd, name

    __known = {}
    @classmethod
    def get(cls, text, stack):
        name, fd = cls.__parse(text)
        return cls._cache_(cls.__known, stack, text, name, fd)

Issue.register(FDLeak, lambda x: x.startswith('Open file descriptor'))

class Leak (LeakBase):
    @staticmethod
    def __parse(text, asint=readint,
                direct=re.compile(r'([0-9,]+) bytes\s*').match,
                burden=re.compile(r'([0-9,]+) \(([0-9,]*) direct, ([0-9,]+) indirect\) bytes\s*').match,
                blocks=re.compile(r'in ([0-9,]+) blocks are\s*').match,
                record=re.compile(r'in loss record ([0-9,]+) of ([0-9,]+)').search):
        it = direct(text)
        if not it:
            it = burden(text)
            if not it: raise ParseError('No byte-total on leak-line', text)
            routes = [asint(x) for x in it.groups()]
            total, routes = routes[0], tuple(routes[1:])
            assert total == sum(routes), text
        else: routes = (asint(it.group(1)), 0)
        text = text[it.end():].strip()

        it = blocks(text)
        if not it: raise ParseError('No block-count on leak-line', text)
        count = asint(it.group(1))

        text = text[it.end():].strip()
        sure = text.startswith('definitely lost')

        it = record(text)
        if not it: raise ParseError('No loss record details in leak-line', text)
        index, total = [asint(x) for x in it.groups()]

        return sure, routes, count, index, total

    __upinit = LeakBase.__init__
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
            return cls(blocks, direct + indirect, 0, 0)
    del Namely

    @staticmethod
    def __size(sure, routes, blocks, gen=LeakSize.fromParsed):
        return gen(sure, blocks, *routes)
    del LeakSize

    @property
    def sure(self): return sum(self.size[2:]) > 0

    __upclear = LeakBase.clear
    def clear(self): # returns (maybe-leaked, direct-leaked, indirect-leaked, blocks)
        self.__upclear()
        return self.size

    __known = {}
    @classmethod
    def get(cls, text, stack):
        sure, routes, count, index, total = cls.__parse(text)
        return cls._cache_(cls.__known, stack, text, sure, routes, count, index), total

Issue.register(Leak, lambda x: 0 <= x.find('in loss record'))

# Package the data __muncher() gathers from one thread's output:
class Thread (object):
    def __init__(self, *parts):
        self.parts = parts
        # Alternating place-holders, for summaries, with tuples of issues.

    def issues(self):
        for part in self.parts:
            if isinstance(part, tuple):
                for it in part:
                    if isinstance(it, Issue):
                        yield it

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

class FDSummary (object):
    def __init__(self, text): self.text = text

class FinalSummary (object):
    def __init__(self, err, ctx, serr=0, sctx=0):
        self.reports = err, ctx
        self.suppressed = serr, sctx

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
    def __frame_out(source, frame, dump):
        saved = None
        for it in source:
            if frame in it.stack:
                leak = it.clear()
                if saved is None: saved = leak
                elif leak is not None: saved += leak
                dump.add(it)

        return saved

    def __ditch(self, frame, dump, leak=True):
        if isinstance(leak, Issue): leak = isinstance(leak, LeakBase)
        return self.__frame_out(self.leaks if leak else self.issues, frame, dump)

    def repair(self, frame, leak=True):
        return self.__ditch(frame, self.fixed, leak)

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
                      head=(re.compile(r'Copyright (C) \d+-\d+.*').match,
                            re.compile(r'Using Valgrind.*').match),
                      cmd=re.compile(r'Command: (.*)').match,
                      parent=re.compile(r'Parent PID: (\d+)').match):
        try:
            for it in head:
                p, n, line = yield
                assert p == pid
                it = it(line)
                if it is None: break
            else:
                p, n, line = yield
                assert p == pid

            it = cmd(line)
            if it:
                command = it.group(1)
                p, n, line = yield
                assert p == pid
            else: command = None

            it = parent(line)
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
    def __parseblocks(dest, mode, command,
                      died=re.compile('Process terminating with default action of '
                                      r'signal ([0-9]+) \(([A-Z0-9]+)\)').match,
                      # Cruft:
                      fore=re.compile(r'\d+ errors in context \d+ of \d+').match,
                      thread=re.compile(r'Thread \d+:$').match):
        items = []
        stanza = addr = value = count = terminal = signal = None

        while True:
            n, line = yield
            if line and all(x.isupper() for x in line.split(None)[:2]): break

            try:
                if not line: # end of stanza
                    if addr or stanza or value:
                        if stack: stack = Stack.get(stack)
                        else: stack = None

                    if value:
                        assert stanza or addr, line
                        value = NoValue.get(value, stack)
                        if isinstance(prior[-1], tuple):
                            addr, stack, prior = prior
                        else:
                            assert not addr
                            stack, mode = prior
                            del prior

                    if addr:
                        assert stanza, line
                        addr = MemoryChunk.get(addr, stack)
                        items.append(addr)
                        stack, mode = prior # restore normal parsing
                        del prior

                    if stanza:
                        block, m = Issue.get(stanza, stack, addr, value)
                        if count is None: count = m
                        else: assert count == m, line
                        items.append(block)

                        if terminal:
                            assert isinstance(terminal, basestring), line
                            terminal, signal = Terminal(terminal, signal, block, addr), None

                        value = addr = stanza = None
                        del stack
                    # else: more than one blank line

                elif thread(line) or fore(line): pass # ignore cruft.
                elif line.startswith('Process terminating'):
                    assert terminal is None, line
                    terminal = line
                    it = died(line)
                    if not it: raise ParseError('Unfamiliar termination', line, lineno=n)
                    signal = int(it.group(1)), it.group(2)

                elif line.startswith(NoValue.preamble):
                    assert value is None and (stanza or addr), line
                    if addr:
                        mode = prior[-1]
                        prior = addr, Stack.get(stack), prior
                    else: prior = Stack.get(stack), mode
                    value, stack, mode = line, [], 'Source of value for ' + mode

                elif line.startswith(MemoryChunk.preamble):
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

            except ParseError as what:
                what.lineno = n
                raise

        try: stack
        except NameError: pass
        else: raise ParseError('Missing blank line before', line, lineno=n)
        assert stanza is None

        dest[:] = [ tuple(items), terminal, line ]

    def traffic(dest, asint=readint,
                inuse=re.compile(r'in use at exit: ([0-9,]+) bytes in ([0-9,]+) blocks').match,
                total=re.compile(r'total heap usage: ([0-9,]+) allocs, ([0-9,]+) frees, ([0-9,]+) bytes allocated').match):
        assert not dest[0]
        try:
            n, line = yield
            it = inuse(line)
            if it: lost, block = [asint(x) for x in it.groups()]
            else: raise ParseError('Failed to parse heap-in-use summary', line, lineno=n)

            n, line = yield
        except GeneratorExit:
            raise ParseError('Incomplete, missing or unterminated heap summary',
                             lineno=n)

        it = total(line)
        if it: grab, free, churn = [asint(x) for x in it.groups()]
        else: raise ParseError('Failed to parse heap traffic totals', line, lineno=n)

        dest[:] = [ Traffic(lost, block, grab, free, churn) ]

    def leaksum(text, n, prefix, # tool function for __parseleaksummary
                asint=readint,
                chunk=re.compile(r'([0-9,]+) bytes in ([0-9,]+) blocks').search):
        if text.startswith(prefix + ': '):
            it = chunk(text)
            if not it: raise ParseError('Malformed "%s" line' % prefix, text, lineno=n)
            return tuple(asint(x) for x in it.groups())
        return None

    def leaksummary(dest, getsum=leaksum,
                    prefixes=('definitely lost', 'indirectly lost',
                              'possibly lost', 'still reachable', 'suppressed'),
                    cruft=('Reachable blocks (those to which a pointer was found) are not shown.',
                           'To see them, rerun with: --leak-check=full --show-reachable=yes',
                           'To see them, rerun with: --leak-check=full --show-leak-kinds=all')):
        assert not dest[0]
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
    def filedescribe(dest):
        dest[:] = [ FDSummary(dest) ]
        raise StopIteration
        yield # to make this be a generator !

    def parsetail(dest,
                  errs=re.compile(r'(\d+) errors from (\d+) contexts\s*').match,
                  skip=re.compile(r'\(suppressed: (\d+) from (\d+)\)').match):
        line = dest[0].strip()
        it = errs(line)
        if not it:
            raise ParseError('Failed to parse final error summary', line, lineno=n)
        data = it.groups()
        line = line[it.end():].strip()
        it = skip(line)
        if line and not it:
            raise ParseError('Unrecognised tail for final summary', line, lineno=n)
        data = [int(x) for x in data + it.groups()]

        dest[:] = [ FinalSummary(*data) ]
        raise StopIteration
        yield # to make this be a generator !

    @classmethod
    def __muncher(cls, fatal, bok, pid, command,
                  handler={ 'HEAP SUMMARY': ('leak stack-frame', traffic),
                            'LEAK SUMMARY': ('error stack-frame', leaksummary),
                            'FILE DESCRIPTORS': ('files', filedescribe),
                            'ERROR SUMMARY': ('tail', parsetail)}):
        """Consumes the content for one pid.

        FIXME: cope with 'FILE DESCRIPTORS: %d open at exit' stanza
        before HEAP SUMMARY.  In any case, the __parseblocks()
        approach is wrong, presuming we know what line shall follow
        it.  This depends what reports we've got; so we need to
        recognise which summary block ended whatever came before.\n"""

        mode, result, loop = 'issue stack-frame', [], True
        while loop:
            dest = []
            if mode == 'tail': loop, mode = False, 'error stack-frame'
            munch = cls.__parseblocks(dest, mode, command)
            munch.next()
            try:
                while True: munch.send((yield))
            except GeneratorExit:
                munch.close()
                if loop: raise
            except StopIteration: pass
            issues, dead, line = dest
            result.append(issues)
            if dead is not None: fatal.add(dead)

            key, tail = line.split(':', 1)
            try: mode, block = handler[key]
            except KeyError:
                raise ParseError('Unrecognised section terminator', key, tail)

            dest = [ tail ]
            munch = block(dest)
            try:
                munch.next()
                while True: munch.send((yield))
            except GeneratorExit:
                munch.close()
                raise
            except ParseError:
                # Duplicate ERROR SUMMARY line before error details is spurious
                if loop and mode == 'tail':
                    assert not result[-1][0] and dead is None
                    result.pop()
                else: raise
            except StopIteration: pass
            assert(len(dest) == 1)
            result.append(dest[0])

        bok[pid] = Thread(*result)

    del traffic, leaksummary, filedescribe, parsetail

    def waffle(line, # tool function for byline
               flood=re.compile(r'More than (100|1000 different) errors detected\.').match,
               boss=re.compile(r'TO (CONTROL|DEBUG) THIS PROCESS USING').match,
               gc=re.compile(r'Searching for pointers to \S+ not-freed blocks').match,
               embed=re.compile(r'embedded gdbserver:\s+').match,
               burble=('For counts of detected and suppressed errors, rerun with: -v',
                       'Use --track-origins=yes to see where uninitialised values come from'),
               # Discard everything up to (but not including) the next blank line:
               untilempty=lambda k: k or None):
        """Decides what lines to simply ignore.

        Returns None for lines to not ignore.  Otherwise, returns a
        callable; call this on each line after the one that returned
        it; if the return is true, ignore the line and continue using
        the callable on later lines; if false, discard the callable.
        A return of None means the block of waffle actually ended on
        the preceding line, so the present line should be examined as
        a candidate for use (but might be fresh waffle); any other
        false return means the line given was the last line of the
        waffle.\n"""
        # Single-line cruft:
        if line in burble or embed(line): return lambda k: None

        # Blocks of cruft:
        if boss(line) or gc(line):
            return untilempty

        it = flood(line)
        if it:
            if it.group(1) == '100':
                return lambda k: not k.endswith('less detail than before')
            return untilempty

        # Not cruft:
        return None

    def numbered(fd): # tool function for byline
        n = 0 # line number
        while True:
            line = fd.readline()
            if not line: break # end of file
            n += 1
            yield n, line

    def byline(fd, # tool function for ingest
               each=numbered, ignore=waffle,
               front=re.compile(r'==([0-9]+)==\s+').match,
               # Valgrind burbling about how it's been configured:
               skip=re.compile(r'--[0-9]+--\s+').match,
               # Stray output from cfengine itself:
               shrug=re.compile(r'\d+-\d+-\d+T\d+:\d+:\d+-\d+\s+').match):
        lines, stem, wait = each(fd), None, None
        for n, line in lines:
            if stem and line.startswith(stem): pass
            elif skip(line) or shrug(line): continue
            else:
                it = front(line)
                if not it:
                    raise ParseError("Line doesn't start with expected prefix",
                                     line, lineno=n)
                stem, proc = it.group(0), int(it.group(1))
                off = len(stem)
            line = line[off:].strip()

            # See waffle()'s doc-string:
            if wait is None: wait, it = ignore(line), True
            else:
                it = wait(line)
                if it is None: wait, it = ignore(line), True

            if wait is None: yield proc, n, line
            if not it: wait = None
    del numbered, waffle

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
        partial[pid] = munch = cls.__muncher(fatal, final, pid, command)
        munch.next()
        for p, n, line in src:
            if p != pid:
                try: munch = partial[p]
                except KeyError:
                    assert not final.has_key(p), line
                    munch = partial[p] = cls.__muncher(fatal, final, p, command)
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
            try: munch.send((n, ''))
            except StopIteration: pass
            else: munch.close()

        return command, ppid, fatal, final

    del byline

    # TODO: also support chowing down on a whole directory, optionally
    # with glob to limit selection of files within it.
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
                except ParseError as what:
                    what.filename = log
                    raise
            finally: fd.close()

            threads = []
            for pid, thread in final.items():
                for it in thread.issues():
                    if isinstance(it, LeakBase): self.leaks.add(it)
                    else: self.issues.add(it)
                threads.append(thread)
            ans.append(Report(cmd, ppid, dead, tuple(threads)))
            for term in dead:
                self.fatal.add(term.error)
        return tuple(ans)

del re, readint
