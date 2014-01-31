"""Parsing valgrind memcheck logs.
"""

import re

class Source (object):
    def __init__(self, sfile, line=None):
        self.source = sfile
        if line is not None: self.line = line
        self.frames = set()

    def __hash__(self):
        ans = hash(self.source)
        try: line = self.line
        except AttributeError: pass
        else: ans = ans ^ hash(line)
        return ans

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
                locat=re.compile(r'\b(by|at)\s+0x(\h+):\s+(\w+\|\?\?\?)\b\s*'),
                inlib=re.compile(r'\s*\(in ([^)]*)\)'),
                sause=re.compile(r'\s*\(([^)]*):(\d+)\)')):

        place = locat.search(text)
        if not place: raise ValueError('No (at|by) stack-frame data', text)
        leaf = place.group(1) == 'at'
        addr, func = place.group(2, 3)
        if func == '???': func = None
        tail = text[match.end():]

        src = inlib.match(tail)
        if src is None:
            src = sause.match(tail)
            if src is None: raise ValueError('No source location found', tail, text)
            src = Source.get(src.match(1), int(src.match(2)))
        else:
            src = Source.get(src.match(1))

        return leaf, addr, func, src

    def __init__(self, text, leaf, addr, func, source):
        source.frames.add(self)
        self.text, self.leaf = text, leaf
        self.addr, self.func, self.source = addr, func, source
        self.stacks = set()

    def __hash__(self):
        return hash(self.leaf) ^ hash(self.addr) ^ hash(self.func) ^ hash(self.source)

    __known = {}
    @classmethod
    def get(cls, text):
        key = cls.__parse(text)
        try: ans = self.__known[key]
        except KeyError:
            ans = self.__known[key] = cls(text, *key)
        return ans

class MemCheck (object):
    def __init__(self):
        pass

    def byline(fd, stem): # tool function for __ingest
        n = 1 # we got stem off the first line.
        off = len(stem)
        while True:
            line = fd.readline()
            if not line: break
            n += 1
            if line.startswith(stem): yield n, line[off:].strip()
            else:
                raise ValueError("Line doesn't start with expected prefix",
                                 line, n, stem)

    @staticmethod
    def __ingest(self, fd,
                 reader=byline,
                 first=re.compile(r'(.* )Memcheck, a memory error detector'),
                 head=(re.compile(r'Copyright (C) \d+-\d+.*'),
                       re.compile(r'Using Valgrind.*')),
                 cmd=re.compile(r'Command: (.*)'),
                 parent=re.compile(r'Parent PID: (\d+)'),
                 heap='HEAP SUMMARY:',
                 leak='LEAK SUMMARY:'):
        line = fd.readline()
        stem = first.match(line)
        if not stem: raise ValueError('Unfamiliar first line', line)
        src = reader(fd, stem.group(1))
        try:
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
        except StopIteration:
            raise ValueError('No blank line after header', n)

        issues, leaks, stanza = [], [], None
        for n, line in src:
            if line == heap: break

        for n, line in src:
            pass

        return Report(command, ppid, issues, leaks)

    def ingest(self, logfile):
        fd = open(logfile)
        try: log, grmbl, leak = self.__ingest(fd)
        finally: fd.close()
