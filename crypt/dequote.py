#!/usr/bin/python
"""Demangler for =-codes in e-mails.

Contents:

  decode(infile, outfile) -- does decoding
  main(programname, args, logstream) -- command-line interface

$Id: dequote.py,v 1.2 2007-12-02 21:06:07 eddy Exp $
"""
import string

def decode(inf, out):
    """Reads inf, decodes what it reads, writes the results to out.

    Arguments: two streams, decode(input, output).  The input stream must
    support readline(), the output stream write().

    Decoding:

      =aa -- with aa some two-digit hex number, standing for the 
      character with the indicated integer value

      = at end of line -- the = and the end of line are skipped

      anything else -- itself

    I think that's what the =-encoding needs, anyway ;^>
    """

    while 1>0:
	line = inf.readline()
	if not line: break

	while '=' in line:
	    at = string.index(line, '=')
	    out.write(line[:at])
	    line = line[1+at:]
	    if line[0] == '\n': line = line[1:]
	    else:
		out.write(chr(string.atoi(line[:2], 16)))
		line = line[2:]

	out.write(line)

def main(myname, args, log):
    """Command-line interface.

    Usage: myname infile outfile

    Command-line is read from left to right; when a -i is encountered, the next
    argument names infile; after -o, outfile is found likewise; unannounced
    arguments (neither -i nor -o, nor yet something found by either of these)
    are taken as infile unless an (unused) infile is currently specified, in
    which case the unannounced argument is taken as outfile; whenever (unused)
    values for infile and outfile are available, the infile is read, outfile
    written and both forgotten, resetting the process.  No check is made for
    dangling - `myname infile` won't raise an error.

    Use of a lone hyphen as either infile or outfile is read as stdin or
    stdout as appropriate; for pipe-line use, | myname - - | will serve.
    """

    import sys, os
    src = dst = None

    try:
	while args:

	    if args[0] == '-i':
		if args[1:]: src = args[1]
		else: raise SyntaxError, 'No input file name given with -i'
		args = args[2:]

	    elif args[0] == '-o':
		if args[1:]: dst = args[1]
		else: raise SyntaxError, 'No output file name given with -o'
		args = args[2:]

	    elif src:
		dst = args[0]
		args = args[1:]

	    else:
		src = args[0]
		args = args[1:]

	    if src and dst:

		if src == '-': src = sys.stdin
		else: src = open(src, 'r')

		if dst == '-': dst = sys.stdout
		else: dst = open(dst, 'w')

		decode(src, dst)
		if src != sys.stdin: src.close()
		if dst != sys.stdout: dst.close()
		src = dst = None

    except SyntaxError, what:
	log.write('What ? ' + what + '\n')
	return 1

    except IOError, (n, reason):
	log.write('IO %d: ' % n + reason + '\n')
	return 2
