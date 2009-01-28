"""Assorted generic cache functionality.

Provides:
  lockdir -- directory locking used by whole
  mapping -- a lazily-populated dictionary, LazyDict
  property -- cached attributes, computed on depand
  weak -- weakly remembering things you can compute at will
  whole -- saving, to disk, data about integer-bounded ranges of the number line

$Id: __init__.py,v 1.3 2009-01-28 08:37:53 eddy Exp $
"""
import lockdir
import mapping
import property
import weak
import whole
