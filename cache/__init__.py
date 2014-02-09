"""Assorted generic cache functionality.

Provides:
  lockdir -- directory locking used by whole
  mapping -- a lazily-populated dictionary, LazyDict
  property -- cached attributes, computed on depand
  weak -- weakly remembering things you can compute at will
  whole -- saving, to disk, data about integer-bounded ranges of the number line

See study.LICENSE for copyright and license information.
"""
import lockdir
# import mapping # cycle with study.snake.sequence
import property
#import weak # cycle with study.snake.sequence.ReadOnlySeq
#import whole # not until it's stable ...
