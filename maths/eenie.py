"""Modelling the 'eenie, meenie, miney mo' game.

See study.LICENSE for copyright and license information.
"""

Theory = """A selection game.

The game has a rhythmic sequence of words (usually a rhyme, not
necessarily making much sense) used to eliminate some members of a
group until only one, or some other predetermined number, of the group
remain.  The group form a loop and someone (typically one of the
group) recites the rhyme, pointing to successive members of the group
on the beat of the rhythm of the words.  (That may be one per word, or
one per sylable, for example.)  The reciter choses who to start at
(and, thereby, in principle, the final outcome) and works round to the
end of the sequence; whoever they point to as they speak the last beat
is eliminated.  They then restart the sequence at the next person
round the loop, skipping the eliminated person, and repeat the process
to eliminate whoever the sequence now ends on.  This continues until
the group is reduced to the target size.

They rhythmic sequence of words has some number N of beats.  In a
round with i members in the group, the elimination point advances N %
i steps round the loop.  (The first round has a nominal previous in
which a space, just before the player first pointed to, has been
vacated.)  The loop then shrinks by one and another round is played.
"""

def meenie(seq, beats, stop=1, offset=0):
    """Select from seq based on a rhyme with a given number of beats.

    First argument, seq, is a sequence (that must support seq[:i] +
    seq[i+1:] as a way to drop an entry) to be reduced by the game.
    Second argument, beats, is the number of beats in the traditional
    recitation of the selected rhythmic sequence of words.

    Optional third argument, stop, is the value of len(seq) at which
    to stop and return what remains.  Final argument, offset, is the
    offset into seq at which to start the first round.  (This is
    equivalent to passing seq[offset:] +seq[:offset] in place of seq,
    aside from the order of the entries in the returned residue.)  The
    first round starts at the given offset; each round advances offset
    by beats - 1 (since its first beat points at the individual at the
    given offset when it started, and we stop on the last beat),
    reduces it modulo len(seq) and eliminates the entry at the
    resulting offset.

    Returns what remains after entries have been eliminated to make
    len(seq) <= stop."""

    while len(seq) > stop:
        offset = (offset +beats -1) % len(seq)
        seq = seq[:offset] +seq[offset+1:]

    return seq
