# key-chord-count

An utility to count 'chord' frequencies in files. A 'chord' is an unordered
pair of lower-case alphabetical characters, e.g., `az`, `ys` and so on.

It is useful for [key-chord.el](http://www.emacswiki.org/emacs/KeyChord). Less
used chords in your texts or programs are good candidates for key bindings.

## Installation

```
% cabal install
% key-chord-count [FILES] | sort -n | head
```

## License

see LICENSE
