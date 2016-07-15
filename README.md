# MobileDeviceKeyboard
This repository provides a scala interface for suggesting words based on an input prefix.

Building this project requires sbt which can be downloaded
[here](http://www.scala-sbt.org/0.13/docs/Setup.html).

To build, from the project directory execute
```bash
$ sbt
> compile
```



The application uses an input stream of words to build a vocabulary. Using this vocabulary, the
application suggests a word completion based on any given input String. One naive approach to
achieving this behavior is to map all valid word prefixes (e.g. "ca" is a valid prefix of "cat"
and "cow") to the words they represent. However, this model creates redundancy, for example, "c" and
"ca" both map to "cat", but the letter "c" must be stored twice.

Instead, in the current approach Each word is represented as a
path through a Left Child Right Sibling binary tree. For example:

```
Input <- "cat cow dog pig"

    c ------- d ------- p
   /         /         /
  a -- o    o         i
 /    /    /         /
t    w    g         g
```
Using the LCRS structure, redundant prefixes are eliminated; the valid words of a prefix are the words
formed by the prefix itself and its children (i.e. left neighbor).


