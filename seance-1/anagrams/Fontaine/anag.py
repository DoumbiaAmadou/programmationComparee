#! /usr/bin/env python
# -*- coding: UTF-8 -*-
from collections import defaultdict
from sys import argv

# helpers

def word_hash(w):
    return "".join(sorted(w))

def get_anagrams(w, bag):
    return bag[word_hash(w)]

# words pre-processing

with open("../words", "r") as f:
    words = defaultdict(list)
    for l in f:
        l = l.strip()
        words[word_hash(l)].append(l)

# args processing

for arg in argv[1:]:
    print "%s: %s" % (arg, " ".join(get_anagrams(arg, words)))
