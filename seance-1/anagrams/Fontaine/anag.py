#! /usr/bin/env python
# -*- coding: UTF-8 -*-
from collections import defaultdict
from sys import argv

# helpers

word_hash = lambda w: "".join(sorted(w))

def get_anagrams(w, bag):
    h = word_hash(w)
    return bag[h]

# words pre-processing

with open("../words", "r") as f:
    words = defaultdict(list)
    for l in f:
        l = l.strip()
        words[word_hash(l)].append(l.strip())

# args processing

for arg in argv[1:]:
    print "%s: %s" % (arg, " ".join(get_anagrams(arg, words)))
