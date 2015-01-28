#! /usr/bin/env python
# -*- coding: UTF-8 -*-

from collections import defaultdict
from sys import argv

sort_elt = lambda e : "".join(sorted(e))

def get_anagram(w, words):
    l = []
    s = sort_elt(w)
    for e in words:
        if sort_elt(e) == s:
            l.append(e)
    return l

with open("../words", "r") as file_words:
    words = defaultdict(list)
    for w in file_words:
        w = w.strip()
        words[len(w)].append(w)

for arg in argv[1:]:
    print "%s: %s" % (arg, " ".join(get_anagram(arg, words[len(arg)])))
