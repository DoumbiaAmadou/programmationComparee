#! /usr/bin/env python
# -*- coding: UTF-8 -*-

from collections import defaultdict
from sys import argv

sort_elt = lambda e : "".join(sorted(e))

def get_anagram(w, dic):
    h = len(w)
    l = dic[h]
    n_l = []
    if w in l:
        l.remove(w)
    s = sort_elt(w)
    for e in l:
        sort_e = sort_elt(e)
        if sort_e == s:
            n_l.append(e)
    return n_l

with open("../words", "r") as file_words:
    words = defaultdict(list)
    for w in file_words:
        w = w.strip()
        words[len(w)].append(w)

for arg in argv[1:]:
    print "%s : %s" % (arg, " ".join(get_anagram(arg, words)))
