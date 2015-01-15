#! /usr/bin/env python
# coding=utf-8

from __future__ import print_function
from collections import defaultdict
from sys import argv

def sorted_string(string):
	str_list = list(string)
	return "".join(sorted(str_list))

def anagrams(word, words):
	return words[sorted_string(word)]

with open('../words', 'r') as f:
	words = defaultdict(list)
	for l in f:
		l = l.strip()
		words[sorted_string(l)].append(l)
			

for arg in argv[1:]:
	print(arg+":"," ".join(anagrams(arg, words)))