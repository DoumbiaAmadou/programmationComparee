#! /usr/bin/env python
# coding=utf-8

from __future__ import print_function
from collections import defaultdict
from sys import argv

def sorted_string(string):
	str_list = list(string)
	return "".join(sorted(str_list))

def anagrams(word, anagrams_dict):
	return anagrams_dict[sorted_string(word)]

with open('../words', 'r') as words:
	anagrams_dict = defaultdict(list)
	for word in words:
		stripped_word = word.strip()
		anagrams_dict[sorted_string(stripped_word)].append(stripped_word)
			

for arg in argv[1:]:
	print(arg+":"," ".join(anagrams(arg, anagrams_dict)))