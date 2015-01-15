# coding=utf-8
from collections import defaultdict

def is_anagram(str1, str2):
    str1_list = list(str1)
    str1_list.sort()
    str2_list = list(str2)
    str2_list.sort()

    return (str1_list == str2_list)

with open('../words', 'r') as f:
	words = defaultdict(list)
	for l in f:
		l = l.strip()
		words[sorted(l)].append(l)

for word in words:
	d[word] = 0
	print word": "dict[word]

