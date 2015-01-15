#!/bin/bash
echo "toto 1"

exec anagram toto chien > ret

ret = 'exec anagram toto chien'

echo $ret

if [[$ret=="^toto:$^chien[(chien niche)(niche chien)]$"]] then echo "OK" else echo "KO"

echo "toto"
