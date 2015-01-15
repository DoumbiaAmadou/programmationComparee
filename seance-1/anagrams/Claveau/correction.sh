#!/bin/bash
ret = ""
exec anagram toto chien > ret
if $ret=="^toto:$^chien[(chien niche)(niche chien)]" then echo "OK" else echo "KO"
