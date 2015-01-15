#!/bin/sh
fsc Anagram.scala
echo "#!/bin/sh" > anagram
echo "scala Anagram \$*" > anagram
chmod +x anagram
