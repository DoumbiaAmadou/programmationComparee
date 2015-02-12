#! /bin/bash

T=hello
DIR=$( cd "$(dirname "${BASH_SOURCE[0]}")" && pwd )

echo "#! $(which bash)" >  $T
echo "clj ${DIR}/fontaine.clj" >> $T
chmod u+x $T
