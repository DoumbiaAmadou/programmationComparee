#! /bin/bash -e

DIR=$( cd "$(dirname "${BASH_SOURCE[0]}")" && pwd )
JAR="${DIR}/target/fontaine-0.1.0-SNAPSHOT-standalone.jar"
TARGET=anagrams

lein uberjar
echo "#! $(which bash)"         >| $TARGET
echo "java -jar \"${JAR}\" \$*" >> $TARGET
chmod +x $TARGET
