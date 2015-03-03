#ici détection des dépendances (java, scala, sbt, sbt eclipse)

#création des répertoires
mkdir -p src/main/scala src/main/java src/test/java src/test/scala
mkdir -p src/main/ressources src/test/ressources

#teste de la config
mkdir -p src/main/scala/hello
echo 'object Hi { def main(args: Array[String]) = println("Hi!") }' > ./src/main/scala/hello/hw.scala

sbt "run-main Hi"

echo "si le message Hi! est apparus la configuration est bonne"
