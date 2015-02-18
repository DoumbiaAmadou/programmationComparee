OUTPUT=`racket -f helloworld.lisp`
echo 'echo '$OUTPUT > helloworld
chmod +x helloworld
