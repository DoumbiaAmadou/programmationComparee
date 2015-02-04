#! /bin/bash -e

#       This is the compile.sh for our C64 BASIC 'ed'. Since we can't compile
#       it to machine code, we'll use an interpreter called `cbmbasic`. We'll
#       try to use an existing version if it exists,

BASIC=

#       or install another if not. We then have to use a script wrapper to
#       start our 'ed' with one command, as requested by our master teacher,
#       YRG. To find the BASIC source, we need to know where it's located,
#       using the current directory,

DIR=$( cd "$(dirname "${BASH_SOURCE[0]}")" && pwd )

#       which is the same as the source code, and the source file.

SOURCE="$DIR/ed-source.bas"

#       those will be used to create our target, 'ed'.

TARGET=ed

#       We need to find a BASIC interpreter, and we'll use a function here to
#       try multiple ways and return from it if one of them works.

find_basic() {

#       If no other way work we'll install one in the user $HOME directory.

  local installdir="$HOME/.c64basic"
  local installbin="$installdir/cbmbasic"

#       This will allow us to reuse it if the user "compiles" our software
#       multiple times.

#       But first, let's try to find the interpreter. It might be already
#       available on the local machine.

  echo -n "Checking for a C64 BASIC interpreter... "

#       We check an executables list, which for now only contains 'cbmbasic',
#       the only one interpreter known to the author of this text.
#       For each one of these,

  for exe in cbmbasic; do

#       we check if a command with its name exists,

    which $exe >/dev/null

#       and if it does indeed exist,

    if [ x"$?" = x0 ]; then

#       we remember it,

      BASIC="$exe"

#       inform the user of our success,

      echo "$BASIC"

#       and return from the function.

      return
    fi

#       If not, and if there are more executables in the list, we check the
#       next one.

  done

#       If we still haven't got anything at the end of the list, we check if we
#       previously installed the interpreter,

  if [ -x "$installbin" ]; then

#       and if so, we remember it,

    BASIC="$installbin"

#       inform the user of our success,

    echo "$BASIC"

#       and... guess what? Return from the function.

    return
  fi

#       Our last solution if we still have nothing is to first inform the user
#       of our failure (or their?),

  echo "nope"

#       and tell them not to worry, because we'll install one for them.

  echo -n "Installing an interpreter..."

#       We download it from mist64 on GitHub, who ported the interpreter for
#       modern architectures. The code is a hell to read, its author should
#       come at our PComp class.

  wget --quiet https://github.com/mist64/cbmbasic/archive/master.zip

#       The code comes as a Zip archive. We assumes our user can handle these
#       files, and we unzip it

  unzip master.zip

#       and move it in our install directory, removing it first if it already
#       exist to avoid conflicts.

  [ -d "$installdir" ] && rm -rf "$installdir"
  mv cbmbasic-master "$installdir"

#       We can then jump in this directory,

  pushd "$installdir" 2>/dev/null

#       compile the code, again assuming here that our user have all the
#       necessary tools,

  make

#       and come back to where we were.

  popd 2>/dev/null

#       If we were successful, we should have an executable binary,

  if [ -x "$installbin" ]; then

#       which we can remember and

    BASIC="$installbin"

#       inform the user about

    echo "Installed: $BASIC"

#       before returning from the function.

    return
  fi

#       If not, there's an error somewhere but we don't know where. We should
#       have written this script in OCaml, its type system  would have saved us
#       from all this crap.
#       Anyway, we kindly inform the user

  echo
  echo "Can't install an interpreter. Please add a 'cbmbasic' in your PATH"

#       and exit with an error code.

  exit 1

#       This is the end of our function,

}

#       we can now run it.

find_basic

#       If we're here now, it means we have a working interpreter and are now
#       able to build our script wrapper.
#
#       We'll first removed any existing target,

rm -f $TARGET

#       and create a new one with a path to the Bash interpreter in its shebang
#       to tell the shell to run it with it when it'll be executable.

echo "#!$(which bash) -e"          >> $TARGET

#       Since we have to provide a target which takes the infile and outfile as
#       argument and our interpreter doesn't handle that, we cheat.
#
#
#                     $ ./ed input.txt output.txt
#
#                Fig. 1 -- How our target need to look like
#
#
#       Our program will read two inputs before really starting, the first one
#       being the input file and the second one the output one. Our script
#       wrapper will then concatenate a fabricated input (the two file paths)
#       and the user input. It first need to create the fabricated input, which
#       will be put in a temporary file.

echo 'in=.c64-$RANDOM'             >> $TARGET

#       This file will contain one path per line.

echo 'echo $1 >  $in'              >> $TARGET
echo 'echo $2 >> $in'              >> $TARGET

#       We can then use `cat` to concatenate both inputs and pipe it to our
#       interpreted program,

echo "cat \$in - | $BASIC $SOURCE" >> $TARGET

#        and remove the temporary file,

echo 'rm $in'                      >> $TARGET

#        before exiting.
#        This wrapper script should be executable.

chmod +x $TARGET

#
#                                  ~ The End ~
#
