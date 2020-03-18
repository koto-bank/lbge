#!/bin/sh
# this script assumes sbcl and that quicklisp is/will be installed to $HOME/quicklisp
INSTDIR="$HOME/quicklisp"
DESTLINK="$INSTDIR/local-projects/lbge"
sbcl --noinform <<LISP
(sb-ext::quit :unix-status
              (if (member :quicklisp *features*)
                  0
                  1))
LISP
if [ $? -eq 1 ]; then
  read -p "Quicklisp not installed. Install?[yn]" -n 1 -r
  if [ $REPLY = 'y' -o $REPLY = 'Y' ]; then
    curl 'https://beta.quicklisp.org/quicklisp.lisp' | sbcl
  fi
fi

if [ ! -L "$DESTLINK" ]; then
   ln -s "$PWD/../.." "$DESTLINK"
fi

sbcl --noinform <<LISP
(ql:quickload "lbge-unit-tests")
(lbge-unit-tests:run)
(exit)
LISP
