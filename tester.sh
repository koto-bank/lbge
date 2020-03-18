#!/usr/bin/env bash
# this script assumes sbcl and that quicklisp is/will be installed to $HOME/quicklisp
INSTDIR="$HOME/quicklisp"
DESTLINK="$INSTDIR/local-projects/lbge"
RESP=$(echo '(if (member :quicklisp *features*) 1 0)' | sbcl --noinform)
if [[ $RESP =~ 0 ]]; then
  echo "Quicklisp not installed. Install?[yn]"
  read -p "Are you sure? " -n 1 -r
  if [[ $REPLY =~ ^[Yy]$ ]]; then
    curl 'https://beta.quicklisp.org/quicklisp.lisp' | sbcl
  fi
fi

if [ ! -L "$DESTLINK" ]; then
   ln -s "$PWD" "$DESTLINK"
fi

sbcl --noinform <<LISP
(ql:quickload "lbge-unit-tests")
(lbge-unit-tests:run)
(exit)
LISP
