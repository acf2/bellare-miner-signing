#!/bin/bash
sbcl --noinform \
     --load $HOME/Lisp/quicklisp/setup.lisp \
     --load `dirname $0`/load.lisp \
     --script `dirname $0`/cli.lisp \
     $@