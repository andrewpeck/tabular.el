#!/usr/bin/env sh
:; ( echo "$EMACS" | grep -q "term" ) && EMACS=emacs || EMACS=${EMACS:-emacs} # -*-emacs-lisp-*-
:; command -v $EMACS >/dev/null || { >&2 echo "Can't find emacs in your PATH"; exit 1; }
:; exec emacs -Q --script "$0" -- "$@"
:; exit 0
;; -*- lexical-binding: t -*-

(add-to-list 'load-path "/home/andy/.emacs.d/.local/straight/repos/dash.el/")
(require 'dash)

(load "/home/andy/Sync/dotfiles/doom.d/lisp/tabular/tabular.el")
(load "/home/andy/Sync/dotfiles/doom.d/lisp/tabular/tests/tabular-test.el")
(ert-run-tests-batch-and-exit)
