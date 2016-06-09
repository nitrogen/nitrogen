#!/bin/sh

# This is quite possibly the hackiest but shortest solution to fix the fact
# that standard erlang indentation does not work quite so well with nested
# Nitrogen elements. This ensures that the indentation used for files with
# ft=nitrogen modeline in files will be overridden with more sensible nitrogen
# indentation while allowing standard erlang files to retain the usual
# indentation rules.

cat nitrogen.vim >> ~/.vimrc
