#! /bin/bash

just build system

fswatch -o system.tex | while read -r _ ; \
  do \
    just build system
  done