#!/bin/bash
python3 script/main.py clean
git add *
git commit
git push
dune build
