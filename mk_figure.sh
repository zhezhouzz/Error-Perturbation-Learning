#!/bin/bash

# Motivation Section.
# make figure: distribution over perturbation functions.
# make data and save to ".moti" file
dune exec -- main/main.exe moti config/config.json data/motivation/pf_enum.ml data/motivation/assertion1.ml 4 100 save
# do bfs from the maximal perturbation functions and save to ".search" file
dune exec -- main/main.exe moti-search config/config.json bfs max
# show the figure
python3 tool/moti_figure.py .search
