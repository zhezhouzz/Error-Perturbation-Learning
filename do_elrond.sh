#!/bin/bash

host_dir=~/elrond/
local_dir=~/.elrond
name=$1
mkdir $host_dir
mkdir $local_dir
cp $host_dir/* $local_dir
dune exec -- main/main.exe elrond config/config.json $local_dir/_$name.elrond.spectable $local_dir/_$name.elrond.alpha
