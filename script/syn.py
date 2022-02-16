#!/bin/bash

dune exec -- main/main.exe synthesize-time config/config.json data/client/rbset/balance.ml data/client/rbset/assertion1.ml 1 300 > .prog
dune exec -- main/main.exe sampling-time config/config.json data/client/rbset/balance.ml data/client/rbset/assertion1.ml .prog 5
dune exec -- main/main.exe baseline-time config/config.json data/client/rbset/balance.ml data/client/rbset/assertion1.ml config/qc_zero_knowledge_conf.json 5
