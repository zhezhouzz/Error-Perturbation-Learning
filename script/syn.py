#!/bin/bash

dune exec -- main/main.exe synthesize-time config/config.json data/client/rbset/balance.ml data/client/rbset/assertion1.ml 1 100 > .prog
dune exec -- main/main.exe sampling-time config/config.json data/client/rbset/balance.ml data/client/rbset/assertion1.ml .prog 10
dune exec -- main/main.exe baseline-time config/config.json data/client/rbset/balance.ml data/client/rbset/assertion1.ml config/qc_zero_knowledge_conf.json 10
