import json
import subprocess
import sys
import argparse
import os
from datetime import datetime

verbose = True
config_file = "config/config.json"

def invoc_cmd(cmd):
    if (verbose):
        print(" ".join(cmd))
    try:
        subprocess.run(cmd)
    except subprocess.CalledProcessError as e:
        print(e.output)

def set_printing_samples(b):
    with open(config_file, 'r') as f:
        config = json.load(f)
    config['show_samples_in_log'] = b
    # print(config)
    with open(config_file, 'w') as f:
        json.dump(config, f)

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("action", type=str,
                        help="the expected you want: make | clean ")
    args = parser.parse_args()
    action = args.action
    if action == "make":
        invoc_cmd(["git", "pull"])
        invoc_cmd(["dune", "build"])
    elif action == "clean":
        invoc_cmd(["dune", "clean"])
        set_printing_samples(False)
    elif action == "printsamples":
        set_printing_samples(True)
    else:
        print("unknown command {}".format(action))
        exit()
