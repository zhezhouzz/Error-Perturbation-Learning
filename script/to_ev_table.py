import os
import re

def to_percent(d):
    "{:.2f}".format(d)

def to_avg_time(d):
    if d == "inf":
        "inf"
    else:
        "{:f}".format((float(d)))

def baseline_show(path):
    for fil in os.listdir(path):
        full_path = os.path.join(path, fil)
        if full_path.endswith(".baseline"):
            with open(full_path) as f:
                lines = f.readlines()
                benchname = (re.sub(r":$", "", lines[0], 0, re.DOTALL))
                avg_time = float(re.sub(r"(^.*:)|\(.*\)", "", lines[2], 0, re.MULTILINE | re.DOTALL))
                sigma_qc = float(re.sub(r".*\(|%\)", "", lines[6], 0, re.MULTILINE | re.DOTALL))
                neg_qc = float(re.sub(r".*\(|%\)", "", lines[7], 0, re.MULTILINE | re.DOTALL))
                uniq_qc = float(re.sub(r".*\(|%\)", "", lines[8], 0, re.MULTILINE | re.DOTALL))
                print("{} & ${:.2f}\%$ & ${:.2f}\%$ & ${:.2f}\%$ & ${:.2f}$ \\\\".
                      format(benchname, sigma_qc, neg_qc, uniq_qc, avg_time))

import sys

if __name__ == "__main__":
    path = sys.argv[1]
    baseline_show(path)
