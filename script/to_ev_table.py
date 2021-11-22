import os
import re

def baseline_show(path):
    files = os.listdir(path)
    files.sort()
    for fil in files:
        full_path = os.path.join(path, fil)
        if full_path.endswith(".baseline"):
            with open(full_path) as f:
                lines = f.readlines()
                benchname = (re.sub(r":$", "", lines[0], 0, re.DOTALL))
                avg_time = float(re.sub(r"(^.*:)|\(.*\)", "", lines[2], 0, re.MULTILINE | re.DOTALL))
                sigma_qc = float(re.sub(r".*\(|%\)", "", lines[6], 0, re.MULTILINE | re.DOTALL))
                neg_qc = float(re.sub(r".*\(|%\)", "", lines[7], 0, re.MULTILINE | re.DOTALL))
                uniq_qc = float(re.sub(r".*\(|%\)", "", lines[8], 0, re.MULTILINE | re.DOTALL))
                print("{} & ${:.3f}\%$ & ${:.3f}\%$ & ${:.3f}\%$ & ${:.3f}$ \\\\".
                      format(benchname, sigma_qc, neg_qc, uniq_qc, avg_time))

import sys

if __name__ == "__main__":
    path = sys.argv[1]
    baseline_show(path)
