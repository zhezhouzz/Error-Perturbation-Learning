import numpy as np
import matplotlib.pyplot as plt
import matplotlib as mpl
from matplotlib.collections import LineCollection
from matplotlib.colors import ListedColormap, BoundaryNorm
from matplotlib.pyplot import figure
import matplotlib.ticker as mtick
import json
import sys
import math
import os
import statistics

def transpose(l):
    return list(map(list, zip(*l)))

def perc_range(l, perc):
    llen = len(l)
    len_perc = int(llen * perc)
    start = int((llen - len_perc)/2)
    end = len_perc + start - 1
    return l[start], l[end]

def load_res(filename):
    data = {}
    with open(filename) as f:
        j = json.load(f)
        for l in j:
            for e in l:
                n = e['n_init']
                if n in data:
                    data[n].append(np.mean(e['acc_mat']))
                else:
                    data[n] = [np.mean(e['acc_mat'])]
    print(filename)
    for (k, v) in data.items():
        print("|Theta| = {}, acc = {}".format(k, np.std(v)))
    return


def run_ind(filenames):
    names = [".result/" + x + ".robuinit.json" for x in filenames]
    ls = []
    for idx, name in enumerate(names):
        load_res(name)

if __name__ == '__main__':
    filenames = sys.argv[1].split(',')
    run_ind(filenames)
