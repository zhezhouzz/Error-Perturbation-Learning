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

def average_ll(ll, total):
    res = []
    need_init = True
    for l in ll:
        if need_init:
            res = [0] * len(l)
            need_init = False
        for (i, v) in enumerate(l):
            res[i] = res[i] + v
    return [100*float(r)/len(ll)/total for r in res]

def load_res(filename):
    data = {}
    with open(filename) as f:
        j = json.load(f)
        total=j['total']
        runs = j['runs']
    return (total, runs)

def plot (total, runs):
    # for one in runs:
    #     print(one)
    #     exit()
    runs = [[x['in_pre'] for x in one] for one in runs]
    y = average_ll(runs, total)
    x = range(len(y))
    plt.plot(x, y, color='black', linewidth=1.0, linestyle='dashed',  markersize=2, marker = 'o')
    plt.show()

if __name__ == '__main__':
    name = sys.argv[1]
    total, runs = load_res(name)
    plot(total, runs)
