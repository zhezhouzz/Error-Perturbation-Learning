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
    return [float(r)/len(ll)/total for r in res]

def load_res(filename):
    data = {}
    with open(filename) as f:
        j = json.load(f)
        total=j['total']
        runs = j['runs']
        union = j['union']
    return (total, union, runs)

def plot (total, union, runs):
    # for one in runs:
    #     print(one)
    #     exit()
    # figure(figsize=(6, 2.2), dpi=100)
    fig, ax = plt.subplots(1, 1, figsize=(6,2.2))
    runs = [[x['in_pre'] for x in one] for one in runs]
    y = average_ll(runs, total)
    x = [u['u_i'] for u in union]
    ax.plot(x, y, color='black', linewidth=1.0, linestyle='dashed',  markersize=2, marker = 'o')
    y = [(u['u_in_pre'])/total for u in union]
    ax.set_xticks(np.arange(min(x), max(x)+1, 10))
    ax.set_xlabel("MCMC steps", fontsize=14)
    ax.set_yticks(np.arange(0.,1.05, 0.2))
    ax.set_yticklabels(['{:.0f}%'.format(100*x) for x in plt.gca().get_yticks()])
    ax.plot(x, y, color='black', linewidth=1.0, linestyle='dashed',  markersize=2, marker = 'o')
    plt.show()

if __name__ == '__main__':
    name = sys.argv[1]
    total, union, runs = load_res(name)
    plot(total, union, runs)
