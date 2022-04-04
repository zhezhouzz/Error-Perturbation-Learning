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
    ll = np.array(ll)
    ll = np.transpose(ll)
    ll = ll/total
    ll = [np.sort(l) for l in ll]
    length = len(ll[0])
    lower = [l[int(length*0.25)] for l in ll]
    upper = [l[int(length*0.75)] for l in ll]
    mean = [np.mean(l) for l in ll]
    return mean, lower, upper

def load_res(filename):
    data = {}
    with open(filename) as f:
        j = json.load(f)
        total=j['total']
        num_runs=j['num_runs']
        num_unions=j['num_unions']
        steps=j['idxs']
        data=j['data']
    m = []
    for u_num in range(0, num_unions):
        m.append([0] * len(steps))
    for d in data:
        union_idx = d['num_unoin'] - 1
        step_idx = steps.index(d['num_step'])
        m[union_idx][step_idx] = m[union_idx][step_idx] + d['in_pre']
    m = [[x/total/num_runs for x in d] for d in m]
    return steps, m

def plot_v2 (x, m):
    fig, ax = plt.subplots(1, 1, figsize=(9,2.2), constrained_layout=True, dpi=100)
    plt.axhline(y = 1.0, color = 'grey', linestyle = 'dotted')
    for y in m:
        ax.plot(x, y, color='black', linewidth=1.0, linestyle='dashed',  markersize=2, marker = 'o')
    ax.set_xticks(np.arange(min(x), max(x)+1, 10))
    ax.set_yticks(np.arange(0.,1.05, 0.2))
    ax.set_yticklabels(['{:.0f}%'.format(100*x) for x in plt.gca().get_yticks()])
    plt.xlabel("MCMC steps", fontsize=14)
    plt.show()

def plot (total, union, runs):
    # for one in runs:
    #     print(one)
    #     exit()
    # figure(figsize=(6, 2.2), dpi=100)
    fig, ax = plt.subplots(1, 1, figsize=(9,2.2), constrained_layout=True, dpi=100)
    plt.axhline(y = 1.0, color = 'grey', linestyle = 'dotted')
    runs = [[x['in_pre'] for x in one] for one in runs]
    y, lower, upper = average_ll(runs, total)
    y = np.array(y[:-1])
    lower = np.array(lower[:-1])
    upper = np.array(upper[:-1])
    x = [u['u_i'] for u in union][:-1]
    ax.plot(x, y, color='black', linewidth=1.0, linestyle='dashed',  markersize=2, marker = 'o')
    ax.errorbar(x, y, np.array([y - lower, upper - y]),
                         fmt='none', solid_capstyle='projecting', capsize=2, color='black', alpha=.7)
    y = [(u['u_in_pre'])/total for u in union][:-1]
    ax.plot(x, y, color='black', linewidth=1.0, linestyle='solid',  markersize=2, marker = 'o')
    ax.set_xticks(np.arange(min(x), max(x)+1, 10))
    ax.set_yticks(np.arange(0.,1.05, 0.2))
    ax.set_yticklabels(['{:.0f}%'.format(100*x) for x in plt.gca().get_yticks()])
    plt.xlabel("MCMC steps", fontsize=14)
    plt.show()

if __name__ == '__main__':
    name = sys.argv[1]
    x, m = load_res(name)
    plot_v2(x, m)
