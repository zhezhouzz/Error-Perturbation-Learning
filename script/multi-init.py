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
                    data[n] = data[n] + (e['acc_mat'])
                else:
                    data[n] = e['acc_mat']
    for (k, v) in data.items():
        v.sort()
    # print(data)
    res = []
    for (n, d) in data.items():
        data_mean = statistics.mean(d)
        print(n, np.std(d))
        data_100_1, data_100_2 = (d[0], d[-1])
        data_80_1, data_80_2 = perc_range(d, 0.8)
        data_50_1, data_50_2  = perc_range(d, 0.5)
        # print(data_mean)
        # print(perc_range(d, 0.8))
        # print(perc_range(d, 0.5))
        res.append((n, data_mean,
                    data_mean - data_80_1, data_80_2 - data_mean,
                    data_mean - data_50_1, data_50_2 - data_mean))
    x, y, a1, a2, b1, b2 = zip(*res)
    exit(0)
    return (x, y, a1, a2, b1, b2)

def plot (ls):
    # figure(figsize=(6, 2.2), dpi=100)
    cs = ['red', 'blue', 'black', 'green']
    caps = [2, 4, 6]
    alphas = [1.0, 0.4, 0.1]
    names = ["customstk", "sortedl", "uniquel"]
    fig = plt.figure(constrained_layout=True, figsize=(10, 1.8), dpi=100)
    # subfigs = fig.subfigures(2, 2, wspace=0.07)
    for idx, (name, l) in enumerate(ls):
        ns, y, a1, a2, b1, b2 = l
        x = range(len(y))
        ax = plt.subplot(1, 3, idx + 1)
        ax.errorbar(x, y, np.array([a1, a2]),
                         fmt='none', solid_capstyle='projecting', capsize=4, color='black', alpha=0.4)
        ax.errorbar(x, y, np.array([b1, b2]),
                         fmt='none', solid_capstyle='projecting', capsize=2, color='black', alpha=1.0)
        ax.plot(x, y, color='black', linewidth=1.0, linestyle='dashed',  markersize=2, marker = 'o', label=name)
        # plt.fill_between(x, data_min, data_max, color=cs[idx], alpha=0.1)
        # ax.set_xticks(ns)
        # print(ns)
        ns = [0] + list(ns)
        ax.set_xticklabels([str(n) for n in ns])
        ax.set_yticks(np.arange(0.,1.05, 0.2))
        ax.set_yticklabels(['{:.0f}%'.format(x*100) for x in plt.gca().get_yticks()])
        ax.set_xlabel("$|A_{}|$ in {}".format("{init}", names[idx]), math_fontfamily='cm', fontsize=14)
        if idx == 0:
            ax.set_ylabel("$Acc_{PF}$", math_fontfamily='cm', fontsize=16, rotation=0, labelpad=30)
    # plt.grid(True)
    # plt.legend()
    # plt.subplots_adjust(left=0.17, bottom=0.22, right=0.97, top=0.95, wspace=0.1, hspace=0.1)
    plt.show()

def run_ind(filenames):
    names = [".result/" + x + ".robuinit.json" for x in filenames]
    ls = []
    for idx, name in enumerate(names):
        # ls.append(load_res(name))
        # try:
        ls.append((filenames[idx], load_res(name)))
        # except:
        #     cwd = os.getcwd()
        #     print("{} cannot found under {}".format(name, cwd))
        #     exit()
    plot (ls)

if __name__ == '__main__':
    filenames = sys.argv[1].split(',')
    run_ind(filenames)
