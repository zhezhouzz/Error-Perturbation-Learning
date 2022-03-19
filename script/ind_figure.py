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

def load_res(filename):
    data = []
    with open(filename) as f:
        j = json.load(f)
        for e in j:
            data = data + e['acc_mat']
    data_T = transpose(data)
    for l in data_T:
        l.sort()
    print(data_T)
    # data_T = [l[1:-1] for l in data_T]
    print(data_T)
    data_mean = [statistics.mean(d) for d in data_T]
    data_min = [d[0] for d in data_T]
    data_max = [d[-1] for d in data_T]
    data_min_80 = [d[1] for d in data_T]
    data_max_80 = [d[-2] for d in data_T]
    data_min_50 = [d[2] for d in data_T]
    data_max_50 = [d[-3] for d in data_T]
    # return data_mean, [(data_min_50, data_max_50), (data_min_80, data_max_80), (data_min, data_max)]
    # return data_mean, [(data_min_80, data_max_80), (data_min, data_max)]
    return data_mean, [(data_min_50, data_max_50), (data_min_80, data_max_80)]

def plot (ls):
    # figure(figsize=(6, 2.2), dpi=100)
    cs = ['red', 'blue', 'black', 'green']
    caps = [2, 4, 6]
    alphas = [1.0, 0.4, 0.1]
    names = ["customstk", "sortedl", "uniquel"]
    fig = plt.figure(constrained_layout=True, figsize=(10, 4), dpi=100)
    # subfigs = fig.subfigures(2, 2, wspace=0.07)
    for idx, (name, l) in enumerate(ls):
        y, errs = l
        x = range(3, len(y) + 3)
        ax = plt.subplot(2, 2, idx + 1)
        for idx_e, (data_min, data_max) in enumerate(errs):
            data_max = [data_max[idx] - v for idx, v in enumerate(y)]
            data_min = [v - data_min[idx] for idx, v in enumerate(y)]
            ax.errorbar(x, y, np.array([data_min, data_max]),
                         fmt='none', solid_capstyle='projecting', capsize=caps[idx_e], color='black', alpha=alphas[idx_e])
        ax.plot(x, y, color='black', linewidth=1.0, linestyle='dashed',  markersize=2, marker = 'o', label=name)
        # plt.fill_between(x, data_min, data_max, color=cs[idx], alpha=0.1)
        ax.set_xticks(np.arange(min(x), max(x)+1, 1.0))
        ax.set_yticks(np.arange(0.,1.05, 0.2))
        ax.set_yticklabels(['{:.0f}%'.format(x*100) for x in plt.gca().get_yticks()])
        ax.set_xlabel("$|\Theta|$ in {}".format(names[idx]), math_fontfamily='cm', fontsize=14)
        ax.set_ylabel("$Acc_{PF}$", math_fontfamily='cm', fontsize=16, rotation=0, labelpad=30)
    # plt.grid(True)
    # plt.legend()
    # plt.subplots_adjust(left=0.17, bottom=0.22, right=0.97, top=0.95, wspace=0.1, hspace=0.1)
    plt.show()

def run_ind(filenames):
    names = [".result/" + x + ".ind.json" for x in filenames]
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
