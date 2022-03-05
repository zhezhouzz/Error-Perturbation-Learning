import numpy as np
import matplotlib.pyplot as plt
import matplotlib as mpl
from matplotlib.collections import LineCollection
from matplotlib.colors import ListedColormap, BoundaryNorm
import matplotlib.ticker as mtick
import json
import sys
import math
import os

def load_res(filename):
    f = open (filename, "r")
    lines = f.readlines()
    line = lines[3].translate({40:None, 41: None})
    l = [float(x) for x in line.split(' ')]
    return l

def plot (ls):
    cs = ['red', 'blue', 'black', 'green']
    for idx, (name, l) in enumerate(ls):
        x = range(3, len(l) + 3)
        plt.plot(x, l, color= cs[idx], linewidth=1.0, marker = 'o', label=name)
        plt.xticks(np.arange(min(x), max(x)+1, 1.0))
    plt.gca().set_yticklabels(['{:.0f}%'.format(x*100) for x in plt.gca().get_yticks()])
    plt.xlabel("$|\Theta|$", math_fontfamily='cm', fontsize=16)
    plt.ylabel("$Acc_{PF}$", math_fontfamily='cm', fontsize=16, rotation=0, labelpad=20)
    plt.grid(True)
    plt.legend()
    plt.show()

def run_ind(filenames):
    names = [".result/" + x + ".ind" for x in filenames]
    ls = []
    for idx, name in enumerate(names):
        # ls.append(load_res(name))
        try:
            ls.append((filenames[idx], load_res(name)))
        except:
            cwd = os.getcwd()
            print("{} cannot found under {}".format(name, cwd))
    plot (ls)

if __name__ == '__main__':
    filenames = sys.argv[1].split(',')
    run_ind(filenames)
