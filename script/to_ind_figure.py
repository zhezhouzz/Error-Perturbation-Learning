import numpy as np
import matplotlib.pyplot as plt
import matplotlib as mpl
from matplotlib.collections import LineCollection
from matplotlib.colors import ListedColormap, BoundaryNorm
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
    for idx, l in enumerate(ls):
        x = range(0, len(l))
        plt.plot(x, l, color=cs[idx], linewidth=1.0)
    plt.xlabel("The number of perturbation operators available")
    plt.ylabel("The accuracy of learned perturbation function")
    plt.show()

def run_ind(filenames):
    names = [".result/" + x + ".ind" for x in filenames]
    ls = []
    for name in names:
        # ls.append(load_res(name))
        try:
            ls.append(load_res(name))
        except:
            cwd = os.getcwd()
            print("{} cannot found under {}".format(name, cwd))
    plot (ls)

if __name__ == '__main__':
    filenames = sys.argv[1].split(',')
    run_ind(filenames)
