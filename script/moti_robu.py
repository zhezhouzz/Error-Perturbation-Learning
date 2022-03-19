import numpy as np
import matplotlib.pyplot as plt
import matplotlib as mpl
from matplotlib.collections import LineCollection
from matplotlib.colors import ListedColormap, BoundaryNorm
from matplotlib.pyplot import figure
import json
import sys
import math

r = 1000
interval = 50
num_bar = 40

def load_res(filename):
    f = open (filename, "r")
    lines = f.readlines()
    num_overbound = int(lines[0])
    data = [int(x) for x in lines[1].split(',')]
    fc = [None] * ((r//interval) + 1)
    fc = [0 for x in fc]
    fc[-1] = num_overbound
    for d in data:
        idx = d // interval
        fc[idx] = fc[idx] + 1
    return (range(0, len(fc)), fc)

def load_res_raw(filename):
    f = open (filename, "r")
    lines = f.readlines()
    num_overbound = int(lines[0])
    data = [int(x) for x in lines[1].split(',')] + ([1000] * num_overbound)
    return data


def plot (fc):
    fc.sort()
    print(len(fc))
    c_50 = fc[len(fc)//2]
    c_90 = fc[(len(fc)*9)//10]
    c_95 = fc[(len(fc)*19)//20]
    figure(figsize=(6, 2), dpi=200)
    plt.hist(fc, num_bar, density=True)
    plt.axvline(x=c_50, color='black', linewidth=1.2, linestyle = (0, (2, 4)))
    plt.text(c_50 + 10,0.0075,'50%', color='black', rotation=0, fontsize=10)
    plt.axvline(x=c_90, color='purple', linewidth=1.2, linestyle = (0, (2, 4)))
    plt.text(c_90 + 10,0.0075,'90%',color='purple', rotation=0, fontsize=10)
    plt.axvline(x=c_95, color='red', linewidth=1.2, linestyle = (0, (2, 4)))
    plt.text(c_95 + 10,0.0075,'95%',color='red', rotation=0, fontsize=10)
    frame1 = plt.gca()
    # frame1.spines["top"].set_visible(False)
    # frame1.get_xaxis().set_ticks([])
    frame1.get_xaxis().set_ticks([x*100 for x in range(0, 10)])
    plt.gca().set_yticklabels(['{:.0f}%'.format(x*100*1000/num_bar) for x in plt.gca().get_yticks()])
    # frame1.get_yaxis().set_ticks([])
    plt.xlabel("Steps to converge")
    plt.ylabel("Frequency")
    plt.subplots_adjust(left=0.1, bottom=0.22, right=0.97, top=0.95, wspace=0.1, hspace=0.1)
    plt.show()

if __name__ == '__main__':
    filename = sys.argv[1]
    # x, fc = load_res(filename)
    # plot(x, fc)
    fc = load_res_raw(filename)
    plot(fc)
