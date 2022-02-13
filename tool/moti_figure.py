import numpy as np
import matplotlib.pyplot as plt
import matplotlib as mpl
from matplotlib.collections import LineCollection
from matplotlib.colors import ListedColormap, BoundaryNorm
import json
import sys
import math

def load_res(filename):
    f = open (filename, "r")
    data = json.loads(f.read())
    score = []
    for p in data['res']:
        score.append(p['v'])
    blocks = data['blocks']
    return (range(0, len(score)), score, blocks)

def soft (n):
    return math.log((n + 1.0))

def analysize (pfs, scores):
    # scores = [sum(l)/len(l) for l in scores]
    # scores = [max(l) for l in scores]
    scores = [soft(x) for x in scores]
    return pfs, scores

def plot (pfs, scores, blocks):
    # fig, axs = plt.subplots(1, 1, figsize=(9, 3), sharey=True)
    plt.bar(pfs, scores)
  # prev = 0
  #   for i in range(0, len(blocks)):
  #       x = pfs[prev: blocks[i]]
  #       y = scores[prev: blocks[i]]
  #       print(cs[i])
  #       plt.bar(x, y, color = cs[i])
  #       prev = blocks[i]
    # axs[1].scatter(pfs, scores)
    # axs[2].plot(pfs, scores)
    # fig.suptitle('Categorical Plotting')
    for xc in ([0] + blocks):
        plt.axvline(x=xc, color='black', linewidth=0.5, linestyle = (0, (5, 10)))
    avg = []
    # b = blocks + [len(scores)]
    b = blocks
    for xc in b:
        l = scores[len(avg): xc]
        avg = avg + ([sum(l)/len(l)]*(len(l)))
    print(len(pfs))
    print(len(avg))
    plt.plot(pfs, avg, color='red', linewidth=0.8)
    frame1 = plt.gca()
    frame1.get_xaxis().set_ticks([])
    frame1.get_yaxis().set_ticks([])
    plt.xlabel("Perturbation functions")
    plt.ylabel("Number of erroneous inputs covered")
    plt.show()

if __name__ == '__main__':
    filename = sys.argv[1]
    pfs, scores, blocks = load_res(filename)
    pfs, scores = analysize(pfs, scores)
    plot(pfs, scores, blocks)
