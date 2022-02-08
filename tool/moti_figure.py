import matplotlib.pyplot as plt
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
    # axs[1].scatter(pfs, scores)
    # axs[2].plot(pfs, scores)
    # fig.suptitle('Categorical Plotting')
    for xc in blocks:
        plt.axvline(x=xc)
    plt.show()

if __name__ == '__main__':
    filename = sys.argv[1]
    pfs, scores, blocks = load_res(filename)
    pfs, scores = analysize(pfs, scores)
    plot(pfs, scores, blocks)
