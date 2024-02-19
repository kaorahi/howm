#!/bin/env -S python3

import matplotlib.pyplot as plt
import numpy as np

fig, axs = plt.subplots(2, 2, figsize=(10, 8))
plt.tight_layout(pad=4.0)

def plot(ax, style, x=[], y=[], yticks=[], yticklabels=[], title=''):
    ax.plot(x, y, style, lw=5)
    ax.set_title(title)
    ax.set_xlim(-20, 30)
    ax.set_ylim(-8, 9)
    ax.tick_params(bottom=True, left=False)
    ax.set_xticks([-10, 0, 10, 20])
    ax.set_xticklabels(['-10', 'xx', '+10', '+20'])
    ax.set_yticks(yticks)
    ax.set_yticklabels(yticklabels)
    ax.xaxis.grid(True, linestyle=':', color='gray')
    ax.yaxis.grid(True, linestyle=':', color='gray')
    ax.axvline(x=0, color='black', linewidth=1, linestyle='-')
    ax.axhline(y=0, color='black', linewidth=1, linestyle='-')
    ax.add_patch(plt.Rectangle((-100, 6), 200, 1, facecolor='#ffffff', zorder=10))
    ax.set_xlabel('date')
    ax.set_ylabel('priority')

plot(axs[0][0], 'r-',
     x=[-10,-10, 0,0, 30],
     y=[-8,-2, 0,8, 9],
     yticks = [-2, 0, 8],
     yticklabels = ['-2', '0', 'inf'],
     title='[2024-02-xx]!10')

plot(axs[0][1], 'y-',
     x=[0,0, 10, 10+12],
     y=[-8,-5, 0, 0.5*12],
     yticks = [-5, 0, 5, 8],
     yticklabels = ['-7', '0', '7', 'inf'],
     title='[2024-02-xx]+10')

plot(axs[1][0], 'b-',
     x=[0,0, 10, 30],
     y=[-8,0, -1, -3],
     yticks = [-2, -1, 0, 8],
     yticklabels = ['-2', '-1', '0', 'inf'],
     title='[2024-02-xx]-10')

plot(axs[1][1], 'm-',
     x=[0,0, 5, 10, 15, 20, 25, 30],
     y=[-8,-6, 0, -6, 0, -6, 0, -6],
     yticks = [-6, 0, 8],
     yticklabels = ['-14', '0', 'inf'],
     title='[2024-02-xx]~10')

plt.savefig('tmp_priority.png')
plt.show()
