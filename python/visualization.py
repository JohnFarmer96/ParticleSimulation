import numpy as np
import matplotlib.pyplot as plt

def show_movement(array, start_idx, end_idx, spacing):
    fig = plt.figure()
    total = end_idx - start_idx
    rows, cols = subplot_size(total)

    for i in range(start_idx, end_idx, spacing):
        ax = fig.add_subplot(rows,cols,i+1, projection='3d')
        ax.scatter(array[i,:,2], array[i,:,3], array[i,:,4], c=array[i,:,16] + array[i,:,17])
        ax.quiver(array[i,:,2], array[i,:,3], array[i,:,4], array[i,:,5], array[i,:,6], array[i,:,7])

    fig.subplots_adjust(left=None, bottom=None, right=None, top=None, wspace=None, hspace=None)
    return fig

def subplot_size(total):
    rows = 1
    cols = 1
    fitting = False
    while (fitting == False):
        if(rows*cols >= total):
            fitting = True
        else:
            if(cols > rows):
                rows = rows + 1
            else: 
                cols = cols + 1
    return rows, cols
