import numpy as np
import matplotlib.pyplot as plt

def show_movement(array, t_start_idx, t_end_idx, t_spacing):
    fig = plt.figure()
    total = (t_end_idx - t_start_idx)/t_spacing
    rows, cols = subplot_size(total)

    plot_num = 0
    for i in range(t_start_idx, t_end_idx, t_spacing):
        # Iterate over plot number
        plot_num = plot_num + 1
        
        # Determine Point Size Factor
        size = array[i,:,9] #/max(array[i,:,9])*100
        
        # Create Plot
        ax = fig.add_subplot(rows,cols,plot_num, projection='3d')
        scatter = ax.scatter(array[i,:,2], array[i,:,3], array[i,:,4], s=size)
        ax.quiver(array[i,:,2], array[i,:,3], array[i,:,4], array[i,:,5], array[i,:,6], array[i,:,7])

        # Produce a legend with a cross section of sizes from the scatter
        handles, labels = scatter.legend_elements(prop="sizes", alpha=0.6)
        ax.legend(handles, labels, loc="upper left", title="Sizes [µm]")

        # Create title and Axis-Annotation
        cur_time = f"{array[i,0,1]:.1f}"
        title = 'Movement at: ' + str(cur_time) + ' Seconds' 
        #ax.title.set_text(title)
        ax.set_title(title, fontweight="bold")
        ax.set_xlabel('X [m]')
        ax.set_ylabel('Y [m]')
        ax.set_zlabel('Z [m]')

    fig.subplots_adjust(left=None, bottom=None, right=None, top=None, wspace=None, hspace=None)
    return fig

def plot_single_feature(array, sf_num, start_idx, end_idx, spacing):
    fig = plt.figure()


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
