import numpy as np
import matplotlib.pyplot as plt
import math

def show_movement_notebook(data, t_start_idx, t_end_idx, t_spacing):
    array = np.array(data)
    plot_num = 0
    for i in range(t_start_idx, t_end_idx, t_spacing):
        # Iterate over plot number
        plot_num = plot_num + 1
        
        # Determine Point Size Factor
        size = array[i,:,9]
        color = color = array[i,:,22]
        colormap = get_colormap(array[i,:,22])
        
        # Create Plot
        fig = plt.figure()
        ax = fig.add_subplot(1,1,1, projection='3d')
        arrow_ratio = get_arrow_ratio(array[i,:,5:8].max())
        
        scatter = ax.scatter(array[i,:,2], array[i,:,3], array[i,:,4], s=size, c=color, cmap=colormap)
        ax.quiver(array[i,:,2], array[i,:,3], array[i,:,4], array[i,:,5], array[i,:,6], array[i,:,7], arrow_length_ratio=arrow_ratio)

        # Produce a legend with a cross section of sizes from the scatter
        handles, labels = scatter.legend_elements(prop="sizes", alpha=0.6)
        ax.legend(handles, labels, loc="upper left", title="Sizes [µm]")

        # Set axis limits
        ax.set_xlim(axis_limit([array[i,:,2], array[i,:,2]+array[i,:,5]]))
        ax.set_ylim(axis_limit([array[i,:,3], array[i,:,3]+array[i,:,6]]))
        ax.set_zlim(axis_limit([array[i,:,4], array[i,:,4]+array[i,:,7]]))

        # Create title and Axis-Annotation
        cur_time = f"{array[i,0,1]:.2f}"
        title = 'Movement at: ' + str(cur_time) + ' Seconds' 
        ax.set_title(title, fontweight="bold")
        ax.set_xlabel('X [m]')
        ax.set_ylabel('Y [m]')
        ax.set_zlabel('Z [m]')
        plt.show()

# Show Movement of Particles
def show_movement(data, t_start_idx, t_end_idx, t_spacing):
    array = np.array(data)
    fig = plt.figure()
    total = (t_end_idx - t_start_idx)/t_spacing
    rows, cols = subplot_size(total)

    plot_num = 0
    for i in range(t_start_idx, t_end_idx, t_spacing):
        # Iterate over plot number
        plot_num = plot_num + 1
        
        # Determine Point Size Factor and Plot Color
        size = array[i,:,9]
        color = array[i,:,22]
        colormap = get_colormap(array[i,:,22])

        # Create Plot
        ax = fig.add_subplot(rows,cols,plot_num, projection='3d')
        arrow_ratio = get_arrow_ratio(array[i,:,5:8].max())

        scatter = ax.scatter(array[i,:,2], array[i,:,3], array[i,:,4], s=size, c=color, cmap=colormap)
        ax.quiver(array[i,:,2], array[i,:,3], array[i,:,4], array[i,:,5], array[i,:,6], array[i,:,7], arrow_length_ratio=arrow_ratio)

        # Produce a legend with a cross section of sizes from the scatter
        handles, labels = scatter.legend_elements(prop="sizes", alpha=0.6)
        ax.legend(handles, labels, loc="upper left", title="Sizes [µm]")

        # Set axis limits
        ax.set_xlim(axis_limit([array[i,:,2], array[i,:,2]+array[i,:,5]]))
        ax.set_ylim(axis_limit([array[i,:,3], array[i,:,3]+array[i,:,6]]))
        ax.set_zlim(axis_limit([array[i,:,4], array[i,:,4]+array[i,:,7]]))

        # Create title and Axis-Annotation
        cur_time = f"{array[i,0,1]:.2f}"
        title = 'Movement at: ' + str(cur_time) + ' Seconds' 
        #ax.title.set_text(title)
        ax.set_title(title, fontweight="bold")
        ax.set_xlabel('X [m]')
        ax.set_ylabel('Y [m]')
        ax.set_zlabel('Z [m]')

    return fig

# Determine needed axis limits
def axis_limit(data):
    array = np.array(data)
    lower_value = array.min()
    upper_value = array.max()
    return lower_value, upper_value

# Determine used Colormap
def get_colormap(data):
    array = np.array(data)
    if(array.min() == 0):
        colormap = 'RdYlGn'
    else:
        colormap = 'summer'
    return colormap

# Determine arrow ratio
def get_arrow_ratio(v_max):
    if(v_max < 1):
        ratio = 0.3
    elif (v_max > 10):
        ratio = 0.03
    else:
        ratio = 0.3/v_max
    return ratio

# Plot Single Feature of Data Array
def plot_single_feature(data, sf_idx, title, ylabel):
    array = np.array(data)
    shape = np.shape(array)
    rows = shape[1]

    fig = plt.figure()

    for row in range(rows):
        plt.plot(array[:,row,1], array[:,row,sf_idx])
    
    plt.title(title)
    plt.xlabel('Time [s]')
    plt.ylabel(ylabel)

    return fig

#Determine Subplot Size
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
