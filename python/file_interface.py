import numpy as np
import matplotlib.pyplot as plt
import os.path

basename_read = 'data/particle_simulation_result_'
basename_write = 'python/data/particle_simulation_python_'

# Load all .dat files
def load_all():
    result_array = []
    end_of_stream = False
    index = 0
    while(end_of_stream != True):
        filename = basename_read + str(index) + '.dat'
        if(os.path.isfile(filename) == True):
            result_array.append(np.genfromtxt(filename, dtype=float, skip_header=1, filling_values=0))
            print("File was read successfully:" + filename)
        else:
            end_of_stream = True
        index = index + 1
    return np.array(result_array)

# Load specific .dat files
def load_specific(index):
    result_array = []
    filename = basename_read + str(index) + '.dat'
    if(os.path.isfile(filename) == True):
        result_array.append(np.genfromtxt(filename, dtype=float, skip_header=1, filling_values=0))
        print("File was read successfully:" + filename)
    return np.array(result_array)

# Write array to .dat file
def write_array(array, index): 
    filename = basename_write + str(index) + '.dat'
    np.savetxt(filename, array, fmt='%18.12f')