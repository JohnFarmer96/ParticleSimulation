import numpy as np
import matplotlib.pyplot as plt
import os.path

basename_read = '/particle_simulation_result_'
basename_write = 'python/data/particle_simulation_python_'

# Load all .dat files
def load_all(directory):
    directory = directory + basename_read
    result_array = []
    end_of_stream = False
    index = 0
    while(end_of_stream != True):
        filename = directory + str(index) + '.dat'
        if(os.path.isfile(filename) == True):
            result_array.append(np.genfromtxt(filename, dtype=float, skip_header=1, filling_values=0))
        else:
            end_of_stream = True

        if(index == 0 and end_of_stream == True):
            print("Directory '" + str(directory) + "' was not found")
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

# Write Config.d file
def write_config(directory, concentration=10, t_total=1, t_res=0.1, v_x=0, v_y=0, v_z=0, vw_x=0, vw_y=0, vw_z=0, humid=0.5, T_env=300):
    if(os.path.isfile(directory) == True):
        output = str(concentration) + " \t :: concentration [1] \n" 
        output = output + str(t_total) + "\t :: t_total [s] \n" 
        output = output + str(t_res) + "\t :: t_resolution [s] \n" 
        output = output + str(v_x) + "\t :: v_x [m/s] \n" 
        output = output + str(v_y) + "\t :: v_y [m/s] \n"
        output = output  + str(v_z) + "\t :: v_z [m/s] \n"
        output = output  + str(vw_x) + "\t :: v_wind_x [m/s] \n"
        output = output  + str(vw_y) + "\t :: v_wind_y [m/s] \n"
        output = output  + str(vw_z) + "\t :: v_wind_z [m/s] \n"
        output = output  + str(humid) + "\t :: humidity [% 0..1] \n"
        output = output + str(T_env) + "\t :: T_environment [K] \n"

        with open(directory, "w") as text_file:
            print(output, file=text_file)

        print("Config written successfully")
    else: 
        print("Directory '" + str(directory) + "' was not found")

