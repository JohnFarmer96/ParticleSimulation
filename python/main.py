# Issue: https://github.com/pearu/f2py/issues/49
# Therefore no f2py implementation possible
import numpy as np
import matplotlib.pyplot as plt 
import file_interface as fi
import visualization as vis
from mpl_toolkits.mplot3d import Axes3D

a = fi.load_all()
print(np.shape(a))

fig = vis.show_movement(a, 0, 25, 1)

plt.show()