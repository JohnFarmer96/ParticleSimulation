# Issue: https://github.com/pearu/f2py/issues/49
# Therefore no f2py implementation possible
import numpy as np
import matplotlib.pyplot as plt 
import file_interface as fi
import visualization as vis
from mpl_toolkits.mplot3d import Axes3D

# ! General information (index and elapsed time)
# array(1) = idx_particles
# array(2) = particles(idx_particles)%time_elapsed

# ! Particle position
# array(3) = particles(idx_particles)%r(1)
# array(4) = particles(idx_particles)%r(2)
# array(5) = particles(idx_particles)%r(3)

# ! Particle velocity
# array(6) = particles(idx_particles)%v(1)
# array(7) = particles(idx_particles)%v(2)
# array(8) = particles(idx_particles)%v(3)

# ! Particle diameters
# array(9) = particles(idx_particles)%d_core
# array(10) = particles(idx_particles)%d_shell

# ! Particle circumstances
# array(11) = particles(idx_particles)%T
# array(12) = particles(idx_particles)%T_environment
# array(13) = particles(idx_particles)%humidity
# array(14) = particles(idx_particles)%v_wind(1)
# array(15) = particles(idx_particles)%v_wind(2)
# array(16) = particles(idx_particles)%v_wind(3)

# ! Particle mass
# array(17) = mass_core(particles(idx_particles))
# array(18) = mass_shell(particles(idx_particles))

# ! Particle acting force
# array(19) = particles(idx_particles)%f(1)
# array(20) = particles(idx_particles)%f(2)
# array(21) = particles(idx_particles)%f(3)

# ! Particle status variables
# array(22) = MERGE(1.d0, 0.d0, particles(idx_particles)%core_only)
# array(23) = MERGE(1.d0, 0.d0, particles(idx_particles)%active)

# ? Start of Main Code ==========================================================================================

a = fi.load_all("data")
num_of_timesteps, num_of_particles, num_of_variables = np.shape(a)

print('Number of Timesteps: ' + str(num_of_timesteps))
print('Number of Particles: ' + str(num_of_particles))
print('Number of Variables: ' + str(num_of_variables))

num_of_charts = 6
start = 0
end = num_of_timesteps
spacing = int((end-start)/(num_of_charts-1))

fig1 = vis.show_movement(a, t_start_idx=start, t_end_idx=num_of_timesteps, t_spacing=spacing)
fig2 = vis.plot_single_feature(a, 9, "Particle Shell Size", "Diameter [µm]")
fig3 = vis.plot_single_feature(a, 18, 'Acting Force (x-Direction)', 'Force [µN]')
fig4 = vis.plot_single_feature(a, 19, 'Acting Force (y-Direction)', 'Force [µN]')
fig5 = vis.plot_single_feature(a, 20, 'Acting Force (z-Direction)', 'Force [µN]')
plt.show()