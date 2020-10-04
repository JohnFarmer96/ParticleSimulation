# ParticleSimulation

ParticleSimulation is a university project created by Jonathan Bauer.
The shared source-code aims to resemble the skills that are taught in the lecture "Anwendung höherer Programmiersprachen" by Daniel Weygand at the Karlsruhe Institute of Technology (KIT).
The project is capable of numerically simulate the movement of aerosoles [50nm - 100 µm] affected by gravitation, evaporation, wind and air-resistance.

## Project Structure

The main structure divides up into two parts:

### 1. Simulation Program (Fortran Based)

For the sake of efficiency the mathematical problems is executed by a fortran based program that is stored in "dist/" directory.
The program is precompiled (GNU-Fortran-Compiler) and ready to be executed, which can be achieved by calling

> make run

If the provided distribution doesn't match your CPU-architecture, it can be re-compiled by calling

> make build

Make sure GNU-Fortran-Compiler is installed (<https://gcc.gnu.org/>).

The **simulation-parameters** for the Fortran-Program can be adapted in "data/config.d" for the desired context:

- concentration [1] *(Number of Particles in Simulation)*
- t_total [s] (*Total Simulation Time)*
- t_resolution [s] *(Time Resolution of Data Output)*
- v_x [m/s] *(Initial Velocity in X-Direction)*
- v_y [m/s] *(Initial Velocity in Y-Direction)*
- v_z [m/s] *(Initial Velocity in Z-Direction)*
- v_wind_x [m/s] *(Wind Velocity in X-Direction)*
- v_wind_y [m/s] *(Wind Velocity in Y-Direction)*
- v_wind_z [m/s] *(Wind Velocity in Z-Direction)*
- humidity [%] (ranging from 0 to 1) *(Humidity of Environment)*
- T_environment [K] *(Temperature of Environment)*

After each iteration the current data is stored in a file named 'particle_simulation_result_XX.dat' where 'XX' is a placeholder for the current iteration step. While stepwidth of the numeric proceduers is calculated automatically, the total number of iterations is determined by the total simulation time and the selected time resolution.  

### 2. Data Processing and Visualization (Python Based)

The resulting data is processed and visualized by Python Code.
The code can be executed in two ways:

- Jupyter Notebook (~python/main.ipynb)
- Plain Python Script (~python/main.py)

The provided source code outputs plots showing the movement at a certain timestamp (3D) and pre-selected single features over the entire simulation-duration (2D).
An example snippet is shown below.

## Documentation

The documentation of physical and mathematical foundations can be reviewed in the "doc/" directory as well as the module-structure of the Fortran Code.
Both - Fortran and Python - Codes are annotated in detail and therefore easy to follow along.

## Get Started

To download the project run:

> $ git clone <https://github.com/JohnFarmer96/ParticleSimulation>

After the project is stored locally you can explore the behaviour by executing the make-commands mentioned above or the provided shell script "execute.sh"
