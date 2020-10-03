MODULE particle_simulation
    USE module_parameters
    USE module_particle
    USE module_init
    USE module_evaporation
    USE module_movement
    USE numeric_integration
    USE omp_lib
    IMPLICIT NONE

    PUBLIC :: initialize
    PUBLIC :: update
    PUBLIC :: output
    
    ! List of particles in simulation
    TYPE(particle), DIMENSION(:), ALLOCATABLE :: particles

CONTAINS

    ! Source: https://www.rosenfluh.ch/ansteckungsrisiken-durch-aerosole
    ! Initialize list of particles
    SUBROUTINE initialize(concentration, velocity, T_environment, humidity, v_wind)
        ! Particle concentration in the air [x/m³] (0...30.000)
        INTEGER, INTENT(IN) :: concentration
        ! Intensity coefficient [%] (0...1) 
        DOUBLE PRECISION, DIMENSION(dim), INTENT(IN) :: velocity
        ! Velocity of Wind [m/s]
        DOUBLE PRECISION, DIMENSION(dim), INTENT(IN) :: v_wind
        ! Temperature of environment [K]
        DOUBLE PRECISION, INTENT(IN) :: T_environment
        ! Relative Humidity in environment [%]
        DOUBLE PRECISION, INTENT(IN) :: humidity

        INTEGER :: idx

        ! Allocate particles list
        ALLOCATE(particles(concentration))

        !$omp PARALLEL DO, NUM_THREADS(MAX)
        do idx = 1, concentration
            call initialize_position(particles(idx))
            call initialize_velocity(particles(idx), velocity)
            call initialize_structure(particles(idx))
            call initialize_circumstances(particles(idx), T_environment, humidity, v_wind)
        end do
        !$omp END PARALLEL DO

    END SUBROUTINE

    ! Update Particle
    SUBROUTINE update(integration_procedure_evaporation, integration_procedure_movement, t_total, all_done, timestamp, breakpoint)
        ! Desired numeric integration procedure for evaporation (Euler, Runge-Kutta, ...)
        PROCEDURE(num_int_procedure) :: integration_procedure_evaporation
        ! Desired numeric integration procedure for movement (Euler, Runge-Kutta, ...)
        PROCEDURE(num_int_procedure) :: integration_procedure_movement
        ! Total Runtime [s]
        DOUBLE PRECISION, INTENT(IN) :: t_total
        ! Timestamp [s]
        DOUBLE PRECISION, INTENT(IN) :: timestamp
        ! Indicate if all particles are done
        LOGICAL, INTENT(INOUT) :: all_done
        ! Indicate if time breakpoint is reached
        LOGICAL, INTENT(INOUT) :: breakpoint

        ! Local Variables
        INTEGER :: num_of_particles
        INTEGER :: num_of_inactive
        INTEGER :: idx
        num_of_particles = SIZE(particles, DIM=1) 

        all_done = .FALSE.
        num_of_inactive = 0
        !$omp PARALLEL DO NUM_THREADS(MAX)
        do idx = 1, num_of_particles
            IF (particles(idx)%time_elapsed .LE. timestamp) THEN
                call evaluate_movement(integration_procedure_movement, particles(idx), t_total)
                call evaluate_evaporation(integration_procedure_evaporation, particles(idx))
                particles(idx)%time_elapsed = particles(idx)%time_elapsed + particles(idx)%dt
            ELSE
                num_of_inactive = num_of_inactive + 1
            END IF
        end do
        !$omp END PARALLEL DO

        IF (num_of_inactive .EQ. num_of_particles) THEN 
            breakpoint = .TRUE. 
        ELSE 
            breakpoint = .FALSE.
        END IF
        IF (particles(1)%time_elapsed .GE. t_total .AND. breakpoint .EQV. .TRUE.) THEN
            all_done = .TRUE.
        END IF
    END SUBROUTINE

    FUNCTION output(start_idx, end_idx)
        INTEGER, INTENT(IN) :: start_idx
        INTEGER, INTENT(IN) :: end_idx
        INTEGER, PARAMETER :: num_of_properties = 23
        DOUBLE PRECISION, DIMENSION(end_idx-start_idx + 1, num_of_properties) :: output

        INTEGER :: idx_particles, idx_array

        !$omp PARALLEL DO NUM_THREADS(MAX)
        do idx_particles = start_idx, end_idx
            ! Adapt indexing
            idx_array = idx_particles - start_idx + 1

            ! General information (index and elapsed time)
            output(idx_array, 1) = idx_particles
            output(idx_array, 2) = particles(idx_particles)%time_elapsed

            ! Output position
            output(idx_array, 3) = particles(idx_particles)%r(1)
            output(idx_array, 4) = particles(idx_particles)%r(2)
            output(idx_array, 5) = particles(idx_particles)%r(3)

            ! Output velocity
            output(idx_array, 6) = particles(idx_particles)%v(1)
            output(idx_array, 7) = particles(idx_particles)%v(2)
            output(idx_array, 8) = particles(idx_particles)%v(3)

            ! Output diameters
            output(idx_array, 9) = particles(idx_particles)%d_core
            output(idx_array, 10) = particles(idx_particles)%d_shell

            ! Output circumstances
            output(idx_array, 11) = particles(idx_particles)%T
            output(idx_array, 12) = particles(idx_particles)%T_environment
            output(idx_array, 13) = particles(idx_particles)%humidity
            output(idx_array, 14) = particles(idx_particles)%v_wind(1)
            output(idx_array, 15) = particles(idx_particles)%v_wind(2)
            output(idx_array, 16) = particles(idx_particles)%v_wind(3)

            ! Output mass
            output(idx_array, 17) = mass_core(particles(idx_particles))
            output(idx_array, 18) = mass_shell(particles(idx_particles))

            ! Output acting force
            output(idx_array, 19) = particles(idx_particles)%f(1)
            output(idx_array, 20) = particles(idx_particles)%f(2)
            output(idx_array, 21) = particles(idx_particles)%f(3)

            ! Output status variables
            output(idx_array, 22) = MERGE(1.d0, 0.d0, particles(idx_particles)%core_only)
            output(idx_array, 23) = MERGE(1.d0, 0.d0, particles(idx_particles)%active)
        end do 
        !$omp END PARALLEL DO
    END FUNCTION

END MODULE particle_simulation