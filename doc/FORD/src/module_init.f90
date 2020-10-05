MODULE module_init
    !! Semi-Automatic Initialization of Particles
    USE module_particle
    IMPLICIT NONE

    PUBLIC :: initialize_circumstances
    PUBLIC :: initialize_position
    PUBLIC :: initialize_velocity 
    PUBLIC :: initialize_structure
    
CONTAINS

    ! Initialize position of particles
    SUBROUTINE initialize_position(prtcl)
        ! Desired particle
        TYPE(particle), INTENT(INOUT) :: prtcl

        DOUBLE PRECISION :: buffer

        buffer = 0.d0
        ! x-Direction [-0.5...0.5]
        call RANDOM_NUMBER(buffer)
        prtcl%r(1) = (buffer-0.5)

        ! y-Direction [-0.5...0.5]
        call RANDOM_NUMBER(buffer)
        prtcl%r(2) = (buffer-0.5)

        ! z-Direction [1...2]
        call RANDOM_NUMBER(buffer)
        prtcl%r(3) = (buffer+1)

        ! Activate Particle
        prtcl%active = .TRUE.
    END SUBROUTINE

    ! Initialize velocity of particles
    SUBROUTINE initialize_velocity(prtcl, velocity)
        ! Desired particle
        TYPE(particle), INTENT(INOUT) :: prtcl
        ! Direction coefficient [0...1]
        DOUBLE PRECISION, DIMENSION(dim), INTENT(IN) :: velocity

        prtcl%v(1) = velocity(1)
        prtcl%v(2) = velocity(2)
        prtcl%v(3) = velocity(3)


    END SUBROUTINE

    ! Initialize structure of particle
    ! Source: https://www.umweltbundesamt.de/themen/gesundheit/umwelteinfluesse-auf-den-menschen/innenraumluft/infektioese-aerosole-in-innenraeumen#was-sind-aerosole-
    SUBROUTINE initialize_structure(prtcl)
        ! Desired particle
        TYPE(particle), INTENT(INOUT) :: prtcl
        ! Conversion factor [nm to mm]
        DOUBLE PRECISION, PARAMETER :: conversion = 1E-3
        
        DOUBLE PRECISION :: buffer
        ! Assign diameter of core [50nm...100.000nm = 100µm] and convert to [µm]
        call RANDOM_NUMBER(buffer)
        prtcl%d_core = (50 + 99950*buffer)*conversion

        ! Assign diameter of shell (water) 10% of core and convert to [µm]
        prtcl%d_shell = prtcl%d_core*1.1

        ! Assign temperature of particle [between 20 and 35 °C] and convert to [K]
        call RANDOM_NUMBER(buffer)
        prtcl%T = T_0 + 20 + 15*buffer
    END SUBROUTINE

    ! Initialize Particle Circumstances
    SUBROUTINE initialize_circumstances(prtcl, T_environment, humidity, v_wind)
        ! Desired particle
        TYPE(particle), INTENT(INOUT) :: prtcl
        ! Velocity of Wind [m/s]
        DOUBLE PRECISION, DIMENSION(dim) :: v_wind
        ! Temperature of environment [K]
        DOUBLE PRECISION :: T_environment
        ! Relative Humidity in environment [%]
        DOUBLE PRECISION :: humidity

        ! Assign values
        prtcl%v_wind = v_wind
        prtcl%T_environment = T_environment
        prtcl%humidity = humidity
        prtcl%time_elapsed = 0
        prtcl%f = 0
    END SUBROUTINE

END MODULE module_init
