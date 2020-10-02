MODULE module_particle
    USE module_parameters
    IMPLICIT NONE

    PUBLIC :: v_euclid
    PUBLIC :: mass_shell
    PUBLIC :: mass_core
    PUBLIC :: verify_status
    PUBLIC :: verify_shell
    PUBLIC :: set_dt
    PRIVATE :: vol_core
    PRIVATE :: vol_shell

    ! Custom Datatype for Particle-Simulation
    TYPE particle
        ! Current Position [m]
        DOUBLE PRECISION, DIMENSION(dim) :: r

        ! Current Velocity [m/s]
        DOUBLE PRECISION, DIMENSION(dim) :: v

        ! Current Force [µN = µg·m/s]
        DOUBLE PRECISION, DIMENSION(dim) :: f

        ! Particle Core Data (d = diameter [µm])
        DOUBLE PRECISION :: d_core

        ! Particle Shell Data (d = diameter [µm])
        DOUBLE PRECISION :: d_shell

        ! Temperature of particle [K]
        DOUBLE PRECISION :: T

        ! Elapsed time [s]
        DOUBLE PRECISION :: time_elapsed

        ! Indicates if particle-shell is evaporated
        LOGICAL :: core_only

        ! Indicates if particle is still in the air
        LOGICAL :: active

        ! Velocity of Wind [m/s]
        DOUBLE PRECISION, DIMENSION(dim) :: v_wind

        ! Temperature of environment [K]
        DOUBLE PRECISION :: T_environment

        ! Relative Humidity in environment [%]
        DOUBLE PRECISION :: humidity

        ! Individual Stepwidth
        DOUBLE PRECISION :: dt

    END TYPE particle
    
CONTAINS

    ! Euclidic norm of velocity of particle
    FUNCTION v_euclid(prtcl)
        ! Desired particle
        TYPE(particle), INTENT(IN) :: prtcl
        ! Euclidic norm of velocity of particle [m/s]
        DOUBLE PRECISION :: v_euclid

        ! Calculation
        v_euclid = abs(sqrt(sum(prtcl%v**2)))
    END FUNCTION

    ! Get mass of particle core [fg = femtogramm]
    FUNCTION mass_core(prtcl)
        ! Desired particle
        TYPE(particle), INTENT(IN) :: prtcl
        ! Mass of particle [fg]
        DOUBLE PRECISION :: mass_core

        ! Calculation
        mass_core = vol_core(prtcl) * rho_cov2
    END FUNCTION

    !Get volume of particle core [µm³]
    FUNCTION vol_core(prtcl)
        ! Desired particle
        TYPE(particle), INTENT(IN) :: prtcl
        ! Volume of particle core [µm³]
        DOUBLE PRECISION :: vol_core

        ! Calculation
        vol_core = (prtcl%d_core/2)**3*PI*4/3
    END FUNCTION

    ! Get mass of particle shell [fg = femtogramm]
    FUNCTION mass_shell(prtcl)
        ! Desired particle
        TYPE(particle), INTENT(IN) :: prtcl
        ! Mass of particle shell [fg]
        DOUBLE PRECISION :: mass_shell

        ! Calculation
        mass_shell = vol_shell(prtcl) * rho_H20
    END FUNCTION

    !Get volume of particle shell [µm³]
    FUNCTION vol_shell(prtcl)
        ! Desired particle
        TYPE(particle), INTENT(IN) :: prtcl
        ! Volume of particle shell [µm³]
        DOUBLE PRECISION :: vol_shell

        ! Calculation
        vol_shell = ((prtcl%d_shell/2)**3 - (prtcl%d_core/2)**3)*PI*4/3
    END FUNCTION

    ! Verify if particle shell is completely evaporated
    SUBROUTINE verify_shell(prtcl)
        ! Desired particle
        TYPE(particle), INTENT(INOUT) :: prtcl

        ! Check if shell diameter is less or equal to certain threshold (hard-coded: 1%) of core diameter
        IF ( prtcl%d_shell .le. 1.01*prtcl%d_core ) THEN
            prtcl%core_only = .TRUE.
            prtcl%d_shell = prtcl%d_core
        ELSE
            prtcl%core_only = .FALSE.
        END IF
    END SUBROUTINE

    ! Verify if particle is still active
    SUBROUTINE verify_status(prtcl)
        ! Desired particle
        TYPE(particle), INTENT(INOUT) :: prtcl

        ! Check if z-coordinate is less or equal to 0 (ground-level)
        IF ( prtcl%r(3) .le. 0.d0 ) THEN
            prtcl%active = .FALSE.
            prtcl%r(3) = 0
        ELSE
            prtcl%active = .TRUE.
        END IF
    END SUBROUTINE

    SUBROUTINE set_dt(prtcl, f_r)
        ! Desired particle
        TYPE(particle), INTENT(INOUT) :: prtcl
        ! Air resistance force [µN]
        DOUBLE PRECISION, INTENT(IN) :: f_r
        ! Max change of acceleration [m/s]
        DOUBLE PRECISION, PARAMETER :: boundary = 5E-4
        ! Conversion Factor
        DOUBLE PRECISION, PARAMETER :: conversion = 1E-9

        ! Local parameters
        DOUBLE PRECISION :: dt1, dt2

        dt1 = boundary * (mass_core(prtcl) + mass_shell(prtcl))/f_r*conversion
        dt2 = prtcl%d_shell/(200*v_euclid(prtcl))

        IF(dt1 .ge. dt2) THEN
            prtcl%dt = dt2
        ELSE
            prtcl%dt = dt1
        END IF
    END SUBROUTINE
    
END MODULE module_particle