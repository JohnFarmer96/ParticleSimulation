MODULE module_particle
    USE module_parameters
    IMPLICIT NONE

    ! Custom Datatype for Particle-Simulation
    TYPE particle
        ! Current Position [m]
        DOUBLE PRECISION, DIMENSION(dim) :: r

        ! Current Velocity [m/s]
        DOUBLE PRECISION, DIMENSION(dim) :: v

        ! Current Force [N = kg·m/s]
        DOUBLE PRECISION, DIMENSION(dim) :: f

        ! Particle Core Data (d = diameter [m])
        DOUBLE PRECISION :: d_core

        ! Particle Shell Data (d = diameter [m])
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

    END TYPE particle
    
CONTAINS

    FUNCTION v_euclid(prtcl)
        ! Desired particle
        TYPE(particle), INTENT(IN) :: prtcl
        ! Mass of particle [kg]
        DOUBLE PRECISION :: v_euclid

        ! Calculation
        v_euclid = sqrt(sum(prtcl%v**2))
    END FUNCTION

    ! Get mass of particle core [kg]
    FUNCTION mass_core(prtcl)
        ! Desired particle
        TYPE(particle), INTENT(IN) :: prtcl
        ! Mass of particle [kg]
        DOUBLE PRECISION :: mass_core

        ! Calculation
        mass_core = vol_core(prtcl) * rho_cov2
    END FUNCTION

    !Get volume of particle core [m³]
    FUNCTION vol_core(prtcl)
        ! Desired particle
        TYPE(particle), INTENT(IN) :: prtcl
        ! Volume of particle core [m³]
        DOUBLE PRECISION :: vol_core

        ! Calculation
        vol_core = (prtcl%d_core/2)**3 *PI*4/3
    END FUNCTION

    ! Get mass of particle shell [kg]
    FUNCTION mass_shell(prtcl)
        ! Desired particle
        TYPE(particle), INTENT(IN) :: prtcl
        ! Mass of particle shell [kg]
        DOUBLE PRECISION :: mass_shell

        ! Calculation
        mass_shell = vol_shell(prtcl) * rho_H20
    END FUNCTION

    !Get volume of particle shell [m³]
    FUNCTION vol_shell(prtcl)
        ! Desired particle
        TYPE(particle), INTENT(IN) :: prtcl
        ! Volume of particle shell [m³]
        DOUBLE PRECISION :: vol_shell

        ! Calculation
        vol_shell = ((prtcl%d_shell/2)**3)*PI*4/3 - vol_core(prtcl)
    END FUNCTION

    ! Verify if particle shell is completely evaporated
    SUBROUTINE verify_shell(prtcl)
        ! Desired particle
        TYPE(particle), INTENT(INOUT) :: prtcl

        ! Check if shell diameter is less or equal to certain threshold (hard-coded: 1%) of core diameter
        IF ( prtcl%d_shell .le. 1.01*prtcl%d_core ) THEN
            prtcl%core_only = .TRUE.
        ELSE
            prtcl%core_only = .FALSE.
        END IF
    END SUBROUTINE

    ! Verify if particle is still active
    SUBROUTINE verify_status(prtcl)
        ! Desired particle
        TYPE(particle), INTENT(INOUT) :: prtcl

        ! Check if z-coordinate is less or equal to 0 (ground-level)
        IF ( prtcl%r(3) .le. 0 ) THEN
            prtcl%active = .FALSE.
        ELSE
            prtcl%active = .TRUE.
        END IF
    END SUBROUTINE
    
END MODULE module_particle