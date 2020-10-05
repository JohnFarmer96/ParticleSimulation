MODULE module_particle
    !! Define custom datatype "particle"
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

    
    TYPE particle
        !! Custom Datatype for Particle-Simulation
    
        DOUBLE PRECISION, DIMENSION(dim) :: r
            !! Current Position [m]
        
        DOUBLE PRECISION, DIMENSION(dim) :: v
            !! Current Velocity [m/s]
        
        DOUBLE PRECISION, DIMENSION(dim) :: f
            !! Current Force [µN = µg·m/s]
        
        DOUBLE PRECISION :: d_core
            !! Particle Core Data (d = diameter [µm])
        
        DOUBLE PRECISION :: d_shell
            !! Particle Shell Data (d = diameter [µm])
        
        DOUBLE PRECISION :: T
            !! Temperature of particle [K]
        
        DOUBLE PRECISION :: time_elapsed
            !! Elapsed time [s]
        
        LOGICAL :: core_only
            !! Indicates if particle-shell is evaporated
        
        LOGICAL :: active
            !! Indicates if particle is still in the air
        
        DOUBLE PRECISION, DIMENSION(dim) :: v_wind
            !! Velocity of Wind [m/s]
        
        DOUBLE PRECISION :: T_environment
            !! Temperature of environment [K]
        
        DOUBLE PRECISION :: humidity
            !! Relative Humidity in environment [%]
        
        DOUBLE PRECISION :: dt
            !! Current Stepwidth [s]

    END TYPE particle
    
CONTAINS

    FUNCTION v_euclid(prtcl)
        !! Euclidic norm of velocity of particle
        TYPE(particle), INTENT(IN) :: prtcl
            !! Desired particle

        ! Euclidic norm of velocity of particle [m/s]
        DOUBLE PRECISION :: v_euclid

        ! Calculation
        v_euclid = abs(sqrt(sum(prtcl%v**2)))
    END FUNCTION

    
    FUNCTION mass_core(prtcl)
        !! Get mass of particle core [fg = femtogramm]
        TYPE(particle), INTENT(IN) :: prtcl
            !! Desired particle

        ! Mass of particle [fg]
        DOUBLE PRECISION :: mass_core

        ! Calculation
        mass_core = vol_core(prtcl) * rho_cov2
    END FUNCTION

    
    FUNCTION vol_core(prtcl)
        !! Get volume of particle core [µm³]
        TYPE(particle), INTENT(IN) :: prtcl
            !! Desired particle
        
        ! Volume of particle core [µm³]
        DOUBLE PRECISION :: vol_core

        ! Calculation
        vol_core = (prtcl%d_core/2)**3*PI*4/3
    END FUNCTION

    
    FUNCTION mass_shell(prtcl)
        !! Get mass of particle shell [fg = femtogramm]
        TYPE(particle), INTENT(IN) :: prtcl
            !! Desired particle

        ! Mass of particle shell [fg]
        DOUBLE PRECISION :: mass_shell

        ! Calculation
        mass_shell = vol_shell(prtcl) * rho_H20
    END FUNCTION

    
    FUNCTION vol_shell(prtcl)
        !!Get volume of particle shell [µm³]
        TYPE(particle), INTENT(IN) :: prtcl
            !! Desired particle

        ! Volume of particle shell [µm³]
        DOUBLE PRECISION :: vol_shell

        ! Calculation
        vol_shell = ((prtcl%d_shell/2)**3 - (prtcl%d_core/2)**3)*PI*4/3
    END FUNCTION

    
    SUBROUTINE verify_shell(prtcl)
        !! Verify if particle shell is completely evaporated
        TYPE(particle), INTENT(INOUT) :: prtcl
            !! Desired particle

        ! Check if shell diameter is less or equal to certain threshold (hard-coded: 1%) of core diameter
        IF ( prtcl%d_shell .le. 1.01*prtcl%d_core ) THEN
            prtcl%core_only = .TRUE.
            prtcl%d_shell = prtcl%d_core
        ELSE
            prtcl%core_only = .FALSE.
        END IF
    END SUBROUTINE


    SUBROUTINE verify_status(prtcl, t_total)
        !! Verify if particle is still active
        TYPE(particle), INTENT(INOUT) :: prtcl
            !! Desired particle
        DOUBLE PRECISION, INTENT(IN) :: t_total
            !! Total Simulaiton Time

        ! Check if z-coordinate is less or equal to 0 (ground-level)
        IF ( prtcl%r(3) .le. 0.d0 ) THEN
            prtcl%active = .FALSE.
            prtcl%r(3) = 0
        ELSE IF (prtcl%time_elapsed .ge. t_total) THEN
            prtcl%active = .FALSE.
        ELSE
            prtcl%active = .TRUE.
        END IF
    END SUBROUTINE


    SUBROUTINE set_dt(prtcl, f_r)
        !! Set current stepwidth [s]
        TYPE(particle), INTENT(INOUT) :: prtcl
            !! Desired Particle
        DOUBLE PRECISION, INTENT(IN) :: f_r
            !! Air resistance force [µN]

        ! Max change of acceleration [m/s]
        DOUBLE PRECISION, PARAMETER :: boundary = 5E-4
        ! Conversion Factor
        DOUBLE PRECISION, PARAMETER :: conversion1 = 1E-9
        ! Conversion Factor
        DOUBLE PRECISION, PARAMETER :: conversion2 = 1E-6
        ! Default Value 
        DOUBLE PRECISION, PARAMETER :: default = 1E-3

        ! Local parameters
        DOUBLE PRECISION :: dt1, dt2

        dt1 = boundary * (mass_core(prtcl) + mass_shell(prtcl))/f_r*conversion1
        dt2 = prtcl%d_shell/(200*v_euclid(prtcl))*conversion2

        IF(dt1 .ge. dt2) THEN
            prtcl%dt = dt2
        ELSE
            prtcl%dt = dt1
        END IF

        IF(prtcl%dt .GT. 1) prtcl%dt = default
    END SUBROUTINE
    
END MODULE module_particle