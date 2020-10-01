MODULE module_force
    use module_parameters
    USE module_particle
    IMPLICIT NONE

CONTAINS

    ! Update Acting Force on Particle (required in each step) [µN = µg*m/s²]
    SUBROUTINE evaluate_force(prtcl)
        ! Desired particle
        TYPE(particle), INTENT(INOUT) :: prtcl

        ! Delete old Value
        call reset(prtcl)
        ! Add gravitation-force
        call gravitation(prtcl)
        ! Add air-resistance-force
        call air_resistance(prtcl)
        ! Add wind-force
        call wind(prtcl)
    END SUBROUTINE

    ! Reinitialize acting force [µN = µg*m/s²]
    SUBROUTINE reset(prtcl)
        ! Desired particle
        TYPE(particle), INTENT(INOUT) :: prtcl

        ! Set to 0
        prtcl%f = 0
    END SUBROUTINE

    ! Calculate gravitational force [µN = µg*m/s²]
    SUBROUTINE gravitation(prtcl)
        ! Desired particle
        TYPE(particle), INTENT(INOUT) :: prtcl
        ! Conversion factor (fg to µg)
        DOUBLE PRECISION, PARAMETER :: conversion = 10.0E-9

        ! Newton Law 
        prtcl%f = prtcl%f + (mass_core(prtcl) + mass_shell(prtcl))*g*conversion
    END SUBROUTINE

    ! Calculate air resistance force [µN = µg*m/s²]
    SUBROUTINE air_resistance(prtcl)
        ! Desired particle
        TYPE(particle), INTENT(INOUT) :: prtcl
        !Conversion factor (kg to µg and µm to m)
        DOUBLE PRECISION, PARAMETER :: conversion = 10.0E3

        ! Stokes Law
        prtcl%f = prtcl%f + 6*PI*etha_air(prtcl%T_environment)*(prtcl%d_shell/2)*prtcl%v*(-1)
    END SUBROUTINE

    ! Calculate wind force [µN = µg*m/s²]
    SUBROUTINE wind(prtcl)
        ! Desired particle
        TYPE(particle), INTENT(INOUT) :: prtcl
        ! Conversion factor (kg to µg and µm to m)
        DOUBLE PRECISION, PARAMETER :: conversion = 10.0E-3

        ! Wind Force on particle 
        prtcl%f = prtcl%f + rho_air(prtcl%T_environment)/2*prtcl%v_wind*((prtcl%d_shell/2)**2*PI)*conversion
    END SUBROUTINE

END MODULE module_force