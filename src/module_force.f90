MODULE module_force
    use module_parameters
    USE module_particle
    IMPLICIT NONE

CONTAINS

    ! Update Acting Force on Particle (required in each step)
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

    ! Reinitialize acting force 
    SUBROUTINE reset(prtcl)
        ! Desired particle
        TYPE(particle), INTENT(INOUT) :: prtcl

        ! Set to 0
        prtcl%f = 0
    END SUBROUTINE

    ! Calculate gravitational force
    SUBROUTINE gravitation(prtcl)
        ! Desired particle
        TYPE(particle), INTENT(INOUT) :: prtcl

        ! Newton Law
        prtcl%f = prtcl%f + (mass_core(prtcl) + mass_shell(prtcl))*g
    END SUBROUTINE

    ! Calculate air resistance force
    SUBROUTINE air_resistance(prtcl)
        ! Desired particle
        TYPE(particle), INTENT(INOUT) :: prtcl

        ! Stokes Law
        prtcl%f = prtcl%f + 6*PI*etha_air(prtcl%T_environment)*(prtcl%d_shell/2)*prtcl%v*(-1)
    END SUBROUTINE

    ! Calculate wind force
    SUBROUTINE wind(prtcl)
        ! Desired particle
        TYPE(particle), INTENT(INOUT) :: prtcl

        ! Wind Force on particle
        prtcl%f = prtcl%f + rho_air(prtcl%T_environment)/2*prtcl%v_wind*((prtcl%d_shell/2)**2*PI)
    END SUBROUTINE

END MODULE module_force