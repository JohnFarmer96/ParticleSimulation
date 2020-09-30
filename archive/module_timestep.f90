MODULE module_timestep
    USE module_particle
    USE module_force
    USE module_evaporation
    USE module_integration
    IMPLICIT NONE

CONTAINS

    SUBROUTINE update_evaporation (prtcl, numeric_integration_procedure, dt)
        PROCEDURE(num_int_procedure) :: numeric_integration_procedure
        TYPE(particle), INTENT(INOUT) :: prtcl
        DOUBLE PRECISION, INTENT(IN) :: dt

        call evaluate_evaporation(numeric_integration_procedure, prtcl, dt)
    END SUBROUTINE

    SUBROUTINE update_movement (prtcl, numeric_integration_procedure, dt)
        PROCEDURE(num_int_procedure) :: numeric_integration_procedure
        TYPE(particle), INTENT(INOUT) :: prtcl
        DOUBLE PRECISION, INTENT(IN) :: dt
        
        INTEGER, PARAMETER :: alg_dim = 2*dim
        DOUBLE PRECISION, DIMENSION(alg_dim) :: y
        
        ! Update Acting Force
        ! =======================================================
        call evaluate_force(prtcl, etha_air, rho_air, v_wind)

        ! Update Position and Velocity
        ! =======================================================
        ! Assign y
        y(1:alg_dim/2) = prtcl%r
        y(alg_dim/2+1:) = prtcl%v

        ! Apply numeric integration procedure (Euler, Runge-Kutta, ...)
        call numeric_integration_procedure(dydt, y, dt, alg_dim)

        ! Assign result to particle
        prtcl%r = y(1:alg_dim/2)
        prtcl%v = y(alg_dim/2+1:)

    END SUBROUTINE

    FUNCTION dydt(y_in, params, params_dim, alg_dim)
        INTEGER, INTENT(IN) :: alg_dim
        INTEGER, INTENT(IN) :: params_dim
        DOUBLE PRECISION, DIMENSION(params_dim), INTENT(IN) :: params
        DOUBLE PRECISION, DIMENSION(alg_dim), INTENT(IN) :: y_in
        DOUBLE PRECISION, DIMENSION(alg_dim) :: dydt

        ! Resolve params (Individually)
        DOUBLE PRECISION, DIMENSION(alg_dim) :: force
        DOUBLE PRECISION :: mass 

        force(1) = params(1)
        force(2) = params(2)
        force(3) = params(3)
        mass = params(4)

        dydt(1:alg_dim/2) = y_in(alg_dim/2+1:)
        dydt(alg_dim/2+1:) = force/mass
    END FUNCTION

END MODULE module_timestep