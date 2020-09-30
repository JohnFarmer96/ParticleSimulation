MODULE module_movement
    USE module_parameters
    USE module_particle
    USE module_force
    USE numeric_integration
    IMPLICIT NONE

CONTAINS

    ! Update Particle Movement 
    SUBROUTINE evaluate_movement(numeric_integration_procedure, prtcl, dt)
        ! Desired numeric integration procedure (Euler, Runge-Kutta, ...)
        PROCEDURE(num_int_procedure) :: numeric_integration_procedure
        ! Desired particle
        TYPE(particle), INTENT(INOUT) :: prtcl
        ! Stepwidth [s]
        DOUBLE PRECISION, INTENT(IN) :: dt

        ! Key Value
        DOUBLE PRECISION, DIMENSION(2*dim) :: y
        ! Parameter Array for Function Arguments
        DOUBLE PRECISION, DIMENSION(4) :: params

        ! Prepare function arguments
        y(1:SIZE(y, DIM=1)/2) = prtcl%r
        y(SIZE(y, DIM=1)/2+1:) = prtcl%v

        call evaluate_force(prtcl)
        params(1) = prtcl%f(1)
        params(2) = prtcl%f(2)
        params(3) = prtcl%f(3)
        params(4) = mass_core(prtcl) + mass_shell(prtcl)

        ! Update Key Value
        call numeric_integration_procedure(dydt, y, dt, params, SIZE(params, DIM=1), SIZE(y, DIM=1))

        ! Assign result to particle
        prtcl%r = y(1:SIZE(y, DIM=1)/2)
        prtcl%v = y(SIZE(y, DIM=1)/2+1:)

    END SUBROUTINE

    FUNCTION dydt(y, params, params_dim, alg_dim)
        INTEGER, INTENT(IN) :: alg_dim
        INTEGER, INTENT(IN) :: params_dim
        DOUBLE PRECISION, DIMENSION(params_dim), INTENT(IN) :: params
        DOUBLE PRECISION, DIMENSION(alg_dim), INTENT(IN) :: y
        DOUBLE PRECISION, DIMENSION(alg_dim) :: dydt

        ! Resolve params (Individually)
        DOUBLE PRECISION, DIMENSION(alg_dim) :: force
        DOUBLE PRECISION :: mass 

        force(1) = params(1)
        force(2) = params(2)
        force(3) = params(3)
        mass = params(4)

        dydt(1:alg_dim/2) = y(alg_dim/2+1:)
        dydt(alg_dim/2+1:) = force/mass
    END FUNCTION

END MODULE module_movement