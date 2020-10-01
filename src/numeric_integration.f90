MODULE numeric_integration
    IMPLICIT NONE
    
    ! Generic Function Interface
    INTERFACE 
        FUNCTION func(y, params, params_dim, alg_dim) result(dydx)
            INTEGER, INTENT(IN) :: alg_dim
            INTEGER, INTENT(IN) :: params_dim
            DOUBLE PRECISION, DIMENSION(params_dim), INTENT(IN) :: params
            DOUBLE PRECISION, DIMENSION(alg_dim), INTENT(IN) :: y
            DOUBLE PRECISION, DIMENSION(alg_dim) :: dydx
        END FUNCTION

        SUBROUTINE num_int_procedure(f, y, dt, params, params_dim, alg_dim)
            IMPORT :: func
            PROCEDURE(func) :: f
            INTEGER, INTENT(IN) :: alg_dim
            INTEGER, INTENT(IN) :: params_dim
            DOUBLE PRECISION, INTENT(IN) :: dt
            DOUBLE PRECISION, DIMENSION(params_dim), INTENT(IN) :: params
            DOUBLE PRECISION, DIMENSION(alg_dim), INTENT(INOUT) :: y
        END SUBROUTINE
    END INTERFACE


CONTAINS

    ! Runge Kutta Calculation (2nd Order)
    SUBROUTINE runge_kutta_2k(f, y, dt, params, params_dim, alg_dim)
        ! Dummy Arguments
        PROCEDURE(func) :: f
        INTEGER, INTENT(IN) :: alg_dim
        INTEGER, INTENT(IN) :: params_dim
        DOUBLE PRECISION, INTENT(IN) :: dt
        DOUBLE PRECISION, DIMENSION(params_dim), INTENT(IN) :: params
        DOUBLE PRECISION, DIMENSION(alg_dim), INTENT(INOUT) :: y
        
        ! Local Variables
        DOUBLE PRECISION, DIMENSION(alg_dim) :: k1, k2

        ! Runge Kutta Algorithm (2nd Order)
        k1 = dt*f(y, params, params_dim, alg_dim)
        k2 = dt*f(y+k1/2, params, params_dim, alg_dim)

        ! Final Value
        y = y+k2
    END SUBROUTINE

    ! Runge Kutta Calculation (4th Order)
    SUBROUTINE runge_kutta_4k(f, y, dt, params, params_dim, alg_dim)
        ! Dummy Arguments
        PROCEDURE(func) :: f
        INTEGER, INTENT(IN) :: alg_dim
        INTEGER, INTENT(IN) :: params_dim
        DOUBLE PRECISION, INTENT(IN) :: dt
        DOUBLE PRECISION, DIMENSION(params_dim), INTENT(IN) :: params
        DOUBLE PRECISION, DIMENSION(alg_dim), INTENT(INOUT) :: y

        ! Local Variables
        DOUBLE PRECISION, DIMENSION(alg_dim) :: k1, k2, k3, k4

        ! Runge Kutta Algorithm (4th Order)
        k1 = dt*f(y, params, params_dim, alg_dim)
        k2 = dt*f(y+k1/2, params, params_dim, alg_dim)
        k3 = dt*f(y+k2/2, params, params_dim, alg_dim)
        k4 = dt*f(y+k3, params, params_dim, alg_dim)

        ! Final Value
        y = y+(k1+2*(k2+k3)+k4)/6
    END SUBROUTINE
    
    ! Euler Routine (Explicit)
    SUBROUTINE euler(f, y, dt, params, params_dim, alg_dim)
        ! Dummy Arguments
        PROCEDURE(func) :: f
        INTEGER, INTENT(IN) :: alg_dim
        INTEGER, INTENT(IN) :: params_dim
        DOUBLE PRECISION, INTENT(IN) :: dt
        DOUBLE PRECISION, DIMENSION(params_dim), INTENT(IN) :: params
        DOUBLE PRECISION, DIMENSION(alg_dim), INTENT(INOUT) :: y

        ! Euler Algorithm (Explicit)
        y = y+dt*f(y, params, params_dim, alg_dim)
    END SUBROUTINE

END MODULE numeric_integration