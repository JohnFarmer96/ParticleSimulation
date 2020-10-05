MODULE numeric_integration
    !! Generic Implementation of numerical integration procedures
    IMPLICIT NONE
    
    INTERFACE 
        FUNCTION func(y, params, params_dim, alg_dim) result(dydx)
            !! Generic Function Interface
            INTEGER, INTENT(IN) :: alg_dim
                !! Dimension of State-Vector
            INTEGER, INTENT(IN) :: params_dim
                !! Dimension of Parameter-Vector
            DOUBLE PRECISION, DIMENSION(params_dim), INTENT(IN) :: params
                !! Parameter Vector
            DOUBLE PRECISION, DIMENSION(alg_dim), INTENT(IN) :: y
                !! State-Vector
            DOUBLE PRECISION, DIMENSION(alg_dim) :: dydx
                !! 1st Derivative of State-Vector
        END FUNCTION

        SUBROUTINE num_int_procedure(f, y, dt, params, params_dim, alg_dim)
            !! Generic Numeric Integration Procedure Interface
            IMPORT :: func
            PROCEDURE(func) :: f
                !! Generic Function that is used in integration procedure
            INTEGER, INTENT(IN) :: alg_dim
                !! Dimension of State-Vector
            INTEGER, INTENT(IN) :: params_dim
                !! Dimension of Parameter-Vector
            DOUBLE PRECISION, INTENT(IN) :: dt
                !! Stepwidth
            DOUBLE PRECISION, DIMENSION(params_dim), INTENT(IN) :: params
                !! Parameter Vector
            DOUBLE PRECISION, DIMENSION(alg_dim), INTENT(INOUT) :: y
                !! State Vector
        END SUBROUTINE
    END INTERFACE


CONTAINS


    SUBROUTINE runge_kutta_2k(f, y, dt, params, params_dim, alg_dim)
        !! Runge Kutta Calculation (2nd Order)
        ! Dummy Arguments
        PROCEDURE(func) :: f
            !! Generic Function that is used in integration procedure
        INTEGER, INTENT(IN) :: alg_dim
            !! Dimension of State-Vector
        INTEGER, INTENT(IN) :: params_dim
            !! Dimension of Parameter-Vector
        DOUBLE PRECISION, INTENT(IN) :: dt
            !! Stepwidth
        DOUBLE PRECISION, DIMENSION(params_dim), INTENT(IN) :: params
            !! Parameter Vector
        DOUBLE PRECISION, DIMENSION(alg_dim), INTENT(INOUT) :: y
            !! State Vector
        
        ! Local Variables
        DOUBLE PRECISION, DIMENSION(alg_dim) :: k1, k2

        ! Runge Kutta Algorithm (2nd Order)
        k1 = dt*f(y, params, params_dim, alg_dim)
        k2 = dt*f(y+k1/2, params, params_dim, alg_dim)

        ! Final Value
        y = y+k2
    END SUBROUTINE


    SUBROUTINE runge_kutta_4k(f, y, dt, params, params_dim, alg_dim)
        !! Runge Kutta Calculation (4th Order)
        ! Dummy Arguments
        PROCEDURE(func) :: f
            !! Generic Function that is used in integration procedure
        INTEGER, INTENT(IN) :: alg_dim
            !! Dimension of State-Vector
        INTEGER, INTENT(IN) :: params_dim
            !! Dimension of Parameter-Vector
        DOUBLE PRECISION, INTENT(IN) :: dt
            !! Stepwidth
        DOUBLE PRECISION, DIMENSION(params_dim), INTENT(IN) :: params
            !! Parameter Vector
        DOUBLE PRECISION, DIMENSION(alg_dim), INTENT(INOUT) :: y
            !! State Vector

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
    

    SUBROUTINE euler(f, y, dt, params, params_dim, alg_dim)
        !! Euler Routine (Explicit)
        ! Dummy Arguments
        PROCEDURE(func) :: f
            !! Generic Function that is used in integration procedure
        INTEGER, INTENT(IN) :: alg_dim
            !! Dimension of State-Vector
        INTEGER, INTENT(IN) :: params_dim
            !! Dimension of Parameter-Vector
        DOUBLE PRECISION, INTENT(IN) :: dt
            !! Stepwidth
        DOUBLE PRECISION, DIMENSION(params_dim), INTENT(IN) :: params
            !! Parameter Vector
        DOUBLE PRECISION, DIMENSION(alg_dim), INTENT(INOUT) :: y
            !! State Vector

        ! Euler Algorithm (Explicit)
        y = y+dt*f(y, params, params_dim, alg_dim)
    END SUBROUTINE

END MODULE numeric_integration