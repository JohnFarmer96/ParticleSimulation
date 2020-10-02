MODULE module_tmanagement
    USE module_parameters
    USE module_particle

    IMPLICIT NONE

CONTAINS

    FUNCTION dt(prtcl, f_r)
        ! Desired particle
        TYPE(particle), INTENT(IN) :: prtcl
        ! Air resistance force [µN]
        DOUBLE PRECISION, DIMENSION(dim), INTENT(IN) :: f_r
        ! Max change of acceleration [m/s]
        DOUBLE PRECISION, PARAMETER :: boundary = 5E-4
        ! Conversion Factor
        DOUBLE PRECISION, PARAMETER :: conversion = 1E-9
        ! Stepwidth [s]
        DOUBLE PRECISION :: dt

        ! Local parameters
        DOUBLE PRECISION :: dt1, dt2

        dt1 = boundary * (mass_core(prtcl) + mass_shell(prtcl))/sqrt(sum(f_r**2))*conversion
        dt2 = prtcl%d_shell/(200*v_euclid(prtcl))

        IF(dt1 .ge. dt2) THEN
            dt = dt2
        ELSE
            dt = dt1
        END IF

    END FUNCTION
    
END MODULE module_tmanagement