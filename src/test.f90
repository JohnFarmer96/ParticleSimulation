PROGRAM test
    USE numeric_integration
    USE particle_simulation
    IMPLICIT NONE

    ! Particle concentration in the air [x/m³] (0...30.000)
    INTEGER, PARAMETER :: concentration = 10
    ! Intensity coefficient [%] (0...1) 
    DOUBLE PRECISION, PARAMETER :: intensity = 0.1
    ! Velocity of Wind [m/s]
    DOUBLE PRECISION, DIMENSION(dim), PARAMETER :: v_wind = (/1.d0,0.d0,0.d0/)
    ! Temperature of environment [K]
    DOUBLE PRECISION, PARAMETER :: T_environment = 300
    ! Relative Humidity in environment [%] (0...1)
    DOUBLE PRECISION, PARAMETER :: humidity = 0.1
    ! Stepwidth [s]
    DOUBLE PRECISION, PARAMETER :: dt = 0.01

    ! Data Array 
    DOUBLE PRECISION, DIMENSION(concentration, 21) :: data_array
    ! Number of Iterations
    INTEGER :: n, niter

    ! Initialize
    call initialize(concentration, intensity, T_environment, humidity, v_wind)
    data_array = output(start_idx=1, end_idx=concentration)
    call print_output(data_array)

    ! Update
    niter = 1000
    do n = 1, niter
        call update(euler, runge_kutta_4k, dt)
    end do

    data_array = output(start_idx=1, end_idx=concentration)
    call print_output(data_array)

CONTAINS 

    SUBROUTINE print_output(array)
        DOUBLE PRECISION, DIMENSION(concentration, 21), INTENT(IN) :: array
        INTEGER :: idx

        print '(21(a14))',"idx","time","r_x", "r_y","r_z","v_x","v_y","v_z","d_core",&
            "d_shell","T_par","T_env","humid","vw_x","vw_y","vw_z","f_x","f_y","f_z","core","active" 
        do idx = 1, SIZE(array, DIM=1) 
            print '(21(g14.6))', array(idx,:)
        end do
    END SUBROUTINE
END PROGRAM