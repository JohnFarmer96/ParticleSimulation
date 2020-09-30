PROGRAM test
    USE numeric_integration
    USE particle_simulation
    IMPLICIT NONE

    ! Particle concentration in the air [x/m³] (0...30.000)
    INTEGER, PARAMETER :: concentration = 10
    ! Intensity coefficient [%] (0...1) 
    DOUBLE PRECISION, PARAMETER :: intensity = 0.5
    ! Velocity of Wind [m/s]
    DOUBLE PRECISION, DIMENSION(dim), PARAMETER :: v_wind = (/1.d0,0.d0,0.d0/)
    ! Temperature of environment [K]
    DOUBLE PRECISION, PARAMETER :: T_environment = 300
    ! Relative Humidity in environment [%] (0...1)
    DOUBLE PRECISION, PARAMETER :: humidity = 0.1

    ! Data Array 
    DOUBLE PRECISION, DIMENSION(concentration, 21) :: data_array

    call initialize(concentration, intensity, T_environment, humidity, v_wind)
    data_array = output(1, 10)

    print *,data_array
END PROGRAM