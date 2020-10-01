MODULE module_parameters
    IMPLICIT NONE

    ! Dimension of Simulation
    INTEGER, PARAMETER :: dim = 3

    ! PHYSICAL PARAMETERS
    ! ========================================================

    ! Gravitational Acceleration [m/s²]
    DOUBLE PRECISION, DIMENSION(dim) :: g = (/ 0.0, 0.0, -9.80665/)    

    ! Gas Constant [kg⋅m²·K⁻¹⋅mol⁻¹⋅s⁻²]
    DOUBLE PRECISION, PARAMETER :: R = 8.31446261815324

    ! Standard Atmosphere [Pa = kg/(m⋅s²)] 
    DOUBLE PRECISION, PARAMETER :: p_atm = 101325

    ! Molar Mass (Water) [kg/mol]
    DOUBLE PRECISION, PARAMETER :: M_H2O = 18.01538/1000.d0

    ! Molar Mass (Dry Air) [kg/mol]
    DOUBLE PRECISION, PARAMETER :: M_air = 28.9644/1000.d0
    
    ! Density of Water [kg/m³] [fg/µm³]
    DOUBLE PRECISION, PARAMETER :: rho_H20 = 1000

    ! Density of SARS-CoV-2 [kg/m³] [fg/µm³]
    DOUBLE PRECISION, PARAMETER :: rho_cov2 = 1000

    ! Base Diffusion Coefficient at 0°C [m²/s]
    DOUBLE PRECISION, PARAMETER :: D_0 = 2.3/100000.0
    ! Corresponding Temperature T_0 at 0°C [K]
    DOUBLE PRECISION, PARAMETER :: T_0 = 273.15

    ! Antoine Equation Parameters (H2O)
    DOUBLE PRECISION, PARAMETER :: A = 8.07131
    DOUBLE PRECISION, PARAMETER :: B = 1730.63
    DOUBLE PRECISION, PARAMETER :: C = 233.426
    DOUBLE PRECISION, PARAMETER :: mmHg_Pa_conversion = 133.32

    ! ========================================================
    


    ! MATHEMATICAL PARAMETERS 
    ! ========================================================

    ! Assign Pi to constant Value
    DOUBLE PRECISION, PARAMETER :: PI = 4.d0 * DATAN(1.d0)

    ! ========================================================
    


    ! TOPIC RELATED PARAMETERS
    ! ========================================================

    ! Maximum Velocity of Particles when sneezing [m/s]
    DOUBLE PRECISION, PARAMETER :: sneeze_vel = 44

    ! ========================================================

CONTAINS

    ! Density of Air [kg/m³] [fg/µm³]
    FUNCTION rho_air(T_environment)
        ! Temperature ov environment [K]
        DOUBLE PRECISION, INTENT(IN) :: T_environment
        ! Density of Air [kg/m³]
        DOUBLE PRECISION :: rho_air

        ! Ideal Gas Law
        rho_air = p_atm*M_air/(R*T_environment)
    END FUNCTION

    ! Kinematic Viscosity [m²/s = µm²/s * 10¹²]
    FUNCTION nu_air(T_environment)
        ! Temperature ov environment [K]
        DOUBLE PRECISION, INTENT(IN) :: T_environment
        ! Density of Air [kg/m³]
        DOUBLE PRECISION :: nu_air

        ! Ideal Gas Law
        nu_air = etha_air(T_environment)/rho_air(T_environment)
    END FUNCTION

    ! Dynamic viscosity [Pa⋅s = kg/(m⋅s) = fg/(µm*s)*10¹²] (Interpolation)
    ! Source: https://www.engineersedge.com/physics/viscosity_of_air_dynamic_and_kinematic_14483.htm
    FUNCTION etha_air(T_environment)
        ! Temperature ov environment [K]
        DOUBLE PRECISION, INTENT(IN) :: T_environment
        ! Density of Air [kg/m³]
        DOUBLE PRECISION :: etha_air

        ! Parameters
        DOUBLE PRECISION, PARAMETER :: offset = 1.338/100000.d0
        DOUBLE PRECISION, PARAMETER :: gradient = 0.968/10000000.d0

        ! Linear Interpolation (acc. to graph in source)
        etha_air = gradient*(T_environment - T_0) + offset
    END FUNCTION

END MODULE module_parameters