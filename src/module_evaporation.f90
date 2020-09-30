MODULE module_evaporation
    USE module_parameters
    USE module_particle
    USE numeric_integration
    IMPLICIT NONE

CONTAINS

    ! Update Particle Diameter 
    SUBROUTINE evaluate_evaporation(numeric_integration_procedure, prtcl, dt)
        ! Desired numeric integration procedure (Euler, Runge-Kutta, ...)
        PROCEDURE(num_int_procedure) :: numeric_integration_procedure
        ! Desired particle
        TYPE(particle), INTENT(INOUT) :: prtcl
        ! Stepwidth [s]
        DOUBLE PRECISION, INTENT(IN) :: dt
        
        ! Key Value
        DOUBLE PRECISION, DIMENSION(1) :: y
        ! Parameter Array for Function Arguments
        DOUBLE PRECISION, DIMENSION(4) :: params

        ! Execute only if particle shell is still present
        IF ( prtcl%core_only .eqv. .FALSE.) THEN

            ! Prepare function arguments
            y(1) = prtcl%d_shell
            params(1) = prtcl%T
            params(2) = prtcl%T_environment
            params(3) = v_euclid(prtcl)
            params(4) = prtcl%humidity
            ! Update Key Value
            call numeric_integration_procedure(dddt, y, dt, params, SIZE(params, DIM=1), SIZE(y, DIM=1))
            ! Store Changes
            prtcl%d_shell = y(1)
            
            ! Verify if particle shell is still present
            call verify_shell(prtcl)
        END IF

    END SUBROUTINE

    ! Diffusion Coefficient
    FUNCTION D_Coeff(T_particle, T_environment)
        ! Temperature of Particle T_prtcl and Environment T_env [K]
        DOUBLE PRECISION, INTENT(IN) :: T_particle, T_environment
        ! Mean Temperature T_m [K]
        DOUBLE PRECISION :: T_m
        ! Result (Diffusion Coefficient) [m²/s]
        DOUBLE PRECISION :: D_Coeff

        ! Computation
        T_m = (T_particle + T_environment)/2
        D_Coeff = D_0*(T_m/T_0)**1.8
    END FUNCTION

    ! Calculate partial pressure of H2O [Pa = kg⋅m⁻¹⋅s⁻²]
    FUNCTION p0_H2O(T)
        ! Desired Temperature [K]
        DOUBLE PRECISION, INTENT(IN) :: T
        ! Resulting partial pressure of H2O [Pa]
        DOUBLE PRECISION :: p0_H2O

        ! Antoine Equation (Result needs to be converted from mmHg to Pa) [Pa = kg⋅m⁻¹⋅s⁻²]
        p0_H2O = 10**(A - B/(C+T))*mmHg_Pa_conversion
    END FUNCTION

    ! Calculate partial pressure of H2O in particle (fluid water) [Pa = kg⋅m⁻¹⋅s⁻²]
    FUNCTION pw_H2O(T_particle)
        ! Temperature of particle [K]
        DOUBLE PRECISION, INTENT(IN) :: T_particle
        ! Partial Pressure of fluid water [Pa]
        DOUBLE PRECISION :: pw_H2O

        ! Calculation
        pw_H2O = p0_H2O(T_particle)
    END FUNCTION

    ! Calculate partial pressure of H2O in environment (steam) [Pa = kg⋅m⁻¹⋅s⁻²]
    FUNCTION pinf_H2O(T_environment, humidity)
        ! Temperature of Environment [K]
        DOUBLE PRECISION, INTENT(IN) :: T_environment
        ! Humidity of environment [%]
        DOUBLE PRECISION, INTENT(IN) :: humidity
        ! Partial Pressure of H2O in environment [Pa]
        DOUBLE PRECISION :: pinf_H2O

        ! Calculation
        pinf_H2O = humidity*p0_H2O(T_environment)
    END FUNCTION

    ! Calculate mass transfer coefficient [m/s]
    FUNCTION h_m(diameter, T_environment, T_particle, velocity, alg_dim)
        ! Temperature of environment and particle [K]
        DOUBLE PRECISION, INTENT(IN) :: T_environment, T_particle
        ! Diameter of particle (amount) [m]
        DOUBLE PRECISION, INTENT(IN) :: velocity
        ! Dimension of Algorithm
        INTEGER, INTENT(IN) :: alg_dim
        ! Diameter of particle [m]
        DOUBLE PRECISION, DIMENSION(alg_dim), INTENT(IN) :: diameter
        ! Mass Transfer Coefficient [m/s]
        DOUBLE PRECISION, DIMENSION(alg_dim) :: h_m

        ! Calculation
        h_m = 1.14*nu_air(T_environment)**(-1.0/6)*D_Coeff(T_particle, T_environment)**(2.0/3)*velocity**(1.0/2)*diameter**(-1.0/2)
    END FUNCTION

    ! 1st Order Derivative of particle diameter [m/s]
    FUNCTION dddt(y, params, params_dim, alg_dim)
        INTEGER, INTENT(IN) :: alg_dim
        INTEGER, INTENT(IN) :: params_dim
        DOUBLE PRECISION, DIMENSION(params_dim), INTENT(IN) :: params
        DOUBLE PRECISION, DIMENSION(alg_dim), INTENT(IN) :: y
        DOUBLE PRECISION, DIMENSION(alg_dim) :: dddt

        ! Resolve params
        DOUBLE PRECISION :: T_particle
        DOUBLE PRECISION :: T_environment 
        DOUBLE PRECISION :: velocity
        DOUBLE PRECISION :: humidity

        T_particle = params(1)
        T_environment = params(2)
        velocity = params(3)
        humidity = params(4)

        ! Calculation
        dddt = 2/(rho_H20)*h_m(y, T_environment, T_particle, velocity, alg_dim)*M_H2O/R*(pw_H2O(T_particle)/T_particle &
            - pinf_H2O(T_environment, humidity)/T_environment)
    END FUNCTION

END MODULE module_evaporation