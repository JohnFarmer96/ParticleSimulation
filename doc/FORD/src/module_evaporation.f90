MODULE module_evaporation
    !! Calculate Evaporation of Particle Shell
    USE module_parameters
    USE module_particle
    USE numeric_integration
    IMPLICIT NONE

    PUBLIC :: evaluate_evaporation
    PRIVATE :: D_Coeff
    PRIVATE :: p0_H2O
    PRIVATE :: pw_H2O
    PRIVATE :: pinf_H2O 
    PRIVATE :: Sh 
    PRIVATE :: Sc 
    PRIVATE :: Re 
    PRIVATE :: h_m
    PRIVATE :: dddt

CONTAINS

    SUBROUTINE evaluate_evaporation(numeric_integration_procedure, prtcl)
        !! Update Particle Diameter [µm]

        PROCEDURE(num_int_procedure) :: numeric_integration_procedure
            !! Desired numeric integration procedure (Euler, Runge-Kutta, ...)
        TYPE(particle), INTENT(INOUT) :: prtcl
            !! Desired particle
        
        ! Key Value
        DOUBLE PRECISION, DIMENSION(1) :: y
        ! Parameter Array for Function Arguments
        DOUBLE PRECISION, DIMENSION(4) :: params

        ! Execute only if particle shell is still present
        IF ( prtcl%core_only .eqv. .FALSE.) THEN

            ! Prepare function arguments
            ! print *,"shell: ",prtcl%d_shell
            y(1) = prtcl%d_shell
            params(1) = prtcl%T
            params(2) = prtcl%T_environment
            params(3) = v_euclid(prtcl)
            params(4) = prtcl%humidity
            ! Update Key Value
            call numeric_integration_procedure(dddt, y, prtcl%dt, params, SIZE(params, DIM=1), SIZE(y, DIM=1))
            ! Store Changes
            prtcl%d_shell = y(1)
            
            ! Verify if particle shell is still present
            call verify_shell(prtcl)
        END IF

    END SUBROUTINE

    FUNCTION D_Coeff(T_particle, T_environment)
        !! Diffusion Coefficient [m²/s = = µm²/s * 10¹²]
        
        DOUBLE PRECISION, INTENT(IN) :: T_particle, T_environment
            !! Temperature of Particle T_prtcl and Environment T_env [K]
        DOUBLE PRECISION :: T_m
            !! Mean Temperature T_m [K]
        DOUBLE PRECISION :: D_Coeff
            !! Result (Diffusion Coefficient) [m²/s]
        
        ! Computation
        T_m = (T_particle + T_environment)/2
        D_Coeff = D_0*(T_m/T_0)**1.8
    END FUNCTION

    FUNCTION p0_H2O(T)
        !! Calculate partial pressure of H2O [Pa = kg⋅m⁻¹⋅s⁻²]
        
        DOUBLE PRECISION, INTENT(IN) :: T
            !! Desired Temperature [K]

        ! Resulting partial pressure of H2O [Pa]
        DOUBLE PRECISION :: p0_H2O

        ! Antoine Equation (Result needs to be converted from mmHg to Pa) [Pa = kg⋅m⁻¹⋅s⁻²]
        p0_H2O = 10**(A - B/(C+T))*mmHg_Pa_conversion
    END FUNCTION

    FUNCTION pw_H2O(T_particle)
        !! Calculate partial pressure of H2O in particle (fluid water) [Pa = kg⋅m⁻¹⋅s⁻²]
        
        DOUBLE PRECISION, INTENT(IN) :: T_particle
            !! Temperature of particle [K]
        
        ! Partial Pressure of fluid water [Pa]
        DOUBLE PRECISION :: pw_H2O

        ! Calculation
        pw_H2O = p0_H2O(T_particle)
    END FUNCTION

    
    FUNCTION pinf_H2O(T_environment, humidity)
        !! Calculate partial pressure of H2O in environment (steam) [Pa = kg⋅m⁻¹⋅s⁻²]
        
        DOUBLE PRECISION, INTENT(IN) :: T_environment
            !! Temperature of Environment [K]
        DOUBLE PRECISION, INTENT(IN) :: humidity
            !! Humidity of environment [%]
        
        ! Partial Pressure of H2O in environment [Pa]
        DOUBLE PRECISION :: pinf_H2O

        ! Calculation
        pinf_H2O = humidity*p0_H2O(T_environment)
    END FUNCTION

    
    FUNCTION Sh(velocity, diameter, T_environment, T_particle)
        !! Calculate Sherwood number [dimensionless]
        
        DOUBLE PRECISION, INTENT(IN) :: T_environment, T_particle
            !! Temperature of environment and particle [K]
        DOUBLE PRECISION, INTENT(IN) :: velocity
            !! Velocity of particle [m/s]
        DOUBLE PRECISION, INTENT(IN) :: diameter
            !! Diameter of particle [µm]
        
        ! Sherwood number
        DOUBLE PRECISION :: Sh

        ! Froessling Equation
        Sh = 2 + 0.552*Re(velocity, diameter, T_environment)**(0.5)*Sc(T_environment, T_particle)**(0.33)
    END FUNCTION

    
    FUNCTION Re(velocity, diameter, T_environment)
        !! Calculate Reynolds number [dimensionless]
        
        DOUBLE PRECISION, INTENT(IN) :: velocity
            !! Velocity of particle [m/s]
        DOUBLE PRECISION, INTENT(IN) :: diameter
            !! Diameter of particle [µm]
        DOUBLE PRECISION, INTENT(IN) :: T_environment
            !! Temperature of environment and particle [K]
        
        ! Reynolds number
        DOUBLE PRECISION :: Re
        ! Conversion Factor (m to µm)
        DOUBLE PRECISION, PARAMETER :: conversion = 1E-6

        ! Reynolds Equation
        Re = velocity/nu_air(T_environment)*diameter*conversion
    END FUNCTION

    
    FUNCTION Sc(T_environment, T_particle)
        !! Calculate Schmidt number [dimensionless]
        
        DOUBLE PRECISION, INTENT(IN) :: T_environment, T_particle
            !! Temperature of environment and particle [K]
        
        ! Schmidt number
        DOUBLE PRECISION :: Sc

        ! Froessling Equation
        Sc = nu_air(T_environment)/D_Coeff(T_particle, T_environment)
    END FUNCTION

    
    FUNCTION h_m(diameter, T_environment, T_particle, velocity)
        !! Calculate mass transfer coefficient [µm/s]
        
        DOUBLE PRECISION, INTENT(IN) :: T_environment, T_particle
            !! Temperature of environment and particle [K]
        DOUBLE PRECISION, INTENT(IN) :: velocity
            !! Velocity of particle [m/s]
        DOUBLE PRECISION, INTENT(IN) :: diameter
            !! Diameter of particle [µm]
        
        ! Mass Transfer Coefficient [µm/s]
        DOUBLE PRECISION :: h_m
        ! Conversion Factor (m to µm)
        DOUBLE PRECISION, PARAMETER :: conversion = 1E12

        h_m = D_Coeff(T_particle, T_environment)/diameter*Sh(velocity, diameter, T_environment, T_particle)*conversion
    END FUNCTION

    
    FUNCTION dddt(y, params, params_dim, alg_dim)
        !! Calculate 1st Order Derivative of particle diameter [µm/s]
        
        INTEGER, INTENT(IN) :: alg_dim
            !! Dimension of State-Vector
        INTEGER, INTENT(IN) :: params_dim
            !! Dimension of Parameter-Vector
        DOUBLE PRECISION, DIMENSION(params_dim), INTENT(IN) :: params
            !! Parameter Vector
        DOUBLE PRECISION, DIMENSION(alg_dim), INTENT(IN) :: y
            !! State-Vector
        DOUBLE PRECISION, DIMENSION(alg_dim) :: dddt
            !! 1st Derivative of State-Vector

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
        dddt(1) = - 2/(rho_H20)*h_m(y(1), T_environment, T_particle, velocity)*M_H2O/R*(pw_H2O(T_particle)/T_particle &
            - pinf_H2O(T_environment, humidity)/T_environment)
    END FUNCTION

END MODULE module_evaporation