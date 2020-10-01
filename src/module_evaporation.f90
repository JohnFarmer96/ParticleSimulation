MODULE module_evaporation
    USE module_parameters
    USE module_particle
    USE numeric_integration
    IMPLICIT NONE

CONTAINS

    ! Update Particle Diameter [µm]
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
            ! print *,"shell: ",prtcl%d_shell
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

    ! Diffusion Coefficient [m²/s = = µm²/s * 10¹²]
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
        ! print *,"D",D_Coeff
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

    ! Calculate Sherwood number [dimensionless]
    FUNCTION Sh(velocity, diameter, T_environment, T_particle)
        ! Temperature of environment and particle [K]
        DOUBLE PRECISION, INTENT(IN) :: T_environment, T_particle
        ! Velocity of particle [m/s]
        DOUBLE PRECISION, INTENT(IN) :: velocity
        ! Diameter of particle [µm]
        DOUBLE PRECISION, INTENT(IN) :: diameter
        ! Sherwood number
        DOUBLE PRECISION :: Sh

        ! Froessling Equation
        Sh = 2 + 0.552*Re(velocity, diameter, T_environment)**(0.5)*Sc(T_environment, T_particle)**(0.33)
        ! print *,"Sherwood: ",Sh
    END FUNCTION

    ! Calculate Reynolds number [dimensionless]
    FUNCTION Re(velocity, diameter, T_environment)
        ! Velocity of particle [m/s]
        DOUBLE PRECISION, INTENT(IN) :: velocity
        ! Diameter of particle [µm]
        DOUBLE PRECISION, INTENT(IN) :: diameter
        ! Temperature of environment and particle [K]
        DOUBLE PRECISION, INTENT(IN) :: T_environment
        ! Reynolds number
        DOUBLE PRECISION :: Re
        ! Conversion Factor (m to µm)
        DOUBLE PRECISION, PARAMETER :: conversion = 10.0E-6

        ! Reynolds Equation
        Re = velocity/nu_air(T_environment)*diameter*conversion
        ! print *,"Velocity: ",velocity
        ! print *,"nu: ", nu_air(T_environment)
        ! print *,"diameter: ", diameter
        ! print *,"Reynolds: ", Re
    END FUNCTION

    ! Calculate Schmidt number [dimensionless]
    FUNCTION Sc(T_environment, T_particle)
        ! Temperature of environment and particle [K]
        DOUBLE PRECISION, INTENT(IN) :: T_environment, T_particle
        ! Schmidt number
        DOUBLE PRECISION :: Sc

        ! Froessling Equation
        Sc = nu_air(T_environment)/D_Coeff(T_particle, T_environment)
        !print *,"Schmidt: ",Sc
    END FUNCTION

    ! Calculate mass transfer coefficient [µm/s]
    FUNCTION h_m(diameter, T_environment, T_particle, velocity)
        ! Temperature of environment and particle [K]
        DOUBLE PRECISION, INTENT(IN) :: T_environment, T_particle
        ! Velocity of particle [m/s]
        DOUBLE PRECISION, INTENT(IN) :: velocity
        ! Diameter of particle [µm]
        DOUBLE PRECISION, INTENT(IN) :: diameter
        ! Mass Transfer Coefficient [µm/s]
        DOUBLE PRECISION :: h_m
        ! Conversion Factor (m to µm)
        DOUBLE PRECISION, PARAMETER :: conversion = 10.0E12

        h_m = D_Coeff(T_particle, T_environment)/diameter*Sh(velocity, diameter, T_environment, T_particle)*conversion
        !print *,"h_m: ",h_m
        !print *,""
    END FUNCTION

    ! 1st Order Derivative of particle diameter [µm/s]
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

        !print *,"y: ",y
        ! Calculation
        dddt(1) = - 2/(rho_H20)*h_m(y(1), T_environment, T_particle, velocity)*M_H2O/R*(pw_H2O(T_particle)/T_particle &
            - pinf_H2O(T_environment, humidity)/T_environment)
        !print *,"dddt: ",dddt(1)
    END FUNCTION

END MODULE module_evaporation