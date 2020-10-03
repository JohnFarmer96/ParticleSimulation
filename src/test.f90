PROGRAM test
    USE numeric_integration
    USE particle_simulation
    IMPLICIT NONE

    ! Relative Path to Configuration file
    CHARACTER(LEN=128), PARAMETER :: config_file = 'data/config.d'
    ! Relative Path to Result files (Extension needs to be added)
    CHARACTER(LEN=128), PARAMETER :: base_path = 'data/particle_simulation_result_'
    ! Hard Coded Number of Variables that are present in output data array
    INTEGER, PARAMETER :: num_of_variables = 23

    ! CONFIG-PARAMETERS
    ! ==============================================================
    ! Particle concentration in the air [x/m³] (0...30.000)
    INTEGER :: concentration
    ! Intensity coefficient [%] (0...1) 
    DOUBLE PRECISION, DIMENSION(dim) :: velocity
    ! Velocity of Wind [m/s]
    DOUBLE PRECISION, DIMENSION(dim) :: v_wind
    ! Temperature of environment [K]
    DOUBLE PRECISION :: T_environment
    ! Relative Humidity in environment [%] (0...1)
    DOUBLE PRECISION :: humidity
    ! Total Simulation Time [s]
    DOUBLE PRECISION :: t_total
    ! ==============================================================

    ! Data Array 
    DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: data_array
    ! Stop computation
    LOGICAL :: all_done
    ! Time breakpoint to print values
    LOGICAL :: breakpoint
    ! Current timestamp 
    DOUBLE PRECISION :: timestamp
    ! Number of Iterations
    INTEGER :: n_cur, pot

    print *,"Started" 

    ! Read Parameters & Apply Configuration
    call read_config(TRIM(config_file))
    ALLOCATE(data_array(concentration, num_of_variables))
    print *,"Preparation Completed"

    ! Initialize Particles
    n_cur = 0
    call initialize(concentration, velocity, T_environment, humidity, v_wind)
    print *,"Initialization Complete"

    ! Save initial configuration
    data_array = output(start_idx=1, end_idx=concentration)
    call write_to_file(base_path, n_cur)
    print *,"Initialization Saved"
    
    ! Execute Simulation
    all_done = .FALSE.
    breakpoint = .FALSE.
    timestamp = 1E-3
    n_cur = 0
    pot = 0
    print *,"Simulation Started"
    do while (all_done .eqv. .FALSE.)
        n_cur = n_cur + 1

        call update(euler, euler, t_total, all_done, 0.1d0, breakpoint)

        IF(breakpoint .EQV. .TRUE.) THEN
            pot = pot + 1
            timestamp = timestamp*10**pot
            ! Write result
            data_array = output(start_idx=1, end_idx=concentration)
            call write_to_file(base_path, pot)
        END IF
    end do
    print *,"Simulation Completed"

    ! Write result
    data_array = output(start_idx=1, end_idx=concentration)
    call write_to_file(base_path, -1)

    ! Print Success
    print *,"Program Done"

CONTAINS 

    ! Read in parameter configuration
    SUBROUTINE read_config(filename)
        ! Dummy Arguments
        CHARACTER(len=*), INTENT(IN) :: filename
        INTEGER :: ios, unit_no 

        ! Open Textfile & Verify Success
        OPEN(newunit=unit_no, file=TRIM(filename), status='unknown', IOSTAT=ios)
        if (ios /= 0) then
            print *, "Cannot Open File. IO-Status-Variable Value:", ios
            STOP
        end if

        ! Read in process variables
        read (unit_no,*) concentration
        print *,"Concentration: ",concentration

        read (unit_no,*) t_total
        print *,"Total Time: ",t_total

        read (unit_no,*) velocity(1)
        print *,"Stepwidth: ",velocity(1)

        read (unit_no,*) velocity(2)
        print *,"Total Time: ",velocity(2)

        read (unit_no,*) velocity(3)
        print *,"Total Time: ",velocity(3)

        read (unit_no,*) v_wind(1)
        print *,"Velocity of Wind (x): ",v_wind(1)
        
        read (unit_no,*) v_wind(2)
        print *,"Velocity of Wind (y): ",v_wind(2)

        read (unit_no,*) v_wind(3)
        print *,"Velocity of Wind (z): ",v_wind(3)

        read (unit_no,*) humidity
        print *,"Humidity: ",humidity

        read (unit_no,*) T_environment
        print *,"Temperature of Environment ",T_environment

        ! Close File Before Ending
        CLOSE(unit_no)
    END SUBROUTINE

    ! Write data to file
    SUBROUTINE write_to_file(filename_base, n_current)
        CHARACTER(len=*), INTENT(IN) :: filename_base
        INTEGER, INTENT(IN) :: n_current
        INTEGER :: idx, ios, unit_no 

        ! Adapt filename
        CHARACTER(LEN=128) :: filename
        CHARACTER(LEN=128) :: filename_suffix 
        CHARACTER(LEN=20) :: FMT

        IF (n_current .GT. 0) THEN
            WRITE(FMT,'("(I", I0, ")")') INT(LOG10(REAL(n_current))+1)
            WRITE (filename_suffix, FMT) n_current
        ELSE IF (n_current .EQ. 0) THEN
            WRITE(filename_suffix, '(a4)') "init"
        ELSE
            WRITE(filename_suffix, '(a5)') "final"
        END IF

        filename = TRIM(filename_base)//TRIM(filename_suffix)//'.dat'

        print *,"Writing at: "//filename
        ! Open Textfile & Verify Success
        OPEN(newunit=unit_no, file=TRIM(filename), status='replace', IOSTAT=ios)
        if (ios /= 0) then
            print *, "Cannot Open File. IO-Status-Variable Value:", ios
            STOP
        end if 

        ! Write out Headline
        WRITE (unit_no, '(23(a14))') "idx","time","r_x", "r_y","r_z","v_x","v_y","v_z","d_core",&
        "d_shell","T_par","T_env","humid","vw_x","vw_y","vw_z","m_c","m_s","f_x","f_y","f_z","core","active" 
        ! Write out Variables
        do idx = 1, concentration
            write(unit_no, '(23(g14.7))') data_array(idx,:)
        end do

        ! Close File Before Ending
        CLOSE(unit_no)
    END SUBROUTINE

    ! Print output to console
    SUBROUTINE print_output(array)
        DOUBLE PRECISION, DIMENSION(concentration, num_of_variables), INTENT(IN) :: array
        INTEGER :: idx

        print '(23(a14))',"idx","time","r_x", "r_y","r_z","v_x","v_y","v_z","d_core",&
            "d_shell","T_par","T_env","humid","vw_x","vw_y","vw_z","f_x","f_y","f_z","core","active" 
        do idx = 1, concentration
            print '(21(g14.6))', array(idx,:)
        end do
    END SUBROUTINE

END PROGRAM