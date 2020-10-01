PROGRAM test
    USE numeric_integration
    USE particle_simulation
    IMPLICIT NONE

    ! Relative Path to Configuration file
    CHARACTER(LEN=128), PARAMETER :: config_file = 'data/config.d'
    ! Relative Path to Result files (Extension needs to be added)
    CHARACTER(LEN=128), PARAMETER :: base_path = 'data/particle_simulation_result_'
    ! Hard Coded Number of Variables that are present in output data array
    INTEGER, PARAMETER :: num_of_variables = 21

    ! CONFIG-PARAMETERS
    ! ==============================================================
    ! Particle concentration in the air [x/m³] (0...30.000)
    INTEGER :: concentration
    ! Intensity coefficient [%] (0...1) 
    DOUBLE PRECISION :: intensity
    ! Velocity of Wind [m/s]
    DOUBLE PRECISION, DIMENSION(dim) :: v_wind
    ! Temperature of environment [K]
    DOUBLE PRECISION :: T_environment
    ! Relative Humidity in environment [%] (0...1)
    DOUBLE PRECISION :: humidity
    ! Stepwidth [s]
    DOUBLE PRECISION :: dt
    ! Total Simulation Time [s]
    DOUBLE PRECISION :: t_total
    ! ==============================================================

    ! Data Array 
    DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: data_array
    ! Number of Iterations
    INTEGER :: n_cur, n_iter

    print *,"Started" 

    ! Read Parameters & Apply Configuration
    call read_config(TRIM(config_file))
    ALLOCATE(data_array(concentration, 21))
    print *,"Preparation Completed"

    ! Initialize Particles
    n_cur = 0
    call initialize(concentration, intensity, T_environment, humidity, v_wind)
    print *,"Initialization Complete"

    ! Save initial configuration
    data_array = output(start_idx=1, end_idx=concentration)
    call write_to_file(base_path, n_cur)
    print *,"Initialization Saved"
    
    ! Execute Simulation
    n_iter = INT(t_total/dt)
    print *,"Simulation Started"
    do n_cur = 1, n_iter
        call update(euler, euler, dt)
        ! Write result
        data_array = output(start_idx=1, end_idx=concentration)
        call write_to_file(base_path, n_cur)
    end do
    print *,"Simulation Completed"

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

        read (unit_no,*) intensity
        print *,"Intensity: ",intensity

        read (unit_no,*) dt
        print *,"Stepwidth: ",dt

        read (unit_no,*) t_total
        print *,"Total Time: ",t_total

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

        WRITE(FMT,'("(I", I0, ")")') INT(n_current/10)+1
        write (filename_suffix, FMT) n_current

        filename = TRIM(filename_base)//TRIM(filename_suffix)//'.dat'

        print *,"Writing at: "//filename
        ! Open Textfile & Verify Success
        OPEN(newunit=unit_no, file=TRIM(filename), status='replace', IOSTAT=ios)
        if (ios /= 0) then
            print *, "Cannot Open File. IO-Status-Variable Value:", ios
            STOP
        end if 

        ! Write out Variables
        do idx = 1, concentration
            write(unit_no, '(21(g14.7))') data_array(idx,:)
        end do

        ! Close File Before Ending
        CLOSE(unit_no)
    END SUBROUTINE

    ! Print output to console
    SUBROUTINE print_output(array)
        DOUBLE PRECISION, DIMENSION(concentration, num_of_variables), INTENT(IN) :: array
        INTEGER :: idx

        print '(21(a14))',"idx","time","r_x", "r_y","r_z","v_x","v_y","v_z","d_core",&
            "d_shell","T_par","T_env","humid","vw_x","vw_y","vw_z","f_x","f_y","f_z","core","active" 
        do idx = 1, concentration
            print '(21(g14.6))', array(idx,:)
        end do
    END SUBROUTINE

END PROGRAM