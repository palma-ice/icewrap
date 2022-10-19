program test_icewrap

    use icewrap 
    
    implicit none

    integer  :: n 
    real(wp) :: time
    real(wp) :: dt 

    type(icewrap_class) :: ice1
    type(icewrap_class) :: ice2


    ! Initialize ice objects for multiple domains =========

    call icewrap_init(ice1,nx=10,ny=15,domain="Greenland")
    call icewrap_init(ice2,nx=20,ny=20,domain="Antarctica")


    ! Initialize the ice domain states =========

    time = 0.0 

    call icewrap_init_state(ice1,time)
    call icewrap_init_state(ice2,time)


    ! Perform timestepping =========

    dt = 1.0 

    do n = 1, 10 

        ! Advance the current time 
        time = time + real(dt,wp)

        ! Advance domain 1 ice to current time
        call icewrap_update(ice1,time)

        ! Advance domain 2 ice to current time
        call icewrap_update(ice2,time)


    end do


    ! Finalize the program  =========

    call icewrap_end(ice1)
    call icewrap_end(ice2)

contains



end program test_icewrap