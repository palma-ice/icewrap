module icewrap
    
    use icewrap_def

    implicit none

    private 
    public :: sp, dp, wp 
    public :: icewrap_class     ! Define in icewrap_def module
    public :: icewrap_init
    public :: icewrap_init_state
    public :: icewrap_update
    public :: icewrap_end

contains
    
    subroutine icewrap_init(iw,nx,ny,domain)
        ! Initialize the ice-sheet model data structures 

        implicit none


        type(icewrap_class), intent(INOUT) :: iw  
        integer,             intent(IN)    :: nx
        integer,             intent(IN)    :: ny
        character(len=*),    intent(IN)    :: domain 

        ! Store general information 
        iw%domain = trim(domain)
        iw%nx     = nx 
        iw%ny     = ny 

        ! Allocate icewrap arrays 
        call icewrap_alloc(iw,nx,ny) 

        ! Define horizontal axes 

        ! To do 


        write(*,*) "icewrap_init:: summary:"
        write(*,*) "domain = ", trim(domain)
        write(*,*) "nx, ny = ", nx, ny   

        return

    end subroutine icewrap_init

    subroutine icewrap_init_state(iw,time)
        ! Initialize the ice-sheet model state 

        implicit none


        type(icewrap_class), intent(INOUT) :: iw  
        real(wp),            intent(IN)    :: time

        ! Do nothing because this is a stub.

        ! Set current model time 
        iw%time = time 

        write(*,"(a,a20)") "icewrap_init_state:: domain state initialized: ", trim(iw%domain) 
        
        return

    end subroutine icewrap_init_state


    subroutine icewrap_update(iw,time)
        ! Advance the ice-sheet model to current `time`. 

        implicit none


        type(icewrap_class), intent(INOUT) :: iw  
        real(wp),            intent(IN)    :: time

        ! Do nothing because this is a stub.

        ! Set current model time 
        iw%time = time 

        write(*,"(a,a20,f12.3)") "icewrap_update:: ", trim(iw%domain), time  
        
        return

    end subroutine icewrap_update

    subroutine icewrap_end(iw)
        ! Destroy the ice-sheet model data structures

        implicit none

        type(icewrap_class), intent(INOUT) :: iw  

        ! De-allocate icewrap arrays 
        call icewrap_dealloc(iw) 

        write(*,"(a,a20)") "icewrap_end:: domain terminated: ", trim(iw%domain)

        return

    end subroutine icewrap_end

end module icewrap