module icewrap

    implicit none

    ! Internal constants
    integer,  parameter :: dp  = kind(1.d0)
    integer,  parameter :: sp  = kind(1.0)

    ! Choose the precision of the library (sp,dp)
    integer,  parameter :: wp = sp 



    type ice_class
        ! Variables on ice sheet grid that interact with the coupler 
        ! All information will be on the ice-sheet native grid 
        
        ! out
        logical :: error
        integer,  allocatable :: mask_extent(:,:) ! should be external in bnd?  fixme
        real(wp), allocatable :: z_sur(:,:) 
        real(wp), allocatable :: z_sur_std(:,:) 
        real(wp), allocatable :: z_bed_std(:,:) 
        real(wp), allocatable :: z_base(:,:) 
        real(wp), allocatable :: H_ice(:,:) 
        real(wp), allocatable :: calv(:,:) 
        real(wp), allocatable :: Q_b(:,:) 

        ! in
        integer,  allocatable :: mask_ocn_lake(:,:)
        real(wp), allocatable :: z_sl(:,:)
        real(wp), allocatable :: z_bed(:,:) 
        real(wp), allocatable :: z_bed_fil(:,:) 
        real(wp), allocatable :: smb(:,:) 
        real(wp), allocatable :: accum(:,:) 
        real(wp), allocatable :: runoff(:,:) 
        real(wp), allocatable :: Q_bm_float(:,:) 
        real(wp), allocatable :: temp_s(:,:) 
        real(wp), allocatable :: temp_g(:,:) 
        real(wp), allocatable :: q_geo(:,:) 
        real(wp), allocatable :: H_sed(:,:) 
        real(wp), allocatable :: t_ocn(:,:) 
        real(wp), allocatable :: s_ocn(:,:) 
        
    end type 


contains





end module icewrap