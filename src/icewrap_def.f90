module icewrap_def

    implicit none

    ! Internal constants
    integer,  parameter :: dp  = kind(1.d0)
    integer,  parameter :: sp  = kind(1.0)

    ! Choose the precision of the library (sp,dp)
    integer,  parameter :: wp = sp 



    type icewrap_class

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



    subroutine ice_alloc(ice,nx,ny)
        ! Allocate ice fields 

        implicit none 

        type(icewrap_class), intent(INOUT) :: ice 
        integer, intent(IN) :: nx 
        integer, intent(IN) :: ny 
        
        ! First ensure all fields are deallocated
        call ice_dealloc(ice) 

        ! Allocate fields 
        allocate(ice%z_sur(nx,ny))
        allocate(ice%z_sur_std(nx,ny))
        allocate(ice%z_bed_std(nx,ny))
        allocate(ice%z_base(nx,ny))
        allocate(ice%z_bed(nx,ny))
        allocate(ice%z_bed_fil(nx,ny))
        allocate(ice%H_ice(nx,ny))
        allocate(ice%mask_ocn_lake(nx,ny))
        allocate(ice%z_sl(nx,ny))
        allocate(ice%mask_extent(nx,ny))
        allocate(ice%calv(nx,ny))
        allocate(ice%Q_b(nx,ny))
        allocate(ice%Q_bm_float(nx,ny))
        allocate(ice%smb(nx,ny))
        allocate(ice%accum(nx,ny))
        allocate(ice%runoff(nx,ny))
        allocate(ice%temp_s(nx,ny))
        allocate(ice%temp_g(nx,ny))
        allocate(ice%q_geo(nx,ny))
        allocate(ice%H_sed(nx,ny))
        allocate(ice%t_ocn(nx,ny))
        allocate(ice%s_ocn(nx,ny))
        
        ! Initialize to zero 
        ice%z_sur       = 0.0_wp
        ice%z_sur_std   = 0.0_wp
        ice%z_bed_std   = 0.0_wp
        ice%z_base      = 0.0_wp
        ice%z_bed       = 0.0_wp
        ice%z_bed_fil   = 0.0_wp
        ice%H_ice       = 0.0_wp
        ice%z_sl        = 0.0_wp
        ice%mask_extent = 0.0_wp
        ice%calv        = 0.0_wp
        ice%Q_b         = 0.0_wp
        ice%Q_bm_float  = 0.0_wp
        ice%smb         = 0.0_wp
        ice%accum       = 0.0_wp
        ice%runoff      = 0.0_wp
        ice%temp_s      = 0.0_wp
        ice%temp_g      = 0.0_wp
        ice%q_geo       = 0.0_wp
        ice%H_sed       = 0.0_wp
        ice%t_ocn       = 0.0_wp
        ice%s_ocn       = 0.0_wp

        return 

    end subroutine ice_alloc
    
    subroutine ice_dealloc(ice)
        ! Deallocate ice fields 

        implicit none 

        type(icewrap_class), intent(INOUT) :: ice 
        
        if (allocated(ice%z_sur))       deallocate(ice%z_sur)
        if (allocated(ice%z_sur_std))   deallocate(ice%z_sur_std)
        if (allocated(ice%z_bed_std))   deallocate(ice%z_bed_std)
        if (allocated(ice%z_base))      deallocate(ice%z_base)
        if (allocated(ice%z_bed))       deallocate(ice%z_bed)
        if (allocated(ice%z_bed_fil))   deallocate(ice%z_bed_fil)
        if (allocated(ice%H_ice))       deallocate(ice%H_ice)
        if (allocated(ice%mask_ocn_lake))deallocate(ice%mask_ocn_lake)
        if (allocated(ice%z_sl))        deallocate(ice%z_sl)
        if (allocated(ice%mask_extent)) deallocate(ice%mask_extent)
        if (allocated(ice%calv))        deallocate(ice%calv)
        if (allocated(ice%Q_b))         deallocate(ice%Q_b)
        if (allocated(ice%Q_bm_float))  deallocate(ice%Q_bm_float)
        if (allocated(ice%smb))         deallocate(ice%smb)
        if (allocated(ice%accum))       deallocate(ice%accum)
        if (allocated(ice%runoff))      deallocate(ice%runoff)
        if (allocated(ice%temp_s))      deallocate(ice%temp_s)
        if (allocated(ice%temp_g))      deallocate(ice%temp_g)
        if (allocated(ice%q_geo))       deallocate(ice%q_geo)
        if (allocated(ice%H_sed))       deallocate(ice%H_sed)
        if (allocated(ice%t_ocn))       deallocate(ice%t_ocn)
        if (allocated(ice%s_ocn))       deallocate(ice%s_ocn)
        
        return 

    end subroutine ice_dealloc

end module icewrap_def