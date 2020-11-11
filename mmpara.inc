! Others
real*8,parameter  :: cutoff=12.0d0
!real*8,parameter  :: cutoff=9.0d0
real*8,parameter  :: cds_cutoff=300.0d0 ! used in ONLY cds_?.f90 
real*8  :: cell(3),basis_r(3,3),basis_k(3,3)
logical :: is_pbc
logical :: is_ewald
logical :: is_ee
real*8,parameter   :: p = 12.0d+0
!real*8,parameter   :: p = 12.0d+0
!real*8,parameter   :: p = 20.0d+0

common /pbcbox/cell,basis_r,basis_k,is_pbc,is_ewald,is_ee
