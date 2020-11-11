! Gaussian parameters (From EIn)
!integer      :: natom_gau,deriv_req,int_charge,int_spin
integer      :: deriv_req,int_charge,int_spin
integer      :: atomic_no(m_natom)
!real*8       :: crd(m_natom,3),charge_gau(m_natom)
real*8       :: charge_gau(m_natom)
logical      :: atom_layer(m_natom)

!common /coord/crd,natom_gau,atom_layer
common /coord/atom_layer
