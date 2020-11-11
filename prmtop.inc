! Prmtop parameters
! Definition of all following parameter are from 
! 'http://ambermd.org/formats.html#topo.cntrl'.
character*4  :: ITITL(20)
integer      :: NATOM,  NTYPES, NBONH,  MBONA,  NTHETH, &
                MTHETA, NPHIH,  MPHIA,  NHPARM, NPARM,  &
                NNB,    NRES,   NBONA,  NTHETA, NPHIA,  &
                NUMBND, NUMANG, NPTRA,  NATYP,  NPHB,   &
                IFPERT, NBPER,  NGPER,  NDPER,  MBPER,  &
                MGPER,  MDPER,  IFBOX,  NMXRS,  IFCAP,  &
                NUMEXTRA, NCOPY
character*4  :: IGRAPH(m_natom)
real*8       :: CHARGE(m_natom)
real*8       :: AMASS(m_natom)
integer      :: IAC(m_natom)
integer      :: NUMEX(m_natom)
integer      :: ICO(m_ntype**2)
character*4  :: LBRES(m_nres)
integer      :: IPRES(m_nres)
real*8       :: RK(m_numbnd),   REQ(m_numbnd)
real*8       :: TK(m_numang),   TEQ(m_numang)
real*8       :: PK(m_nptra),    PN(m_nptra),    PHASE(m_nptra) 
real*8       :: ONE_SCEE(m_nptra),   ONE_SCNB(m_nptra)         
real*8       :: SOLTY(m_natyp)
real*8       :: CN1(m_ntype*(m_ntype+1)/2)
real*8       :: CN2(m_ntype*(m_ntype+1)/2)
integer      :: IJICBH(3*m_nbonh)
integer      :: IJICB(3*m_nbona)
integer      :: IJKICTH(4*m_ntheth)
integer      :: IJKICT(4*m_ntheta)
integer      :: IJKLICPH(5*m_nphih)
integer      :: IJKLICP(5*m_nphia)
integer      :: INB(m_nnb)
real*8       :: ASOL(m_nphb),  BSOL(m_nphb)
character*4  :: ISYMBL(m_natom)
character*4  :: ITREE(m_natom)
character*80 :: TYPE
real*8       :: RBORN(m_natom)
real*8       :: FS(m_natom)

!common /prmtop1/ITITL,                                     &
!                NATOM,  NTYPES, NBONH,  MBONA,  NTHETH,    &
!                MTHETA, NPHIH,  MPHIA,  NHPARM, NPARM,     &
!                NNB,    NRES,   NBONA,  NTHETA, NPHIA,     &
!                NUMBND, NUMANG, NPTRA,  NATYP,  NPHB,      &
!                IFPERT, NBPER,  NGPER,  NDPER,  MBPER,     &
!                MGPER,  MDPER,  IFBOX,  NMXRS,  IFCAP,     &
!                NUMEXTRA, NCOPY,                           &
!                IGRAPH,                                    &
!                CHARGE,                                    &
!                AMASS,                                     &
!                IAC,                                       &
!                NUMEX,                                     &
!                ICO,                                       &
!                LBRES, IPRES,                              &
!                IJICBH, IJICB, IJKICTH, IJKICT,            &
!                IJKLICPH, IJKLICP,                         &
!                INB,                                       &
!                ISYMBL,                                    &
!                ITREE,                                     &
!                TYPE                                      
!common /prmtop2/RK, REQ, TK, TEQ, PK,  PN,  PHASE,         &
!                ONE_SCEE, ONE_SCNB, SOLTY,                 &
!                CN1,CN2,                                   &
!                ASOL, BSOL,                                &
!                RBORN,                                     &
!                FS
!
