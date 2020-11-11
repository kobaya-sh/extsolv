subroutine modprm
implicit none
include 'size.inc'
include 'prmtop.inc'
character*120     :: line
integer           :: eof,err 
integer,parameter :: fh_optgeom=9
integer           :: i,j
integer           :: iatom
integer           :: flg
logical           :: hi(m_natom)
character*20      :: linktype(m_natom)
integer           :: linkto(m_natom)
integer           :: start,last
character*6       :: chr_tmp
logical           :: idbg
logical           :: needprm_bond(m_numbnd)
logical           :: needprm_bend(m_numang)
logical           :: needprm_tors(m_nptra)

idbg=.false.
!
!   1. Read Layer and Link-atom
!
open(fh_optgeom,file='optgeom.qmmm.dat')
iatom=0
hi=.false.
10 do
  read(fh_optgeom,'(a120)',iostat=eof) line
  if(eof/=0)exit
  do i=20,120 !first 20 letters is assumed to be atom assign.
    if(line(i:i)=='H')then
      iatom=iatom+1
      hi(iatom)=.true.
      goto 10 
    elseif(line(i:i)=='L')then
      iatom=iatom+1
      do j=i+1,120
        if(line(j:j)/=' ')then
          hi(iatom)=.true.
          start=j
          exit
        elseif(j==120)then
          goto 10 
        endif
      enddo
      do j=start,120
        if(line(j:j)==' ')then
          last=j
          linktype(iatom)=line(start:last-1)
          exit
        endif
      enddo
      do j=last+1,120
        if(line(j:j)/=' ')then
          start=j
          exit
        elseif(j==120)then
          goto 10
        endif
      enddo
      do j=start,120
        if(line(j:j)==' ')then
          last=j
          chr_tmp=line(start:last-1)
          read(chr_tmp,'(i6)') linkto(iatom)
          exit
        endif
      enddo
      exit
    endif
  enddo
enddo
if(idbg)then
  do iatom=1,30
    write(*,*) hi(iatom),linktype(iatom),linkto(iatom)
  enddo
endif
!
!   2. Modify bond and angle
!
idbg=.true.
needprm_bond=.false.
needprm_bend=.false.
needprm_tors=.false.
call modlist(ijicb   ,m_nbona ,nbona ,needprm_bond,m_numbnd,3)
call modlist(ijicbh  ,m_nbonh ,nbonh ,needprm_bond,m_numbnd,3)
call modpara(rk      ,m_numbnd,numbnd,needprm_bond)
call modpara(req     ,m_numbnd,numbnd,needprm_bond)
call modlist(ijkict  ,m_ntheta,ntheta,needprm_bend,m_numang,4)
call modlist(ijkicth ,m_ntheth,ntheth,needprm_bend,m_numang,4)
call modpara(tk      ,m_numang,numang,needprm_bend)
call modpara(teq     ,m_numang,numang,needprm_bend)
call modlist(ijklicp ,m_nphia ,nphia ,needprm_tors,m_nptra ,5)
call modlist(ijklicph,m_nphih ,nphih ,needprm_tors,m_nptra ,5)
call modpara(pk      ,m_numang,numang,needprm_bend)
call modpara(pn      ,m_numang,numang,needprm_bend)
call modpara(phase   ,m_numang,numang,needprm_bend)
call modiac (iac     ,m_natom ,natom)
return

contains

  subroutine modlist(list,m_nitem,nitem,needprm,m_needprm,step)
  implicit none
  integer,intent(in)    :: m_nitem
  integer,intent(inout) :: nitem
  integer,intent(in)    :: step
  integer,intent(inout) :: list(step*m_nitem)
  integer,intent(in)    :: m_needprm
  logical,intent(inout) :: needprm(m_needprm)
  integer               :: listtmp(step*m_nitem)
  integer               :: i,j
  logical               :: is_hi
  integer               :: i_list
  intrinsic iabs
  i_list=0
  listtmp=0
  do i=1,nitem
    is_hi=.true.
    do j=1,step-1 ! last element is not about atom
      is_hi=is_hi.and.hi(iabs(list(step*(i-1)+j)/step+1))
    enddo
    if(is_hi)then
      i_list=i_list+1
      listtmp(step*(i_list-1)+1:step*i_list)=list(step*(i-1)+1:step*i)
!      write(0,*)list(step*(i-1)+1),listtmp(step*(i_list-1)+1)
      needprm(list(step*i_list))=.true.
    endif
    if(idbg)then
      write(0,*) (list(step*(i-1)+j)/step+1,j=1,step-1),is_hi
    endif
  enddo
  list=listtmp
  nitem=i_list
  return
  endsubroutine

  subroutine modpara(para,m_nitem,nitem,needprm)
  implicit none
  integer,intent(in)    :: m_nitem
  integer,intent(inout) :: nitem
  real*8 ,intent(inout) :: para(m_nitem)
  logical,intent(in)    :: needprm(m_nitem)
  integer               :: paratmp(m_nitem)
  integer               :: i,j
  logical               :: is_hi
  integer               :: i_para
  intrinsic iabs
  i_para=0
  do i=1,nitem
    if(needprm(i))then
      i_para=i_para+1
      paratmp(i_para)=para(i)
    endif
  enddo
  nitem=i_para
  return
  endsubroutine

  subroutine modiac(iac,m_natom,natom)
  implicit none
  integer,intent(in)    :: m_natom
  integer,intent(in)    :: natom
  integer,intent(inout) :: iac(m_natom)
  integer               :: i
  do i=1,natom
    if(.not.hi(i))then
      iac(i)=-1
    endif
  enddo
  return
  endsubroutine

endsubroutine
