!real*8,parameter :: eh2kj_mol   = 2625.49962d0 !obsoluted
real*8,parameter :: eh2kcal_mol = 627.5095d0
real*8,parameter :: bohr2ang    = 0.5291772086d0
real*8,parameter :: ang2bohr    = 1.0d0 / bohr2ang
real*8,parameter :: pi          = 3.1415926535897932384d0
real*8,parameter :: scalecharge = 18.2223d0

character*2 :: asym(18) 
data asym/'H' ,'He', &
          'Li','Be','B' ,'C' ,'N' ,'O' ,'F' ,'Ne', &
          'Na','Mg','Al','Si','P' ,'S' ,'Cl','Ar'/
 
