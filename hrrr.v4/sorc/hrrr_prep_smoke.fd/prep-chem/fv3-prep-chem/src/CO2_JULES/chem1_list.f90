MODULE chem1_list
  IMPLICIT NONE
  
  
  CHARACTER(LEN=24),PARAMETER :: chemical_mechanism='CO2'
  INTEGER,PARAMETER :: maxnspecies= 200
  INTEGER,PARAMETER :: nspecies=1
  
  
  !Name of species 
  CHARACTER(LEN=8),PARAMETER,DIMENSION(nspecies) :: spc_name=(/ &
    'CO2     '  &
   /)
  
  
  !Number of each specie   
  INTEGER,PARAMETER :: CO2   =001

  
  !for memory allocattion: 
  !This parameters are use for documentation only. 
  !Use them in a program in substitution of numerical terms.
  INTEGER,PARAMETER :: src = 1 ! source term 
  INTEGER,PARAMETER :: ddp = 2 ! dry deposition 
  INTEGER,PARAMETER :: wdp = 3 ! wet deposition 
  INTEGER,PARAMETER :: fdda = 4! four-dim assimilation 
  INTEGER,PARAMETER :: offline = 5! ! off-line emissions: 
                                  !=1, emission will be read from file
                                  !=0, emission will be calculated during the model simulation (on-line emission)
  INTEGER,PARAMETER :: transport = 6! ! off-line emissions: 
                                  !=1, yes
                                  !=0, no transport
  INTEGER,PARAMETER :: on = 1
  INTEGER,PARAMETER :: off = 0
  
  
  ! spaction(specie,[1=source,2=drydep,3=wetdep,4=fdda,5=offline emission,6=transport]) ]) 
  INTEGER,PARAMETER,DIMENSION(6,nspecies) :: spc_alloc=RESHAPE((/ &
   1 , 1 , 1 , 1 , 0 , 1     & ! CO2      - 001

    /),(/6,nspecies/))
  
  
  INTEGER,PARAMETER,DIMENSION(nspecies) :: spc_uveq=(/ &
   00  & ! CO2      - 001
   /)
  
  
!     HENRYS LAW COEFFICIENTS
!     Henrys law coefficient
!     [KH298]=mole/(l atm)
!     Referencias em http://www.mpch-mainz.mpg.de/~sander/res/henry.html
!     * indica artigos nao encontrados nesse endereço eletronico
  REAL,PARAMETER,DIMENSION(nspecies) :: hstar=(/&
    9.90E-4    & ! CO2      - 001
    /)
  
! [1] Noziere B. et al. The uptake of methyl vinyl ketone, methacrolein,
! and 2-methyl-3-butene-2-olonto sulfuric acid solutions,Journal of Physical
! Chemistry A, Vol.110, No.7, 2387-2395, 2006.
! [2] Abraham M. H. et al. Partition of compounds from gas to water and
! from gas to physiological saline at 310K: Linear free energy relationships,
! elsevier, 2006.
  REAL,PARAMETER,DIMENSION(nspecies) :: f0=(/&
    0.0    & ! CO2      - 001
    /)
  
  
  REAL,PARAMETER,DIMENSION(nspecies) :: difrat=(/&
    0.0    & ! CO2      - 001
    /)
  
  
!     DIFFUSION COEFFICIENTS
!     [DV]=cm2/s (assumed: 1/SQRT(molar mass) when not known)
  REAL,PARAMETER,DIMENSION(nspecies) :: dvj=(/&
    0.1890000    & ! CO2      - 001
    /)
  
  
!     -DH/R (for temperature correction)
!     [-DH/R]=K
  REAL,PARAMETER,DIMENSION(nspecies) :: dhr=(/&
    1300.    & ! CO2      - 001
    /)
  
  
  REAL,PARAMETER,DIMENSION(nspecies) :: weight=(/&
    44.    & ! CO2      - 001
   /)
  
  
  REAL,PARAMETER,DIMENSION(nspecies) :: init_ajust=(/&
    1.0    & ! CO2      - 001
   /)
  
  
  REAL,PARAMETER,DIMENSION(nspecies) :: emiss_ajust=(/&
    1.0    & ! CO2      - 001
   /)
  
  
!    ACID DISSOCIATION CONSTANT AT 298K 
!     [mole/liter of liquid water]
!     Referencias: Barth et al. JGR 112, D13310 2007
!     Martell and Smith, 1976, Critical stability
!     vol1-4 Plenum Press New York
  REAL,PARAMETER,DIMENSION(nspecies) :: ak0=(/&
    0.00E+00    & ! CO2      - 001
   /)
  
  
!     Temperature correction factor for
!     acid dissociation constants
!     [K]
!     Referencias: Barth et al. JGR 112, D13310 2007
  REAL,PARAMETER,DIMENSION(nspecies) :: dak=(/&
    0.    & ! CO2      - 001
    /)
  
  
  INTEGER,PARAMETER :: nr   =103!Number of gas-phase reactions
  INTEGER,PARAMETER :: nrt  =103!Total Number of reactions
  INTEGER,PARAMETER :: nrh2o=000!Number of aqueous-phase reactions
  
 !------------------------------------------------------------------------------  
  
 ! Photolysis Rate Calculation: method used (LUT=look_up_table, FAST-JX= on-line)
  
  CHARACTER(LEN=10),PARAMETER :: PhotojMethod= 'LUT'
  INTEGER,PARAMETER :: maxJcomb=    5, nr_photo=    1
  INTEGER,PARAMETER,DIMENSION(nr_photo) :: nfactors=(/ &
                            0/)!  1
 
  DOUBLE PRECISION,PARAMETER,DIMENSION(maxJcomb,nr_photo) :: factor=RESHAPE((/ &
                                                0.000D+00, 0.000D+00, 0.000D+00, 0.000D+00, 0.000D+00/),(/maxJcomb,nr_photo/))
  CHARACTER(LEN=07 ),PARAMETER,DIMENSION(maxJcomb,nr_photo) :: JReactionComp=RESHAPE((/ &
                                               "NONE   ","NONE   ","NONE   ","NONE   ","NONE   "/),(/maxJcomb,nr_photo/))

  
END MODULE chem1_list
