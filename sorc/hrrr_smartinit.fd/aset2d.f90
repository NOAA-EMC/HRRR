  MODULE aset2d
!=======================================================================
!  Initialize 2-D atmosphere arrays
!=======================================================================
   REAL,    ALLOCATABLE :: PSFC(:,:),ZSFC(:,:)
   REAL,    ALLOCATABLE :: U10(:,:),V10(:,:),BASEZ(:,:),WETFRZ(:,:)
   REAL,    ALLOCATABLE :: T2(:,:),Q2(:,:),BLI(:,:),REFC(:,:),GUST(:,:)
   REAL,    ALLOCATABLE :: D2(:,:),VIS(:,:),COAST(:,:),TOPO_NDFD(:,:)
   REAL,    ALLOCATABLE :: LCLD(:,:),MCLD(:,:),HCLD(:,:),TCLD(:,:)
   REAL,    ALLOCATABLE :: CEIL(:,:),SLP(:,:),SFCR(:,:),SNOD(:,:),SST(:,:)
   REAL,    ALLOCATABLE :: GAM(:,:)

   LOGICAL, ALLOCATABLE :: VALIDPT(:,:),BITMAP(:)

  END MODULE aset2d



