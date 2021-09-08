  MODULE asetdown
!=======================================================================
!  Initialize OUTPUT GRID  arrays
!=======================================================================
   REAL,    ALLOCATABLE :: DOWNT(:,:),DOWNDEW(:,:),DOWNU(:,:), DOWNV(:,:),DOWNQ(:,:), DOWNP(:,:)
   REAL,    ALLOCATABLE :: WGUST(:,:),SKY(:,:),BLR(:,:)
   REAL,    ALLOCATABLE :: DIRTRANS(:,:),MGTRANS(:,:),LAL(:,:),MIXHGT(:,:)
  END MODULE asetdown


