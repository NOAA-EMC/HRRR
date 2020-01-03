C   01-10-22  H CHUANG - MODIFIED TO PROCESS HYBRID MODEL OUTPUT
C   02-04-17  BALDWIN  - MODIFIED TO INCLUDE ALL 3D ARRAYS
      module vrbls3d
      real, allocatable :: UH(:,:,:),VH(:,:,:),WH(:,:,:)
     &,U(:,:,:),V(:,:,:),T(:,:,:),Q(:,:,:),W(:,:,:)
     &,CWM(:,:,:),Q2(:,:,:),PMID(:,:,:)
     &,PINT(:,:,:),ALPINT(:,:,:),ZMID(:,:,:)
     &,ZINT(:,:,:),RAINW(:,:,:),OMGA(:,:,:)
     &,TH(:,:,:),F_RAIN(:,:,:),F_ICE(:,:,:)
     &,CLDFRA(:,:,:),F_RIMEF(:,:,:)
      end module vrbls3d
