      SUBROUTINE CALHEL(U,V,P,ZINT,PINT,LMH,LM,HELI,UST,VST)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    CALHEL      COMPUTE STORM-RELATIVE HELICITY   
C   PRGMMR: BALDWIN         ORG: W/NMC2     DATE: 95-07-31       
C     
C ABSTRACT:  THIS ROUTINE COMPUTES STORM MOTION AND
C   STORM RELATIVE HELICITY FOLLWING DAVIES-JONES (1990).
C   CALLED BY SOUNDING POST-PROCESSOR.
C     
C PROGRAM HISTORY LOG:
C   95-07-31  MIKE BALDWIN
C     
C USAGE:    CALL CHKOUT
C   INPUT ARGUMENT LIST:
C     U - U COMPONENT OF WINDS IN SOUNDING (M/S)
C     V - V COMPONENT OF WINDS IN SOUNDING (M/S)
C     P - PRESSURE AT MID-LAYER (Pa)
C     ZINT - HEIGHTS OF LAYER INTERFACES (m)
C     PINT - PRESSURE OF LAYER INTERFACES (Pa)
C     LMH - NUMBER OF ABOVE GROUND LAYERS
C     LM - DIMENSION OF VERTICAL ARRAYS
C
C   OUTPUT ARGUMENT LIST: 
C     HELI - STORM-RELATIVE HELICITY (M**2/S**2)
C     UST  - U COMPONENT OF STORM MOTION (M/S)
C     VST  - V COMPONENT OF STORM MOTION (M/S)
C     
C   OUTPUT FILES:
C     NONE
C     
C   SUBPROGRAMS CALLED:
C       NONE  
C     
C   ATTRIBUTES:
C     LANGUAGE: FORTRAN
C     MACHINE : CRAY C-90
C$$$  
      DIMENSION U(LM),V(LM),P(LM),ZINT(LM+1),PINT(LM+1)
      PARAMETER (P150=15000.0,P300=30000.0)
      PARAMETER (PI=3.141592654,PI6=PI/6.0,PI9=PI/9.0)
C
         UMEAN6 = 0.0
         VMEAN6 = 0.0
         EMEAN6 = 0.0
         UMEAN5 = 0.0
         VMEAN5 = 0.0
         EMEAN5 = 0.0
         UMEAN1 = 0.0
         VMEAN1 = 0.0
         EMEAN1 = 0.0
         SREH  = 0.0
         LLMV  = LMH
         LLM1  = LLMV+1
         PSFCK = PINT(LLM1)
         HTSFC = ZINT(LLM1)

C     
C     COMPUTE MASS WEIGHTED MEAN WIND FROM 150 MB ABOVE
C      SURFACE TO 300 MB LAYER
C
         DO L = LLMV,1,-1
            Z50 = 0.5 * ( ZINT(L) + ZINT(L+1) )
            DZABV = Z50 - HTSFC
            DETA= PINT(L+1)-PINT(L)
            IF (DZABV.LE.6000.) THEN
               UMEAN6 = UMEAN6 + U(L) * DETA
               VMEAN6 = VMEAN6 + V(L) * DETA
               EMEAN6 = EMEAN6 + DETA
            ENDIF

            IF (DZABV.LT.6000. .AND. DZABV.GE.5500.) THEN
               UMEAN5 = UMEAN5 + U(L) * DETA
               VMEAN5 = VMEAN5 + V(L) * DETA
               EMEAN5 = EMEAN5 + DETA
            ENDIF

            IF (DZABV.LE.500.) THEN
               UMEAN1 = UMEAN1 + U(L) * DETA
               VMEAN1 = VMEAN1 + V(L) * DETA
               EMEAN1 = EMEAN1 + DETA
            ENDIF

         ENDDO

         IF (EMEAN5.EQ.0.0) THEN
           DO L = LLMV,1,-1
            Z50 = 0.5 * ( ZINT(L) + ZINT(L+1) )
            DZABV = Z50 - HTSFC
            DETA= PINT(L+1)-PINT(L)
            IF (DZABV.LT.7000. .AND. DZABV.GT.6000.) THEN
               UMEAN5 = U(L) * DETA
               VMEAN5 = V(L) * DETA
               EMEAN5 = DETA
            ENDIF
           ENDDO
         ENDIF

      IF(EMEAN6 .EQ.0. .OR. EMEAN5 .EQ. 0. .OR. EMEAN1 
     1      .EQ. 0.) THEN
        UST = 0.0
        VST = 0.0
      ELSE
        UMEAN6 = UMEAN6 / EMEAN6
        VMEAN6 = VMEAN6 / EMEAN6
        UMEAN5 = UMEAN5 / EMEAN5
        VMEAN5 = VMEAN5 / EMEAN5
        UMEAN1 = UMEAN1 / EMEAN1
        VMEAN1 = VMEAN1 / EMEAN1

C      COMPUTE STORM MOTION VECTOR
        USHR = UMEAN5 - UMEAN1
        VSHR = VMEAN5 - VMEAN1
        UST = UMEAN6 + (7.5*VSHR/SQRT(USHR**2+VSHR**2))
        VST = VMEAN6 - (7.5*USHR/SQRT(USHR**2+VSHR**2))
      ENDIF

C
C       COMPUTE STORM-RELATIVE HELICITY
C
         DO L = LLMV-1,2,-1
           Z50 = 0.5 * ( ZINT(L) + ZINT(L+1) )
           DZABV = Z50 - HTSFC
           IF (DZABV.LT.3000.0) THEN
            Z2 = 0.5 * ( ZINT(L) + ZINT(L+1) )
            Z1  = 0.5 * ( ZINT(L+1) + ZINT(L+2) )
            Z3  = 0.5 * ( ZINT(L-1) + ZINT(L) )
            DZ =(ZINT(L)-ZINT(L+1))
            DZ1=Z1-Z2
            DZ2=Z2-Z3
            DU1=U(L+1)-U(L)
            DU2=U(L)-U(L-1)
            DV1=V(L+1)-V(L)
            DV2=V(L)-V(L-1)
           SREH=((V(L)-VMEAN)*(DZ2*(DU1/DZ1)+DZ1*(DU2/DZ2))
     1       -(U(L)-UMEAN)*(DZ2*(DV1/DZ1)+DZ1*(DV2/DZ2)))
     2       *DZ/(DZ1+DZ2)+SREH
           ENDIF
         ENDDO
         HELI=SREH
         RETURN
         END
