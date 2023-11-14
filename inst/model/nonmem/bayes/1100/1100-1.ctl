$PROBLEM From bbr: see 1100.yaml for details

$INPUT C NUM ID TIME SEQ CMT EVID AMT DV AGE WT HT EGFR ALB BMI SEX AAG
  SCR AST ALT CP TAFD TAD LDOS MDV BLQ PHASE

$DATA ../../../../../extdata/analysis3.csv IGNORE=(C='C', BLQ=1)

$SUBROUTINE ADVAN4 TRANS4

$PK

;log transformed PK parms

  CLEGFR = LOG(EGFR/90) * THETA(6)
  CLAGE  = LOG(AGE/35) * THETA(7)
  CLALB  = LOG(ALB/4.5) * THETA(8)

  V2WT   = LOG(WT/70)
  CLWT   = LOG(WT/70) * 0.75
  V3WT   = LOG(WT/70)
  QWT    = LOG(WT/70) * 0.75

  MU_1   = THETA(1)
  MU_2   = THETA(2) + V2WT
  MU_3   = THETA(3) + CLWT + CLEGFR + CLAGE + CLALB
  MU_4   = THETA(4) + V3WT
  MU_5   = THETA(5) + QWT

  KA     = EXP(MU_1 + ETA(1))
  V2     = EXP(MU_2 + ETA(2))
  CL     = EXP(MU_3 + ETA(3))
  V3     = EXP(MU_4 + ETA(4))
  Q      = EXP(MU_5 + ETA(5))

  S2     = V2/1000 ; dose in mcg, conc in mcg/mL

$ERROR

  IPRED = F
  Y     = IPRED * (1 + EPS(1))

$THETA
; log values
  (0.5)   ; 1 KA (1/hr) - 1.5
  (3.5)   ; 2 V2 (L) - 60
  (1)     ; 3 CL (L/hr) - 3.5
  (4)     ; 4 V3 (L) - 70
  (2)     ; 5 Q  (L/hr) - 4
  (1)     ; 6 CLEGFR~CL ()
  (1)     ; 7 AGE~CL ()
  (0.5)   ; 8 ALB~CL ()

$OMEGA BLOCK(3)
  0.2           ; ETA(KA)
  0.01 0.2      ; ETA(V2)
  0.01 0.01 0.2 ; ETA(CL)
$OMEGA
  0.025 FIX    ; ETA(V3)
  0.025 FIX    ; ETA(Q)

$SIGMA
  0.05          ; 1 pro error

$PRIOR NWPRI

$THETAP
  (0.5) FIX      ; 1 KA (1/hr) - 1.5
  (3.5) FIX      ; 2 V2 (L) - 60
  (1)   FIX      ; 3 CL (L/hr) - 3.5
  (4)   FIX      ; 4 V3 (L) - 70
  (2)   FIX      ; 5 Q  (L/hr) - 4
  (1)   FIX   ; 6 CLEGFR~CL ()
  (1)   FIX   ; 7 AGE~CL ()
  (0.5) FIX   ; 8 ALB~CL ()
$THETAPV BLOCK(8) VALUES(10, 0) FIX

$OMEGAP BLOCK(3) VALUES(0.2, 0.01) FIX

$OMEGAPD (3 FIX)

$SIGMAP
  0.05 FIX           ; 1 pro error

$SIGMAPD (1 FIX)

$EST METHOD=CHAIN FILE=../init.chn NSAMPLE=0 ISAMPLE=1 SEED=1 CTYPE=0 IACCEPT=0.3 DF=10 DFS=0
$EST METHOD=BAYES CTYPE=0 SEED=1 NBURN=10 NITER=50 PRINT=10 MSFO=./1100.msf RANMETHOD=P PARAFPRINT=10000 BAYES_PHI_STORE=1

$TABLE NUM CL V2 Q V3 KA ETAS(1:LAST) EPRED IPRED NPDE EWRES NOPRINT ONEHEADER FILE=1100.tab RANMETHOD=P
