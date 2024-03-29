$PROBLEM PK model 1 cmt base

$INPUT ID TIME MDV EVID DV AMT  SEX WT ETN NUM
$DATA ../../../../extdata/acop.csv IGNORE=@
IGNORE(ID.EQ.2)
$SUBROUTINES ADVAN2 TRANS2

$PK
ET=1
IF(ETN.EQ.3) ET=1.3
KA = THETA(1)
CL = THETA(2)*((WT/70)**0.75)* EXP(ETA(1))
V = THETA(3)*EXP(ETA(2))
SC=V


$THETA
(0, 2)  ; KA
(0, 3)  ; CL
(0, 10) ; V2
(0.02)  ; RUVp
(1)     ; RUVa

$OMEGA
0.05    ; iiv CL
0.2     ; iiv V2

$SIGMA
1 FIX

$ERROR
IPRED = F
IRES = DV-IPRED
W = IPRED*THETA(4) + THETA(5)
IF (W.EQ.0) W = 1
IWRES = IRES/W
Y= IPRED+W*ERR(1)
FAKE=1 ; for testing

$EST METHOD=1 INTERACTION MAXEVAL=9999 SIG=3 PRINT=5 NOABORT POSTHOC
$COV
$TABLE NUM IPRED NPDE CWRES NOPRINT ONEHEADER FILE=1.tab
$TABLE NUM CL V KA ETAS(1:LAST) NOAPPEND NOPRINT ONEHEADER FILE=1par.tab
$TABLE ID CL NOAPPEND NOPRINT ONEHEADER FIRSTONLY FILE=1first1.tab ; for testing firstonly
$TABLE NUM CL NOAPPEND NOPRINT ONEHEADER FIRSTONLY FILE=1first2.tab ; for testing firstonly
$TABLE NUM ID CL NOAPPEND NOPRINT ONEHEADER FIRSTONLY FILE=1first3.tab ; for testing firstonly
$TABLE NUM CL KA FAKE NOAPPEND NOPRINT ONEHEADER FILE=1dups.tab ; for testing dropping duplicate columns
