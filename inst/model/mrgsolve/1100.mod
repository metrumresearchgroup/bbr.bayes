[ prob ]
1100

[ pkmodel ] cmt = "GUT,CENT,PERIPH", depot = TRUE

[ param ]
WT   = 70
EGFR = 90
AGE  = 35
ALB  = 4.5
ETA1 = 0
ETA2 = 0
ETA3 = 0
ETA4 = 0
ETA5 = 0

[ nmext ]
run = "1100_1"
project = "../nonmem/bayes/1100"
root = "cppfile"

[ main ]
double CLEGFR = log(EGFR/90) * THETA6;
double CLAGE  = log(AGE/35) * THETA7;
double CLALB  = log(ALB/4.5) * THETA8;

double V2WT   = log(WT/70);
double CLWT   = log(WT/70) * 0.75;
double V3WT   = log(WT/70);
double QWT    = log(WT/70) * 0.75;

capture KA = exp(THETA1 + ETA1 + ETA(1));
capture V2 = exp(THETA2 + V2WT + ETA2 + ETA(2));
capture CL = exp(THETA3 + CLWT + CLEGFR + CLAGE + CLALB + ETA3 + ETA(3));
capture V3 = exp(THETA4 + V3WT + ETA4 + ETA(4));
capture Q  = exp(THETA5 + QWT + ETA5 + ETA(5));

double S2 = V2/1000; //; dose in mcg, conc in mcg/mL

[ table ]
capture IPRED = CENT/S2;
capture Y = IPRED * (1 + EPS(1));
