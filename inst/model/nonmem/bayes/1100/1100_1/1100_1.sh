#!/bin/bash

#$ -wd /data/home/kylem/src/github/metrumresearchgroup/bbr.bayes/inst/model/nonmem/bayes/1100/1100_1

/opt/NONMEM/nm75/run/nmfe75 1100_1.ctl  1100_1.lst  -maxlim=2
