#!/bin/bash

#$ -wd /data/home/kylem/src/github/metrumresearchgroup/bbr.bayes/inst/model/nonmem/bayes/1100

/opt/NONMEM/nm75/run/nmfe75 1100.ctl  1100.lst  -maxlim=2
