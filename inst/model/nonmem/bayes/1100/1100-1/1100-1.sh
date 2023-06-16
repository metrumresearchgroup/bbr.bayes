#!/bin/bash

#$ -wd /data/home/kylem/src/github/metrumresearchgroup/bbr.bayes/inst/model/nonmem/bayes/1100/1100-1

/opt/NONMEM/nm75/run/nmfe75 1100-1.ctl  1100-1.lst  -maxlim=2
