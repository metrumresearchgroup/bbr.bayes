#!/bin/bash

#$ -wd /data/home/kylem/src/github/metrumresearchgroup/bbr.bayes/inst/model/nonmem/bayes/1100/1100-2

/opt/NONMEM/nm75/run/nmfe75 1100-2.ctl  1100-2.lst  -maxlim=2
