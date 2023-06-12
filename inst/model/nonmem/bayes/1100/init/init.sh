#!/bin/bash

#$ -wd /data/home/kylem/src/github/metrumresearchgroup/bbr.bayes/inst/model/nonmem/bayes/1100/init

/opt/NONMEM/nm75/run/nmfe75 init.ctl  init.lst  -maxlim=2
