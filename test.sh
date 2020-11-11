make
./mm.x T cyclo.ein cyclo.eou
diff mm.log mm.log.ans
diff cyclo.eou cyclo.eou.ans
./mm.x c1apb.opt c1apb.ein c1apb.eou
diff c1apb.eou c1apb.eou.ans
