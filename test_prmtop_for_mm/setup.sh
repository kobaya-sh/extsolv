for f in *.crd; do
  perl crd2ein.pl $f
  base=${f%.*}
  ./mm.x $base.real.opt $base.ein $base.real.eou.ans \
    > $base.real.log.ans 2> $base.real.err.ans
  ./mm.x $base.model.opt $base.ein $base.model.eou.ans \
    > $base.model.log.ans 2> $base.model.err.ans
  #rm -f $base.ein $base.model.opt $base.real.opt
done
