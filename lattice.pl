#!/usr/bin/perl

( $ra   , $rb   , $rc    ) = ( 20.0, 20.0, 20.0 );
( $alpha, $beta , $gamma ) = ( 90.0, 90.0, 90.0 );
( $na   , $nb   , $nc    ) = ( 10  , 10  , 10   );
( $da   , $db   , $dc    ) = ( $ra/$na, $rb/$nb, $rc/$nc );
$charge = 1.0;
$n = $na * $nb * $nc; 

#print $n*8;
print $n;
print $/;
foreach my $ia ( 0 .. $na - 1 ) {
  foreach my $ib ( 0 .. $nb - 1 ) {
    foreach my $ic ( 0 .. $nc - 1 ) {
      printf("%6.2f%6.2f%6.2f%6.2f\n", $da*($ia+0.0), $db*($ib+0.0), $dc*($ic+0.0), $charge);
      printf("%6.2f%6.2f%6.2f%6.2f\n", $da*($ia+0.0), $db*($ib+0.0), $dc*($ic+0.5), $charge);
      printf("%6.2f%6.2f%6.2f%6.2f\n", $da*($ia+0.0), $db*($ib+0.5), $dc*($ic+0.0), $charge);
      printf("%6.2f%6.2f%6.2f%6.2f\n", $da*($ia+0.0), $db*($ib+0.5), $dc*($ic+0.5), $charge);
      printf("%6.2f%6.2f%6.2f%6.2f\n", $da*($ia+0.5), $db*($ib+0.0), $dc*($ic+0.0), $charge);
      printf("%6.2f%6.2f%6.2f%6.2f\n", $da*($ia+0.5), $db*($ib+0.0), $dc*($ic+0.5), $charge);
      printf("%6.2f%6.2f%6.2f%6.2f\n", $da*($ia+0.5), $db*($ib+0.5), $dc*($ic+0.0), $charge);
      printf("%6.2f%6.2f%6.2f%6.2f\n", $da*($ia+0.5), $db*($ib+0.5), $dc*($ic+0.5), $charge);
    }
  }
}
print "numex\n";
foreach ( 0 .. $n - 1 ) { print ' 1' }
print "\n";
print "inb\n";
foreach ( 0 .. $n - 1 ) { print ' 0' }
print "\n";


