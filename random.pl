#!/usr/bin/perl

$size = 20.0;
$cmax =  1.0;
$ctot =  0.0;
$n    = 1000;

foreach ( 0 .. $n-1 ) {
  $crg[$_] = rand ( $cmax * 2 ) - $cmax;
  $ctot += $crg[$_];
}

$ctot /= $n;

print $n.$/;
foreach ( 0 .. $n - 1 ) {
  foreach ( 'x' .. 'z' ) {
    printf ( "%12.6f", rand ( $size ) );
  }
  printf ( "%12.6f\n", $crg[$_] - $ctot );
}
print "numex\n";
foreach ( 0 .. $n - 1 ) { print ' 1' }
print "\n";
print "inb\n";
foreach ( 0 .. $n - 1 ) { print ' 0' }
print "\n";


