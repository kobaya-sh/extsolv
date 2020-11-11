#!/usr/bin/perl

( $ni, $nj, $nk ) = ( 10, 10, 10 );
print ( $ni * $nj * $nk );
print "\n";
foreach $i ( 0 .. $ni - 1 ) {
  foreach $j ( 0 .. $nj - 1 ) {
    foreach $k ( 0 .. $nk - 1 ) {
      printf ( "%6.2f%6.2f%6.2f%6.2f\n", 
          $i * 2, $j * 2, $k * 2, 
          0.05 * ( ( -1 ) ** ( $i + $j + $k ) ) ) ;
    }
  }
}
print "numex\n";
foreach ( 0 .. 1000 - 1 ) { print ' 1' }
print "\n";
print "inb\n";
foreach ( 0 .. 1000 - 1 ) { print ' 0' }
print "\n";

