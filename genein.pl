#!/usr/bin/perl

$/ = undef;
$file = <>;
$file =~ s/\n//g;
@file = split ( /\s+/, $file );
if ( ! shift ( @file ) ) { printf ( "%10s", shift @file ) }
printf ( "%10i%10i%10i\n", 1, 0, 1 );
while ( @file ) {
  printf ( "%10i", 0 );
  foreach ( 1 .. 3 ) { printf ( "%20.12f", ( shift @file ) / 0.5291772086 ) }
  printf ( "%20.12f", 0 );
  print " ??\n";
}
    
  
