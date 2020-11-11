#!/usr/bin/perl

open my $optgeomfh,'<', 'optgeom.qmmm.dat';
my @layer = map { (/\b[HL]\b/)[0] } grep { /\b[HL]\b/ } <>;
$_ = shift;
print;
while (<>) {
  if (shift @layer eq 'L') {
    printf "%20.12f%20.12f%20.12f\n", 0, 0, 0;
  } else {
    print;
  }
}
