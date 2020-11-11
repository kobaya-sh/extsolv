#!/usr/bin/perl

use strict;
use warnings;
use Data::Dumper;

my (@key, %type, %val, %comment, $header);

while (<>) {
  if (/=/) {
    my ($type, $key, $val, $comment)
      = (/^(.*::\s*)(\S+\s*=)(\s*\d+)(\s+!.*)$/);
    if (! $type{$key}) {
      push @key, $key;
      $type{$key} = $type;
      $comment{$key} = $comment;
    }
    if (! defined $val{$key}) {
      $val{$key} = -1;
    }
    if ($val{$key} < $val) {
      $val{$key} = $val;
    }
  } else {
    $header = $_;
  }
}

print $header;
for (@key) {
  #print $type{$_}, $_, $val{$_}, $comment{$_}, "\n";
  print $type{$_}, $_, $val{$_}, $comment{$_}, "\n";
}
