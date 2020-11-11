#!/usr/bin/perl

use strict;
use warnings;
use Data::Dumper;
use List::Util qw/max min/;

my $z2a = { 'H' => 1, 'C' => 6, 'N' => 7, 'O' => 8 };

sub read_crd {
  my ($crdname) = @_;
  open my $fh, '<', $crdname;
  my $title = <$fh>;
  #my $natom = <$fh>;
  local $_ = <$fh>;
  my ($natom) = (/(\d+)/)[0];
  local $/ = undef;
  local $_ = <$fh>;
  my @token = (split);
  my $crd = [map { [@token[$_ * 3 .. $_ * 3 + 2]] }
    (0 .. $natom - 1)];
  my $box = [@token[$natom * 3 .. $natom * 3 + 2]];
  $crd, $box;
}

sub read_prm {
  my ($prmname) = @_;
  #local $/ = undef;
  open my $fh, '<', $prmname;
  my $prm = {};
  my $key;
  for (<$fh>) {
    if (/FLAG/) {
      $key = (split)[1];
      $prm->{$key} = [];
    } elsif (/^\%FORMAT/) {
      next;
    } elsif (! defined $key) {
      next;
    } else {
      push @{$prm->{$key}}, (split);
    }
  }
  $prm;
}

sub write_ein {
  my ($crd, $prm, $einname) = @_;
  open my $fh, '>', $einname;
  my $natom = @$crd;
  printf $fh "%10d" x 4 . "\n", $natom, 1, 0, 1;
  my $type = $prm->{'AMBER_ATOM_TYPE'};
  my $zs = [map { (/([A-Z][a-z]?)/)[0] } @$type];
  my $chg = [map { $_ / 18.2223 } @{$prm->{'CHARGE'}}];
  for (0 .. $natom - 1) {
    printf $fh "%10d" . "%20.12f" x 4 . "%3s\n",
      $z2a->{$zs->[$_]} || 0, @{$crd->[$_]},
      $chg->[$_], $type->[$_];
  }
}

sub write_opt {
  my ($box, $prmname, $optname) = @_;
  open my $fh, '>', $optname;
  my $pbc = ($optname =~ /\.real\.opt$/)
    ? '.true.' : 'false';
  my $fmtd_box = [map { sprintf "%12.7f", $_ } @$box];
  print $fh <<"  >>"
    &filename
      prmfile_name ='$prmname'
      logfile_name ='stdout'
      log_level    = 1
    /
    &pbc
      is_pbc       = $pbc
      is_ewald     = $pbc
      cell_x       = $fmtd_box->[0]
      cell_y       = $fmtd_box->[1]
      cell_z       = $fmtd_box->[2]
    /
  >>
}

sub guess_box {
  my ($crd) = shift;
  my $box = [];
  for my $ixyz (0 .. 2) {
    my $proj = [map { $_->[$ixyz] } @$crd ];
    my $max = max(@$proj);
    my $min = min(@$proj);
    $box->[$ixyz] = ($max - $min + 2.0);
  }
  $box;
}

sub mainloop {
  for my $inputname (@_) {
    (my $basename = $inputname) =~ s/\.[^\.]*$//;
    my $crdname = $basename . ".crd";
    my $prmname = $basename . ".prmtop";
    my $einname = $basename . ".ein";
    my $realoptname  = $basename . ".real.opt";
    my $modeloptname = $basename . ".model.opt";
    my ($crd, $box) = read_crd($crdname);
    if (! defined $box->[0]) {
      $box = guess_box($crd);
    }
    my ($prm) = read_prm($prmname);
    write_ein($crd, $prm, $einname);
    write_opt($box, $prmname, $realoptname);
    write_opt($box, $prmname, $modeloptname);
  }
}

mainloop(@ARGV);
