#!/usr/bin/perl

use strict;
use warnings;

my ($prgrm, $startyr, $stopyr) = @ARGV;
if (not defined $prgrm) {
    die "Need Program To Process!\n";
}

if (not defined $startyr){
    die "Need Starting Year!\n";
}

if (not defined $stopyr){
   $stopyr = $startyr;
   #print "Processing $startyr \n";
}
else {
  #print "Processing $startyr to $stopyr \n";
}

for (my $i=$startyr; $i <= $stopyr; $i++){
    my $outname = "$prgrm" . "$i" . ".out";
    print "Submitting $prgrm $i \n";
    system("sib4post $prgrm $i >$outname &");
}

