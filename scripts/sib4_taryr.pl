#!/usr/bin/perl
# This script creates tar files per year of SiB4 output.

$prefix = 'sib4_0.5x0.5_hourly_';
$suffix = '.pft.nc';
$pathin = '/spinning-scratch/kdhaynes/output/global/0.5x0.5/hourly/';
$pathout= '/spinning-scratch/kdhaynes/sib4_gdrive/sib4_output/global/0.5x0.5/hourly/';

chdir $pathin;

for($year=1998; $year<=2018; $year++)
{

   $mkdiryr = "mkdir -p $year";
   system($mkdiryr);
   chdir $year;
   
   $lnfiles = "ln -s ../$prefix$year*$suffix .";
   system($lnfiles);

   chdir $pathin;
   $taryr = "tar cvfh $pathout$prefix$year.tar $year";
   print "$taryr\n";
   system($taryr);
   
   $rmdir = "rm -r $year";
   system($rmdir);
}