#!/usr/bin/perl

# OPTIONS
#
# this program takes a LaTeX file and subtitutes the following keywords
# in \OptLine by their corresponding values in the top.ini file:
# TYPE, VALUE, DEFAULT and DESCRIPTION. It was designed for the MODELLER
# manual, but it might as well work for the ASGL manual. The modified file
# goes to standard output.
#
# usage : options latexfile > newlatexfile
#

$topini_file = "../src/top.ini" ;      # here you can specify the location of the 
                                # top.ini file
$description_separator = "#" ;  # change this if you decide to use a different
                                # separator for descriptions in top.ini
$logfile = "options.log";	# name of the log file

open(LOG,">$logfile");

# READ AND PARSE top.ini

$flag = 0 ;
$n = 0 ;
open(TOPINI, $topini_file) || die "couldn't open $topini_file" ;
while (<TOPINI>) {
  chop;
  $description_test = "" ;
  $a = index($_,$description_separator) ;
  if ($a > 0) {
    $description_test = substr($_,$a+1) ;
  }
  s/#.*//;
  @Fld = split(" ",$_) ;
# do KEYWORDS end here?
  if ($Fld[1] eq "END") {
    $flag = 2;
  }
# store values only if line is in the "--- KEYWORDS:" block
#    $description[$n] =~ s/\{/\\\{/g;
#    $description[$n] =~ s/\}/\\\}/g;
  if ($flag==1) {
    ++$n ;
    $line[$n] = $_;
    $description[$n] = $description_test;
    $description[$n] =~ s/_/\\_/g;
    $description[$n] =~ s/\|/\\OR\\/g;
    $description[$n] =~ s/\$/\\\$/g;
  }
# do KEYWORDS start here?
  if ($Fld[1] eq "KEYWORDS:") {
    $flag = 1;
  }
}
close(TOPINI);

# PROCESS THE LaTeX FILE

while (<>) {
# find OptLine and extract the KEYWORD name (first word in {}'s)
  if(/^\\OptLine/) {
    $a = index($_,"\{") ;
    $b = index($_,"\}") ;
    $key = substr($_,$a+1,$b-$a-1) ;
    $key2 = $key ;
    $key =~s/\\//g;
#   search the stored top.ini lines for KEYWORD ($key)
    for ($i=1; $i<=$n; $i++) {
      if ($line[$i] =~ /\b$key\b/) {
        $line2 = $line[$i];
        $line2 =~ s/_/\\_/g;
        $line2 =~ s/\$/\\\$/g;
        $line2 =~ s/\{/\\\{/g;
        $line2 =~ s/\}/\\\}/g;
        @Fld = split(" ",$line2) ;
        ($dummy,@Fld) = @Fld ;
        ($type,@Fld) = @Fld ;
        ($name,@Fld) = @Fld ;
        ($values,@Fld) = @Fld ;
        @default = @Fld ;
#       check if the number of default values corresponds to the number in
#       the $values field. If $values is zero any number default values is
#       accepted.
	$nfld = scalar @Fld;
        ( ($values == $nfld)||($values == 0)||($Fld[0]=~/^'/ && $Fld[$nfld-1]=~/\'$/) ) || print LOG "wrong number of default values in $name" ;
#       change $type to lowercase (except first character) and add a '\' to
#       make it work as a LaTeX command (e.g. REAL -> \Real)
        substr($type,1) =~ tr/A-Z/a-z/;
        $type = "\\$type" ;
#       substitute in \OptLine
        s/$key2/$name/;
        s/\{TYPE\}/\{$type\}/;
        s/\{VALUES\}/\{$values\}/;
        s/\{DEFAULT\}/\{@default\}/;
        s/\{DESCRIPTION\}/\{$description[$i]\}/;
      }
    }
  }
  print;
}
close(LOG);
