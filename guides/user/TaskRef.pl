#!/usr/local/bin/perl
use strict;

my @list;
my $title;
my $f;my $line;my $n;

foreach $f ( <node*.html> ){
  ($n) = ($f =~ m{.*node(\d+)\.html});
  $_ = `cat $f`;
  ($title) = m{<TITLE>([^<]*)</TITLE>};
  while (m{<A\s+([^>]*\s+|)HREF="\.\./(\w+)\.html"(\s+[^>]*|)\s*>}g){
    if( -x ($ENV{"MIRBIN"} . "/" . $2)){
      $line = sprintf("%-10s %4d %s",$2,$n,$title);
      push @list,$line;
    }
  }
}

@list = sort @list;
$line = $list[0];
&genline($line);
foreach $f (@list){
  if( $f ne $line){
    $line = $f;
    &genline($line);
  }
}

sub genline{
  my $task; my $n; my $title;
  ($task,$n,$title) = ($_[0] =~m{(\w+)\s+(\d+)\s+(.*)});
  print "$task <A HREF=userguide/node$n.html>$title</A>\n";
}
