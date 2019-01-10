#!/usr/local/bin/perl
#
# $Source: /var/tmp/RrsvEF/cvsroot/miriad-dist/RCS/scripts/web/mirman.pl,v $
# $Id: mirman.pl,v 1.11 2019/01/10 02:40:54 mci156 Exp $
#
# Depends on 'rman' (PolyglotMan - formerly RosettaMan)
#
# Set up the environment.

use strict;
use CGI qw/:standard :form/;

$CGI::POST_MAX        = 1024 * 10; #max 10kb post size
$CGI::DISABLE_UPLOADS = 1;         #no uploads

my ($TOP,$miriad_uri,$miriad_dir);

$ENV{"MANPATH"} = "/nfs/atapplic/miriad/man:/usr/local/man:/usr/local/site/man:/usr/share/man:/usr/man:/usr/local/karma/man";
$TOP='/var/www/vhosts/www.atnf.csiro.au';
$miriad_uri = "/computing/software/miriad";
$miriad_dir = "$TOP/htdocs/$miriad_uri";

my ($q, %inputs, $topic, $dest, $line, $page);

$q = new CGI;

if ( param('topic') ) {

  $topic = param('topic');
  $topic =~ s/[^a-zA-Z0-9]//g;     #defang input

  if( -f "$miriad_dir/$topic.html"){
    $dest = "http://www.atnf.csiro.au/computing/software/miriad/$topic.html";
    print <<"EOF";
Content-type: text/html

<HTML><HEAD>
<TITLE>Redirect to Miriad $topic file</TITLE>
<META HTTP-EQUIV="REFRESH" CONTENT="0;URL=$dest">
</HEAD><BODY>
</BODY></HTML>
EOF
#This page is being redirected to 
#<A HREF=$dest>$dest</A>.
  }else{
    print "Content-type: text/html\n\n";
    $line = '/usr/bin/man ' . $topic . '|/usr/bin/rman -f html -r "mirman.pl?topic=%s"';
    $page = `$line`;
    $page =~ s{(http://[\w/\+\?\&;%.~=-]*[\w/])}{&deamp1($1)}eg;
    $page =~ s{(ftp://[\w/\.-]*\w)}{<A HREF="\1">\1</A>}g;
    print $page;
  }
}else{
  print <<"EOF";
Content-type: text/html

<HTML><HEAD>
<TITLE>Web-Based Miriad Topic/UNIX Man Command</TITLE>
</HEAD><BODY BGCOLOR=white>
<CENTER><H1>Web-Based Miriad Topic/UNIX Man Command</H1></CENTER>
<FORM METHOD=get ACTION=mirman.pl>
<CENTER>Topic:&nbsp;&nbsp;&nbsp;
<INPUT NAME=topic SIZE=20>
<INPUT TYPE=submit VALUE="Lookup"></CENTER>
</FORM>
</BODY></HTML>
EOF
}
#------------------------------------------------------------------------
sub deamp1{
  my ($a,$b,$c);
  ($a) = @_;
  if($a =~ m{^(.+)(&\w+)$}){
    $a = $1;
    $b = $a;
    $c = $2;
  }else{
    $b = $a;
    $c = "";
  }
  $a =~ s{&amp;}{&}g;
  return "<A HREF=\"$a\">$b</A>$c";
}
