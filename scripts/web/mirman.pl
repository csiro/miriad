#!/usr/local/bin/perl
#
# $Source: /var/tmp/RrsvEF/cvsroot/miriad-dist/RCS/scripts/web/mirman.pl,v $
# $Id: mirman.pl,v 1.7 2019/01/10 02:32:54 mci156 Exp $
#
# Set up the environment.

use strict;

my ($TOP,$miriad_uri,$miriad_dir);

$ENV{"MANPATH"} = "/nfs/atapplic/miriad/man:/usr/local/man:/usr/local/site/man:/usr/share/man:/usr/man:/usr/local/karma/man";
$TOP='/var/www/vhosts/www.atnf.csiro.au';
$miriad_uri = "/computing/software/miriad";
$miriad_dir = "$TOP/htdocs/$miriad_uri";

my ($q, %inputs, $topic, $dest, $line, $page);

%inputs = &getcgivars;
$topic = $inputs{"topic"};
#$topic = "man";

if($topic ne ""){
  if( -f "$miriad_dir/$topic.html"){

#    print "Content-type: text/html\n\n";
#    open IN,"$TOP/$miriad_dir/$topic.html";
#    while(<IN>){print $_;}
#    close IN;

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
    $line = '/usr/bin/man ' . $topic . '|/usr/local/bin/rman -f html -r "mirman.pl?topic=%s"';
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
#------------------------------------------------------------------------
# Read all CGI vars into an associative array.
# If multiple input fields have the same name, they are concatenated into
#   one array element and delimited with the \0 character (which fails if
#   the input has any \0 characters, very unlikely but conceivably possible).
# This is a simple version, that assumes a request method of GET.
sub getcgivars {
    my(%in) ;
    my($name, $value) ;

    # Resolve and unencode name/value pairs into %in
    foreach (split('&', $ENV{'QUERY_STRING'})) {
        s/\+/ /g ;
        ($name, $value)= split('=', $_, 2) ;
        $name=~ s/%(..)/chr(hex($1))/ge ;
        $value=~ s/%(..)/chr(hex($1))/ge ;
        $in{$name} .= "\0" if defined($in{$name}) ;
        $in{$name} .= $value ;
    }

    return %in ;

}

