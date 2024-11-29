#!/usr/local/bin/perl
#
# $Source: /var/tmp/RrsvEF/cvsroot/miriad-dist/RCS/scripts/web/mirman.pl,v $
# $Id: mirman.pl,v 1.17 2019/01/10 23:18:37 mci156 Exp $
#
# Depends on 'rman' (PolyglotMan - formerly RosettaMan)
#
# Set up the environment.

use strict;
use CGI qw/:standard :form/;
use URI;

$CGI::POST_MAX        = 1024 * 10; #max 10kb post size
$CGI::DISABLE_UPLOADS = 1;         #no uploads

my ($site,$TOP,$miriad_uri,$miriad_dir);

$ENV{"MANPATH"} = "/nfs/atapplic/miriad/man:/usr/local/man:/usr/local/site/man:/usr/share/man:/usr/man:/usr/local/karma/man";

$site = 'www.atnf.csiro.au';
$TOP="/var/www/vhosts/$site";
$miriad_uri = "/computing/software/miriad";
$miriad_dir = "$TOP/htdocs/$miriad_uri";

die("No miriad dir $miriad_dir\n") if ( ! -d $miriad_dir );

my ($q, %inputs, $topic, $dest, $line, $page, $u, $scheme);

$q = new CGI;

if ( param('topic') ) {

  $topic = param('topic');
  $topic =~ s/[^a-zA-Z0-9]//g;     #defang input

  # Determine the request's scheme so we can redirect using the same.
  $u = url(-base => 1);
  $scheme = URI->new( $u )->scheme;
  $scheme = 'http' if ($scheme =~ /^$/);

  $dest = '';

  if ( -f "$miriad_dir/$topic.html") {
    $dest .= "$miriad_uri/$topic.html";
  } elsif (-f "$miriad_dir/doc/$topic.html"){
    $dest .= "$miriad_uri/doc/$topic.html";
  }

  if ($dest =~ /\S/) {
    # Explictly reconstruct the base url to avoid XSS
    $dest = $scheme . '://' . $site . $dest;

    print <<"EOF";
Content-type: text/html

<HTML><HEAD>
<TITLE>Redirect to Miriad $topic file</TITLE>
<META HTTP-EQUIV="REFRESH" CONTENT="0;URL=$dest">
</HEAD><BODY>
</BODY></HTML>
EOF

  } else {

    # Try to look up a manual page
    my $testresult;
    $testresult = `/usr/bin/man $topic 2>&1`;
    if ($testresult =~ /[Nn]o  *manual \S+  *for /) {
        $page = '<html>'
              . '<head><TITLE>Web-Based Miriad Topic/UNIX Man Command</TITLE></head>'
              . '<body BGCOLOR=white>'
              . '<H1 align="center">Web-Based Miriad Topic/UNIX Man Command</H1>'
              . 'No manual page for "' . $topic . '".'
              . '</body>'
              . '</html>';
    } else {
        $line = '/usr/bin/man ' . $topic . '|/usr/bin/rman -f html -r "mirman.pl?topic=%s"';
        $page = `$line`;
        $page =~ s{(https?://[\w/\+\?\&;%.~=-]*[\w/])}{&deamp1($1)}eg;
        $page =~ s{(ftp://[\w/\.-]*\w)}{<A HREF="\1">\1</A>}g;
    }

    print "Content-type: text/html\n\n";
    print $page;
  }

} else {

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
