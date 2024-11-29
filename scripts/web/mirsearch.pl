#!/usr/local/bin/perl -- -*- perl -*-
#
# $Source: /var/tmp/RrsvEF/cvsroot/miriad-dist/RCS/scripts/web/mirsearch.pl,v $
# $Id: mirsearch.pl,v 1.6 2019/01/10 23:05:43 mci156 Exp $
#
# miriadsearch.pl    David Mar  13-Mar-1996
# modified from: sitesearch.pl    David Mar  29-Sep-1995
# modified for new server         Henrietta May 12 Mar 1998
# Processes information from form
# and produces a list of matching documents.

use strict;

my ($TOP, $miriad_uri, $miriad_dir);
my ($Matches, $Search);


$TOP='/var/www/vhosts/www.atnf.csiro.au';
push(@INC, "$TOP/cgi-bin");
require "www-lib.pl";
$miriad_uri = "/computing/software/miriad";
$miriad_dir = "$TOP/htdocs/$miriad_uri";

sub ReadParse
{
  my (@in, %out);
  if (@_) {
    (@in) = @_;
  }

  my ($i, $loc, $key, $val, $instr);

  # Read in text
  if ($ENV{'REQUEST_METHOD'} eq "GET") {
    $instr = $ENV{'QUERY_STRING'};
  } elsif ($ENV{'REQUEST_METHOD'} eq "POST") {
    for ($i = 0; $i < $ENV{'CONTENT_LENGTH'}; $i++) {
      $instr .= getc;
    }
  }

  @in = split(/&/,$instr);

  foreach $i (0 .. $#in) {
    # Convert plus's to spaces
    $in[$i] =~ s/\+/ /g;

    # Convert %XX from hex numbers to alphanumeric
    $in[$i] =~ s/%(..)/pack("c",hex($1))/ge;

    # Split into key and value.
    $loc = index($in[$i],"=");
    $key = substr($in[$i],0,$loc);
    $val = substr($in[$i],$loc+1);
    $out{$key} .= '\0' if (defined($out{$key})); # \0 is the multiple separator
    $out{$key} .= $val;
  }

  return %out;
}

sub searchfile
{
	my($dir, $file) = @_;
        my ($filespec, $Title, $TitleGo, $Matches);

#	print "$dir/$file\n";          # DEBUG

	return if ($file !~ /html/);
	$TitleGo = 0;
	open(FILE, $file) || die;
	while (<FILE>)
	{
		$Title = $Title.$_ if ($TitleGo);
		if ($_ =~ /<TITLE/i)
		{
			$Title = $_;
			$TitleGo = 1;
		}
		$TitleGo = 0 if ($_ =~ /\/TITLE/i);
		last if ($_ =~ />Home</);         # Don't search footer links!
		next if ($_ !~ /$Search/io);
		$Title =~ s#</*TITLE>|</*title>##g;
                $filespec = "$miriad_uri/$dir/$file";
                $filespec = "$miriad_uri/$file" if ($dir eq '.');
		print "<LI><A HREF=\"$filespec\">$Title</A>\n";
		$Matches++;
		last;
	}
	close(FILE);
}

sub searchdir
{
	my($dir, $nlink) = @_;
	my($dev, $ino, $mode, $subcount, $DIR, @filenames, $name, $subcount);
	($dev, $ino, $mode, $nlink) = stat('.') unless $nlink;

	# Get files in current directory.
	opendir(DIR, '.') || die;
	my(@filenames) = readdir(DIR);
	closedir(DIR);

	if ($nlink == 2)            # This dir has no subdirectories.
	{
		foreach (@filenames)
		{
			next if ($_ eq '.');
			next if ($_ eq '..');
			$name = "$dir/$_";
			&searchfile($dir, $_) if (substr($_, -4) eq 'html');
		}
	}
	else                        # This dir has subdirectories.
	{
		$subcount = $nlink - 2;
		foreach (@filenames)
        {
            next if ($_ eq '.');
            next if ($_ eq '..');
			$name = "$dir/$_";
			&searchfile($dir, $_) if (substr($_, -4) eq 'html');
			next if ($subcount == 0);
			($dev, $ino, $mode, $nlink) = lstat($_);
			# Skip unless file is a directory and starts with capital letter.
			next unless ((-d _) && (ord($_) > 63) && (ord($_) < 95));
			# Do next directory recursively.
			chdir($_) || die;
			&searchdir($name, $nlink);
			chdir('..');
			--$subcount;
		}
	}
}

my %in;
%in = ReadParse();			# get input;

print << "EOF";
Content-type: text/html

<HTML><HEAD>
<TITLE>Miriad Documentation Search Results</TITLE>
</HEAD>\n<BODY BACKGROUND="/computing/software/miriad/gmiriad.gif">
<TABLE CELLSPACING="0" CELLPADDING="1"><TR VALIGN=TOP>

<TD WIDTH=160 HEIGHT=800 NOWRAP>
<BR><BR><BR><BR><BR><BR><BR><BR><BR>
<BR><BR>

<!-- General Search -->

<FORM ACTION="/cgi-bin/miriad/mirsearch.pl" METHOD=POST>
<TABLE BORDER="0" CELLSPACING="0" CELLPADDING="0" NOWRAP>
<TR><TH ALIGN=LEFT COLSPAN=2><FONT COLOR="#FFFFFF"><B>Search</B></FONT></TH></TR>
<TR><TD><INPUT TYPE="Text" NAME="searchstring" SIZE="10" MAXLENGTH="50"></TD>
    <TD><INPUT TYPE="Image" NAME="Go" SRC="/computing/software/miriad/gblue.gif" BORDER="0"></TD></TR>
</TABLE>
</FORM>
<BR>

<!-- Task Lookup -->

<FORM ACTION="/cgi-bin/miriad/mirman.pl" METHOD=GET>
<TABLE BORDER="0" CELLSPACING="0" CELLPADDING="0" NOWRAP>
<TR><TH ALIGN=LEFT COLSPAN=2><FONT COLOR="#FFFFFF"><B>Topic Lookup</B></FONT></TH></TR>
<TR><TD><INPUT TYPE="Text" NAME="topic" SIZE="10" MAXLENGTH="50"></TD>
    <TD><INPUT TYPE="Image" NAME="Go" SRC="/computing/software/miriad/gblue.gif" BORDER="0"></TD></TR>
</TABLE>
</FORM>


</TD><TD>
<BR>
<CENTER><H1><I>Miriad</I> Documentation Search Results</H1></CENTER>
<HR>
EOF

$Matches = 0;


$Search = $in{'searchstring'};
print "Your search string: $Search<P>\n";
if ($Search =~ /^\s*$/)         # Search string is white space or blank.
{
	print "<STRONG>Please enter a search string.</STRONG>\n";
}
else                            # Search string contains something.
{
	print "<UL>\n";
	chdir("$miriad_dir");
	&searchdir('.');
	print "</UL>\n";

	if ($Matches == 0)
	{
		print "<STRONG>Sorry. No matches.</STRONG><BR>\n";
		print "You may wish to go back and try again with a slightly ";
		print "different search string.\n";
	}
}

print "<HR><EM>miriad\@atnf.csiro.au</EM></TD></TR></TABLE></BODY></HTML>\n";
# end
