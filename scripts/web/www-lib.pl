# Perl Routines to Manipulate Intranet web pages 
# Steve Barker
#
# Copyright (c) 1996 Steve Barker, CSIRO Division of Radiophysics
#
# Changed for ATNF use, Sally Houghton 13-Dec-1997

($www_lib'version = '$Revision: 1.1 $') =~ s/[^.\d]//g;

#----------------------------------------------------------------------------
# Write the default ATNF header into an html document.
#----------------------------------------------------------------------------
sub atHeader 
{
    local($PageTitle) = @_;
    print "Content-type: text/html\n\n"; # Start of output
    print `sed '4s/here/${PageTitle}/' /nfs/wwwatdocs/common/standard_head`;
    print "<H1 ALIGN=\"CENTER\">$PageTitle</H1>\n";
    print "<P>\n\n";
}

#----------------------------------------------------------------------------
# Write the default ATNF footer into an html document.
#----------------------------------------------------------------------------
sub atFooter
{
print `cat /nfs/wwwatdocs/common/standard_tail`;
}

#----------------------------------------------------------------------------
# Write the default RP header into an html document.
#----------------------------------------------------------------------------
sub RpHeader 
{
    print "Content-type: text/html\n\n"; # Start of output
    print "<HTML>\n<HEAD>\n";
    print "<TITLE> $_[0] </TITLE>\n";
    print "</HEAD>\n";
    print "<BODY BACKGROUND = \"/backgrounds/$_[1]\" BGCOLOR = \"#ffffff\">\n";
    print "<H1 ALIGN=\"CENTER\"> $_[0] </H1>\n";
    print "<HR>\n\n";
}
			  

#----------------------------------------------------------------------------
# Write the default RP footer into an html document.
#----------------------------------------------------------------------------
sub RpFooter 
{
    
    print "<BR>\n<HR ALIGN = left SIZE = 4 WIDTH = 100%>\n<BR>\n";
    print "<CENTER>\n<FONT SIZE = 2>\n\n";
    
    print "<A HREF = \"/forms/search.html\">\n";
    print "<IMG SRC = \"/gifs/search_bt.gif\" BORDER = 0 ALT = \"[Search ...]\"></A>\n";
    print "<A HREF = \"/forms/suggest.html\">\n";
    print "<IMG SRC = \"/gifs/suggest_bt.gif\" BORDER = 0 ALT = \"[Suggestions]\"></A>\n";
    print "<A HREF = \"http://wwwrp.rp.csiro.au/\">";
    print "<IMG SRC = \"/gifs/homepage_bt.gif\" BORDER = 0 ALT = \"[Home Page]\"></A>\n\n";
    
    print "<P>\n";
    print "<STRONG><EM> CSIRO - Division of Radiophysics </EM></STRONG><BR>\n";
    print "<EM> Last Modified: ". $date." </EM><BR>\n";
    print "<ADDRESS><A HREF = \"mailto:wwwrp\@rp.csiro.au\">wwwrp\@rp.csiro.au</A></ADDRESS>\n\n";

    print "</FONT>\n</CENTER>\n</BODY>\n</HTML>\n";
}

#----------------------------------------------------------------------------
# Get the current date
#----------------------------------------------------------------------------
sub GetDate 
{
    %DoW = ( 'Sun', 'Sunday', 'Mon', 'Monday', 'Tue', 'Tuesday',
            'Wed', 'Wednesday', 'Thu', 'Thursday', 'Fri', 'Friday',
            'Sat', 'Saturday');
    %MoY = ( 'Jan', 'January', 'Feb', 'February', 'Mar', 'March',
            'Apr', 'April', 'May', 'May', 'Jun', 'June', 'Jul', 'July',
            'Aug', 'August', 'Sep', 'September', 'Oct', 'October',
            'Nov', 'November', 'Dec', 'December');
 
 
    @dt = split( /[ \t]/, `date` );
    $date = $MoY{$dt[1]}." ".$dt[3].", ".$dt[6];
 
    chop( $date );              # Remove the line feed at the end of the date
}
