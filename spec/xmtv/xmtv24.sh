#!/bin/sh
#
# xmtv24.sh
# Get xmtv to display on a 24-bit display, via a VNC session.
#
# $Source: /var/tmp/RrsvEF/cvsroot/miriad-dist/RCS/spec/xmtv/xmtv24.sh,v $
# $Id: xmtv24.sh,v 1.2 2009/05/31 07:39:42 mci156 Exp $
#
# Config ------------------------------------------------------------------------
GEOMETRY=1024x768
PATH=/usr/local/bin:/usr/bin:/bin;
XVNC_STARTUP=${HOME}/.vnc/xstartup
XVNC_PASSWD=${HOME}/.vnc/passwd
MIRIAD=/nfs/atapplic/miriad
# Tell the 'Xrealvnc' X server to use a PseudoColor visual. This is crucial.
# See Xrealvnc(1)
XVNC_OPTS="-cc 3" 
#
# Functions --------------------------------------------------------------------
export PATH; unalias -a

Fail() { while test $# -gt 0; do echo $1; shift; done; exit 1; };
ExitTrap() {
    if test -d "$TMPDIR" ; then
        case "$TMPDIR" in
            /tmp/*) /bin/rm -rf "$TMPDIR"
            ;;
        esac
    fi
    if test -h "$XVNC_STARTUP" ; then
        # remove the symlink
	rm "$XVNC_STARTUP"
        if test -f "$BACKUP_XVNC_STARTUP" ; then
            mv "$BACKUP_XVNC_STARTUP" "$XVNC_STARTUP" || \
              echo "Whoops, something happened when restoring $XVNC_STARTUP"
        fi
    fi
    if test -h "$XVNC_PASSWD" ; then
        # remove the symlink
	rm "$XVNC_PASSWD"
        if test -f "$BACKUP_XVNC_PASSWD" ; then
            mv "$BACKUP_XVNC_PASSWD" "$XVNC_PASSWD" || \
              echo "Whoops, something happened when restoring $XVNC_PASSWD"
        fi
    fi

    # Experience has shown we need to watch out for xauth problems.
    # Somehow the Xauthority file can get zapped.
    # Try to at least add the interactive display back.
    c=`xauth list "$DISPLAY" 2>/dev/null`
    test "X" = "X${c}" && xauth add "$DISPLAY" . `mcookie`

    trap - 0 1 2 3 15
} #ExitTrap
#
# Main -------------------------------------------------------------------------
trap 'ExitTrap;' 0 1 2 3 15
# sanity
for v in vncviewer vncserver twm mktemp
do
    x=`which $v 2>/dev/null`

    message="No '$v' program, please install it"
    case $v in
       *vnc*) message="You're missing the VNC program '$v', please install it"
       ;;
       *wm*)  message="You don't have the '$v' window manager, please install it"
       ;;
    esac
    test "X" = "X${x}" && Fail "$message"
    test -x "$x"       || Fail "$message"
done

if test -f "$MIRIAD/MIRRC.sh" ; then
    . $MIRIAD/MIRRC.sh; PATH=${PATH}:${MIRBIN}; export PATH;
else
    Fail "Can't find miriad setup file, tried '$MIRIAD/MIRRC.sh'"
fi

TMPDIR=`mktemp -d /tmp/XXXXXX`
chmod 0700 "$TMPDIR"
passwdfile="$TMPDIR/vncp"
xstartupfile="$TMPDIR/xstartup"

echo "Please set your session password"
vncpasswd  "$passwdfile"

# Write the startup file
timestamp=`date`
echo "#!/bin/sh"                                          >  "$xstartupfile"
echo "# written by $0 at $timestamp"                      >> "$xstartupfile"
echo "xsetroot -solid '#aab0b0'"                          >> "$xstartupfile"
# miriad, xpanel etc should be in $PATH already
echo "xterm -g 80x24+10+50 -e miriad -n miriad  &"        >> "$xstartupfile"
echo "xmtv -geom -10+10&"                                 >> "$xstartupfile"
echo "xpanel&"                                            >> "$xstartupfile"
# the xmessage window needs to be on top
echo "sleep 1"                                            >> "$xstartupfile"
echo "xmessage -center 'Welcome to your XMTV session' &"  >> "$xstartupfile"
echo "twm&"                                               >> "$xstartupfile"

chmod 0700 "$xstartupfile"


# vncserver has the X startup file and password file locations hardwired.
#   FIXME actually, it's just the passwd file.
# Temporarily replace them with the ones we just wrote.
idstr="xmtvsession.`date +%H%M%S`.$$"
BACKUP_XVNC_STARTUP="${XVNC_STARTUP}.${idstr}"
if test -f "$XVNC_STARTUP" ; then
    mv "$XVNC_STARTUP" "$BACKUP_XVNC_STARTUP" || \
        Fail "Unable to back up existing VNC session startup ($XVNC_STARTUP)"
fi
ln -s "$xstartupfile" "$XVNC_STARTUP"

BACKUP_XVNC_PASSWD="${XVNC_PASSWD}.${idstr}"
if test -f "$XVNC_PASSWD" ; then
    mv "$XVNC_PASSWD" "$BACKUP_XVNC_PASSWD" || \
        Fail "Unable to back up existing VNC password file ($XVNC_PASSWD)"
fi
ln -s "$passwdfile" "$XVNC_PASSWD"

echo ""
echo "Starting vnc server"
out=`vncserver -depth 8 -geometry $GEOMETRY \
               -name "$idstr" -startup "$XVNC_STARTUP" \
               $XVNC_OPTS 2>&1 | grep desktop` || \
    Fail "Failed. Quitting now."

sessionid=`echo $out | grep "$idstr" | \
               awk '$NF ~ /:/{print $NF;exit}' | awk -F: '{print $2}'`
test "X" = "X$sessionid" && \
    Fail "Unable to determine the session id number, quitting."

echo ""
echo "Starting vnc client (you will be prompted for your password again)"
vncviewer -owncmap -passwd "$passwdfile" ":${sessionid}"

# If xmtv dies with:
#  XMTV: Version 1.2 19-dec-95
#  Screen width and height: 1024X768,  the maximum grey level is 199.
#  XMTV: WARNING -- Creating a virtual colormap.
#  X Error of failed request:  BadMatch (invalid parameter attributes)
#    Major opcode of failed request:  78 (X_CreateColormap)
#    Serial number of failed request:  170
#    Current serial number in output stream:  171
# 
# This is caused by the display not being a PseudoColor visual.
# This is why the magical "-cc 3" argument is needed.

echo ""
echo "Cleaning up..."
vncserver -kill ":${sessionid}"


#eof
