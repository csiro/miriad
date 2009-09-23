#!/bin/sh
#
# xmtv24.sh
# Get xmtv to display on a 24-bit display, via a VNC session.
#
# $Source: /var/tmp/RrsvEF/cvsroot/miriad-dist/RCS/spec/xmtv/xmtv24.sh,v $
# $Id: xmtv24.sh,v 1.5 2009/09/23 00:48:41 mci156 Exp $
#
# Config ------------------------------------------------------------------------
GEOMETRY=1024x768
PATH=/usr/local/bin:/usr/bin:/bin;
XVNC_RC="${HOME}/.vncrc"
XVNC_PASSWD="${HOME}/.vnc/passwd"
XVNC_XSTARTUP="${HOME}/.vnc/xstartup"

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

    echo "Restoring your vnc config files..."
    if test -h "$XVNC_PASSWD" ; then
        # remove the symlink
	rm "$XVNC_PASSWD"
        if test -f "$BACKUP_XVNC_PASSWD" ; then
            mv "$BACKUP_XVNC_PASSWD" "$XVNC_PASSWD" || \
              echo "Whoops, something happened when restoring $XVNC_PASSWD"
        fi
    fi
    if test -f "$BACKUP_XVNC_PASSWD" && test -f "$XVNC_PASSWD" ; then
        # replace the file
	rm "$XVNC_PASSWD"
        mv "$BACKUP_XVNC_PASSWD" "$XVNC_PASSWD" || \
              echo "Whoops, something happened when restoring $XVNC_PASSWD"
    fi
    if test -h "$XVNC_RC" ; then
        # remove the symlink
	rm "$XVNC_RC"
        if test -f "$BACKUP_XVNC_RC" ; then
            mv "$BACKUP_XVNC_RC" "$XVNC_RC" || \
              echo "Whoops, something happened when restoring $XVNC_RC"
        fi
    fi
    if test -h "$XVNC_XSTARTUP" ; then
        # remove the symlink
	rm "$XVNC_XSTARTUP"
        if test -f "$BACKUP_XVNC_XSTARTUP" ; then
            mv "$BACKUP_XVNC_XSTARTUP" "$XVNC_XSTARTUP" || \
              echo "Whoops, something happened when restoring $XVNC_XSTARTUP"
        fi
    fi

    # Experience has shown we need to watch out for xauth problems.
    # Somehow the Xauthority file can get zapped.
    # Try to at least add the interactive display back.
    c=`xauth list "$DISPLAY" 2>/dev/null`
    test "X" = "X${c}" && xauth add "$DISPLAY" . `mcookie`

    echo "Done"

    trap - 0 1 2 3 15
} #ExitTrap
#
# Main -------------------------------------------------------------------------
trap 'ExitTrap;' 0 1 2 3 15

# sanity
[ "X" = "X${MIR}" ] && Fail "No 'MIR' variable in your environment - do you have Miriad installed?"
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

if test -f "$MIR/MIRRC.sh" ; then
    . $MIR/MIRRC.sh; PATH=${PATH}:${MIRBIN}; export PATH;
else
    Fail "Can't find miriad setup file, tried '$MIR/MIRRC.sh'"
fi

TMPDIR=`mktemp -d /tmp/XXXXXX`
chmod 0700 "$TMPDIR"
rcfile="$TMPDIR/vncrc"
passwdfile="$TMPDIR/vncp"
xstartupfile="$TMPDIR/xstartup"
messagefile="$TMPDIR/message"

timestamp=`date`

# prepare the config files
# .vncrc
cat >"$rcfile" <<EOF
# temporary ~/.vncrc file
# written by $0 at $timestamp
\$vncStartup = "$xstartupfile";

EOF
chmod 0600 "$rcfile"

# .vnc/passwd
echo "Please set your session password - it will only apply to this session."
vncpasswd  "$passwdfile"

# .vnc/xstartup
cat >$xstartupfile <<EOF
#!/bin/sh
# written by $0 at $timestamp
xsetroot -solid '#aab0b0'
xterm -g 80x24+10+50 -e miriad -n miriad  &
xmtv -geom -10+10&
xpanel&
# the xmessage window needs to be on top, start it last.
sleep 1
xmessage -center -file $messagefile &
twm&

EOF

chmod 0700 "$xstartupfile"

cat >"$messagefile" <<EOF
Welcome to your XMTV session.

When you close the VNC window, the session will exit - 
you will not be able to reconnect to it.
EOF


# Temporarily replace the config files with the ones we just wrote.
idstr="xmtvsession.`date +%H%M%S`.$$"

BACKUP_XVNC_PASSWD="${XVNC_PASSWD}.${idstr}"
if test -f "$XVNC_PASSWD" ; then
    mv "$XVNC_PASSWD" "$BACKUP_XVNC_PASSWD" || \
        Fail "Unable to back up existing VNC password file ($XVNC_PASSWD)"
fi
# tightvncserver complains if the 'vncpasswd file' is a symbolic link.
# xvnc4viewer does not support the -passwd option.
cp -p "$passwdfile" "$XVNC_PASSWD"

# vnc4server allows you to specify the startup file location with -startup.
# tightvnc does not. Instead it expects to read the location in a ~/.vncrc file.
# symbolic links seem to be allowed.
BACKUP_XVNC_RC="${XVNC_RC}.${idstr}"
if test -f "$XVNC_RC" ; then
    mv "$XVNC_RC" "$BACKUP_XVNC_RC" || \
        Fail "Unable to back up existing VNC config file ($XVNC_RC)"
fi
ln -s "$rcfile" "$XVNC_RC"

# vnc4server does not look for the .vncrc file, so we replace
# the one in the standard location with a temporary one.
# symbolic links appear to be allowed.
BACKUP_XVNC_XSTARTUP="${XVNC_XSTARTUP}.${idstr}"
if test -f "$XVNC_XSTARTUP" ; then
    mv "$XVNC_XSTARTUP" "$BACKUP_XVNC_XSTARTUP" || \
        Fail "Unable to back up existing VNC xstartup file ($XVNC_XSTARTUP)"
fi
ln -s "$xstartupfile" "$XVNC_XSTARTUP"


# finally, start the server
echo ""
echo "Starting vnc server"
out=`vncserver -depth 8 -geometry $GEOMETRY \
               -name "$idstr" \
               $XVNC_OPTS 2>&1 | grep desktop` || \
    Fail "Failed. Quitting now."

sessionid=`echo $out | grep "$idstr" | \
               awk '$NF ~ /:/{print $NF;exit}' | awk -F: '{print $2}'`
test "X" = "X$sessionid" && \
    Fail "Unable to determine the session id number, quitting."


# and start a client connected to it
echo ""
echo "Starting vnc client"
echo ""
echo "NB: *** This session will exit when you close the window ***"
echo ""
# xtightvncviewer supports the useful -owncmap option.
# But xvnc4viewer does not. So do without and hope we have enough colours.
xtightvncviewer  ":${sessionid}"

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
