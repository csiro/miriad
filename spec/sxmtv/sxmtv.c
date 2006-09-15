/*= sxmtv - Simple version of XMTV      .                          */
/*& jm                                                             */
/*: visual display                                                 */
/*+      SXMTV is MIRIAD's X-Window Screen Server.  It is available
         only when running X-windows, and must be invoked separately
         before being used.  To invoke SXMTV, enter "xmtv &" in any
         X-window.  If you are running this from a remote machine,
         remember to set the environment variable DISPLAY before
         running sxmtv so it will display on the proper screen.

         To use SMTV with MIRIAD's TV routines, (1) invoke SXMTV in
         the manner described above, and (2) specify the server
         correctly in the TV routine.  For example, if you are using
         X-windows on machine "saturn", you might do the following:
         (1) enter "xmtv &" to invoke SXMTV; (2) perhaps remotely log
         onto another machine (eg, the Cray) and run the MIRIAD task
         TVDISP, specifying "server=xmtv@saturn".
                                                                   */
/*--*/

/*
 *  History:
 *  jm   04may92 Modified original X-AIPS code for Miriad.
 *  jm   06oct92 Modified accept calls to cast second argument correctly.
 *               Also modified XLookupString to correctly cast event argument.
 */

#include "sxmtv.h"
#include <sys/types.h>
#include <sys/time.h>
#ifdef AIX
#include <sys/select.h>
#endif

static Bool connected = False;

main (argc, argv)
int argc;
char *argv[];
{
    XEvent report;
    KeySym key;
    char CharBuf[4];
    XComposeStatus compose;
    int sx, sy, w, h, exposed;
#if BSD
    int select_size;
    int XLink, len;
    struct sockaddr_un from_un;
    struct sockaddr_in from_in;
    fd_set fdmask;
#endif
#if VMS
    int status;
#endif

    /* Init global variables */
    exposed = False;
    init();

    /* Connect to X server, get window ID and generate gray scale */
    SetupWindow (argc, argv);

#if BSD
    /* Create communications link. */
    if (MakeLink() < 0) {
        (void) shutdown (MirSocket, 0);
        (void) close (MirSocket);
        (void) closedown();
        exit(-1);
       }

    /* Get file descriptor of X server socket */
    XLink = ConnectionNumber (display);
    select_size = getdtablesize();
#endif

#if VMS
    /* Create communications link. */
    Makelink();
    write_in_progress = 0;
    read_in_progress = 0;

    /* wake up about once every 1/4 second */
    status = sys(&dtime,&delta);
    if (!(status & SS)) lib(status);
    status = sys(0,0,&delta,&delta);
    if (!(status & SS)) lib(status);

    /* Start the IO */
    ReadLink (MirLink);
#endif

                                        /* display window             */
    XMapWindow (display, win);

    /*
     * Event loop, first Expose displays blank window;
     * Press ESC to exit.
     */
    while (1) {

#if VMS
        sys(0);
        while (XCheckWindowEvent (display, win, emask, &report))
        {
#endif

#if BSD
         if (XPending (display)) {
            XNextEvent (display, &report);
            }
         else {
            if (connected) {
                FD_ZERO (&fdmask);
                FD_SET (MirLink, &fdmask);
                FD_SET (XLink, &fdmask);
                /* select for an X event or I/O */
                if (select (select_size, &fdmask, (fd_set *)NULL,
                   (fd_set *)NULL, (struct timeval *)0) < 0) {
                   perror ("select error - MirLink");
                   continue;
                   }
                if (FD_ISSET (XLink, &fdmask)) {
                   XNextEvent (display, &report);
                   }
                else {
                   ReadLink (MirLink, &xbuf, &ybuf);
                   if (ybuf.status != OK) xbuf.opcode = CLOSE;
                   ProcessMirRequest();
                   if (connected)
                      WriteLink (MirLink, &xbuf, &ybuf);
                   continue;
                   }
               }
            else {  /* not connected */
                FD_ZERO (&fdmask);
                FD_SET (MirSocket, &fdmask);
                FD_SET (XLink, &fdmask);
                /* select for an X event or connect request */
                if (select (select_size, &fdmask, (fd_set *)NULL,
                    (fd_set *)NULL, (struct timeval *)0) < 0) {
                    perror ("select error - MirSocket");
                    continue;
                   }
                if (FD_ISSET (XLink, &fdmask)) {
                   XNextEvent(display, &report);
                   }
                else {                  /* Connect to a task    */
                   if (domain_type == INET_DOMAIN ) {
                      len = sizeof(from_in);
                      MirLink = accept (MirSocket,
                        (struct sockaddr *)&from_in, &len);
                      if (MirLink < 0) {
                         perror ("Accept");
                         connected = False;
                         }
                      else {
                         connected = True;
                         }
                      }
                   else {               /* UNIX_DOMAIN                */
                      len = sizeof(from_un);
                      MirLink = accept(MirSocket,
                        (struct sockaddr *)&from_un, &len);
                      if (MirLink < 0) {
                         perror ("Accept");
                         connected = False;
                         }
                      else {
                         connected = True;
                         }
                      }
                   continue;
                   }
               }
           }
#endif
           if (XDebug)
               fprintf (stderr,"  %s\n",event_names[report.type]);
           switch (report.type) {
               case Expose:
                   while (XCheckTypedEvent (display, Expose, &report));
                   if (SXMTVDebug) fprintf (stderr,
                      "expose event w,h,x,y %d %d %d %d\n",
                      Cur_Xsize, Cur_Ysize, Cur_Xzero, Cur_Yzero);
                                        /* first time force it       */
                   if (!exposed) XResizeWindow (display, win,
                      Cur_Xsize, Cur_Ysize);
                   exposed = True;
                                        /* repaint the picture       */
                   scrwrt (0, 0, Screen_Width - 1, Screen_Height - 1);
                   break;
               case ButtonPress:
                   RecordCursor (report.xbutton.x, report.xbutton.y);
                   break;
               case MotionNotify:
                   RecordCursor (report.xmotion.x, report.xmotion.y);
                   break;
               case KeyPress:
                   (void) XLookupString (&(report.xkey), CharBuf,
                       sizeof(CharBuf), &key, &compose );
                   if (CheckKey(key) < 0)
                     closedown(MirLink);
                   break;
               case ConfigureNotify:
                   if (exposed) {
                      Cur_Xsize = report.xconfigure.width;
                      Cur_Ysize = report.xconfigure.height;
                      if ((report.xconfigure.x != 0) ||
                         (report.xconfigure.y != 0)) {
                         Cur_Xzero = report.xconfigure.x;
                         Cur_Yzero = report.xconfigure.y;
                         }
                      }
                   if (SXMTVDebug) fprintf (stderr,
                      "configure event w,h,x,y %d %d %d %d\n",
                      Cur_Xsize, Cur_Ysize, Cur_Xzero, Cur_Yzero);
                   (void) resize_canvas (Cur_Xsize, Cur_Ysize,
                       Cur_Xzero, Cur_Yzero);
                   break;
               default:
                   /* all events selected by StructureNotifyMask
                    * except ConfigureNotify are thrown away here,
                    * since nothing is done with them                */
                   break;
              } /* end switch */
#if VMS
           } /* end while */
        sys(1);  /* enable AST's while hibernating */
        sys();  /* go to sleep, wait for wakeup */
#endif
      } /* end while */
} /* end main */
/*-------------------------------------------------------------------*/
ProcessMirRequest()
{

    ybuf.status = 0;
    ybuf.return_data_length = 0;
    if ((XDebug) && (xbuf.opcode != IMWRT) && (xbuf.opcode != RCURS)
       && (xbuf.opcode != RCURB)) {
       fprintf (stderr, "%s\n",opcodes[xbuf.opcode]);
       printbufin ();
       }
   switch (xbuf.opcode) {
      case OPEN:
         if (SXMTVDebug) fprintf (stderr, "SXMTV Open \n");
         ybuf.status = 0;
         ybuf.return_data_length = 0;
         break;
      case CLOSE:
         if (SXMTVDebug) fprintf (stderr, "SXMTV Close now \n");
#if BSD
         shutdown (MirLink, 0);
         close (MirLink);
#endif
         connected = False;
         break;
      case INTGT:
         if (SXMTVDebug) fprintf (stderr, "SXMTV parameters requested");
         ybuf.status = Interogate (&ybuf.return_data_length);
         if (SXMTVDebug) fprintf (stderr, " done\n");
         break;
      case CLEAR:                       /* init */
         if (SXMTVDebug)
            fprintf (stderr, "CLEAR %d", xbuf.parms[0]);
         ybuf.status = ClearChan();
         ybuf.return_data_length = 0;
         if (SXMTVDebug) fprintf (stderr, " done\n");
         break;
      case IMWRT:
         ybuf.status = imwrt();
         ybuf.return_data_length = 0;
         break;
      case IMRD:
         ybuf.status = imrd(&ybuf.return_data_length);
         break;
      case WLUT:
         if (SXMTVDebug) fprintf (stderr, "WLUT %d", xbuf.parms[0]);
         ybuf.status = cmap_wlut();
         ybuf.return_data_length = 0;
         if (SXMTVDebug) fprintf (stderr, " done\n");
         break;
      case RLUT:
         if (SXMTVDebug) fprintf (stderr, "RLUT %d", xbuf.parms[0]);
         ybuf.status = cmap_rlut();
         ybuf.return_data_length = NColour;
         if (SXMTVDebug) fprintf (stderr, " done\n");
         break;
      case WOFM:
         if (SXMTVDebug) fprintf (stderr, "WOFM %d", xbuf.parms[0]);
         ybuf.status = cmap_wofm();
         ybuf.return_data_length = 0;
         if (SXMTVDebug) fprintf (stderr, " done\n");
         break;
      case ROFM:
         if (SXMTVDebug) fprintf (stderr, "ROFM %d", xbuf.parms[0]);
         ybuf.status = cmap_rofm();
         ybuf.return_data_length = NINTENS;
         if (SXMTVDebug) fprintf (stderr, " done\n");
         break;
      case GRAPH:
         if (SXMTVDebug) fprintf (stderr, "GRAPH %d %d",
            xbuf.parms[0],xbuf.parms[1]);
         ybuf.status = cmap_graph();
         ybuf.return_data_length = 0;
         if (SXMTVDebug) fprintf (stderr, " done\n");
         break;
      case SPLIT:
         if (SXMTVDebug) fprintf (stderr, "SPLIT %d", xbuf.parms[0]);
         ybuf.status = cmap_split();
         ybuf.return_data_length = 0;
         if (SXMTVDebug) fprintf (stderr, " done\n");
         break;
      case WGRFX:
         if (SXMTVDebug) fprintf (stderr, "WGRFX %d", xbuf.parms[0]);
         ybuf.status = cmap_wgrfx();
         ybuf.return_data_length = 0;
         if (SXMTVDebug) fprintf (stderr, " done\n");
         break;
      case RGRFX:
         if (SXMTVDebug) fprintf (stderr, "RGRFX %d", xbuf.parms[0]);
         ybuf.status = cmap_rgrfx();
         ybuf.return_data_length = 3;
         if (SXMTVDebug) fprintf (stderr, " done\n");
         break;
      case RCURS:
         ybuf.status = GetCursor(&ybuf.data[0], &ybuf.data[1]);
         ybuf.return_data_length = 2;
         break;
      case RBUTT:
         ybuf.status = readbuttons();
         ybuf.return_data_length = 4;
         break;
      case WCURS:
         if (SXMTVDebug) fprintf (stderr, "WCURS %d %d",
            xbuf.parms[0], xbuf.parms[1]);
         ybuf.status = movecursor();
         ybuf.return_data_length = 0;
         if (SXMTVDebug) fprintf (stderr, " done\n");
         break;
      case RCURB:
         ybuf.status = cursor_button();
         ybuf.return_data_length = 6;
         break;
      case WZSCR:
         if (SXMTVDebug) fprintf (stderr, "ZOOM %d %d %d %d",
            xbuf.parms[0], xbuf.parms[1], xbuf.parms[2], xbuf.parms[3]);
         ybuf.status = zoom();
         ybuf.return_data_length = 0;
         if (SXMTVDebug) fprintf (stderr, " done\n");
         break;
      case RZSCR:
         if (SXMTVDebug) fprintf (stderr, "SXMTV zoom parameters requested");
         ybuf.status = read_zoom(&ybuf.return_data_length);
         if (SXMTVDebug) fprintf (stderr, " done\n");
         break;
      case WINDO:
         if (SXMTVDebug) fprintf (stderr, "WINDO %d %d %d %d\n",
            xbuf.parms[0], xbuf.parms[1], xbuf.parms[2], xbuf.parms[3]);
         ybuf.status = windo_status();
         ybuf.return_data_length = 4;
         if (SXMTVDebug) fprintf (stderr, " done\n");
         break;
      case SLCTP:
         if (SXMTVDebug) fprintf (stderr, "SLCTP inputs: %d %d %d\n",
            xbuf.parms[0], xbuf.parms[1], xbuf.parms[2]);
         ybuf.status = selectPoints(&ybuf.return_data_length);
         if (SXMTVDebug) fprintf (stderr, "SLCTP output: %d %d %d %d\n",
            ybuf.data[0], ybuf.data[1], ybuf.data[2], ybuf.data[3]);
         break;
      case SCALE:
         if (SXMTVDebug) fprintf (stderr, "SCALE %d %d %d %d",
            xbuf.parms[0], xbuf.parms[1], xbuf.parms[2], xbuf.parms[3]);
         ybuf.status = 0; /* Not implemented yet! */
         ybuf.return_data_length = 0;
         if (SXMTVDebug) fprintf (stderr, " done\n");
         break;
      default:
         fprintf (stderr, "SXMTV: opcode %d not yet implemented.\n",
             xbuf.opcode);
         ybuf.status = -1;
         ybuf.return_data_length = 0;
         break;
         } /* end switch */

}
