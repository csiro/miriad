/*
 *	<process.c> Branch routine.
 *	History:
 *	  jm   04may92 Modified original XAS code for Miriad.
 */

#include "xmtv.h"

/************************************************************************/
static void printbufin()
{
    register int i, j, limit;

    (void)fprintf(stderr,  "parms[]= %4d %4d %4d %4d\n", xbuf.parms[0],
        xbuf.parms[1], xbuf.parms[2], xbuf.parms[3]);
    (void)fprintf(stderr, "data_length= %d\n", xbuf.data_length);
    limit = 16;                                  /* xbuf.data_length; */
    for (i = 0; i < limit; i += 16) {
      for (j = i; (j < i+16) && (j < limit); j++)
        (void)fprintf(stderr, " %4d", xbuf.data[j]);
      (void)fprintf(stderr, "\n");
    }
}

/************************************************************************/
static void printbufout()
{
    register int i, j, limit;

    (void)fprintf(stderr, "status =  %4d\n", ybuf.status);
    (void)fprintf(stderr, "return_data_length= %d\n", ybuf.return_data_length);
    limit = ybuf.return_data_length;
    for (i = 0; i < limit; i += 16) {
      for (j = i; (j < i+16) && (j < limit); j++)
        (void)fprintf(stderr, " %4d", ybuf.data[j]);
      (void)fprintf(stderr, "\n");
    }
}

/* Returns -1 if it received a CLOSE request; 0 otherwise. */
/************************************************************************/
int ProcessThisRequest()
{
    int returnStatus = 0;

    if ((XDebug) && (xbuf.opcode != IMWRT) && (xbuf.opcode != RCURS) &&
        (xbuf.opcode != RCURB)) {
      (void)fprintf(stderr, "%s\n", opcodes[xbuf.opcode]);
      printbufin();
    }

    switch (xbuf.opcode) {
      case OPEN:
         if (AppDebug) (void)fprintf(stderr, "XMTV Open \n");
         ybuf.status = 0;
         ybuf.return_data_length = 0;
         break;
      case CLOSE:
         if (AppDebug) (void)fprintf(stderr, "XMTV Close now \n");
         ybuf.status = 0;
         ybuf.return_data_length = 0;
         returnStatus = -1; /* ==> connected = False; */
         break;
      case INTGT:
         if (AppDebug) (void)fprintf(stderr, "XMTV parameters requested");
         ybuf.status = Interogate(&ybuf.return_data_length);
         if (AppDebug) (void)fprintf(stderr, " done\n");
         break;
      case CLEAR:
         if (AppDebug) (void)fprintf(stderr, "CLEAR %d", xbuf.parms[0]);
         ybuf.status = clearChannel((int)xbuf.parms[0]);
         ybuf.return_data_length = 0;
         if (AppDebug) (void)fprintf(stderr, " done\n");
         break;
      case IMWRT:
         ybuf.status = imwrt();
         ybuf.return_data_length = 0;
         break;
      case IMRD:
         ybuf.status = imrd(&ybuf.return_data_length);
         break;
      case WLUT:
         if (AppDebug) (void)fprintf(stderr, "WLUT %d", xbuf.parms[0]);
         ybuf.status = cmap_wlut();
         ybuf.return_data_length = 0;
         if (AppDebug) (void)fprintf(stderr, " done\n");
         break;
      case RLUT:
         if (AppDebug) (void)fprintf(stderr, "RLUT %d", xbuf.parms[0]);
         ybuf.status = cmap_rlut();
         ybuf.return_data_length = NColour;
         if (AppDebug) (void)fprintf(stderr, " done\n");
         break;
      case WOFM:
         if (AppDebug) (void)fprintf(stderr, "WOFM %d", xbuf.parms[0]);
         ybuf.status = cmap_wofm();
         ybuf.return_data_length = 0;
         if (AppDebug) (void)fprintf(stderr, " done\n");
         break;
      case ROFM:
         if (AppDebug) (void)fprintf(stderr, "ROFM %d", xbuf.parms[0]);
         ybuf.status = cmap_rofm();
         ybuf.return_data_length = NINTENS;
         if (AppDebug) (void)fprintf(stderr, " done\n");
         break;
      case GRAPH:
         if (AppDebug) (void)fprintf(stderr, "GRAPH %d %d",
            xbuf.parms[0],xbuf.parms[1]);
         ybuf.status = cmap_graph();
         ybuf.return_data_length = 0;
         if (AppDebug) (void)fprintf(stderr, " done\n");
         break;
      case SPLIT:
         if (AppDebug) (void)fprintf(stderr, "SPLIT %d", xbuf.parms[0]);
         ybuf.status = cmap_split();
         ybuf.return_data_length = 0;
         if (AppDebug) (void)fprintf(stderr, " done\n");
         break;
      case WGRFX:
         if (AppDebug) (void)fprintf(stderr, "WGRFX %d", xbuf.parms[0]);
         ybuf.status = cmap_wgrfx();
         ybuf.return_data_length = 0;
         if (AppDebug) (void)fprintf(stderr, " done\n");
         break;
      case RGRFX:
         if (AppDebug) (void)fprintf(stderr, "RGRFX %d", xbuf.parms[0]);
         ybuf.status = cmap_rgrfx();
         ybuf.return_data_length = 3;
         if (AppDebug) (void)fprintf(stderr, " done\n");
         break;
      case RCURS:
         ybuf.status = GetCursor(&ybuf.data[0], &ybuf.data[1]);
         ybuf.return_data_length = 2;
         break;
      case RBUTT:
         ybuf.status = readButtons(ybuf.data);
         ybuf.return_data_length = 4;
         break;
      case WCURS:
         if (AppDebug) (void)fprintf(stderr, "WCURS %d %d",
            xbuf.parms[0], xbuf.parms[1]);
         ybuf.status = movecursor((int)xbuf.parms[0], (int)xbuf.parms[1]);
         ybuf.return_data_length = 0;
         if (AppDebug) (void)fprintf(stderr, " done\n");
         break;
      case RCURB:
         ybuf.status = cursorButton(ybuf.data);
         ybuf.return_data_length = 6;
         break;
      case WZSCR:
         if (AppDebug) (void)fprintf(stderr, "ZOOM %d %d %d %d",
            xbuf.parms[0], xbuf.parms[1], xbuf.parms[2], xbuf.parms[3]);
         ybuf.status = zoom((int)xbuf.parms[0], (int)xbuf.parms[1],
           (int)xbuf.parms[2], (int)xbuf.parms[3]);
         ybuf.return_data_length = 0;
         if (AppDebug) (void)fprintf(stderr, " done\n");
         break;
      case RZSCR:
         if (AppDebug) (void)fprintf(stderr, "XMTV zoom parameters requested");
         ybuf.status = read_zoom(&ybuf.return_data_length);
         if (AppDebug) (void)fprintf(stderr, " done\n");
         break;
      case WINDO:
         if (AppDebug) (void)fprintf(stderr, "WINDO %d %d %d %d\n",
            xbuf.parms[0], xbuf.parms[1], xbuf.parms[2], xbuf.parms[3]);
         ybuf.status = windo_status(XtParent(canvas));
         ybuf.return_data_length = 4;
         if (AppDebug) (void)fprintf(stderr, " done\n");
         break;
      case SLCTP:
         if (AppDebug) (void)fprintf(stderr, "SLCTP inputs: %d %d %d\n",
            xbuf.parms[0], xbuf.parms[1], xbuf.parms[2]);
         ybuf.status = selectPoints(canvas, &ybuf.return_data_length);
         if (AppDebug) (void)fprintf(stderr, "SLCTP output: %d %d %d %d\n",
            ybuf.data[0], ybuf.data[1], ybuf.data[2], ybuf.data[3]);
         break;
      case SCALE:
         if (AppDebug) (void)fprintf(stderr, "SCALE inputs: %d %d %d %d\n",
            xbuf.parms[0], xbuf.parms[1], xbuf.parms[2], xbuf.parms[3]);
         ybuf.status = setScale((int)xbuf.parms[0], (int)xbuf.parms[1],
            (int)xbuf.parms[2], (int)xbuf.parms[3]);
         ybuf.return_data_length = 0;
         if (AppDebug) (void)fprintf(stderr, " done\n");
         break;
      default:
         (void)fprintf(stderr, "XMTV: opcode %d not yet implemented.\n",
             xbuf.opcode);
         ybuf.status = -1;
         ybuf.return_data_length = 0;
         break;
    } /* end switch */

    if ((XDebug) && (xbuf.opcode != IMWRT) && (xbuf.opcode != RCURS) &&
        (xbuf.opcode != RCURB)) {
      (void)fprintf(stderr, "%s\n", opcodes[xbuf.opcode]);
      printbufout();
    }

    return(returnStatus);

} /* ProcessThisRequest */
