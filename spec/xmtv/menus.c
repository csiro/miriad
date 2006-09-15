/*
	<menus.c> - code for widgets with spring-loaded menus.

	03jun92 jm  Original code.
*/
#include "xmtv.h"

#include <X11/Shell.h>
#include <X11/Xaw/Box.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Toggle.h>

/* Private variables. */

typedef struct {
  int maxMenuItems;
  Widget toggle;
} MenuStruct;

static Widget LutParent = (Widget)NULL;
static Widget ChanParent = (Widget)NULL;

/* Source code. */

/************************************************************************/
/* ARGSUSED */
static void nextMenu(w, client_data, call_data)
Widget w;                /* Unused. */
XtPointer client_data;
XtPointer call_data;     /* Unused. */
{
    int menuIndex;
    MenuStruct *ms;

    ms = (MenuStruct *)client_data;
    menuIndex = (int)XawToggleGetCurrent(ms->toggle);
    menuIndex = (menuIndex % ms->maxMenuItems) + 1;
    XawToggleSetCurrent(ms->toggle, (XtPointer)menuIndex);

    return;
}

/************************************************************************/
/* ARGSUSED */
static void menuButtonPushed(w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;     /* Unused. */
{
    char label[100];
    int j;
    Arg args[10];
    Cardinal i;
    String name;
    Widget button, shell;

    i = 0;
    XtSetArg(args[i], XtNradioData, (XtArgVal)&j);  i++;
    XtGetValues(w, args, i);

    if (j != (int)XawToggleGetCurrent(w)) return;

    /*
     *  If we get this far, then this widget is the menu item
     *  that was set; not all the ones that were unset.
     */

    name = XtName(w);
    button = (Widget)client_data;
    (void)strcpy(label, (char *)name);

    if (strcmp(XtName(button), "Luts") == 0) {
      changeOFM(name);
    } else if (strcmp(XtName(button), "Channels") == 0) {
      j = atoi((char *)name);
      newChannel(j, False);   /* False: don't issue a resetMenu() call. */
      (void)sprintf(label, "Channel %d", j);
    }

    i = 0;
    XtSetArg(args[i], XtNlabel, (XtArgVal)label);  i++;
    XtSetValues(button, args, i);

    /*
     *  If this callback was made due to a call to nextMenu(),
     *  then the popup was never raised.  However, I think that
     *  XtPopdown() does nothing if the shell widget is not
     *  currently pop'd up so we should be okay.
     */
    shell = XtParent(XtParent(w)); /* shell<-menuBox<-w */
    XtPopdown(shell);

    return;
}

/************************************************************************/
/* ARGSUSED */
static void placeMenu(w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;     /* Unused. */
{
    Arg args[10];
    Cardinal i;
    Dimension width, height;
    Position x, y;
    Widget button;

    button = (Widget)client_data;
    XtTranslateCoords(button, (Position)0, (Position)0, &x, &y);

    i = 0;
    XtSetArg(args[i], XtNwidth,  (XtArgVal)&width);    i++;
    XtSetArg(args[i], XtNheight, (XtArgVal)&height);   i++;
    XtGetValues(button, args, i);

    i = 0;
    XtSetArg(args[i], XtNx, (XtArgVal)(x + width/2));  i++;
    XtSetArg(args[i], XtNy, (XtArgVal)(y + height/2)); i++;
    XtSetValues(w, args, i);

    return;
}

/************************************************************************/
static Widget CreateMenuList(name, list, listsize, parent)
String name;
String list[];
int listsize;
Widget parent;
{
    char trans[BUFSIZ];
    register int j;
    Arg args[10];
    Cardinal i;
    Widget popup, box, entry, toggle;

    static String PopTrans = "#override \n\
                              <Btn3Up>: XtMenuPopdown(%s)";

    i = 0;
    popup = XtCreatePopupShell(name, transientShellWidgetClass, parent,
            args, i);
    XtAddCallback(popup, XtNpopupCallback, placeMenu, (XtPointer)parent);
    (void)sprintf(trans, PopTrans, name);
    XtOverrideTranslations(popup, XtParseTranslationTable(trans));

    i = 0;
    box = XtCreateManagedWidget("menuBox", boxWidgetClass, popup, args, i);

    toggle = (Widget)NULL;
    for (j = 0; j < listsize; j++) {
      i = 0;
      if (j == 0) {
        XtSetArg(args[i], XtNstate,     (XtArgVal)True);   i++;
      }
      XtSetArg(args[i], XtNradioData,   (XtArgVal)(j+1));  i++;
      XtSetArg(args[i], XtNradioGroup,  (XtArgVal)toggle); i++;
      entry = XtCreateManagedWidget(list[j], toggleWidgetClass, box, args, i);
      XtAddCallback(entry, XtNcallback, menuButtonPushed, (XtPointer)parent);
      if (j == 0) toggle = entry;
    }

    return(toggle);
}

/************************************************************************/
void resetMenus(lutValue, chanValue)
int lutValue, chanValue;
{
    Widget toggle;

    if ((lutValue > 0) && (LutParent != (Widget)NULL)) {
      toggle = XtNameToWidget(LutParent, ".Luts.lutmenu.menuBox.IRAF");
      if (toggle != (Widget)NULL)
        XawToggleSetCurrent(toggle, (XtPointer)lutValue);
    }

    if ((chanValue > 0) && (ChanParent != (Widget)NULL)) {
      toggle = XtNameToWidget(ChanParent, ".Channels.channelmenu.menuBox.1");
      if (toggle != (Widget)NULL)
        XawToggleSetCurrent(toggle, (XtPointer)chanValue);
    }
}

/************************************************************************/
Widget CreateLUTMenu(parent)
Widget parent;
{
    Arg args[10];
    Cardinal i;
    Widget btn;
    static MenuStruct msLut;
    static String Lutlist[] = {"B&W", "Colour", "IRAF",
                              "-B&W", "-Colour", "-IRAF"};
    static String PopTrans = "#override \n\
                              <Btn3Down>: XtMenuPopup(lutmenu)";

    LutParent = parent; /* Set global parameter. */

    i = 0;
    btn = XtCreateManagedWidget("Luts", commandWidgetClass, parent, args, i);
    XtOverrideTranslations(btn, XtParseTranslationTable(PopTrans));

    msLut.maxMenuItems = XtNumber(Lutlist);
    msLut.toggle = CreateMenuList("lutmenu", Lutlist, msLut.maxMenuItems, btn);
    XtAddCallback(btn, XtNcallback, nextMenu, (XtPointer)&msLut);

    return(btn);
}

/************************************************************************/
Widget CreateChannelMenu(parent, maxgrey)
Widget parent;
int maxgrey;
{
    Arg args[10];
    Cardinal i;
    Widget button;
    static MenuStruct msChan;
    static String Chanlist[] = {"1", "2", "3", "4", "5",
                                "6", "7", "8", "9", "10"};
    static String PopTrans = "#override \n\
                              <Btn3Down>: XtMenuPopup(channelmenu)";

    ChanParent = parent; /* Set global parameter. */

    i = 0;
    button = XtCreateManagedWidget("Channels", commandWidgetClass, parent,
             args, i);
    XtOverrideTranslations(button, XtParseTranslationTable(PopTrans));

    if (maxgrey > XtNumber(Chanlist)) {
      maxgrey = XtNumber(Chanlist);
      (void)fprintf(stderr, "Too many channels for static code.\n");
      (void)fprintf(stderr, "Menu size limited to %d channels!\n", maxgrey);
    }
    msChan.maxMenuItems = maxgrey;
    msChan.toggle = CreateMenuList("channelmenu", Chanlist, maxgrey, button);
    XtAddCallback(button, XtNcallback, nextMenu, (XtPointer)&msChan);

    return(button);
}
