
/* -- xmtv.c -- */
/* -- -- static routines -- -- */
/*
 * static void bug();
 * static void privateXtAppMainLoop();
 * static void startTimer();
 * static void initializeSocket();
 * static void readSocket();
 * static void checkSocket();
 * static void ParseIconGeometry();
 * static int initScreen();
 * static XVisualInfo *getVisualList();
 */
extern void CloseDown();

/* -- colors.c -- */
/* -- -- static routines -- -- */
/*
 * static void colorCursor();
 * static void crscol();
 * static void iraf_ofm();
 * static int cmap_change();
 */
extern void initGraphics();
extern int initColors();
extern void freeColors();
extern int newChannel();
extern int lutRamp();
extern int changeOFM();
extern int cmap_wlut();
extern int cmap_rlut();
extern int cmap_wofm();
extern int cmap_rofm();
extern int cmap_graph();
extern int cmap_split();
extern int cmap_wgrfx();
extern int cmap_rgrfx();

/* -- comm.c -- */
extern short int dontohs();
extern int MakeLink();
extern int CheckLink();
extern int ReadLink();
extern int WriteLink();
extern void closeLink();

/* -- cursor.c -- */
extern void RecordCursor();
extern void currentCursor();
extern int GetCursor();
extern int movecursor();
extern void zoomCursor();
extern void buttonPressed();
extern int readButtons();
extern int cursorButton();

/* -- image.c -- */
extern int setScale();
extern float pixelValue();
extern int initImage();
extern void freeImage();
extern int imwrt();
extern int imrd();
extern void resizePorthole();
extern int windo_status();
extern int Interogate();
extern int clearChannel();
extern int zoom();
extern int read_zoom();

/* -- local.c -- */
/* -- -- static routines -- -- */
/*
 * static void privateZoom();
 */
extern void localResize();
extern void resizeCallback();
extern void localZoomDown();
extern void localZoomUp();
extern void localReset();
extern void localPanEvent();

/* -- menus.c -- */
/* -- -- static routines -- -- */
/*
 * static void nextMenu();
 * static void menuButtonPushed();
 * static void placeMenu();
 * static Widget CreateMenuList();
 */
extern void resetMenus();
extern Widget CreateLUTMenu();
extern Widget CreateChannelMenu();

/* -- process.c -- */
/* -- -- static routines -- -- */
/*
 * static void printbufin();
 * static void printbufout();
 */
extern int ProcessThisRequest();

/* -- screen.c -- */
/* -- -- static routines -- -- */
/*
 * static void scrdoit();
 */
extern void initCanvas();
extern void freeCanvas();
extern void scrwrt();
extern void imageRefresh();

/* -- selectpt.c -- */
/* -- -- static routines -- -- */
/*
 * static void drawLine();
 * static void selectDone();
 * static void changeCursor();
 * static void setPosition();
 * static void scaleCursor();
 * static int EventCycle();
 * static void getPoints();
 */
extern int selectPoints();

/* -- widgets.c -- */
/* -- -- static routines -- -- */
/*
 * static void portholeCallback();
 * static void pannerCallback();
 * static void toggleCallback();
 * static void atodCallback();
 * static void quitCallback();
 * static Widget CreatePanner();
 * static Widget BuildControlPanel();
 * static Widget BuildCanvas();
 */
extern Widget BuildWindow();
extern void keyboardPressed();
extern void canvasExpose();
extern void cursorMotionEvent();
extern void buttonEvent();
extern void setSizeOfPanner();
extern void movePanner();
extern void zoomEnable();
extern void toggleReset();
