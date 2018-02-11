/* opensolar.c
 * opensolar displays a planet with a moon, orbiting a sun.
 * To compile:
 * cc -O -o opensolar opensolar.c -lXm -lGLw -lm -lGLU -lGL
 */

#include <Xm/Xm.h>
#include <Xm/Frame.h>
#include <Xm/Form.h>
#include <X11/keysym.h>
#include <X11/StringDefs.h>
#include <GL/GLwMDrawA.h>

#include <GL/gl.h>
#include <GL/glu.h>
#include <GL/glx.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "malloc.h"

typedef struct _spin {
  short year;
} SPINDATA, *SPINPTR;

/* function prototypes */
void main(int argc, char **argv);
void initCB (Widget w, XtPointer client_data,
             XtPointer call_data);
void exposeCB (Widget w, XtPointer spin,
               XtPointer call_data);
void resizeCB (Widget w, XtPointer spin,
               XtPointer call_data);
void inputCB (Widget w, XtPointer client_data,
              XtPointer call_data);
Boolean drawWP (XtPointer spin);
void drawscene(SPINPTR spin);
void setbeachball(int stripes);
void beachball(unsigned long color1, unsigned long color2);

XtAppContext app_context;
XtWorkProcId workprocid = NULL;

GLXContext glx_context;
Display * global_display;
Window global_window;


/* main
 * This program shows a solar system, with a sun, planet, and
 * moon (in OpenGL). The user can exit with the ESCape key
 * or through the window manager menu.
 */
void main(int argc, char **argv)
{
  Arg wargs[15];
  int n;
  Widget glw, toplevel, frame, form;
  SPINPTR spin;
  static String fallback_resources[] = {
    "*frame*shadowType: SHADOW_IN", "*glwidget*width: 750",
    "*glwidget*height: 600", "*glwidget*rgba: TRUE",
    "*glwidget*doublebuffer: TRUE",
    "*glwidget*allocateBackground: TRUE", NULL
  };

  /* create main data structure, spin pointer */
  spin = (SPINPTR) malloc (sizeof (SPINDATA));
  spin->year = 0;
  toplevel = XtAppInitialize(
     &app_context, /* Application context */
     "Opensolar", /* Application class */
     NULL, 0, /* command line option list */
     &argc, argv, /* command line args */
     fallback_resources,
     NULL, /* argument list */
     0); /* number of arguments */

  n = 0;
  form = XmCreateForm(toplevel, "form", wargs, n);
  XtManageChild(form);

  n = 0;
  XtSetArg(wargs[n], XtNx, 30); 
  n++;
  XtSetArg(wargs[n], XtNy, 30); 
  n++;
  XtSetArg(wargs[n], XmNbottomAttachment, XmATTACH_FORM); 
  n++;
  XtSetArg(wargs[n], XmNleftAttachment, XmATTACH_FORM); 
  n++;
  XtSetArg(wargs[n], XmNrightAttachment, XmATTACH_FORM); 
  n++;
  XtSetArg(wargs[n], XmNtopAttachment, XmATTACH_FORM); 
  n++;
  XtSetArg(wargs[n], XmNleftOffset, 30); 
  n++;
  XtSetArg(wargs[n], XmNbottomOffset, 30); 
  n++;
  XtSetArg(wargs[n], XmNrightOffset, 30); 
  n++;
  XtSetArg(wargs[n], XmNtopOffset, 30); 
  n++;
  frame = XmCreateFrame (form, "frame", wargs, n);
  XtManageChild (frame);

  n = 0;
  glw = GLwCreateMDrawingArea(frame, "glwidget", wargs, n);
  XtManageChild (glw);
  XtAddCallback(glw, GLwNginitCallback, initCB,
                (XtPointer) NULL);
  XtAddCallback(glw, GLwNexposeCallback, exposeCB,
                (XtPointer) spin);
  XtAddCallback(glw, GLwNresizeCallback, resizeCB,
                (XtPointer) spin);
  XtAddCallback(glw, GLwNinputCallback, inputCB,
                (XtPointer) NULL);

  XtRealizeWidget(toplevel); /* instantiate it now */
  XtAppMainLoop(app_context); /* loop for events */
} /* end main() */


/* initCB 
 * The initCB subroutine initializes graphics modes and
 * transformation matrices.
 */
void initCB (Widget w, XtPointer client_data,
             XtPointer call_data)
{
  Arg args[1];
  XVisualInfo *vi;

  XtSetArg(args[0], GLwNvisualInfo, &vi);
  XtGetValues(w, args, 1);

  global_display = XtDisplay(w);
  global_window = XtWindow(w);
  glx_context = glXCreateContext(global_display, vi, 0,
                                 GL_FALSE);
} /* end initCB() */


/* exposeCB() and resizeCB() are called when the window
 * is uncovered, moved, or resized.
 */
void exposeCB (Widget w, XtPointer ptr, XtPointer call_data)
{
  SPINPTR spin;
  static char firstTime = 0x1;
  GLwDrawingAreaCallbackStruct *call_ptr;

  call_ptr = (GLwDrawingAreaCallbackStruct *) call_data;
  GLwDrawingAreaMakeCurrent(w, glx_context);
  if (firstTime) {
    glClearColor(0.0, 0.0, 0.0, 0.0);
    glShadeModel (GL_FLAT);
    glEnable(GL_DEPTH_TEST);
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity ();
    gluPerspective(45.0, (GLfloat)(call_ptr->width)
                   /(GLfloat)(call_ptr->height), 1.0, 25.0);
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity ();
    glTranslatef(0.0, 0.0, -12.0);
    workprocid = XtAppAddWorkProc(app_context, drawWP, ptr);
      /* ptr is spin */
    firstTime = 0;
  }
  spin = (SPINPTR) ptr;
  drawscene(spin);
}

void resizeCB (Widget w, XtPointer ptr, XtPointer call_data)
{
  GLwDrawingAreaCallbackStruct *call_ptr;
  SPINPTR spin;

  spin = (SPINPTR) ptr;
  call_ptr = (GLwDrawingAreaCallbackStruct *) call_data;
  GLwDrawingAreaMakeCurrent(w, glx_context);
  glViewport (0, 0, (GLsizei) (call_ptr->width-1),
              (GLsizei) (call_ptr->height-1));
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity ();
  gluPerspective(45.0, (GLfloat)(call_ptr->width) /
                 (GLfloat)(call_ptr->height), 1.0, 25.0);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity ();
  glTranslatef(0.0, 0.0, -12.0);
  drawscene(spin);
}


/* inputCB() handles all types of input from the GL widget.
 * The KeyRelease handles the ESCape key, so that it exits
 * the program. 
 */
void inputCB (Widget w, XtPointer client_data,
              XtPointer call_data)
{
  char buffer[1];
  KeySym keysym;
  GLwDrawingAreaCallbackStruct *call_ptr;
  XKeyEvent *kevent;

  call_ptr = (GLwDrawingAreaCallbackStruct *) call_data;
  kevent = (XKeyEvent *) (call_ptr->event);
  switch(call_ptr->event->type) {
  case KeyRelease:
    /* Must convert the keycode to a keysym before
     * checking if it is an escape
     */
    if (XLookupString(kevent,buffer,1,&keysym,NULL) == 1 
                      && keysym == (KeySym)XK_Escape)
      exit(0);
    break;
  default:
    break;
  }
}


/* drawWP() is called by the WorkProc. When the scene
 * is in automatic motion, the WorkProc calls this routine,
 * which adds 1 degree (10 tenths) to the cumulative amount
 * of rotation. drawscene() is called, so the image is
 * redrawn. It returns(FALSE) so the WorkProc does not
 * discontinue operation.
 */
Boolean drawWP (XtPointer ptr)
{
  SPINPTR spin;

  spin = (SPINPTR) ptr;
  spin->year = (spin->year + 10) % 3600;
  drawscene (spin);
  return (FALSE);
}

/* drawscene
 * drawscene calculates angles relative to the spin->year
 * and then draws sun, planet, and moon.
 */
void drawscene(SPINPTR spin)
{
  short sunangle;
  /* actual dist is 1.5e8 km; mult by 3.0e-8 fudgefactor */
  float earthdist = 4.5;
  short dayangle;
  float earthscale = 0.5;
  short monthangle;
  float moondist = 0.9;
  float moonscale = 0.2;

  glClear(GL_DEPTH_BUFFER_BIT|GL_COLOR_BUFFER_BIT);

  glPushMatrix();
  glRotatef(10.0, 1.0, 0.0, 0.0); /* tilt entire scene */
  glPushMatrix();
  sunangle = (spin->year*365/25) % 3600;
  /* sun rotates on axis every 25 days */
  glRotatef(.1*(sunangle), 0.0, 1.0, 0.0);
  /* cpack format color1, color2 */
  /* swapped by hand: was beachball(0x20C0FF, 0x200FFFF); */
  beachball(0xFFC02000, 0xFFFF0020);
  glPopMatrix();
  glPushMatrix();
  glRotatef(.1*(spin->year), 0.0, 1.0, 0.0);
  glTranslatef(earthdist, 0.0, 0.0);
  glPushMatrix();
  dayangle = (spin->year*50) % 3600;
  /* dayangle fudged so earth rotation can be seen */
  glRotatef(.1*(dayangle), 0.0, 1.0, 0.0);
  glScalef(earthscale, earthscale, earthscale);
  glColor3f(0.0, 0.0, 1.0);
  /* swap by hand; was beachball(0xFF0000, 0xC02000);*/
  beachball(0x0000FF00, 0x0020C000); /* earth */
  glPopMatrix();
  monthangle = (spin->year*365/28) % 3600;
  glRotatef(.1*(monthangle), 0.0, 1.0, 0.0);
  glTranslatef(moondist, 0.0, 0.0);
  glScalef(moonscale, moonscale, moonscale);
  glColor3f(1.0, 1.0, 1.0);
  /* swap by hand; was beachball(0xFFFFFF, 0xC0C0C0); */
  beachball(0xFFFFFF00, 0xC0C0C000); /* moon */
  glPopMatrix();
  glPopMatrix();
  glXSwapBuffers(global_display, global_window);
} /* end drawscene() */


/*
 * BEACHBALL
 */

/* three dimensional vector */
typedef float vector[3];
vector front =  { 0.0, 0.0, 1.0 };
vector back =   { 0.0, 0.0, -1.0 };
vector top =    { 0.0, 1.0, 0.0 };
vector bottom = { 0.0, -1.0, 0.0 };
vector right =  { 1.0, 0.0, 0.0 };
vector left =   { -1.0, 0.0, 0.0 };
vector center = { 0.0, 0.0, 0.0 };

/* Number of colored stripes. Should be even to look right */
#define BEACHBALL_STRIPES 12
/* Default number of polygons making up a stripe. Should */
/* be even */
#define BEACHBALL_POLYS 16

/* array of vertices making up a stripe */
vector stripe_point[BEACHBALL_POLYS + 3];

/* has the beachball been initialized */
Boolean beachball_initialized = FALSE;

/* Number of polygons making up a stripe */
int beachball_stripes;

/* Number of vertices making up a stripe */
int stripe_vertices;

/* Initializes beachball_point array to a stripe of unit */
/* radius. */
void setbeachball(int stripes)
{
  int i,j;
  float x,y,z; /* vertex points */
  float theta,delta_theta; /* angle from top pole to bottom*/
  float offset; /* offset from center of stripe to vertex */
  /* radius of cross-section at current latitude */
  float cross_radius;
  float cross_theta; /* angle occupied by a stripe */

  beachball_stripes = stripes;

  /* polys distributed by even angles from top to bottom */
  delta_theta = M_PI/((float)BEACHBALL_POLYS/2.0);
  theta = delta_theta;
  cross_theta = 2.0*M_PI/(float)beachball_stripes;

  j = 0;
  stripe_point[j][0] = top[0];
  stripe_point[j][1] = top[1];
  stripe_point[j][2] = top[2];
  j++;

  for (i = 0; i < BEACHBALL_POLYS; i += 2) {
    cross_radius = sin(theta);
    offset = cross_radius * tan(cross_theta/2.0);

    stripe_point[j][0] = - offset;
    stripe_point[j][1] = cos(theta);
    stripe_point[j][2] = cross_radius;
    j++;

    stripe_point[j][0] = offset;
    stripe_point[j][1] = stripe_point[j-1][1];
    stripe_point[j][2] = stripe_point[j-1][2];
    j++;

    theta += delta_theta;
  } /* end for */

  stripe_point[j][0] = bottom[0];
  stripe_point[j][1] = bottom[1];
  stripe_point[j][2] = bottom[2];

  stripe_vertices = j + 1;

  beachball_initialized = TRUE;
}


/* Draws a canonical beachball. The colors are cpack values
 * when in RGBmode.
 */
void beachball(unsigned long c1, unsigned long c2)
{
  float angle, delta_angle;
  int i, j;

  if (! beachball_initialized)
    setbeachball(BEACHBALL_STRIPES);

  angle = 0.0;
  delta_angle = 360.0/(float)beachball_stripes;

  for (i = 0; i < beachball_stripes; i++) {
    if ( i%2 == 0)
      glColor4ubv((GLubyte *)(&c1));
    else
      glColor4ubv((GLubyte *)(&c2));
    glPushMatrix();
    glRotatef(angle, 0.0, 1.0, 0.0);
    angle += delta_angle;

    glBegin(GL_TRIANGLE_STRIP);
    for (j = 0; j < stripe_vertices; j++)
      glVertex3fv(stripe_point[j]);
    glEnd();
    glPopMatrix();
  }
}

