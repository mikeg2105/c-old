/* mixed.c
 */

#include <Xm/Xm.h>
#include <Xm/Form.h>
#include <X11/keysym.h>
#include <X11/StringDefs.h>
#include "GL/GLwMDrawA.h"

#include <GL/gl.h>
#include <GL/glu.h>
#include <stdio.h>
#include <stdlib.h>

static void input(Widget, XtPointer, XtPointer);
static void draw_scene_callback (Widget, XtPointer,
                                 XtPointer);
static void do_resize(Widget, XtPointer, XtPointer);
static void init_window(Widget, XtPointer, XtPointer);

static GLXContext glx_context;

void main(int argc, char** argv)
{
    Arg args[20];
    int n;
    Widget glw, toplevel, form;
    static XtAppContext app_context;
    static String fallback_resources[] = {
        "*glwidget*width: 300",
        "*glwidget*height: 300",
        "*glwidget*rgba: TRUE",
        "*glwidget*doublebuffer: TRUE",
        "*glwidget*allocateBackground: TRUE",
        NULL
    };

    toplevel = XtAppInitialize(&app_context, "Mixed", NULL,
                               0, &argc, argv,
                               fallback_resources, NULL, 0);
    n = 0;
    form = XmCreateForm(toplevel, "form", args, n);
    XtManageChild(form);

    n = 0;
    XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM);
    n++;
    XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM);
    n++;
    XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM);
    n++;
    XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM);
    n++;
    glw = GLwCreateMDrawingArea(form, "glwidget", args, n);
    XtManageChild (glw);
    XtAddCallback(glw, GLwNexposeCallback,
                  draw_scene_callback, (XtPointer) NULL);
    XtAddCallback(glw, GLwNresizeCallback, do_resize,
                  (XtPointer) NULL);
    XtAddCallback(glw, GLwNginitCallback, init_window,
                  (XtPointer) NULL);
    XtAddCallback(glw, GLwNinputCallback, input,
                  (XtPointer) NULL);

    XtRealizeWidget(toplevel);
    XtAppMainLoop(app_context);
}

static int rotation = 0;

void spin (void)
{
    rotation = (rotation + 5) % 360;
}

static void draw_scene (Widget w)
{
    GLUquadricObj *quadObj;

    glClear(GL_COLOR_BUFFER_BIT);
    glColor3f (1.0, 1.0, 1.0);
    glPushMatrix();
    glTranslatef (0.0, 0.0, -5.0);
    glRotatef ((GLfloat) rotation, 1.0, 0.0, 0.0);

    glPushMatrix ();
    glRotatef (90.0, 1.0, 0.0, 0.0);
    glTranslatef (0.0, 0.0, -1.0);
    quadObj = gluNewQuadric ();
    gluQuadricDrawStyle (quadObj, GLU_LINE);
    gluCylinder (quadObj, 1.0, 1.0, 2.0, 12, 2);
    glPopMatrix ();

    glPopMatrix();
    glFlush();
    glXSwapBuffers (XtDisplay(w), XtWindow(w));
}

/* Process all Input callbacks*/
static void input(Widget w, XtPointer client_data,
                  XtPointer call)
{
    char buffer[1];
    KeySym keysym;
    GLwDrawingAreaCallbackStruct *call_data;

    call_data = (GLwDrawingAreaCallbackStruct *) call;

    switch(call_data->event->type)
    {
    case KeyRelease:
         /* It is necessary to convert the keycode to a
          * keysym before it is possible to check if it is
          * an escape.
          */
         if (XLookupString( (XKeyEvent *) call_data->event,
                            buffer, 1, &keysym,
                            (XComposeStatus *) NULL ) == 1
             && keysym == (KeySym) XK_Escape)
             exit(0);
    break;

    case ButtonPress:
        switch (call_data->event->xbutton.button)
        {
        case Button1:
            spin();
            draw_scene(w);
        break;
        }
    break;

    default:
    break;
    }
}

static void draw_scene_callback(Widget w, XtPointer client_data, XtPointer call)
{
    static char firstTime = 0x1;
    GLwDrawingAreaCallbackStruct *call_data;

    call_data = (GLwDrawingAreaCallbackStruct *) call;
    GLwDrawingAreaMakeCurrent(w, glx_context);

    if (firstTime) {
        glViewport(0, 0, call_data->width,call_data->height);
        glMatrixMode(GL_PROJECTION);
        glLoadIdentity();
        gluPerspective(65.0, (float) call_data->width /
                       (float)call_data->height, 1.0, 20.0);
        glMatrixMode(GL_MODELVIEW);
        glLoadIdentity();
        firstTime = 0;
    }
    draw_scene (w);
}

static void do_resize(Widget w, XtPointer client_data,
                      XtPointer call)
{
    GLwDrawingAreaCallbackStruct *call_data;

    call_data = (GLwDrawingAreaCallbackStruct *) call;

    GLwDrawingAreaMakeCurrent(w, glx_context);
    glViewport(0, 0, call_data->width, call_data->height);
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    gluPerspective(65.0, (GLfloat) call_data->width /
                   (GLfloat)call_data->height, 1.0, 20.0);
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();
}

static void init_window(Widget w, XtPointer client_data,
                        XtPointer call_data)
{
    Arg args[1];
    XVisualInfo *vi;
    GLUquadricObj *quadObj;

    XtSetArg(args[0], GLwNvisualInfo, &vi);
    XtGetValues(w, args, 1);
    glx_context = glXCreateContext(XtDisplay(w), vi, 0,
                                   GL_FALSE);
}
