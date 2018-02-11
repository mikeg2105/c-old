#include <stdlib.h>
#include <stdio.h>

#include <Xm/Xm.h>
#include <Xm/MainW.h>
#include <Xm/CascadeB.h>
#include <Xm/Label.h>
#include <X11/keysym.h>
#include <GL/glx.h>
#include <GL/GLwMDrawA.h>

/* prototype functions */

void quit_call(Widget , int), 
       menu_call(Widget , int),
       help_call(void);
       
static int      attribs[] = { GLX_RGBA, None};

Widget label;
String food[] = { "Chicken",  "Beef", "Pork", "Lamb", "Cheese"};


/*Clear the window and draw 3 rectangles*/

void
draw_scene(void) {
    static GLboolean   displayListInited = GL_FALSE;

    if (displayListInited) {
        /* if display list already exists, just execute it */
        glCallList(1);
    } else {
        /* otherwise compile and execute to create the display list */
        glNewList(1, GL_COMPILE_AND_EXECUTE);
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
        /* front face */
        glBegin(GL_QUADS);
        glColor3f(0.0, 0.7, 0.1);       /* green */
        glVertex3f(-1.0, 1.0, 1.0);
        glVertex3f(1.0, 1.0, 1.0);
        glVertex3f(1.0, -1.0, 1.0);
        glVertex3f(-1.0, -1.0, 1.0);
        /* back face */
        glColor3f(0.9, 1.0, 0.0);       /* yellow */
        glVertex3f(-1.0, 1.0, -1.0);
        glVertex3f(1.0, 1.0, -1.0);
        glVertex3f(1.0, -1.0, -1.0);
        glVertex3f(-1.0, -1.0, -1.0);
        /* top side face */
        glColor3f(0.2, 0.2, 1.0);       /* blue */
        glVertex3f(-1.0, 1.0, 1.0);
        glVertex3f(1.0, 1.0, 1.0);
        glVertex3f(1.0, 1.0, -1.0);
        glVertex3f(-1.0, 1.0, -1.0);
        /* bottom side face */
        glColor3f(0.7, 0.0, 0.1);       /* red */
        glVertex3f(-1.0, -1.0, 1.0);
        glVertex3f(1.0, -1.0, 1.0);
        glVertex3f(1.0, -1.0, -1.0);
        glVertex3f(-1.0, -1.0, -1.0);
        glEnd();
        glEndList();
        displayListInited = GL_TRUE;
    }
   /* if(doubleBuffer) glXSwapBuffers(dpy, win); *//* buffer swap does implicit glFlush */
       glFlush(); /* explicit flush for single buffered case */
}

/*Process input events*/

static void
input(Widget w, XtPointer client_data, XtPointer call) {
   char buffer[31];
   KeySym keysym;
   XEvent *event = ((GLwDrawingAreaCallbackStruct *) call)->event;

   switch(event->type) {
   case KeyRelease:
      XLookupString(&event->xkey, buffer, 30, &keysym, NULL);
      switch(keysym) {
      case XK_Escape :
         exit(EXIT_SUCCESS);
         break;
      default: break;
      }
      break;
   }
}

/*Process window resize events*/
 /* calling glXWaitX makes sure that all x operations like *
 * XConfigureWindow to resize the window happen befor the *
 * OpenGL glViewport call.*/

static void
resize(Widget w, XtPointer client_data, XtPointer call) {
   GLwDrawingAreaCallbackStruct *call_data;
   call_data = (GLwDrawingAreaCallbackStruct *) call;
   glXWaitX();
   glViewport(0, 0, call_data->width, call_data->height);
}

/*Process window expose events*/

static void
expose(Widget w, XtPointer client_data, XtPointer call) {
    draw_scene();
}





main(int argc, char **argv)

{   
	Display        *dpy;
	Widget top_wid, main_w, help;
    Widget  menubar, menu, widget,glxwidget;
    XtAppContext app;
    XColor back, fore, spare;
    XmString  quit, menu_str,  help_str, chicken, beef, pork, 
              lamb, cheese, label_str;
	 XVisualInfo    *vi;
    GLXContext      glxcontext;   
    int n = 0;
    Arg args[2];

    /* Initialize toolkit */
    top_wid = XtVaAppInitialize(&app, "Demos",
        NULL, 0, &argc, argv, NULL, NULL);
      dpy = XtDisplay(top_wid);  
    

    /* main window will contain a MenuBar and a Label  */
    main_w = XtVaCreateManagedWidget("main_window",
        xmMainWindowWidgetClass,   top_wid,
        XmNwidth, 600,         
        XmNheight, 600,
        NULL);

     /* specify visual directly */
    if (!(vi = glXChooseVisual(dpy, DefaultScreen(dpy), attribs)))
        XtAppError(app, "no suitable RGB visual");
        
    glxwidget = XtVaCreateManagedWidget("glxwidget", 
               glwMDrawingAreaWidgetClass, main_w, GLwNvisualInfo, 
               vi, NULL);
               
    XtAddCallback(glxwidget, GLwNexposeCallback, expose, NULL);
    XtAddCallback(glxwidget, GLwNresizeCallback, resize, NULL);
    XtAddCallback(glxwidget, GLwNinputCallback, input, NULL);


    /* Create a simple MenuBar that contains three menus */
    quit = XmStringCreateLocalized("Quit");
    menu_str = XmStringCreateLocalized("Menu");
    help_str = XmStringCreateLocalized("Help");

    
    menubar = XmVaCreateSimpleMenuBar(main_w, "menubar",
        XmVaCASCADEBUTTON, quit, 'Q',
        XmVaCASCADEBUTTON, menu_str, 'M',
        XmVaCASCADEBUTTON, help_str, 'H',
        NULL);
        
        
    XmStringFree(menu_str); /* finished with this so free */
    XmStringFree(help_str);
    
    /* First menu is the quit menu -- callback is quit_call() */
    
    XmVaCreateSimplePulldownMenu(menubar, "quit_menu", 0, quit_call,
        XmVaPUSHBUTTON, quit, 'Q', NULL, NULL,
        NULL);
    XmStringFree(quit);

    /* Second menu is the food menu -- callback is menu_call() */
    chicken = XmStringCreateLocalized(food[0]);
    beef = XmStringCreateLocalized(food[1]);
    pork = XmStringCreateLocalized(food[2]);
    lamb = XmStringCreateLocalized(food[3]);
    cheese = XmStringCreateLocalized(food[4]);
    
    menu = XmVaCreateSimplePulldownMenu(menubar, "edit_menu", 1, 
        menu_call,
        XmVaRADIOBUTTON, chicken, 'C', NULL, NULL,
        XmVaRADIOBUTTON, beef, 'B', NULL, NULL,
        XmVaRADIOBUTTON, pork, 'P', NULL, NULL,
        XmVaRADIOBUTTON, lamb, 'L', NULL, NULL,
        XmVaRADIOBUTTON, cheese, 'h', NULL, NULL,
        /* RowColumn resources to enforce */
        XmNradioBehavior, True,    
        /* select radio behavior in Menu */ 
        XmNradioAlwaysOne, True,   
        NULL);
    XmStringFree(chicken);
    XmStringFree(beef);
    XmStringFree(pork);
    XmStringFree(lamb);
    XmStringFree(cheese);


    /* Initialize menu so that "chicken" is selected. */
    if (widget = XtNameToWidget(menu, "button_1"))
       { XtSetArg(args[n],XmNset, True);
         n++; 
         XtSetValues(widget, args, n);
       }

    n=0; /* reset n */

     /* get help widget ID to add callback */
        
    help = XtVaCreateManagedWidget( "Help",
        xmCascadeButtonWidgetClass, menubar,
        XmNmnemonic, 'H',
        NULL);
     
    XtAddCallback(help, XmNactivateCallback, help_call, NULL);

     /* Tell the menubar which button is the help menu  */

     XtSetArg(args[n],XmNmenuHelpWidget,help);
     n++;
     XtSetValues(menubar,args,n);
     n=0; /* reset n */
   


    
    XtManageChild(menubar);
    
   /* create a label text widget that will be "work area" 
      selections from "Menu"  menu change label
      default label is item 0 */

    label_str = XmStringCreateLocalized(food[0]);
    
    label = XtVaCreateManagedWidget("main_window",
        xmLabelWidgetClass,   main_w,
        XmNlabelString, label_str,
        NULL);
        
     XmStringFree(label_str);
      
    /* set the label as the "work area" of the main window */
    XtVaSetValues(main_w,
        XmNmenuBar,    menubar,
        XmNworkWindow, label, glxwidget,
        NULL);
 


    XtRealizeWidget(top_wid);

    glxcontext = glXCreateContext(dpy, vi, 0, GL_TRUE);
    GLwDrawingAreaMakeCurrent(glxwidget, glxcontext);
    XtAppMainLoop(app);
}

/* Any item the user selects from the File menu calls this function.
   It will "Quit" (item_no == 0).  */

void
quit_call(Widget w, int item_no)

   /* w = menu item that was selected 
      item_no = the index into the menu */

{
    

    if (item_no == 0) /* the "quit" item */
        exit(0);

   }


/* Called from any of the food "Menu" items.  
   Change the XmNlabelString of the label widget. 
   Note: we have to use dynamic setting with setargs().
 */

void  menu_call(Widget w, int item_no)
                          

{
    int n =0;
    Arg args[1];
    
    XmString   label_str;     
    
    label_str = XmStringCreateLocalized(food[item_no]);
           
    XtSetArg(args[n],XmNlabelString, label_str);      
    ++n;
    XtSetValues(label, args, n);
  
}

 

void help_call()

{   printf("Sorry, I'm Not Much Help\n");
}
