//rotating and expanding cylinder animation
//with menu bar and dialog widget


#include <stream.h>

#include <Xm/Xm.h>
#include <Xm/MainW.h>
#include <Xm/RowColumn.h>
#include <Xm/Frame.h>
#include <Xm/PushB.h>
#include <Xm/CascadeB.h>
#include <Xm/MessageB.h>
#include <Xm/Form.h>

#include <Inventor/Xt/SoXt.h>
#include <Inventor/Xt/SoXtRenderArea.h>

#include <Inventor/SoDB.h>
#include <Inventor/nodes/SoTranslation.h>
#include <Inventor/nodes/SoScale.h>	
#include <Inventor/nodes/SoRotationXYZ.h>
#include <Inventor/nodes/SoCube.h>
#include <Inventor/nodes/SoCylinder.h>
#include <Inventor/nodes/SoSphere.h>
#include <Inventor/nodes/SoMaterial.h>
#include <Inventor/nodes/SoSeparator.h>
#include <Inventor/nodes/SoDrawStyle.h>
#include <Inventor/nodes/SoShapeHints.h>




#include <Inventor/Xt/viewers/SoXtExaminerViewer.h>
#include <Inventor/nodes/SoEnvironment.h>
#include <Inventor/sensors/SoTimerSensor.h>
#include <Inventor/sensors/SoTimerSensor.h>

SoSeparator *loadModel(char *name);

#define NUMBOIDS 40    // Number of boids.
#define UPDATEFPS 25   // Update rate of the boids (internally) in frames-per-second.

/* --- This is a good value for an Indy --- */
#define GRAPHICSFPS 4  // Update rate of the graphics in frames-per-second.

/* --- This is a good value if you have a Reality Engine, or if you are
       running the program remotely and displaying on an Indy.
#define GRAPHICSFPS 12  // Update rate of the graphics in frames-per-second.
--- */

//#define SPHERE      // Use the sphere as bounding object? If this is
		    // not defined, a rectangle will be used.

double RAD = 25;    // RAD of the bounding sphere of the world.

//---------------- Globals ---------------------------

//CIoSimulation *mySimulation;


Widget myWindow,myForm, theparent, thebar, theframe, thefilebutton,theviewbutton, theviewmenu, thefilemenu, quitbutton;
//Window theparent, thebar, theframe, thebutton, themenu, quitbutton;
SoXtExaminerViewer *myViewer;
SoSeparator *root;

SoNode * simobject;
SoMaterial *mat;
//SoTransform *tran;

//------------------Support Routines------------------------

static void
animateSimObjects(void *, SoSensor *) {

  // for (int i = 0; i < NUMSIMOBJECTS; i++) {
  //    mySimObjects[i]->updateGraphics();
  // }
}

static void
updateSimObjects(void *, SoSensor *) {

  static double elapsedTime = 0;
  
  SoCylinder *cyl =(SoCylinder *)simobject;
  cyl->radius =1.5+2*sin(elapsedTime);
  cyl->height =1.5+2*cos(elapsedTime);
  //for (int i = 0; i < NUMSIMOBJECTS; i++) {
   //   mySimObject[i]->update(elapsedTime);
   //}

  // increment elapsed time
  elapsedTime += 1.0/UPDATEFPS;
}

/*Callback for quit button*/
void quitbutton_func(Widget w, XtPointer client_data, XtPointer call_data)
{
	printf("Quit button selected, terminating program.");
	exit(0);		
}



void
setUpGraphics(void) {

   myWindow = SoXt::init("SimViewer");
   if (myWindow == NULL) {
     cerr << "myWindow is null. bye!\n";
     exit(1); 
   }

   root = new SoSeparator;
   root->ref();

   
   SoEnvironment *Env = new SoEnvironment;
   Env->ambientIntensity.setValue(.5);
   root->addChild(Env);

    myForm = XtCreateWidget("Form", xmFormWidgetClass, myWindow, NULL, 0);


 
    thebar = XmCreateMenuBar(myForm,"MenuBar", NULL, 0);
    XtManageChild(thebar);
    theframe=XtVaCreateManagedWidget("theframe", xmFrameWidgetClass, myForm, NULL);
   	
   	XmMainWindowSetAreas(myForm, thebar, NULL, NULL, NULL, theframe);
   	thefilebutton=XtVaCreateManagedWidget("thefilebutton",xmCascadeButtonWidgetClass,thebar,NULL);
   	theviewbutton=XtVaCreateManagedWidget("theviewbutton",xmCascadeButtonWidgetClass,thebar,NULL);
   	
   	thefilemenu=XmCreatePulldownMenu(thebar, "file", NULL, 0);
   	theviewmenu=XmCreatePulldownMenu(thebar, "view", NULL, 0);
   	
   	quitbutton=XtVaCreateManagedWidget("quitbutton", xmPushButtonWidgetClass, thefilemenu, NULL);
   	XtVaSetValues(thefilebutton, XmNsubMenuId, thefilemenu, NULL);
   	XtVaSetValues(theviewbutton, XmNsubMenuId, theviewmenu, NULL);   	
   	XtAddCallback(quitbutton, XmNactivateCallback, quitbutton_func, 0);
     
 
 
   
   // Set up viewer:
   //myViewer = new SoXtExaminerViewer(myWindow);
   myViewer = new SoXtExaminerViewer(myForm);
   //myViewer->setDrawStyle(SoXtViewer::STILL, SoXtViewer::VIEW_LINE);
   myViewer->setSize(SbVec2s(400, 250)); 
   myViewer->setTitle("SimViewer");
   myViewer->setTransparencyType(SoGLRenderAction::DELAYED_ADD);
   myViewer->setSceneGraph(root);
   myViewer->setHeadlight(TRUE);
   

 
   myViewer->show();
   
    //layout components in form
   Arg args[8];
   XtSetArg(args[0], XmNtopAttachment, XmATTACH_FORM);
   XtSetArg(args[1], XmNbottomAttachment, XmATTACH_FORM);
   XtSetArg(args[2], XmNleftAttachment, XmATTACH_FORM);
   XtSetArg(args[3], XmNrightAttachment, XmATTACH_POSITION);
   XtSetArg(args[4], XmNrightPosition, 40);
   
   XtSetValues(myViewer->getWidget(),args,5);
   XtSetArg(args[2], XmNrightAttachment, XmATTACH_FORM);
   XtSetArg(args[3], XmNleftAttachment, XmATTACH_POSITION);
   XtSetArg(args[4], XmNleftPosition, 41);  
   XtSetValues(myForm,args,5);   
   SoXt::show(myForm);  
   SoXt::show(myWindow);

}


void
makeScenery(void) {

  // Make a subgraph for the boundary obstacle
  SoSeparator *boundary = new SoSeparator;
  SoMaterial *m =  new SoMaterial;
  m->transparency.setValue(.9);
  m->ambientColor.setValue(1, 1, 1);
  boundary->addChild(m);

  // Make a subgraph for the other obstacles
  SoSeparator *r = new SoSeparator;
  r->addChild(boundary);
  SoMaterial *m2 =  new SoMaterial;
  m2->ambientColor.setValue(0, 0, 1);
  m2->diffuseColor.setValue(0, 0, 1);
  m2->specularColor.setValue(.5, .5, .5);
  m2->shininess.setValue(0.2);
  r->addChild(m2);

  root->addChild(r);

}

void
setupUpdateCallbacks() {

  SoTimerSensor *updateTimer = new SoTimerSensor(updateSimObjects, NULL);
  updateTimer->setInterval(1.0/UPDATEFPS);
  updateTimer->schedule(); 

  SoTimerSensor *graphicsTimer = new SoTimerSensor(animateSimObjects, NULL);
  graphicsTimer->setInterval(1.0/GRAPHICSFPS);
  graphicsTimer->schedule(); 

}

bool
makeConfig(void) {
      
 
    simobject = new SoCylinder;
  mat =  new SoMaterial;
  mat->transparency.setValue(.9);
  mat->ambientColor.setValue(0.3, 1, 0.8);
      
    root->addChild(simobject);
    root->addChild(mat);
    return TRUE;
}


//----------------Simulation Routines-----------------------

void
initSimulation(char *simfilename, char *sceneryfile) {
  
  setUpGraphics();
  
  cerr << "Graphics set up\n";

  if (makeConfig() == FALSE)
     cerr << "\nmakeConfig() returned an error\n";
  
  cerr << "Made config\n";
  
  makeScenery();

  cerr << "Scenery set up\n";

  // Set up an asynchronous callback to update boids.
  setupUpdateCallbacks();

}

int
main (int argc, char **argv) {

  initSimulation(argv[1], argv[2]);
  
  myViewer->viewAll(); 
  SoXt::mainLoop();

  // This will never be reached, but it stops compiler warnings.
  return 0;
}
