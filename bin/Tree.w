&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME mainMenu
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS mainMenu 
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

DEFINE SHARED VARIABLE session_UserType AS CHARACTER.
DEFINE SHARED VARIABLE session_UsersName AS CHARACTER.
DEFINE NEW GLOBAL SHARED VARIABLE session_Window AS INT INIT 0.
DEFINE NEW GLOBAL SHARED VARIABLE session_Path AS CHAR.
DEFINE NEW GLOBAL SHARED VARIABLE session_icon AS CHAR.
/* Parameters Definitions ---                                           */
DEFINE VARIABLE mvarTreeMenu            AS COM-HANDLE.                   
DEFINE VARIABLE tmpNode                 AS COM-HANDLE.  

DEFINE VARIABLE Levels   AS INTEGER     NO-UNDO.
DEFINE VARIABLE SubNodes AS INTEGER     NO-UNDO.

DEFINE VARIABLE NameLevels   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE NameSubNodes AS CHARACTER   NO-UNDO.

DEFINE VARIABLE IDLevels   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE IDSubNodes AS CHARACTER   NO-UNDO.


DEFINE VARIABLE windowName AS CHARACTER   NO-UNDO.

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define DISPLAYED-OBJECTS fillDatetime 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR mainMenu AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlFrame AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE CtrlFrame-3 AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame-3 AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE CtrlFrame-4 AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame-4 AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE fillDatetime AS DATETIME FORMAT "99/99/9999 HH:MM:SS AM":U 
      VIEW-AS TEXT 
     SIZE 26 BY .65
     FGCOLOR 0 FONT 12 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     fillDatetime AT ROW 1.19 COL 4.72 NO-LABEL WIDGET-ID 14 NO-TAB-STOP 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 33 BY 26.85
         FONT 10 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW mainMenu ASSIGN
         HIDDEN             = YES
         TITLE              = ""
         COLUMN             = 1.57
         ROW                = 1
         HEIGHT             = 25.88
         WIDTH              = 33
         MAX-HEIGHT         = 26.85
         MAX-WIDTH          = 35.72
         VIRTUAL-HEIGHT     = 26.85
         VIRTUAL-WIDTH      = 35.72
         SMALL-TITLE        = yes
         SHOW-IN-TASKBAR    = yes
         CONTROL-BOX        = no
         MIN-BUTTON         = no
         MAX-BUTTON         = no
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT mainMenu:LOAD-ICON("E:/ICS/img/archive.ico":U) THEN
    MESSAGE "Unable to load icon: E:/ICS/img/archive.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW mainMenu
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN fillDatetime IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       fillDatetime:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(mainMenu)
THEN mainMenu:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 2.08
       COLUMN          = 1.57
       HEIGHT          = 25.31
       WIDTH           = 31.72
       WIDGET-ID       = 2
       HIDDEN          = no
       SENSITIVE       = yes.

CREATE CONTROL-FRAME CtrlFrame-4 ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 13.92
       COLUMN          = 12
       HEIGHT          = 1.08
       WIDTH           = 4
       WIDGET-ID       = 18
       HIDDEN          = yes
       SENSITIVE       = yes.

CREATE CONTROL-FRAME CtrlFrame-3 ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 21.46
       COLUMN          = 15
       HEIGHT          = 1.62
       WIDTH           = 9
       WIDGET-ID       = 6
       HIDDEN          = yes
       SENSITIVE       = yes.
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {0713E8A2-850A-101B-AFC0-4210102A8DA7} type: TreeView */
/* CtrlFrame-4 OCXINFO:CREATE-CONTROL from: {F0B88A90-F5DA-11CF-B545-0020AF6ED35A} type: PSTimer */
/* CtrlFrame-3 OCXINFO:CREATE-CONTROL from: {2C247F23-8591-11D1-B16A-00C0F0283628} type: ImageList */
      CtrlFrame-4:MOVE-AFTER(CtrlFrame).
      CtrlFrame-3:MOVE-AFTER(CtrlFrame-4).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME mainMenu
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mainMenu mainMenu
ON END-ERROR OF mainMenu
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. 
  IF THIS-PROCEDURE:PERSISTENT THEN */ RETURN NO-APPLY. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mainMenu mainMenu
ON WINDOW-CLOSE OF mainMenu
DO:
  /* This event will close the window and terminate the procedure.  */
    MESSAGE "Conferm to close ICS ?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE yn AS LOGICAL.
    IF yn = YES THEN
    DO:
        APPLY "CLOSE":U TO THIS-PROCEDURE.
    END.                                  
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame mainMenu OCX.Collapse
PROCEDURE CtrlFrame.TreeView.Collapse .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  Required for OCX.
    Node
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER p-Node AS COM-HANDLE NO-UNDO.

IF p-Node:INDEX > 1 THEN
p-Node:IMAGE = 1.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame mainMenu OCX.Expand
PROCEDURE CtrlFrame.TreeView.Expand .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  Required for OCX.
    Node
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER p-Node AS COM-HANDLE NO-UNDO.

IF p-Node:INDEX > 1 THEN
p-Node:IMAGE = 2.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame mainMenu OCX.NodeClick
PROCEDURE CtrlFrame.TreeView.NodeClick .
DEFINE INPUT PARAMETER p-Node   AS COM-HANDLE NO-UNDO.

session_Path = p-Node:FullPath + " | Logged in " + session_UsersName + " as " + session_UserType.
session_icon = "E:\ICS\img\archive.ico".

windowName = p-Node:KEY + ".w".

IF p-Node:INDEX = 1 THEN p-Node:expanded = NOT p-Node:expanded.

IF p-Node:children <> 0 THEN p-Node:expanded = NOT p-Node:expanded.

IF p-node:TEXT = "Exit" THEN
    APPLY "WINDOW-CLOSE":U TO {&WINDOW-NAME}.
ELSE IF p-node:KEY = "Backup" THEN
DO:
    APPLY "WINDOW-CLOSE":U TO mainMenu.
    RUN bak.p.
END.
ELSE IF p-node:children = 0 THEN
    DO:
        mainMenu:SENSITIVE = FALSE.
        RUN VALUE(p-Node:KEY + ".w") NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            MESSAGE "Window not found." VIEW-AS ALERT-BOX INFO BUTTONS OK.
        mainMenu:SENSITIVE = TRUE.
        mainMenu:VISIBLE = TRUE.

    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame-4 mainMenu OCX.Tick
PROCEDURE CtrlFrame-4.PSTimer.Tick .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/

fillDatetime = DATETIME(TODAY, MTIME).

DISPLAY fillDatetime WITH FRAME DEFAULT-FRAME.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK mainMenu 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  PROPATH = "E:\ICS\bin," + PROPATH.


 RUN nodes.

 {&WINDOW-NAME}:TITLE = "ICS Menu | Welcome : " + session_UsersName.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load mainMenu  _CONTROL-LOAD
PROCEDURE control_load :
/*------------------------------------------------------------------------------
  Purpose:     Load the OCXs    
  Parameters:  <none>
  Notes:       Here we load, initialize and make visible the 
               OCXs in the interface.                        
------------------------------------------------------------------------------*/

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN
DEFINE VARIABLE UIB_S    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE OCXFile  AS CHARACTER  NO-UNDO.

OCXFile = SEARCH( "Tree.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chCtrlFrame = CtrlFrame:COM-HANDLE
    UIB_S = chCtrlFrame:LoadControls( OCXFile, "CtrlFrame":U)
    CtrlFrame:NAME = "CtrlFrame":U
    chCtrlFrame-3 = CtrlFrame-3:COM-HANDLE
    UIB_S = chCtrlFrame-3:LoadControls( OCXFile, "CtrlFrame-3":U)
    CtrlFrame-3:NAME = "CtrlFrame-3":U
    chCtrlFrame-4 = CtrlFrame-4:COM-HANDLE
    UIB_S = chCtrlFrame-4:LoadControls( OCXFile, "CtrlFrame-4":U)
    CtrlFrame-4:NAME = "CtrlFrame-4":U
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "Tree.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI mainMenu  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(mainMenu)
  THEN DELETE WIDGET mainMenu.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI mainMenu  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  RUN control_load.
  DISPLAY fillDatetime 
      WITH FRAME DEFAULT-FRAME IN WINDOW mainMenu.
  VIEW FRAME DEFAULT-FRAME IN WINDOW mainMenu.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW mainMenu.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE nodes mainMenu 
PROCEDURE nodes :
mvarTreeMenu = chCtrlFrame:TreeView.
chCtrlFrame:TreeView:ImageList = chCtrlFrame-3:ImageList.

    mvarTreeMenu:indentation = 20.

    ASSIGN tmpNode = mvarTreeMenu:nodes:ADD(,4,"Home"," Home",3,3) tmpNode:expanded = TRUE .
    
    IF session_UserType = "Administrator" OR session_UserType = "Super Admin" THEN
    DO:
        ASSIGN tmpNode = mvarTreeMenu:nodes:ADD("Home",4,"Reference","Reference",2) tmpNode:expanded = FALSE .
            ASSIGN tmpNode = mvarTreeMenu:nodes:ADD("Reference",4,"ItemCat","Item Categories",4,5) tmpNode:expanded = TRUE .
            ASSIGN tmpNode = mvarTreeMenu:nodes:ADD("Reference",4,"UserCat","User Categories",4,5) tmpNode:expanded = TRUE .
            ASSIGN tmpNode = mvarTreeMenu:nodes:ADD("Reference",4,"Users","Users",4,5) tmpNode:expanded = TRUE .
            ASSIGN tmpNode = mvarTreeMenu:nodes:ADD("Reference",4,"CustomerCat","Customer Categories",4,5) tmpNode:expanded = TRUE .
            ASSIGN tmpNode = mvarTreeMenu:nodes:ADD("Reference",4,"Customers","Customers",4,5) tmpNode:expanded = TRUE .
            ASSIGN tmpNode = mvarTreeMenu:nodes:ADD("Reference",4,"Banks","Banks",4,5) tmpNode:expanded = TRUE .
            ASSIGN tmpNode = mvarTreeMenu:nodes:ADD("Reference",4,"Branches","Branches",4,5) tmpNode:expanded = TRUE .
            ASSIGN tmpNode = mvarTreeMenu:nodes:ADD("Reference",4,"Areas","Areas",4,5) tmpNode:expanded = TRUE .
            ASSIGN tmpNode = mvarTreeMenu:nodes:ADD("Reference",4,"Vehicals","Vehicals",4,5) tmpNode:expanded = TRUE .
            ASSIGN tmpNode = mvarTreeMenu:nodes:ADD("Reference",4,"PaymentMethods","Payment Methods",4,5) tmpNode:expanded = TRUE .
            ASSIGN tmpNode = mvarTreeMenu:nodes:ADD("Reference",4,"bsChange","Item Admin Edit",4,5) tmpNode:expanded = TRUE .
            ASSIGN tmpNode = mvarTreeMenu:nodes:ADD("Reference",4,"BSEdit","BS Controll",4,5) tmpNode:expanded = TRUE .
            ASSIGN tmpNode = mvarTreeMenu:nodes:ADD("Reference",4,"ItemSorting","Item Sorting",4,5) tmpNode:expanded = TRUE .
            ASSIGN tmpNode = mvarTreeMenu:nodes:ADD("Reference",4,"ExpenseType","Expense Type",4,5) tmpNode:expanded = TRUE .
            IF session_UserType = "Super Admin" THEN
            DO:
                ASSIGN tmpNode = mvarTreeMenu:nodes:ADD("Reference",4,"para","Parameters",4,5) tmpNode:expanded = TRUE .
            END.
            ASSIGN tmpNode = mvarTreeMenu:nodes:ADD("Reference",4,"BSSave","BSSaved",4,5) tmpNode:expanded = TRUE .

        ASSIGN tmpNode = mvarTreeMenu:nodes:ADD("Home",4,"Inventory","Inventory",2) tmpNode:expanded = TRUE .
            ASSIGN tmpNode = mvarTreeMenu:nodes:ADD("Inventory",4,"Items","Stock Control",4,5) tmpNode:expanded = TRUE .
            ASSIGN tmpNode = mvarTreeMenu:nodes:ADD("Inventory",4,"Invoices","Invoicing",4,5) tmpNode:expanded = TRUE .
    
        ASSIGN tmpNode = mvarTreeMenu:nodes:ADD("Home",4,"Recipts","Recipts",2) tmpNode:expanded = TRUE .
            ASSIGN tmpNode = mvarTreeMenu:nodes:ADD("Recipts",4,"Billing","Billing",4,5) tmpNode:expanded = TRUE .
            ASSIGN tmpNode = mvarTreeMenu:nodes:ADD("Recipts",4,"DaySale","Loading Unloading",4,5) tmpNode:expanded = TRUE .
    
        ASSIGN tmpNode = mvarTreeMenu:nodes:ADD("Home",4,"Payments","Payments",2) tmpNode:expanded = TRUE .
            ASSIGN tmpNode = mvarTreeMenu:nodes:ADD("Payments",4,"Cash","Cash",4,5) tmpNode:expanded = TRUE .
            ASSIGN tmpNode = mvarTreeMenu:nodes:ADD("Payments",4,"Cheques","Cheques",4,5) tmpNode:expanded = TRUE .

        ASSIGN tmpNode = mvarTreeMenu:nodes:ADD("Home",4,"InfoCenter","Info Center",2) tmpNode:expanded = TRUE .
            ASSIGN tmpNode = mvarTreeMenu:nodes:ADD("InfoCenter",4,"CollectionReport","Collection Report",4,5) tmpNode:expanded = TRUE .
            ASSIGN tmpNode = mvarTreeMenu:nodes:ADD("InfoCenter",4,"FinancialState","Transaction Report",4,5) tmpNode:expanded = TRUE .
            ASSIGN tmpNode = mvarTreeMenu:nodes:ADD("InfoCenter",4,"SpotStockTest","Spot Stock Test",4,5) tmpNode:expanded = TRUE .
            ASSIGN tmpNode = mvarTreeMenu:nodes:ADD("InfoCenter",4,"DailySummary","Daily Bills Summary",4,5) tmpNode:expanded = TRUE .
            ASSIGN tmpNode = mvarTreeMenu:nodes:ADD("InfoCenter",4,"BillingReport","Billing Report",4,5) tmpNode:expanded = TRUE .
            ASSIGN tmpNode = mvarTreeMenu:nodes:ADD("InfoCenter",4,"Expense","Expenses",4,5) tmpNode:expanded = TRUE .
            ASSIGN tmpNode = mvarTreeMenu:nodes:ADD("InfoCenter",4,"Summary","Amount Summary",4,5) tmpNode:expanded = TRUE .
            ASSIGN tmpNode = mvarTreeMenu:nodes:ADD("InfoCenter",4,"DaySaleReport","Quantity Summary",4,5) tmpNode:expanded = TRUE .
      END.
    ELSE
    DO:
        ASSIGN tmpNode = mvarTreeMenu:nodes:ADD("Home",4,"Reference","Reference",2) tmpNode:expanded = FALSE .
            ASSIGN tmpNode = mvarTreeMenu:nodes:ADD("Reference",4,"Customers","Customers",4,5) tmpNode:expanded = TRUE .
            ASSIGN tmpNode = mvarTreeMenu:nodes:ADD("Reference",4,"Banks","Banks",4,5) tmpNode:expanded = TRUE .
            ASSIGN tmpNode = mvarTreeMenu:nodes:ADD("Reference",4,"Branches","Branches",4,5) tmpNode:expanded = TRUE.
            ASSIGN tmpNode = mvarTreeMenu:nodes:ADD("Reference",4,"ItemSorting","Item Sorting",4,5) tmpNode:expanded = TRUE .
            ASSIGN tmpNode = mvarTreeMenu:nodes:ADD("Reference",4,"ExpenseType","Expense Type",4,5) tmpNode:expanded = TRUE .
    
        ASSIGN tmpNode = mvarTreeMenu:nodes:ADD("Home",4,"Inventory","Inventory",2) tmpNode:expanded = TRUE .
            ASSIGN tmpNode = mvarTreeMenu:nodes:ADD("Inventory",4,"Items","Stock Control",4,5) tmpNode:expanded = TRUE .
            ASSIGN tmpNode = mvarTreeMenu:nodes:ADD("Inventory",4,"Invoices","Invoicing",4,5) tmpNode:expanded = TRUE .
    
        ASSIGN tmpNode = mvarTreeMenu:nodes:ADD("Home",4,"Recipts","Recipts",2) tmpNode:expanded = TRUE .
            ASSIGN tmpNode = mvarTreeMenu:nodes:ADD("Recipts",4,"Billing","Billing",4,5) tmpNode:expanded = TRUE .
            ASSIGN tmpNode = mvarTreeMenu:nodes:ADD("Recipts",4,"DaySale","Loading Unloading",4,5) tmpNode:expanded = TRUE .
    
        ASSIGN tmpNode = mvarTreeMenu:nodes:ADD("Home",4,"Payments","Payments",2) tmpNode:expanded = TRUE .
            ASSIGN tmpNode = mvarTreeMenu:nodes:ADD("Payments",4,"Cash","Cash",4,5) tmpNode:expanded = TRUE .
            ASSIGN tmpNode = mvarTreeMenu:nodes:ADD("Payments",4,"Cheques","Cheques",4,5) tmpNode:expanded = TRUE .

        ASSIGN tmpNode = mvarTreeMenu:nodes:ADD("Home",4,"InfoCenter","Info Center",2) tmpNode:expanded = TRUE .
            ASSIGN tmpNode = mvarTreeMenu:nodes:ADD("InfoCenter",4,"CollectionReport","Collection Report",4,5) tmpNode:expanded = TRUE .
            ASSIGN tmpNode = mvarTreeMenu:nodes:ADD("InfoCenter",4,"FinancialState","Transaction Report",4,5) tmpNode:expanded = TRUE .
            ASSIGN tmpNode = mvarTreeMenu:nodes:ADD("InfoCenter",4,"SpotStockTest","Spot Stock Test",4,5) tmpNode:expanded = TRUE .
            ASSIGN tmpNode = mvarTreeMenu:nodes:ADD("InfoCenter",4,"DailySummary","Daily Bills Summary",4,5) tmpNode:expanded = TRUE .
            ASSIGN tmpNode = mvarTreeMenu:nodes:ADD("InfoCenter",4,"BillingReport","Billing Report",4,5) tmpNode:expanded = TRUE .
            ASSIGN tmpNode = mvarTreeMenu:nodes:ADD("InfoCenter",4,"Expense","Expenses",4,5) tmpNode:expanded = TRUE .
            ASSIGN tmpNode = mvarTreeMenu:nodes:ADD("InfoCenter",4,"Summary","Amount Summary",4,5) tmpNode:expanded = TRUE .
            ASSIGN tmpNode = mvarTreeMenu:nodes:ADD("InfoCenter",4,"DaySaleReport","Quantity Summary",4,5) tmpNode:expanded = TRUE .
    END.

    ASSIGN tmpNode = mvarTreeMenu:nodes:ADD("Home",4,"Exit","Exit",6) tmpNode:expanded = TRUE .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

