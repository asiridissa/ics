&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

DEFINE VARIABLE calendr AS COM-HANDLE   NO-UNDO.
/* Local Variable Definitions ---                                       */

DEFINE TEMP-TABLE ttlorryStock LIKE lorryStock.
DEFINE VARIABLE addModify AS CHARACTER   NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME brw

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttlorryStock

/* Definitions for BROWSE brw                                           */
&Scoped-define FIELDS-IN-QUERY-brw itmName weight "Weight" BSC BSP   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brw BSC BSP   
&Scoped-define SELF-NAME brw
&Scoped-define QUERY-STRING-brw FOR EACH ttlorryStock NO-LOCK
&Scoped-define OPEN-QUERY-brw OPEN QUERY brw FOR EACH ttlorryStock NO-LOCK.
&Scoped-define TABLES-IN-QUERY-brw ttlorryStock
&Scoped-define FIRST-TABLE-IN-QUERY-brw ttlorryStock


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-brw}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-14 cmbVeh btnPopulate btnSave btnCancel 
&Scoped-Define DISPLAYED-OBJECTS cmbVeh 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlFrame AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCancel 
     LABEL "Cancel" 
     SIZE 14 BY 1.

DEFINE BUTTON btnPopulate 
     LABEL "Populate" 
     SIZE 14 BY 1.

DEFINE BUTTON btnSave 
     LABEL "Save" 
     SIZE 14 BY 1.

DEFINE VARIABLE cmbVeh AS INTEGER FORMAT ">>>>9":U INITIAL 0 
     LABEL "Vehical" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEM-PAIRS "--Select Here--",0
     DROP-DOWN-LIST
     SIZE 23.72 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-14
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL  GROUP-BOX  ROUNDED 
     SIZE 61.29 BY 2.69.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brw FOR 
      ttlorryStock SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brw
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brw C-Win _FREEFORM
  QUERY brw NO-LOCK DISPLAY
      itmName FORMAT "X(50)" LABEL "Item"
    weight    FORMAT ">>9.999" LABEL    "Weight"
    BSC       FORMAT ">>>9" WIDTH 7 
    BSP       FORMAT ">>>9" WIDTH 7
    ENABLE BSC BSP
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 61.29 BY 22.96
         FONT 10
         TITLE "BS Decleration" ROW-HEIGHT-CHARS .55 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     cmbVeh AT ROW 1.46 COL 7 COLON-ALIGNED WIDGET-ID 84
     btnPopulate AT ROW 2.58 COL 12.29 WIDGET-ID 234
     btnSave AT ROW 2.58 COL 26.72 WIDGET-ID 236
     btnCancel AT ROW 2.58 COL 41.14 WIDGET-ID 240
     brw AT ROW 4.12 COL 1.72 WIDGET-ID 200
     "Date:" VIEW-AS TEXT
          SIZE 4 BY .58 AT ROW 1.65 COL 33.86 WIDGET-ID 244
     RECT-14 AT ROW 1.15 COL 1.86 WIDGET-ID 252
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 62.72 BY 26.5
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
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Stock Admin Edit"
         COLUMN             = 36.57
         ROW                = 1.27
         HEIGHT             = 26.5
         WIDTH              = 62.72
         MAX-HEIGHT         = 27.15
         MAX-WIDTH          = 195.14
         VIRTUAL-HEIGHT     = 27.15
         VIRTUAL-WIDTH      = 195.14
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
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB brw btnCancel DEFAULT-FRAME */
/* SETTINGS FOR BROWSE brw IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brw
/* Query rebuild information for BROWSE brw
     _START_FREEFORM
OPEN QUERY brw FOR EACH ttlorryStock NO-LOCK.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE brw */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 1.54
       COLUMN          = 38.57
       HEIGHT          = .81
       WIDTH           = 23.72
       WIDGET-ID       = 232
       HIDDEN          = no
       SENSITIVE       = yes.
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {20DD1B9E-87C4-11D1-8BE3-0000F8754DA1} type: DTPicker */
      CtrlFrame:MOVE-AFTER(cmbVeh:HANDLE IN FRAME DEFAULT-FRAME).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Stock Admin Edit */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. 
  IF THIS-PROCEDURE:PERSISTENT THEN  */ RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Stock Admin Edit */
DO:
  /* This event will close the window and terminate the procedure.  */
  MESSAGE "Confirm to close?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE yn AS LOGICAL.
  IF yn THEN
  DO:
    APPLY "CLOSE":U TO THIS-PROCEDURE.
    RETURN NO-APPLY.
  END.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCancel C-Win
ON CHOOSE OF btnCancel IN FRAME DEFAULT-FRAME /* Cancel */
DO:
    MESSAGE "Conferm to cancel?"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE yn AS LOGICAL.
    IF yn THEN
    DO:
        EMPTY TEMP-TABLE ttlorryStock.
        OPEN QUERY brw FOR EACH ttlorryStock.
        DISABLE brw btnCancel btnSave WITH FRAME {&FRAME-NAME}.
        ENABLE btnPopulate cmbVeh WITH FRAME {&FRAME-NAME}.
        calendr:ENABLED = TRUE.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPopulate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPopulate C-Win
ON CHOOSE OF btnPopulate IN FRAME DEFAULT-FRAME /* Populate */
DO:
    IF cmbVeh = 0 THEN
    DO:
        MESSAGE "Select vehical first." VIEW-AS ALERT-BOX WARNING BUTTONS OK.
        RETURN.
    END.         

    DEFINE VARIABLE tempDate AS DATE        NO-UNDO.
    tempDate = calendr:VALUE.
    
    FIND FIRST lorryStock WHERE lorryStock.crDate = tempDate AND lorryStock.VehID = cmbVeh NO-LOCK NO-ERROR.
    IF AVAILABLE lorryStock THEN
    DO:
      MESSAGE "BS already saved for this Vehicle and Date." SKIP "Conferm to modify records?" 
          VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE yn AS LOGICAL.
      IF yn THEN
      DO:
        addModify = "modify".
        EMPTY TEMP-TABLE ttlorryStock.
        FOR EACH itms WHERE itms.stat = YES BY itms.SortID.
            FIND FIRST lorryStock WHERE lorryStock.crDate = tempDate AND lorryStock.VehID = cmbVeh AND lorryStock.itmID = itms.itmID NO-LOCK NO-ERROR.
            CREATE ttlorryStock.
            ttlorryStock.itmID    = lorryStock.itmID     .   
            ttlorryStock.ID       = lorryStock.ID     .   
            ttlorryStock.itmName  = lorryStock.itmName   .   
            ttlorryStock.weight   = lorryStock.weight . 
            ttlorryStock.BSP      = lorryStock.BSP.
            ttlorryStock.BSC      = lorryStock.BSC.
            RELEASE ttlorryStock.
        END.
      END.
    END.
    ELSE IF NOT AVAILABLE lorryStock THEN
    DO:
      addModify = "add".
      EMPTY TEMP-TABLE ttlorryStock.
      FOR EACH itms WHERE itms.stat = YES BY itms.SortID.
          CREATE ttlorryStock.
             ttlorryStock.crDate   = tempDate       .   
             ttlorryStock.itmID    = itms.itmID     .   
             ttlorryStock.itmName  = itms.itmName   .   
             ttlorryStock.VehID    = cmbVeh         .   
             ttlorryStock.weight   = itms.unitWeightKG              .   
      END.
    END.
    
    OPEN QUERY brw FOR EACH ttlorryStock.
    
    ENABLE brw btnCancel btnSave WITH FRAME {&FRAME-NAME}.
    DISABLE btnPopulate cmbVeh WITH FRAME {&FRAME-NAME}.
    calendr:ENABLED = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSave C-Win
ON CHOOSE OF btnSave IN FRAME DEFAULT-FRAME /* Save */
DO:
    DEFINE VARIABLE tempPara AS INTEGER     NO-UNDO.
    MESSAGE "Conferm to save?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE yn AS LOGICAL.
    IF yn THEN
    DO:
        IF addModify = "add" THEN
        DO:
            FIND FIRST paramtrs WHERE name = "lastLorryStockID".
            tempPara = int(val).
    
            FOR EACH ttlorryStock.
            CREATE lorryStock.
            ASSIGN
                 lorryStock.BSC      =  ttlorryStock.BSC    
                 lorryStock.BSP      =  ttlorryStock.BSP    
                 lorryStock.crDate   =  ttlorryStock.crDate 
                 lorryStock.ID       =  tempPara + 1   
                 lorryStock.itmID    =  ttlorryStock.itmID  
                 lorryStock.itmName  =  ttlorryStock.itmName
                 lorryStock.VehID    =  ttlorryStock.VehID  
                 lorryStock.weight   =  ttlorryStock.weight
                .
            tempPara = tempPara + 1.
            END.
            val = STRING(tempPara).
            RELEASE paramtrs.
        END.
        IF addModify = "modify" THEN
        DO:
            FOR EACH ttlorryStock.
                FIND FIRST lorryStock WHERE lorryStock.ID = ttlorryStock.ID.
                    ASSIGN
                        lorryStock.BSC = ttlorryStock.BSC
                        lorryStock.BSP = ttlorryStock.BSP
                        .
            END.
        END.
    END.
    IF NOT ERROR-STAT:ERROR THEN
    DO:
        MESSAGE "Saved successfully." VIEW-AS ALERT-BOX INFO BUTTONS OK.
        EMPTY TEMP-TABLE ttlorryStock.
        OPEN QUERY brw FOR EACH ttlorryStock.
    END.

  DISABLE brw btnCancel btnSave WITH FRAME {&FRAME-NAME}.
  ENABLE btnPopulate cmbVeh WITH FRAME {&FRAME-NAME}.
  calendr:ENABLED = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cmbVeh
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbVeh C-Win
ON LEAVE OF cmbVeh IN FRAME DEFAULT-FRAME /* Vehical */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbVeh C-Win
ON VALUE-CHANGED OF cmbVeh IN FRAME DEFAULT-FRAME /* Vehical */
DO:
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame C-Win OCX.Change
PROCEDURE CtrlFrame.DTPicker.Change .
IF cmbVeh = 0 THEN
    DO:
        MESSAGE "Select Vehical First." VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN.
    END.

/*   RUN QueryLDUNLD. */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brw
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


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

  DEFINE SHARED VARIABLE session_Path AS CHAR.
  DEFINE SHARED VARIABLE session_icon AS CHAR.
  {&WINDOW-NAME}:TITLE = session_Path.
  {&WINDOW-NAME}:LOAD-ICON(session_icon).

  RUN VehLoad.

  calendr = chCtrlFrame:DTPicker.
  calendr:ENABLED = TRUE.
  calendr:VALUE = TODAY - 1.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load C-Win  _CONTROL-LOAD
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

OCXFile = SEARCH( "BSEdit.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chCtrlFrame = CtrlFrame:COM-HANDLE
    UIB_S = chCtrlFrame:LoadControls( OCXFile, "CtrlFrame":U)
    CtrlFrame:NAME = "CtrlFrame":U
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "BSEdit.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  DISPLAY cmbVeh 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-14 cmbVeh btnPopulate btnSave btnCancel 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VehLoad C-Win 
PROCEDURE VehLoad :
FOR EACH vehical.
    cmbVeh:ADD-LAST(veh# + " - " + descrip,ID) IN FRAME {&FRAME-NAME}.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

