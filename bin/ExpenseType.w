&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          ics              PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

DEFINE SHARED VARIABLE session_Window AS INT.
/* Local Variable Definitions ---                                       */

DEFINE VARIABLE addModify AS CHARACTER   NO-UNDO.
DEFINE VARIABLE ModifyVal AS CHARACTER   NO-UNDO.
DEFINE VARIABLE ModifyCnt AS INTEGER   NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME arbrw

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ExpenseType

/* Definitions for BROWSE arbrw                                         */
&Scoped-define FIELDS-IN-QUERY-arbrw ExpenseType.Id ExpenseType.Code ~
ExpenseType.Description 
&Scoped-define ENABLED-FIELDS-IN-QUERY-arbrw 
&Scoped-define QUERY-STRING-arbrw FOR EACH ExpenseType NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-arbrw OPEN QUERY arbrw FOR EACH ExpenseType NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-arbrw ExpenseType
&Scoped-define FIRST-TABLE-IN-QUERY-arbrw ExpenseType


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-arbrw}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS arbrw btnAdd btnModify btnDelete 
&Scoped-Define DISPLAYED-OBJECTS filID filCode filDescr 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnAdd 
     LABEL "Add" 
     SIZE 14 BY 1.

DEFINE BUTTON btnCancel 
     LABEL "Cancel" 
     SIZE 14 BY 1.

DEFINE BUTTON btnDelete 
     LABEL "Delete" 
     SIZE 14 BY 1.

DEFINE BUTTON btnModify 
     LABEL "Modify" 
     SIZE 14 BY 1.

DEFINE BUTTON btnSave 
     LABEL "Save" 
     SIZE 14 BY 1.

DEFINE VARIABLE filCode AS CHARACTER FORMAT "X(8)":U 
     LABEL "Code" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88
     BGCOLOR 15 FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE filDescr AS CHARACTER FORMAT "X(100)":U 
     LABEL "Description" 
     VIEW-AS FILL-IN 
     SIZE 50 BY .88
     BGCOLOR 15 FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE filID AS INTEGER FORMAT ">>>,>>9":U INITIAL 0 
     LABEL "ID" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88
     BGCOLOR 7 FGCOLOR 11  NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY arbrw FOR 
      ExpenseType SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE arbrw
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS arbrw C-Win _STRUCTURED
  QUERY arbrw NO-LOCK DISPLAY
      ExpenseType.Id FORMAT ">>>9":U
      ExpenseType.Code FORMAT "x(8)":U WIDTH 16
      ExpenseType.Description FORMAT "x(250)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 80 BY 10.5
         BGCOLOR 15 FGCOLOR 4 FONT 10 ROW-HEIGHT-CHARS .66 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     arbrw AT ROW 1 COL 1 WIDGET-ID 200
     filID AT ROW 12.04 COL 18 COLON-ALIGNED WIDGET-ID 16
     filCode AT ROW 13.12 COL 18 COLON-ALIGNED WIDGET-ID 2
     filDescr AT ROW 14.19 COL 18 COLON-ALIGNED WIDGET-ID 4
     btnAdd AT ROW 15.54 COL 3 WIDGET-ID 6
     btnModify AT ROW 15.54 COL 17.29 WIDGET-ID 8
     btnDelete AT ROW 15.54 COL 31.72 WIDGET-ID 10
     btnSave AT ROW 15.54 COL 50 WIDGET-ID 12
     btnCancel AT ROW 15.54 COL 64.29 WIDGET-ID 14
     "*" VIEW-AS TEXT
          SIZE 5 BY .62 AT ROW 13.12 COL 37.29 WIDGET-ID 56
          FGCOLOR 12 
     "*" VIEW-AS TEXT
          SIZE 5 BY .62 AT ROW 14.19 COL 71 WIDGET-ID 58
          FGCOLOR 12 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80.14 BY 16
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
         TITLE              = "Areas"
         COLUMN             = 37
         ROW                = 6
         HEIGHT             = 16
         WIDTH              = 80.14
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 80.14
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 80.14
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
/* BROWSE-TAB arbrw 1 DEFAULT-FRAME */
/* SETTINGS FOR BUTTON btnCancel IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnSave IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN filCode IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN filDescr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN filID IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE arbrw
/* Query rebuild information for BROWSE arbrw
     _TblList          = "ics.ExpenseType"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   = ics.ExpenseType.Id
     _FldNameList[2]   > ics.ExpenseType.Code
"ExpenseType.Code" ? ? "character" ? ? ? ? ? ? no ? no no "16" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   = ics.ExpenseType.Description
     _Query            is OPENED
*/  /* BROWSE arbrw */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Areas */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit.
  IF THIS-PROCEDURE:PERSISTENT THEN  */ RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Areas */
DO:
  /* This event will close the window and terminate the procedure.  */
  MESSAGE "Confrm to close the window?" VIEW-AS ALERT-BOX INFO BUTTONS YES-NO UPDATE yn AS LOGICAL.
  IF yn = YES THEN
    DO:
      session_Window = session_Window - 1.
      APPLY "CLOSE":U TO THIS-PROCEDURE.
      RETURN NO-APPLY.
    END.
  ELSE
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME arbrw
&Scoped-define SELF-NAME arbrw
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL arbrw C-Win
ON VALUE-CHANGED OF arbrw IN FRAME DEFAULT-FRAME
DO:
    IF AVAILABLE ExpenseType THEN
        ASSIGN
        filID    = ID
        filCode  = Code
        filDescr = DESCRIPTION.
    DISPLAY filCode filDescr filID WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAdd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAdd C-Win
ON CHOOSE OF btnAdd IN FRAME DEFAULT-FRAME /* Add */
DO:
  ENABLE filCode filDescr btnSave btnCancel WITH FRAME {&FRAME-NAME}.
  DISABLE btnAdd btnDelete btnModify arbrw WITH FRAME {&FRAME-NAME}.
  
  FIND FIRST paramtrs WHERE NAME = "lastExpenseTypeId".
  IF AVAILABLE paramtrs THEN
      filID    = INT(val) + 1.

  filCode  = "".
  filDescr = "".
  addModify = "add".
  DISPLAY filCode filDescr filID WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCancel C-Win
ON CHOOSE OF btnCancel IN FRAME DEFAULT-FRAME /* Cancel */
DO:
  filCode  = "".
  filDescr = "".
  filID    = 0.

  DISABLE filCode filDescr filID btnSave btnCancel WITH FRAME {&FRAME-NAME}.
  ENABLE btnAdd btnDelete btnModify arbrw WITH FRAME {&FRAME-NAME}.

  OPEN QUERY arbrw FOR EACH ExpenseType BY ID.
  APPLY "VALUE-CHANGED":U TO arbrw.

  DISPLAY filCode filDescr filID WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDelete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDelete C-Win
ON CHOOSE OF btnDelete IN FRAME DEFAULT-FRAME /* Delete */
DO:
    MESSAGE "No Access."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
/*     IF filCode = "" THEN                                                                                  */
/*         MESSAGE "No records to delete." VIEW-AS ALERT-BOX WARNING BUTTONS OK .                            */
/*     ELSE                                                                                                  */
/*     DO:                                                                                                   */
/*         MESSAGE "Conferm to save the record?" VIEW-AS ALERT-BOX INFO BUTTONS YES-NO UPDATE yn AS LOGICAL. */
/*         IF yn THEN                                                                                        */
/*         DO:                                                                                               */
/*             FIND FIRST ExpenseType WHERE Code = filCode.                                                  */
/*             IF AVAILABLE ExpenseType THEN                                                                 */
/*                 DELETE ExpenseType.                                                                       */
/*                                                                                                           */
/*             OPEN QUERY arbrw FOR EACH ExpenseType BY ID.                                                  */
/*             APPLY "VALUE-CHANGED":U TO arbrw.                                                             */
/*         END.                                                                                              */
/*     END.                                                                                                  */
    

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnModify
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnModify C-Win
ON CHOOSE OF btnModify IN FRAME DEFAULT-FRAME /* Modify */
DO:
  ENABLE filCode filDescr filID btnSave btnCancel WITH FRAME {&FRAME-NAME}.
  DISABLE btnAdd btnDelete btnModify arbrw WITH FRAME {&FRAME-NAME}.
  addModify = "modify".
  ModifyVal = filCode.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSave C-Win
ON CHOOSE OF btnSave IN FRAME DEFAULT-FRAME /* Save */
DO:
    IF filCode = "" THEN
    DO:
        MESSAGE "Area Code cannot be a blank." VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
    END.
    IF LENGTH(filCode) < 3  THEN
    DO:
        MESSAGE "Area Code must have Three charactors." VIEW-AS ALERT-BOX ERROR BUTTONS OK .
        RETURN .
    END.
    IF filDescr = "" THEN
    DO:
        MESSAGE "Description cannot be a blank." VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN .
    END.

/********************************************save************************************************/

    IF addModify = "add" THEN
    DO:
        FIND FIRST ExpenseType WHERE CODE = filCode OR DESCRIPTION  = filDescr EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE ExpenseType THEN
            DO:
                MESSAGE "Area Code or Description already exists.1" VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                RETURN.
            END.
            ELSE IF NOT AVAILABLE ExpenseType THEN
            DO:
                CREATE ExpenseType.
                ID       = filID.
                Code = filCode.
                DESCRIPTION  = filDescr.
                .
                FIND FIRST paramtrs WHERE NAME = "lastExpenseTypeId".
                IF AVAILABLE paramtrs THEN
                    val    = STRING(INT(val) + 1).
                RELEASE paramtrs. 

            END.
    END.
    
    ELSE IF addModify = "modify" THEN
    DO:
        FOR EACH ExpenseType WHERE Code = filCode OR ExpenseType.DESCRIPTION  = filDescr.
            ModifyCnt = ModifyCnt + 1.
        END.
        IF ModifyCnt > 1 THEN
        DO:
            MESSAGE "Area Code or Description already exists.11" VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            ModifyCnt = 0.
            APPLY "ENTRY":U TO filCode.
            RETURN.
        END.
        ELSE IF ModifyCnt = 1 THEN
        DO:
            FIND FIRST ExpenseType WHERE ID = filID EXCLUSIVE-LOCK NO-ERROR.
                ASSIGN
                    ID       = filID
                    Code = filCode
                    ExpenseType.DESCRIPTION  = filDescr.
        END.
        
    END.

    RELEASE ExpenseType.

    DISABLE filCode filDescr filID btnSave btnCancel WITH FRAME {&FRAME-NAME}.
    ENABLE btnAdd btnDelete btnModify arbrw WITH FRAME {&FRAME-NAME}.

    OPEN QUERY arbrw FOR EACH ExpenseType BY ID.
    APPLY "VALUE-CHANGED":U TO arbrw.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME filCode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filCode C-Win
ON ANY-KEY OF filCode IN FRAME DEFAULT-FRAME /* Code */
DO:
  ASSIGN filCode.
  filCode = CAPS(filCode).
  DISPLAY filCode WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filCode C-Win
ON LEAVE OF filCode IN FRAME DEFAULT-FRAME /* Code */
DO:
  ASSIGN {&SELF-NAME}.
  

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filCode C-Win
ON RETURN OF filCode IN FRAME DEFAULT-FRAME /* Code */
DO:
  
    APPLY "CHOOSE":U TO btnSave.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filCode C-Win
ON VALUE-CHANGED OF filCode IN FRAME DEFAULT-FRAME /* Code */
DO:
  ASSIGN filCode.
  filCode = CAPS(filCode).
  DISPLAY filCode WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME filDescr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filDescr C-Win
ON LEAVE OF filDescr IN FRAME DEFAULT-FRAME /* Description */
DO:
    ASSIGN {&SELF-NAME}.
    filDescr = CAPS(SUBSTRING(filDescr,1,1)) + SUBSTRING(filDescr,2).
  DISPLAY filDescr WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filDescr C-Win
ON RETURN OF filDescr IN FRAME DEFAULT-FRAME /* Description */
DO:
  APPLY "CHOOSE":U TO btnSave.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filDescr C-Win
ON VALUE-CHANGED OF filDescr IN FRAME DEFAULT-FRAME /* Description */
DO:
    ASSIGN {&SELF-NAME}.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME filID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filID C-Win
ON ANY-KEY OF filID IN FRAME DEFAULT-FRAME /* ID */
DO:
  ASSIGN filCode.
  filCode = CAPS(filCode).
  DISPLAY filCode WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filID C-Win
ON LEAVE OF filID IN FRAME DEFAULT-FRAME /* ID */
DO:
  ASSIGN {&SELF-NAME}.
  

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filID C-Win
ON RETURN OF filID IN FRAME DEFAULT-FRAME /* ID */
DO:
  
    APPLY "CHOOSE":U TO btnSave.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filID C-Win
ON VALUE-CHANGED OF filID IN FRAME DEFAULT-FRAME /* ID */
DO:
  ASSIGN filCode.
  filCode = CAPS(filCode).
  DISPLAY filCode WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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

  session_Window = session_Window + 1.
  APPLY "VALUE-CHANGED":U TO arbrw.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  DISPLAY filID filCode filDescr 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE arbrw btnAdd btnModify btnDelete 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

