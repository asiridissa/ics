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
&Scoped-define INTERNAL-TABLES emp

/* Definitions for BROWSE arbrw                                         */
&Scoped-define FIELDS-IN-QUERY-arbrw emp.emp# emp.name emp.uname ~
emp.userType 
&Scoped-define ENABLED-FIELDS-IN-QUERY-arbrw 
&Scoped-define QUERY-STRING-arbrw FOR EACH emp NO-LOCK ~
    BY emp.emp# DESCENDING INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-arbrw OPEN QUERY arbrw FOR EACH emp NO-LOCK ~
    BY emp.emp# DESCENDING INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-arbrw emp
&Scoped-define FIRST-TABLE-IN-QUERY-arbrw emp


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-arbrw}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS arbrw btnAdd btnModify btnDelete 
&Scoped-Define DISPLAYED-OBJECTS filID filCode filDescr cmbUserCat 

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

DEFINE BUTTON btnResetPwrd 
     LABEL "Reset Password" 
     SIZE 17 BY 1.

DEFINE BUTTON btnSave 
     LABEL "Save" 
     SIZE 14 BY 1.

DEFINE VARIABLE cmbUserCat AS CHARACTER FORMAT "X(32)":U 
     LABEL "User Category" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 35 BY 1
     BGCOLOR 15 FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE filDescr AS CHARACTER 
     VIEW-AS EDITOR MAX-CHARS 100
     SIZE 35 BY 2.15
     BGCOLOR 15 FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE filCode AS CHARACTER FORMAT "X(32)":U 
     LABEL "Username" 
     VIEW-AS FILL-IN 
     SIZE 35 BY .88
     BGCOLOR 15 FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE filID AS INTEGER FORMAT ">>>,>>9":U INITIAL 0 
     LABEL "ID" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88
     BGCOLOR 7 FGCOLOR 11  NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY arbrw FOR 
      emp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE arbrw
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS arbrw C-Win _STRUCTURED
  QUERY arbrw NO-LOCK DISPLAY
      emp.emp# COLUMN-LABEL "Emp No" FORMAT "9999":U WIDTH 7
      emp.name COLUMN-LABEL "                                    Name" FORMAT "x(50)":U
      emp.uname COLUMN-LABEL "                   Username" FORMAT "x(32)":U
      emp.userType COLUMN-LABEL "                                                 User Group" FORMAT "x(20)":U
            WIDTH 31.43
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 101.57 BY 16.96
         BGCOLOR 15 FGCOLOR 4 FONT 10 ROW-HEIGHT-CHARS .66 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     arbrw AT ROW 1.08 COL 1.29 WIDGET-ID 200
     filID AT ROW 18.5 COL 7.43 COLON-ALIGNED WIDGET-ID 16
     filCode AT ROW 18.5 COL 60.14 COLON-ALIGNED WIDGET-ID 2
     filDescr AT ROW 19.77 COL 9.43 NO-LABEL WIDGET-ID 26
     cmbUserCat AT ROW 19.92 COL 60.14 COLON-ALIGNED WIDGET-ID 44
     btnAdd AT ROW 22.54 COL 2.86 WIDGET-ID 6
     btnModify AT ROW 22.54 COL 17.14 WIDGET-ID 8
     btnDelete AT ROW 22.54 COL 31.57 WIDGET-ID 10
     btnResetPwrd AT ROW 22.54 COL 54 WIDGET-ID 46
     btnSave AT ROW 22.54 COL 71.29 WIDGET-ID 12
     btnCancel AT ROW 22.54 COL 85.57 WIDGET-ID 14
     "*" VIEW-AS TEXT
          SIZE 3 BY .62 AT ROW 19.85 COL 45 WIDGET-ID 58
          FGCOLOR 12 
     "*" VIEW-AS TEXT
          SIZE 1.43 BY .62 AT ROW 19.85 COL 98.43 WIDGET-ID 62
          FGCOLOR 12 
     "*" VIEW-AS TEXT
          SIZE 1.43 BY .62 AT ROW 18.5 COL 98.43 WIDGET-ID 60
          FGCOLOR 12 
     "Name:" VIEW-AS TEXT
          SIZE 5.57 BY .62 AT ROW 19.81 COL 3.57 WIDGET-ID 28
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 102.14 BY 23.15
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
         TITLE              = "ICS - Users"
         COLUMN             = 39.43
         ROW                = 2.65
         HEIGHT             = 23.15
         WIDTH              = 102.14
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
/* BROWSE-TAB arbrw TEXT-2 DEFAULT-FRAME */
/* SETTINGS FOR BUTTON btnCancel IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnResetPwrd IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnSave IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX cmbUserCat IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       cmbUserCat:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN filCode IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR EDITOR filDescr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       filDescr:AUTO-INDENT IN FRAME DEFAULT-FRAME      = TRUE.

/* SETTINGS FOR FILL-IN filID IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE arbrw
/* Query rebuild information for BROWSE arbrw
     _TblList          = "ics.emp"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "ics.emp.emp#|no"
     _FldNameList[1]   > ics.emp.emp#
"emp.emp#" "Emp No" ? "integer" ? ? ? ? ? ? no ? no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ics.emp.name
"emp.name" "                                    Name" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ics.emp.uname
"emp.uname" "                   Username" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ics.emp.userType
"emp.userType" "                                                 User Group" "x(20)" "character" ? ? ? ? ? ? no ? no no "31.43" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE arbrw */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* ICS - Users */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. 
  IF THIS-PROCEDURE:PERSISTENT THEN  */ RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* ICS - Users */
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
    IF AVAILABLE emp THEN
        ASSIGN
        filID       = emp#
        filCode     = uname
        filDescr    = name
        cmbUserCat  = userType
        .
    DISPLAY filCode filDescr filID cmbUserCat WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAdd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAdd C-Win
ON CHOOSE OF btnAdd IN FRAME DEFAULT-FRAME /* Add */
DO:
  ENABLE filCode filDescr cmbUserCat btnSave btnCancel WITH FRAME {&FRAME-NAME}.
  DISABLE btnAdd btnDelete btnModify arbrw WITH FRAME {&FRAME-NAME}.
  
  FIND FIRST paramtrs WHERE NAME = "lastUserID".
  IF AVAILABLE paramtrs THEN
      filID    = INT(val) + 1.

  filCode    = "".
  filDescr   = "".
  cmbUserCat = "--Select Here--".
  addModify = "add".


  DISPLAY filCode filDescr filID cmbUserCat WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCancel C-Win
ON CHOOSE OF btnCancel IN FRAME DEFAULT-FRAME /* Cancel */
DO:

  DISABLE filCode filDescr btnResetPwrd cmbUserCat btnSave btnCancel WITH FRAME {&FRAME-NAME}.
  ENABLE btnAdd btnDelete btnModify arbrw WITH FRAME {&FRAME-NAME}.

  OPEN QUERY arbrw FOR EACH emp BY emp#.
  APPLY "VALUE-CHANGED":U TO arbrw.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDelete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDelete C-Win
ON CHOOSE OF btnDelete IN FRAME DEFAULT-FRAME /* Delete */
DO:
    IF filID = 1 THEN
    DO:
      MESSAGE "You cannot Delete this user." VIEW-AS ALERT-BOX WARNING BUTTONS OK.
      RETURN.
    END.
    IF filCode = "" THEN
        MESSAGE "No records to delete." VIEW-AS ALERT-BOX WARNING BUTTONS OK .
    ELSE
    DO:
        MESSAGE "Conferm to Delete the record?" VIEW-AS ALERT-BOX INFO BUTTONS YES-NO UPDATE yn AS LOGICAL.
        IF yn THEN
        DO:
            FIND FIRST emp WHERE emp.emp# = filID.
            IF AVAILABLE emp THEN
                DELETE emp.
            RELEASE emp.
    
            OPEN QUERY arbrw FOR EACH emp BY emp#.
            APPLY "VALUE-CHANGED":U TO arbrw.
        END.
    END.
    

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnModify
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnModify C-Win
ON CHOOSE OF btnModify IN FRAME DEFAULT-FRAME /* Modify */
DO:
  IF filID = 1 THEN
  DO:
      MESSAGE "You cannot Modify this user." VIEW-AS ALERT-BOX WARNING BUTTONS OK.
      RETURN.
  END.
  ENABLE filDescr cmbUserCat btnResetPwrd btnSave btnCancel WITH FRAME {&FRAME-NAME}.
  DISABLE btnAdd btnDelete btnModify arbrw WITH FRAME {&FRAME-NAME}.
  addModify = "modify".
  ModifyVal = filCode.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnResetPwrd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnResetPwrd C-Win
ON CHOOSE OF btnResetPwrd IN FRAME DEFAULT-FRAME /* Reset Password */
DO:
  MESSAGE "Conferm to reset the Password?"
      VIEW-AS ALERT-BOX INFO BUTTONS YES-NO UPDATE yn AS LOGICAL.
  IF yn = TRUE THEN
  DO:
      FIND FIRST emp WHERE uname = filCode EXCLUSIVE-LOCK NO-ERROR.
        IF AVAILABLE emp THEN
                        pwrd     = STRING(ENCODE("abcd1234")).
      IF NOT ERROR-STATUS:ERROR THEN
        MESSAGE "Password reset successful."
            VIEW-AS ALERT-BOX INFO BUTTONS OK .
      RUN logger.r("Password reset for "+ filDescr + " : " + filCode + " : " + STRING(filID)).
  END.

    DISABLE filCode filDescr btnResetPwrd btnSave btnCancel WITH FRAME {&FRAME-NAME}.
    ENABLE btnAdd btnDelete btnModify arbrw WITH FRAME {&FRAME-NAME}.
    
    DISPLAY cmbUserCat WITH FRAME {&FRAME-NAME}.
    
    OPEN QUERY arbrw FOR EACH emp BY emp#.
    APPLY "VALUE-CHANGED":U TO arbrw.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSave C-Win
ON CHOOSE OF btnSave IN FRAME DEFAULT-FRAME /* Save */
DO:
    IF filDescr = "" THEN
    DO:
        MESSAGE "Name cannot be a blank." VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN .
    END.
    IF filCode = "" THEN
    DO:
        MESSAGE "Username cannot be a blank." VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN .
    END.
    IF LENGTH(filCode) < 5 THEN
    DO:
        MESSAGE "Username must have atleast 5 characters." VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN .
    END.
    IF cmbUserCat = "--Select Here--" THEN
    DO:
        MESSAGE "User category cannot be a blank." VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN .
    END.

/*add*******************************************************************************************/

    IF addModify = "add" THEN
    DO:
        FIND FIRST emp WHERE uname = filCode EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE emp THEN
            DO:
                MESSAGE "Username already exists." VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                RETURN.
            END.
            ELSE IF NOT AVAILABLE emp THEN
            DO:
                MESSAGE "Conferm to save record?" VIEW-AS ALERT-BOX INFO BUTTONS YES-NO UPDATE yn AS LOGICAL.
                IF yn = TRUE THEN
                DO:
                    CREATE emp.
                        ASSIGN
                            emp#     = filID
                            uname    = filCode
                            emp.NAME = filDescr
                            pwrd     = STRING(ENCODE("abcd1234"))
                            userType = cmbUserCat
                            .
                    FIND FIRST paramtrs WHERE paramtrs.NAME = "lastUserID".
                    IF AVAILABLE paramtrs THEN
                        val    = STRING(INT(val) + 1).
                    RELEASE paramtrs. 
                END.
            END.
    END.
/*modify*****************************************************************************************************/
    ELSE IF addModify = "modify" THEN
    DO:
/*modify******************************************************validation Begins*******************************************************/
        


/*modify******************************************************validation Ends*******************************************************/
        FIND FIRST emp WHERE uname = filCode EXCLUSIVE-LOCK NO-ERROR.
        IF AVAILABLE emp THEN
        DO:
            MESSAGE "Conferm to save record?" VIEW-AS ALERT-BOX INFO BUTTONS YES-NO UPDATE yn1 AS LOGICAL.
                IF yn1 = TRUE THEN
                DO:
                    ASSIGN
                        emp#     = filID
                        uname    = filCode
                        emp.name = filDescr
                        userType = cmbUserCat
                        .
                END.
        END.
    END.

    RELEASE userCat.

    DISABLE filCode filDescr btnResetPwrd btnSave btnCancel WITH FRAME {&FRAME-NAME}.
    ENABLE btnAdd btnDelete btnModify arbrw WITH FRAME {&FRAME-NAME}.

    DISPLAY cmbUserCat WITH FRAME {&FRAME-NAME}.

    OPEN QUERY arbrw FOR EACH emp BY emp#.
    APPLY "VALUE-CHANGED":U TO arbrw.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cmbUserCat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbUserCat C-Win
ON VALUE-CHANGED OF cmbUserCat IN FRAME DEFAULT-FRAME /* User Category */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME filCode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filCode C-Win
ON LEAVE OF filCode IN FRAME DEFAULT-FRAME /* Username */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filCode C-Win
ON RETURN OF filCode IN FRAME DEFAULT-FRAME /* Username */
DO:
    APPLY "CHOOSE":U TO btnSave.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME filDescr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filDescr C-Win
ON LEAVE OF filDescr IN FRAME DEFAULT-FRAME
DO:
  ASSIGN {&SELF-NAME}.
  {&SELF-NAME} = CAPS(SUBSTRING({&SELF-NAME},1,1)) + SUBSTRING({&SELF-NAME},2).

  DISPLAY {&SELF-NAME} WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME filID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filID C-Win
ON LEAVE OF filID IN FRAME DEFAULT-FRAME /* ID */
DO:
  ASSIGN filID.
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

  cmbUserCat:ADD-LAST("--Select Here--").
  FOR EACH userCat NO-LOCK .
      cmbUserCat:ADD-LAST(userCat.descrip).
  END.

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
  DISPLAY filID filCode filDescr cmbUserCat 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE arbrw btnAdd btnModify btnDelete 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

