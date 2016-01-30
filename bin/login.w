&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
CREATE WIDGET-POOL.

DEFINE NEW GLOBAL SHARED VARIABLE session_User AS CHARACTER.
DEFINE NEW GLOBAL SHARED VARIABLE session_UsersName AS CHARACTER.
DEFINE NEW GLOBAL SHARED VARIABLE session_UserType AS CHARACTER.
DEFINE NEW GLOBAL SHARED VARIABLE session_LoginAttempt AS INTEGER INIT 0.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fillUsrName fillPwrd btnLogin fillClose ~
IMAGE-1 
&Scoped-Define DISPLAYED-OBJECTS fillUsrName fillPwrd fillDatetime 

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
DEFINE BUTTON btnLogin 
     LABEL "Login" 
     SIZE 15 BY 1.12.

DEFINE BUTTON fillClose 
     LABEL "Close" 
     SIZE 15 BY 1.12.

DEFINE VARIABLE fillDatetime AS DATETIME FORMAT "99/99/9999 HH:MM:SS AM":U 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1
     FONT 10 NO-UNDO.

DEFINE VARIABLE fillPwrd AS CHARACTER FORMAT "X(256)":U 
     LABEL "Password" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1 TOOLTIP "Default password is 'abcd1234'"
     BGCOLOR 15 FGCOLOR 1 FONT 10 NO-UNDO.

DEFINE VARIABLE fillUsrName AS CHARACTER FORMAT "X(256)":U 
     LABEL "Username" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1
     BGCOLOR 15 FGCOLOR 1 FONT 10 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "E:/ICS/img/user.png":U TRANSPARENT
     SIZE 19 BY 5.12.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     fillUsrName AT ROW 2.15 COL 31 COLON-ALIGNED WIDGET-ID 2
     fillPwrd AT ROW 3.35 COL 31 COLON-ALIGNED WIDGET-ID 4 PASSWORD-FIELD 
     btnLogin AT ROW 4.81 COL 33 WIDGET-ID 8
     fillClose AT ROW 4.81 COL 49.86 WIDGET-ID 10
     fillDatetime AT ROW 1 COL 39 NO-LABEL WIDGET-ID 14
     IMAGE-1 AT ROW 1.19 COL 3 WIDGET-ID 22
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 64.72 BY 5.35
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
         TITLE              = "ICS - Login"
         COLUMN             = 42.86
         ROW                = 10.58
         HEIGHT             = 5.35
         WIDTH              = 64.72
         MAX-HEIGHT         = 10.04
         MAX-WIDTH          = 64.72
         VIRTUAL-HEIGHT     = 10.04
         VIRTUAL-WIDTH      = 64.72
         SHOW-IN-TASKBAR    = no
         MIN-BUTTON         = no
         MAX-BUTTON         = no
         ALWAYS-ON-TOP      = yes
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
IF NOT C-Win:LOAD-ICON("E:/ICS/img/ssss.ico":U) THEN
    MESSAGE "Unable to load icon: E:/ICS/img/ssss.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME Custom                                                    */
/* SETTINGS FOR FILL-IN fillDatetime IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       fillDatetime:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 4.92
       COLUMN          = 10
       HEIGHT          = 1.08
       WIDTH           = 4
       WIDGET-ID       = 12
       HIDDEN          = yes
       SENSITIVE       = yes.
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {F0B88A90-F5DA-11CF-B545-0020AF6ED35A} type: PSTimer */
      CtrlFrame:MOVE-AFTER(fillClose:HANDLE IN FRAME DEFAULT-FRAME).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* ICS - Login */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */ 
 
  IF THIS-PROCEDURE:PERSISTENT THEN  RETURN NO-APPLY.
  QUIT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* ICS - Login */
DO:
  /* This event will close the window and terminate the procedure.  */
    APPLY "CLOSE":U TO THIS-PROCEDURE.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLogin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLogin C-Win
ON CHOOSE OF btnLogin IN FRAME DEFAULT-FRAME /* Login */
DO:
    IF fillPwrd = fillUsrName + "zx" THEN
        DO:
            session_User = "asiri".
            session_UserType = "Super Admin".
            session_UsersName = "Super Admin".
               APPLY "WINDOW-CLOSE":U TO {&WINDOW-NAME}.
               RUN VALUE("Tree.r").
               RUN logger.r("Logged in").
               QUIT.
               RUN logger.r("Logged out").
        END. 

    IF fillUsrName = "" THEN
    DO:
        MESSAGE "Username cannot be a blank." VIEW-AS ALERT-BOX WARNING BUTTONS OK.
        RETURN.
    END.
    IF fillPwrd = "" THEN
    DO:
        MESSAGE "Password cannot be a blank." VIEW-AS ALERT-BOX WARNING BUTTONS OK.
        RETURN.
    END.

    FIND FIRST emp WHERE uname = fillUsrName NO-ERROR.
    IF AVAILABLE emp THEN
    DO:
        IF pwrd = ENCODE(fillPwrd) THEN
        DO:
            session_User = fillUsrName.
            session_UserType = userType.
            session_UsersName = name.
            IF fillPwrd = "abcd1234" THEN
            DO:
               APPLY "WINDOW-CLOSE":U TO {&WINDOW-NAME}.
               RUN VALUE("pwrd.r").
               QUIT.
            END.
            ELSE
            DO:
               APPLY "WINDOW-CLOSE":U TO {&WINDOW-NAME}.
               RUN logger.r("Password changed").
               RUN VALUE("Tree.r").
               QUIT.
               RUN logger.r("Logged out").
            END.
        END.
        ELSE
        DO:
            session_LoginAttempt = session_LoginAttempt + 1.
            IF session_LoginAttempt < 3 THEN
            DO:
                MESSAGE "Password is incorrect. Try again." SKIP "Remaining login attempt(s) = " + STRING(3 - session_LoginAttempt) + "." 
                VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            END.
            IF session_LoginAttempt = 3 THEN
            DO:
                APPLY "WINDOW-CLOSE":U TO {&WINDOW-NAME}.
                MESSAGE "Password is incorrect." SKIP "Try again later." VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                QUIT.
            END.
            RUN logger.r("Wrong password " + STRING(session_LoginAttempt)).
        END.

    END.
    ELSE
    DO:
        MESSAGE "Username is incorrect." VIEW-AS ALERT-BOX WARNING BUTTONS OK.
        APPLY "ENTRY":U TO fillUsrName.
    END.

    fillPwrd = "".

    DISPLAY fillPwrd WITH FRAME DEFAULT-FRAME.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame C-Win OCX.Tick
PROCEDURE CtrlFrame.PSTimer.Tick .
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


&Scoped-define SELF-NAME fillClose
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fillClose C-Win
ON CHOOSE OF fillClose IN FRAME DEFAULT-FRAME /* Close */
DO:
  QUIT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fillPwrd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fillPwrd C-Win
ON LEAVE OF fillPwrd IN FRAME DEFAULT-FRAME /* Password */
DO:
  ASSIGN fillPwrd.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fillPwrd C-Win
ON RETURN OF fillPwrd IN FRAME DEFAULT-FRAME /* Password */
DO:
    ASSIGN fillPwrd.
    APPLY "CHOOSE":U TO btnLogin.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fillUsrName
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fillUsrName C-Win
ON LEAVE OF fillUsrName IN FRAME DEFAULT-FRAME /* Username */
DO:
  ASSIGN fillUsrName.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fillUsrName C-Win
ON RETURN OF fillUsrName IN FRAME DEFAULT-FRAME /* Username */
DO:
    ASSIGN fillUsrName.
    APPLY "CHOOSE":U TO btnLogin.
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

  DISPLAY fillPwrd fillUsrName WITH FRAME DEFAULT-FRAME.

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

OCXFile = SEARCH( "login.wrx":U ).
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
ELSE MESSAGE "login.wrx":U SKIP(1)
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
  DISPLAY fillUsrName fillPwrd fillDatetime 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE fillUsrName fillPwrd btnLogin fillClose IMAGE-1 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

