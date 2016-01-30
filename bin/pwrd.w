&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */




/* Local Variable Definitions ---                                       */

/* DEFINE SHARED VARIABLE Appserver1 AS HANDLE. */

DEFINE SHARED VARIABLE session_User AS CHARACTER.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fillOldPword fillPwrdNew fillPwrdNew2 ~
btnLogin fillClose 
&Scoped-Define DISPLAYED-OBJECTS fillOldPword fillPwrdNew fillPwrdNew2 ~
fillUsrName 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnLogin 
     LABEL "Save" 
     SIZE 15 BY 1.12.

DEFINE BUTTON fillClose 
     LABEL "Close" 
     SIZE 15 BY 1.12.

DEFINE VARIABLE fillOldPword AS CHARACTER FORMAT "X(256)":U 
     LABEL "Old Password" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1
     BGCOLOR 15 FGCOLOR 1 FONT 10 NO-UNDO.

DEFINE VARIABLE fillPwrdNew AS CHARACTER FORMAT "X(256)":U 
     LABEL "New Password" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1
     BGCOLOR 15 FGCOLOR 1 FONT 10 NO-UNDO.

DEFINE VARIABLE fillPwrdNew2 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Repeat Password" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1
     BGCOLOR 15 FGCOLOR 1 FONT 10 NO-UNDO.

DEFINE VARIABLE fillUsrName AS CHARACTER FORMAT "X(256)":U 
     LABEL "Username" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1
     BGCOLOR 15 FGCOLOR 1 FONT 10 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     fillOldPword AT ROW 2.69 COL 16.72 COLON-ALIGNED WIDGET-ID 16 PASSWORD-FIELD 
     fillPwrdNew AT ROW 4.77 COL 16.72 COLON-ALIGNED WIDGET-ID 22 PASSWORD-FIELD 
     fillPwrdNew2 AT ROW 6.04 COL 16.72 COLON-ALIGNED WIDGET-ID 18 PASSWORD-FIELD 
     btnLogin AT ROW 7.31 COL 20.14 WIDGET-ID 8
     fillClose AT ROW 7.31 COL 35.86 WIDGET-ID 10
     fillUsrName AT ROW 1.38 COL 16.72 COLON-ALIGNED WIDGET-ID 2 NO-TAB-STOP 
     "Default is 'abcd1234'" VIEW-AS TEXT
          SIZE 26 BY .62 AT ROW 3.85 COL 18.86 WIDGET-ID 20
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 53.29 BY 7.73
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
         TITLE              = "Set New Password"
         COLUMN             = 49.86
         ROW                = 9.5
         HEIGHT             = 7.73
         WIDTH              = 52.86
         MAX-HEIGHT         = 10.04
         MAX-WIDTH          = 60
         VIRTUAL-HEIGHT     = 10.04
         VIRTUAL-WIDTH      = 60
         CONTROL-BOX        = no
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
/* SETTINGS FOR FILL-IN fillUsrName IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Set New Password */
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
ON WINDOW-CLOSE OF C-Win /* Set New Password */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLogin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLogin C-Win
ON CHOOSE OF btnLogin IN FRAME DEFAULT-FRAME /* Save */
DO:
    IF fillOldPword = "" THEN
    DO:
        MESSAGE "Old Password cannot be a blank." VIEW-AS ALERT-BOX WARNING BUTTONS OK.
        RUN emptyF.
    END.
    IF fillPwrdNew = "" THEN
    DO:
        MESSAGE "New Password cannot be a blank." VIEW-AS ALERT-BOX WARNING BUTTONS OK.
        RUN emptyF.
    END.
    IF LENGTH(fillPwrdNew) < 8 THEN
    DO:
        MESSAGE "Password must have atleast 8 characters.." VIEW-AS ALERT-BOX WARNING BUTTONS OK.
        RUN emptyF.
    END.
    IF fillPwrdNew2 = "" THEN
    DO:
        MESSAGE "Repeat Password cannot be a blank." VIEW-AS ALERT-BOX WARNING BUTTONS OK.
        RUN emptyF.
    END.
    IF fillPwrdNew <> fillPwrdNew2 THEN
    DO:
        MESSAGE "Password mismatch." VIEW-AS ALERT-BOX WARNING BUTTONS OK.
        RUN emptyF.
    END.
    IF fillPwrdNew2 = "abcd1234" THEN
    DO:
        MESSAGE "You cannot set 'abcd1234' as your password." VIEW-AS ALERT-BOX WARNING BUTTONS OK.
        RUN emptyF.
    END.

    FIND FIRST emp WHERE uname = session_User EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE emp AND fillOldPword = "abcd1234" THEN
    DO:
        MESSAGE "Password saved successfully." VIEW-AS ALERT-BOX INFO BUTTONS OK.
        ASSIGN pwrd = ENCODE(fillPwrdNew2).
        RELEASE emp.
        APPLY "WINDOW-CLOSE":U TO {&WINDOW-NAME}.
/*         RUN logger.r("Password changed to" + STRING(ENCODE(fillPwrdNew2))). */
        RUN VALUE("tree.r").
        QUIT.
    END.
    ELSE
    DO:
        MESSAGE "Old Password is incorrect." VIEW-AS ALERT-BOX WARNING BUTTONS OK.
        RUN emptyF.
        RUN logger.r("Password change attempt wrong current password").
    END.

END.

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


&Scoped-define SELF-NAME fillOldPword
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fillOldPword C-Win
ON LEAVE OF fillOldPword IN FRAME DEFAULT-FRAME /* Old Password */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fillPwrdNew
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fillPwrdNew C-Win
ON LEAVE OF fillPwrdNew IN FRAME DEFAULT-FRAME /* New Password */
DO:
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fillPwrdNew2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fillPwrdNew2 C-Win
ON LEAVE OF fillPwrdNew2 IN FRAME DEFAULT-FRAME /* Repeat Password */
DO:
    ASSIGN fillPwrdNew2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fillPwrdNew2 C-Win
ON RETURN OF fillPwrdNew2 IN FRAME DEFAULT-FRAME /* Repeat Password */
DO:
    ASSIGN fillPwrdNew2.
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

  fillUsrName = session_User.
  DISPLAY fillUsrName WITH FRAME DEFAULT-FRAME.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE emptyF C-Win 
PROCEDURE emptyF :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
fillOldPword = "".
    fillPwrdNew  = "".
    fillPwrdNew2 = "".

    DISPLAY fillOldPword fillPwrdNew fillPwrdNew2 WITH FRAME DEFAULT-FRAME.
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
  DISPLAY fillOldPword fillPwrdNew fillPwrdNew2 fillUsrName 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE fillOldPword fillPwrdNew fillPwrdNew2 btnLogin fillClose 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

