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

DEFINE SHARED VARIABLE session_User AS CHARACTER.
DEFINE SHARED VARIABLE session_Window AS INT.
/* Local Variable Definitions ---                                       */

DEFINE VARIABLE calendr AS COM-HANDLE   NO-UNDO.
DEFINE VARIABLE dateFrom AS COM-HANDLE   NO-UNDO.
DEFINE VARIABLE dateTo AS COM-HANDLE   NO-UNDO.

DEFINE VARIABLE addModify AS CHARACTER   NO-UNDO.
DEFINE VARIABLE ModifyVal AS CHARACTER   NO-UNDO.

DEFINE VARIABLE FromDate AS DATE        NO-UNDO.
DEFINE VARIABLE ToDate AS DATE        NO-UNDO.

DEFINE TEMP-TABLE ttExpense LIKE Expense.
DEFINE TEMP-TABLE ttemp LIKE emp.
DEFINE TEMP-TABLE ttExpenseType LIKE ExpenseType.

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
&Scoped-define INTERNAL-TABLES ttExpense ExpenseType emp

/* Definitions for BROWSE arbrw                                         */
&Scoped-define FIELDS-IN-QUERY-arbrw emp.name ExpenseType.Code ttExpense.Date ttExpense.Amount ttExpense.Note   
&Scoped-define ENABLED-FIELDS-IN-QUERY-arbrw   
&Scoped-define SELF-NAME arbrw
&Scoped-define QUERY-STRING-arbrw FOR EACH ttExpense NO-LOCK, ~
             EACH ExpenseType WHERE ttExpense.ExpenseTypeId = ExpenseType.Id NO-LOCK, ~
             EACH emp WHERE ttExpense.Employee# = emp.emp# NO-LOCK     BY ttExpense.Date DESCENDING      BY ttExpense.Id DESCENDING INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-arbrw OPEN QUERY {&SELF-NAME} FOR EACH ttExpense NO-LOCK, ~
             EACH ExpenseType WHERE ttExpense.ExpenseTypeId = ExpenseType.Id NO-LOCK, ~
             EACH emp WHERE ttExpense.Employee# = emp.emp# NO-LOCK     BY ttExpense.Date DESCENDING      BY ttExpense.Id DESCENDING INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-arbrw ttExpense ExpenseType emp
&Scoped-define FIRST-TABLE-IN-QUERY-arbrw ttExpense
&Scoped-define SECOND-TABLE-IN-QUERY-arbrw ExpenseType
&Scoped-define THIRD-TABLE-IN-QUERY-arbrw emp


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS radTimePeriod btnView btnPrint btnAdd ~
btnModify btnDelete cmbUserCat cmbExpenseType arbrw RECT-1 RECT-4 
&Scoped-Define DISPLAYED-OBJECTS radTimePeriod filID cmbUserCat ~
cmbExpenseType fillAmount fillNote 

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
DEFINE VARIABLE CtrlFrame-2 AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame-2 AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE CtrlFrame-3 AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame-3 AS COMPONENT-HANDLE NO-UNDO.

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

DEFINE BUTTON btnPrint 
     LABEL "Print" 
     SIZE 14 BY 1.

DEFINE BUTTON btnSave 
     LABEL "Save" 
     SIZE 14 BY 1.

DEFINE BUTTON btnView 
     LABEL "View" 
     SIZE 14 BY 1.

DEFINE VARIABLE cmbExpenseType AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "Expense Type" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "--Select Here--",0
     DROP-DOWN-LIST
     SIZE 44.43 BY 1
     BGCOLOR 15 FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE cmbUserCat AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "User" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "--Select Here--",0
     DROP-DOWN-LIST
     SIZE 44.29 BY 1
     BGCOLOR 15 FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE filID AS INTEGER FORMAT ">>>>>9":U INITIAL 0 
     LABEL "ID" 
     VIEW-AS FILL-IN 
     SIZE 10.14 BY .88
     BGCOLOR 7 FGCOLOR 11  NO-UNDO.

DEFINE VARIABLE fillAmount AS DECIMAL FORMAT ">>>,>>9.99":U INITIAL 0 
     LABEL "Amount" 
     VIEW-AS FILL-IN 
     SIZE 34 BY .88
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE fillNote AS CHARACTER FORMAT "X(200)":U 
     LABEL "Note" 
     VIEW-AS FILL-IN 
     SIZE 48 BY 2.15
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE radTimePeriod AS CHARACTER INITIAL "Daily" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Daily", "Daily",
"Monthly", "Monthly",
"Yearly", "Yearly",
"Custom", "Custom"
     SIZE 36 BY 1.08 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL  GROUP-BOX  ROUNDED 
     SIZE 138.57 BY 1.65.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL  GROUP-BOX  ROUNDED 
     SIZE 38 BY 1.65.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY arbrw FOR 
      ttExpense, 
      ExpenseType, 
      emp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE arbrw
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS arbrw C-Win _FREEFORM
  QUERY arbrw NO-LOCK DISPLAY
      emp.name COLUMN-LABEL "Employee" FORMAT "x(50)":U WIDTH 30
      ExpenseType.Code FORMAT "x(8)":U WIDTH 20
      ttExpense.Date FORMAT "99/99/9999":U WIDTH 12
      ttExpense.Amount FORMAT ">>>,>>>9.99":U WIDTH 15
      ttExpense.Note FORMAT "x(200)":U WIDTH 55.57
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 138 BY 19
         FONT 10
         TITLE "Expenses" ROW-HEIGHT-CHARS .65 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     radTimePeriod AT ROW 1.54 COL 65 NO-LABEL WIDGET-ID 264
     btnView AT ROW 1.54 COL 106 WIDGET-ID 278
     btnPrint AT ROW 1.54 COL 121.72 WIDGET-ID 234
     btnAdd AT ROW 26.46 COL 15 WIDGET-ID 6
     filID AT ROW 22.42 COL 24.86 COLON-ALIGNED WIDGET-ID 16
     btnModify AT ROW 26.46 COL 29.29 WIDGET-ID 8
     btnDelete AT ROW 26.46 COL 43.72 WIDGET-ID 10
     cmbUserCat AT ROW 23.62 COL 24.72 COLON-ALIGNED WIDGET-ID 44
     cmbExpenseType AT ROW 24.81 COL 24.57 COLON-ALIGNED WIDGET-ID 238
     fillAmount AT ROW 22.38 COL 79 COLON-ALIGNED WIDGET-ID 236
     fillNote AT ROW 23.73 COL 79 COLON-ALIGNED WIDGET-ID 240
     btnSave AT ROW 26.46 COL 91 WIDGET-ID 12
     btnCancel AT ROW 26.46 COL 105.29 WIDGET-ID 14
     arbrw AT ROW 2.88 COL 1.43 WIDGET-ID 200
     "To :" VIEW-AS TEXT
          SIZE 4 BY .62 AT ROW 1.69 COL 34 WIDGET-ID 286
     "From :" VIEW-AS TEXT
          SIZE 6 BY .62 AT ROW 1.65 COL 3.29 WIDGET-ID 284
     "Date" VIEW-AS TEXT
          SIZE 4 BY .58 AT ROW 22.5 COL 42 WIDGET-ID 242
     RECT-1 AT ROW 1.23 COL 1 WIDGET-ID 260
     RECT-4 AT ROW 1.23 COL 101.57 WIDGET-ID 276
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 138.57 BY 26.62
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
         COLUMN             = 3
         ROW                = 1.31
         HEIGHT             = 26.62
         WIDTH              = 138.57
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
   FRAME-NAME Custom                                                    */
/* BROWSE-TAB arbrw btnCancel DEFAULT-FRAME */
ASSIGN 
       arbrw:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE.

/* SETTINGS FOR BUTTON btnCancel IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       btnDelete:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON btnSave IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       cmbExpenseType:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       cmbUserCat:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN filID IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fillAmount IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fillNote IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE arbrw
/* Query rebuild information for BROWSE arbrw
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttExpense NO-LOCK,
      EACH ExpenseType WHERE ttExpense.ExpenseTypeId = ExpenseType.Id NO-LOCK,
      EACH emp WHERE ttExpense.Employee# = emp.emp# NO-LOCK
    BY ttExpense.Date DESCENDING
     BY ttExpense.Id DESCENDING INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _TblOptList       = ",,"
     _OrdList          = "ics.Expense.Date|no,ics.Expense.Id|no"
     _JoinCode[2]      = "Expense.ExpenseTypeId = ExpenseType.Id"
     _JoinCode[3]      = "Expense.Employee# = emp.emp#"
     _Query            is NOT OPENED
*/  /* BROWSE arbrw */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame-3 ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 1.65
       COLUMN          = 9.29
       HEIGHT          = .81
       WIDTH           = 23
       WIDGET-ID       = 280
       HIDDEN          = no
       SENSITIVE       = yes.

CREATE CONTROL-FRAME CtrlFrame-2 ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 1.62
       COLUMN          = 38
       HEIGHT          = .81
       WIDTH           = 23
       WIDGET-ID       = 282
       HIDDEN          = no
       SENSITIVE       = yes.

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 22.42
       COLUMN          = 47
       HEIGHT          = .81
       WIDTH           = 23.72
       WIDGET-ID       = 232
       HIDDEN          = no
       SENSITIVE       = yes.
/* CtrlFrame-3 OCXINFO:CREATE-CONTROL from: {20DD1B9E-87C4-11D1-8BE3-0000F8754DA1} type: DTPickerFrom */
/* CtrlFrame-2 OCXINFO:CREATE-CONTROL from: {20DD1B9E-87C4-11D1-8BE3-0000F8754DA1} type: DTPickerTo */
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {20DD1B9E-87C4-11D1-8BE3-0000F8754DA1} type: DTPicker */
      CtrlFrame-2:MOVE-BEFORE(radTimePeriod:HANDLE IN FRAME DEFAULT-FRAME).
      CtrlFrame:MOVE-AFTER(btnDelete:HANDLE IN FRAME DEFAULT-FRAME).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


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
ON LEFT-MOUSE-DBLCLICK OF arbrw IN FRAME DEFAULT-FRAME /* Expenses */
DO:
/*    IF AVAILABLE ttExpense THEN                                                            */
/*         ASSIGN                                                                            */
/*         filID       = ttExpense.id                                                        */
/*         cmbUserCat  = ttExpense.Employee#                                                 */
/*         cmbExpenseType = ttExpense.ExpenseTypeId                                          */
/*         fillAmount = ttExpense.Amount                                                     */
/*         calendr:VALUE = ttExpense.Date                                                    */
/*         fillNote = ttExpense.Note                                                         */
/*         .                                                                                 */
/*     DISPLAY filID cmbUserCat cmbExpenseType fillAmount fillNote WITH FRAME {&FRAME-NAME}. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAdd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAdd C-Win
ON CHOOSE OF btnAdd IN FRAME DEFAULT-FRAME /* Add */
DO:
  ENABLE fillNote cmbUserCat cmbExpenseType fillAmount btnSave btnCancel WITH FRAME {&FRAME-NAME}.
  calendr:ENABLED = TRUE.
  DISABLE btnAdd btnDelete btnModify arbrw WITH FRAME {&FRAME-NAME}.
  
  FIND FIRST paramtrs WHERE NAME = "lastExpenseId".
  IF AVAILABLE paramtrs THEN
      filID    = INT(val) + 1.

  cmbUserCat = 0.
  cmbExpenseType = 0.
  fillAmount = 0.
  fillNote = "".
  calendr:VALUE = TODAY - 1.

  APPLY "CHOOSE":U TO cmbUserCat.

  addModify = "add".

  DISPLAY fillNote filID cmbUserCat cmbExpenseType fillAmount WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCancel C-Win
ON CHOOSE OF btnCancel IN FRAME DEFAULT-FRAME /* Cancel */
DO:

  DISABLE fillNote fillAmount btnSave btnCancel WITH FRAME {&FRAME-NAME}.
  ENABLE btnAdd btnDelete btnModify arbrw WITH FRAME {&FRAME-NAME}.
  calendr:ENABLED = FALSE.

  RUN ttShow.
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
    MESSAGE "Conferm to Delete the record?" VIEW-AS ALERT-BOX INFO BUTTONS YES-NO UPDATE yn AS LOGICAL.
    IF yn THEN
    DO:
        FIND FIRST Expense WHERE id = filID.
        IF AVAILABLE Expense THEN
            DELETE Expense.
        RELEASE Expense.

        RUN ttShow.
        APPLY "VALUE-CHANGED":U TO arbrw.
    END.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnModify
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnModify C-Win
ON CHOOSE OF btnModify IN FRAME DEFAULT-FRAME /* Modify */
DO:
  
  ENABLE fillNote cmbUserCat cmbExpenseType fillAmount btnSave btnCancel WITH FRAME {&FRAME-NAME}.
  calendr:ENABLED = TRUE.
  DISABLE btnAdd btnDelete btnModify arbrw WITH FRAME {&FRAME-NAME}.
  addModify = "modify".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPrint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPrint C-Win
ON CHOOSE OF btnPrint IN FRAME DEFAULT-FRAME /* Print */
DO:
    FIND FIRST ttExpense NO-LOCK NO-ERROR.
    IF NOT AVAILABLE ttExpense THEN
        DO:
            MESSAGE "No records available to print." VIEW-AS ALERT-BOX INFO BUTTONS OK.
            RETURN.
        END.
    RELEASE ttExpense.
    
    {&SELF-NAME}:LABEL = "Working..".
    
    RUN print.
    
    {&SELF-NAME}:LABEL = "Print".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSave C-Win
ON CHOOSE OF btnSave IN FRAME DEFAULT-FRAME /* Save */
DO:
    IF cmbUserCat = 0 THEN
    DO:
        MESSAGE "Employee cannot be a blank." VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN .
    END.
    IF cmbExpenseType = 0 THEN
    DO:
        MESSAGE "Expense Type cannot be a blank." VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN .
    END.
    IF fillAmount = 0 THEN
    DO:
        MESSAGE "Amount cannot be 0." VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN .
    END.

/*add*******************************************************************************************/

    IF addModify = "add" THEN
    DO:
        FIND FIRST Expense WHERE id = filID EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE Expense THEN
            DO:
                MESSAGE "Expense already exists." VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                RETURN.
            END.
            ELSE IF NOT AVAILABLE Expense THEN
            DO:
                MESSAGE "Conferm to save record?" VIEW-AS ALERT-BOX INFO BUTTONS YES-NO UPDATE yn AS LOGICAL.
                IF yn = TRUE THEN
                DO:
                    CREATE Expense.
                        ASSIGN
                            id     = filID
                            Expense.Employee# = cmbUserCat
                            ics.Expense.ExpenseTypeid = cmbExpenseType
                            Expense.Amount = fillAmount
                            Expense.DATE = DATE(calendr:VALUE)
                            Expense.Note = fillNote
                            .
                    FIND FIRST paramtrs WHERE paramtrs.NAME = "lastExpenseId".
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
        FIND FIRST Expense WHERE Id = filID EXCLUSIVE-LOCK NO-ERROR.
        IF AVAILABLE Expense THEN
        DO:
            MESSAGE "Conferm to save record?" VIEW-AS ALERT-BOX INFO BUTTONS YES-NO UPDATE yn1 AS LOGICAL.
                IF yn1 = TRUE THEN
                DO:
                    ASSIGN
                        id     = filID
                        Expense.Employee# = cmbUserCat
                        ics.Expense.ExpenseTypeid = cmbExpenseType
                        Expense.Amount = fillAmount
                        Expense.DATE = DATE(calendr:VALUE)
                        Expense.Note = fillNote
                        .
                END.
        END.
    END.


    cmbUserCat = 0.
    cmbExpenseType = 0.

    DISABLE fillNote fillAmount btnSave btnCancel WITH FRAME {&FRAME-NAME}.
    ENABLE btnAdd btnDelete btnModify arbrw WITH FRAME {&FRAME-NAME}.

    calendr:ENABLED = TRUE.
    DISPLAY fillNote cmbUserCat cmbExpenseType fillAmount WITH FRAME {&FRAME-NAME}.

    RUN ttShow.
    APPLY "VALUE-CHANGED":U TO arbrw.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnView
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnView C-Win
ON CHOOSE OF btnView IN FRAME DEFAULT-FRAME /* View */
DO:
    {&SELF-NAME}:LABEL = "Working".
    FromDate = dateFrom:VALUE.
    ToDate = dateTo:VALUE.

    RUN ttShow.

    {&SELF-NAME}:LABEL = "View".

    APPLY "VALUE-CHANGED":U TO arbrw.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cmbExpenseType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbExpenseType C-Win
ON VALUE-CHANGED OF cmbExpenseType IN FRAME DEFAULT-FRAME /* Expense Type */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cmbUserCat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbUserCat C-Win
ON VALUE-CHANGED OF cmbUserCat IN FRAME DEFAULT-FRAME /* User */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame C-Win OCX.Change
PROCEDURE CtrlFrame.DTPicker.Change .
/*   RUN QueryLDUNLD. */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame-3 C-Win OCX.Change
PROCEDURE CtrlFrame-3.DTPickerFrom.Change .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/

/* dateFrom = calendrFrom:VALUE. */

END PROCEDURE.

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


&Scoped-define SELF-NAME fillAmount
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fillAmount C-Win
ON LEAVE OF fillAmount IN FRAME DEFAULT-FRAME /* Amount */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fillAmount C-Win
ON RETURN OF fillAmount IN FRAME DEFAULT-FRAME /* Amount */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fillAmount C-Win
ON VALUE-CHANGED OF fillAmount IN FRAME DEFAULT-FRAME /* Amount */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fillNote
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fillNote C-Win
ON LEAVE OF fillNote IN FRAME DEFAULT-FRAME /* Note */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fillNote C-Win
ON RETURN OF fillNote IN FRAME DEFAULT-FRAME /* Note */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fillNote C-Win
ON VALUE-CHANGED OF fillNote IN FRAME DEFAULT-FRAME /* Note */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME radTimePeriod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL radTimePeriod C-Win
ON VALUE-CHANGED OF radTimePeriod IN FRAME DEFAULT-FRAME
DO:
  ASSIGN {&SELF-NAME}.
  IF {&SELF-NAME} = "Custom" THEN
      dateTo:ENABLED = TRUE.
  ELSE
      dateTo:ENABLED = FALSE.
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

  FOR EACH emp  NO-LOCK .
      cmbUserCat:ADD-LAST(emp.NAME,emp.emp#).
  END.

  FOR EACH ics.ExpenseType  NO-LOCK .
      cmbExpenseType:ADD-LAST(ExpenseType.Code,ExpenseType.Id).
  END.

  calendr = chCtrlFrame:DTPicker.
  calendr:ENABLED = FALSE.
  calendr:VALUE = TODAY - 1.

  dateFrom = chCtrlFrame-3:DTPickerFrom.
  dateFrom:VALUE = TODAY - 1.
  
  dateTo = chCtrlFrame-2:DTPickerTo.
  dateTo:ENABLED = FALSE.
  dateTo:VALUE = TODAY - 1.

  APPLY "VALUE-CHANGED":U TO arbrw.

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

OCXFile = SEARCH( "Expense.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chCtrlFrame = CtrlFrame:COM-HANDLE
    UIB_S = chCtrlFrame:LoadControls( OCXFile, "CtrlFrame":U)
    CtrlFrame:NAME = "CtrlFrame":U
    chCtrlFrame-2 = CtrlFrame-2:COM-HANDLE
    UIB_S = chCtrlFrame-2:LoadControls( OCXFile, "CtrlFrame-2":U)
    CtrlFrame-2:NAME = "CtrlFrame-2":U
    chCtrlFrame-3 = CtrlFrame-3:COM-HANDLE
    UIB_S = chCtrlFrame-3:LoadControls( OCXFile, "CtrlFrame-3":U)
    CtrlFrame-3:NAME = "CtrlFrame-3":U
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "Expense.wrx":U SKIP(1)
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
  DISPLAY radTimePeriod filID cmbUserCat cmbExpenseType fillAmount fillNote 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE radTimePeriod btnView btnPrint btnAdd btnModify btnDelete cmbUserCat 
         cmbExpenseType arbrw RECT-1 RECT-4 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE print C-Win 
PROCEDURE print :
DEFINE VARIABLE period AS CHARACTER   NO-UNDO.
    
    OUTPUT TO VALUE("print\Expense.txt").
    
    CASE radTimePeriod:
            WHEN "Custom" THEN
                period = "From : " + string(dateFrom:VALUE,"99/99/9999") + "  To : " + string(dateTo:VALUE,"99/99/9999").
            WHEN "Daily" THEN
                period = "Date : " + STRING((dateFrom:VALUE),"99/99/9999").
            WHEN "Monthly" THEN
                period = "Month : " + STRING(MONTH(dateFrom:VALUE)) + " - " + STRING(YEAR(dateFrom:VALUE)).
            WHEN "Yearly" THEN
                period = "Year : " + STRING(YEAR(dateFrom:VALUE)).
        END CASE.
    
        PUT UNFORMAT "|" + period + "||By user : " + session_User SKIP.
        PUT UNFORMAT "|Employee|Type|Date|Amount|Note" SKIP.
    
    EMPTY TEMP-TABLE ttemp.
    EMPTY TEMP-TABLE ttExpenseType.

    FOR EACH ttExpense NO-LOCK.
        FIND FIRST ttemp WHERE ttemp.emp# = ttExpense.Employee# NO-ERROR.
        IF NOT AVAILABLE ttemp THEN
        DO:      
            FIND FIRST emp WHERE emp.emp# = ttExpense.Employee# NO-ERROR.
            IF AVAILABLE emp THEN
            DO: 
            CREATE ttemp.
                ttemp.emp#       = emp.emp#       .
                ttemp.name       = emp.name       .
                ttemp.pwrd       = emp.pwrd       .
                ttemp.uname      = emp.uname      .
                ttemp.userType   = emp.userType   .
            END.
        END.

        FIND FIRST ttExpenseType WHERE ttExpenseType.Id = ttExpense.ExpenseTypeId NO-ERROR.
        IF NOT AVAILABLE ttExpenseType THEN
        DO:      
            FIND FIRST ExpenseType WHERE ExpenseType.Id = ttExpense.ExpenseTypeId NO-ERROR.
            IF AVAILABLE ExpenseType THEN
            DO: 
            CREATE ttExpenseType.
                ttExpenseType.Id           = ExpenseType.Id           .
                ttExpenseType.Description  = ExpenseType.Description  .
                ttExpenseType.Code         = ExpenseType.Code         .
            END.
        END.
    END.

    DEFINE VARIABLE tolSum AS DECIMAL        NO-UNDO.
    DEFINE VARIABLE IsInlineName AS LOGICAL     NO-UNDO.
    DEFINE VARIABLE IsInlineType AS LOGICAL     NO-UNDO.

    FOR EACH ttemp BY ttemp.name.
        PUT UNFORMAT "|" + ttemp.NAME.
        IsInlineName = YES.

        DEFINE VARIABLE empSum AS DECIMAL     NO-UNDO.
        FOR EACH ttExpenseType.
            FIND FIRST ttExpense WHERE ttExpenseType.Id = ttExpense.ExpenseTypeId AND ttemp.emp# = ttExpense.Employee# NO-ERROR.
            IF AVAILABLE ttExpense THEN 
            DO:
                DEFINE VARIABLE pre AS CHARACTER   NO-UNDO.
                IF IsInlineName THEN pre = "|".
                ELSE pre = "||".
                PUT UNFORMAT pre + ttExpenseType.Code + " - " + ttExpenseType.DESCRIPTION.
                IsInlineType = YES.
                IsInlineName = NO.

                FOR EACH ttExpense WHERE ttExpenseType.Id = ttExpense.ExpenseTypeId AND ttemp.emp# = ttExpense.Employee#.
                    IF IsInlineType THEN pre = "|".
                    ELSE pre = "|||".

                    PUT UNFORMAT pre + STRING(ttExpense.DATE,"99/99/9999") + "|" + STRING(ttExpense.Amount) + "|" + ttExpense.Note  SKIP.
                    ACCUMULATE ttExpense.Amount (TOTAL).

                    IsInlineType = NO.
                END.
                
                PUT UNFORMAT "||Type Total||" + STRING(ACCUM TOTAL ttExpense.Amount) SKIP.    
                empSum = empSum + ACCUM TOTAL ttExpense.Amount.
            END.
        END.
        PUT UNFORMAT "||Sub Total||" + STRING(empSum) SKIP.    
        PUT UNFORMAT "~n". 
        tolSum = tolSum + empSum.
    END.
    PUT UNFORMAT "|Total|||" + STRING(tolSum) SKIP.    

    OUTPUT CLOSE.
    DOS SILENT START VALUE("print\Expense.bat").
    DOS SILENT START excel VALUE("print\Expense.xlsx").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ttShow C-Win 
PROCEDURE ttShow :
IF radTimePeriod = "Daily" THEN
        ToDate = FromDate.
    IF radTimePeriod = "Monthly" THEN
    DO:
        FromDate = DATE(MONTH(FromDate),01,YEAR(FromDate)).
        ToDate   = ADD-INTERVAL(FromDate,1,"months") - 1.
    END.
    IF radTimePeriod = "Yearly" THEN
    DO:
        FromDate = DATE(1,1,YEAR(FromDate)).
        ToDate   = ADD-INTERVAL(FromDate,1,"years") - 1.
    END.     

    EMPTY TEMP-TABLE ttExpense.
              
    FOR EACH Expense WHERE DATE(Expense.DATE) >= FromDate AND Date(Expense.DATE) <= ToDate NO-LOCK,
        EACH ExpenseType WHERE Expense.ExpenseTypeId = ExpenseType.Id NO-LOCK,
        EACH emp WHERE Expense.Employee# = emp.emp# NO-LOCK
        BY Expense.Date DESCENDING.
        CREATE ttExpense.
            ttExpense.Id              = Expense.Id            .  
            ttExpense.ExpenseTypeId   = Expense.ExpenseTypeId .  
            ttExpense.Employee#       = Expense.Employee#     .  
            ttExpense.Date            = Expense.Date          .  
            ttExpense.Amount          = Expense.Amount        .  
            ttExpense.Note            = Expense.Note          .  
    END.

    IF cmbUserCat <> 0 THEN DO:
        FOR EACH ttExpense WHERE ttExpense.Employee# <> cmbUserCat.
            DELETE ttExpense.
        END.
    END.

    IF cmbExpenseType <> 0 THEN DO:
        FOR EACH ttExpense WHERE ttExpense.ExpenseTypeId <> cmbExpenseType.
            DELETE ttExpense.
        END.
    END.

    OPEN QUERY arbrw FOR 
        EACH ttExpense NO-LOCK,
        EACH ExpenseType WHERE ttExpense.ExpenseTypeId = ExpenseType.Id NO-LOCK,
        EACH emp WHERE ttExpense.Employee# = emp.emp# NO-LOCK
        BY ttExpense.Date DESCENDING
        BY ttExpense.Id DESCENDING INDEXED-REPOSITION.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

