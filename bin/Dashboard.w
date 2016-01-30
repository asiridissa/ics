&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
CREATE WIDGET-POOL.

DEFINE SHARED VARIABLE session_User AS CHARACTER.

DEFINE VARIABLE calendr AS COM-HANDLE   NO-UNDO.
DEFINE VARIABLE calendrTo AS COM-HANDLE   NO-UNDO.

DEFINE VARIABLE fromDate AS DATE        NO-UNDO.
DEFINE VARIABLE toDate AS DATE        NO-UNDO.
DEFINE VARIABLE bilTol AS DECIMAL     NO-UNDO.
DEFINE VARIABLE ExpTol AS DECIMAL     NO-UNDO.
DEFINE VARIABLE DmgTol AS DECIMAL     NO-UNDO.

DEFINE TEMP-TABLE tt-area
    FIELD Id            AS INT
    FIELD Code          AS CHAR
    FIELD Description          AS CHAR
    .

DEFINE TEMP-TABLE tt-bill
    FIELD bill#            AS INT
    FIELD Area          AS INT
    FIELD Date          AS DATE
    FIELD Tol          AS DECIMAL
    FIELD Varience          AS INT
    FIELD Vehicle          AS INT
    .

DEFINE TEMP-TABLE tt-dates
    FIELD Dates          AS DATE
    .

DEFINE TEMP-TABLE tt-Veh
    FIELD Id          AS INT
    FIELD veh#          AS CHAR
    .

DEFINE TEMP-TABLE tt-receipts
    FIELD bill#            AS INT
    FIELD Expiery          AS INT
    FIELD Damage          AS INT
    FIELD Free          AS INT
    FIELD GR          AS INT
    .

DEFINE TEMP-TABLE tt-payments
    FIELD bill#            AS INT
    FIELD datePay       AS DATE
    FIELD PayMethod          AS CHAR
    FIELD Amount          AS DECIMAL
    .

DEFINE TEMP-TABLE tt-db
    FIELD DATE            AS DATE                 
/*     FIELD Area            AS CHAR */
    FIELD Vehicle            AS CHAR                 
    FIELD Expiery         AS DECIMAL                 
    FIELD Damage          AS DECIMAL                 
    FIELD Free            AS DECIMAL                 
    FIELD GR              AS DECIMAL                 
    FIELD Varience        AS DECIMAL                 
    FIELD Tol            AS DECIMAL         
              
    FIELD Cash            AS DECIMAL                 
    FIELD Credit          AS DECIMAL                 
    FIELD Cheque          AS DECIMAL                 
    FIELD Collection      AS DECIMAL     

    FIELD Expenses        AS DECIMAL                 
    FIELD CashIn          AS DECIMAL                 
    FIELD RealizedCheques AS DECIMAL                 
    FIELD Income          AS DECIMAL                 
    .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME charDEFAULT-FRAME
&Scoped-define BROWSE-NAME brw

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-db

/* Definitions for BROWSE brw                                           */
&Scoped-define FIELDS-IN-QUERY-brw DATE Vehicle /* Area */ Expiery Damage Free GR Varience Tol Cash Credit Cheque Collection Expenses CashIn RealizedCheques Income /* Unpaid */   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brw   
&Scoped-define SELF-NAME brw
&Scoped-define QUERY-STRING-brw FOR EACH tt-db BY DATE
&Scoped-define OPEN-QUERY-brw OPEN QUERY brw FOR EACH tt-db BY DATE.
&Scoped-define TABLES-IN-QUERY-brw tt-db
&Scoped-define FIRST-TABLE-IN-QUERY-brw tt-db


/* Definitions for FRAME charDEFAULT-FRAME                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-charDEFAULT-FRAME ~
    ~{&OPEN-QUERY-brw}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnView filBill filDates filVeh filArea ~
filReceipts filPayments brw 
&Scoped-Define DISPLAYED-OBJECTS filBill filDates filVeh filArea ~
filReceipts filPayments 

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

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnView 
     LABEL "View / Refresh" 
     SIZE 15 BY 1.12.

DEFINE VARIABLE filArea AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Area" 
     VIEW-AS FILL-IN 
     SIZE 11.72 BY .92 NO-UNDO.

DEFINE VARIABLE filBill AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Bill" 
     VIEW-AS FILL-IN 
     SIZE 11.72 BY .92 NO-UNDO.

DEFINE VARIABLE filDates AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Dates" 
     VIEW-AS FILL-IN 
     SIZE 11.72 BY .92 NO-UNDO.

DEFINE VARIABLE filPayments AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Payments" 
     VIEW-AS FILL-IN 
     SIZE 11.72 BY .92 NO-UNDO.

DEFINE VARIABLE filReceipts AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Receipts" 
     VIEW-AS FILL-IN 
     SIZE 11.72 BY .92 NO-UNDO.

DEFINE VARIABLE filVeh AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Veh" 
     VIEW-AS FILL-IN 
     SIZE 11.72 BY .92 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brw FOR 
      tt-db SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brw
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brw C-Win _FREEFORM
  QUERY brw DISPLAY
      DATE              FORMAT "99-99-9999"                 width 10 
        Vehicle         FORMAT "X(20)"        width 8 
/*         Area            FORMAT "X(20)"        width 10 */
        Expiery         FORMAT ">,>>>,>>9.99" width 10 
        Damage          FORMAT ">,>>>,>>9.99" width 10 
        Free            FORMAT ">,>>>,>>9.99" width 10 
        GR              FORMAT ">,>>>,>>9.99" width 10                                 
        Varience        FORMAT "->,>>9.99" width 8 
        Tol             FORMAT ">,>>>,>>9.99" width 10 
        
        Cash            FORMAT ">,>>>,>>9.99" width 10 
        Credit          FORMAT ">,>>>,>>9.99" width 10 
        Cheque          FORMAT ">,>>>,>>9.99" width 10 
        Collection      FORMAT ">,>>>,>>9.99" width 10 
                                         
        Expenses        FORMAT ">,>>>,>>9.99" width 10 
        CashIn          FORMAT ">,>>>,>>9.99" width 10 
        RealizedCheques FORMAT ">,>>>,>>9.99" width 10 
        Income          FORMAT ">,>>>,>>9.99" width 10 
        
        /*         Unpaid   FORMAT ">,>>>,>>9.99" LABEL "Credit" COLUMN-FGCOLOR 12 */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 143 BY 21.65
         FONT 10
         TITLE "Summary" ROW-HEIGHT-CHARS .46 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME charDEFAULT-FRAME
     btnView AT ROW 1.54 COL 42 WIDGET-ID 2
     filBill AT ROW 1.81 COL 63 COLON-ALIGNED WIDGET-ID 256
     filDates AT ROW 1.81 COL 82 COLON-ALIGNED WIDGET-ID 262
     filVeh AT ROW 1.81 COL 107 COLON-ALIGNED WIDGET-ID 264
     filArea AT ROW 3.69 COL 63 COLON-ALIGNED WIDGET-ID 266
     filReceipts AT ROW 3.69 COL 84 COLON-ALIGNED WIDGET-ID 268
     filPayments AT ROW 3.69 COL 107 COLON-ALIGNED WIDGET-ID 270
     brw AT ROW 5.85 COL 1.43 WIDGET-ID 200
     "To:" VIEW-AS TEXT
          SIZE 2.57 BY .62 AT ROW 3.77 COL 5.43 WIDGET-ID 274
     "From:" VIEW-AS TEXT
          SIZE 5 BY .62 AT ROW 2.5 COL 3.43 WIDGET-ID 254
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 143.86 BY 26.5
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
         TITLE              = "Summary"
         COLUMN             = 1.57
         ROW                = 1
         HEIGHT             = 26.5
         WIDTH              = 143.86
         MAX-HEIGHT         = 26.54
         MAX-WIDTH          = 144.43
         VIRTUAL-HEIGHT     = 26.54
         VIRTUAL-WIDTH      = 144.43
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
/* SETTINGS FOR FRAME charDEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB brw filPayments charDEFAULT-FRAME */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brw
/* Query rebuild information for BROWSE brw
     _START_FREEFORM
OPEN QUERY brw FOR EACH tt-db BY DATE.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE brw */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME charDEFAULT-FRAME:HANDLE
       ROW             = 2.42
       COLUMN          = 8.86
       HEIGHT          = .81
       WIDTH           = 23.72
       WIDGET-ID       = 232
       HIDDEN          = no
       SENSITIVE       = yes.

CREATE CONTROL-FRAME CtrlFrame-2 ASSIGN
       FRAME           = FRAME charDEFAULT-FRAME:HANDLE
       ROW             = 3.69
       COLUMN          = 8.86
       HEIGHT          = .81
       WIDTH           = 23.72
       WIDGET-ID       = 272
       HIDDEN          = no
       SENSITIVE       = yes.
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {20DD1B9E-87C4-11D1-8BE3-0000F8754DA1} type: DTPicker */
/* CtrlFrame-2 OCXINFO:CREATE-CONTROL from: {20DD1B9E-87C4-11D1-8BE3-0000F8754DA1} type: DTPicker */
      CtrlFrame:MOVE-AFTER(filVeh:HANDLE IN FRAME charDEFAULT-FRAME).
      CtrlFrame-2:MOVE-AFTER(CtrlFrame).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Summary */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit.
  IF THIS-PROCEDURE:PERSISTENT THEN  */ RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Summary */
DO:
  /* This event will close the window and terminate the procedure.  */
      APPLY "CLOSE":U TO THIS-PROCEDURE.
      RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnView
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnView C-Win
ON CHOOSE OF btnView IN FRAME charDEFAULT-FRAME /* View / Refresh */
DO:
   define variable tDATE            AS DATE        .
   define variable tArea            AS CHAR        .
   define variable tExpiery         AS DECIMAL     .
   define variable tDamage          AS DECIMAL     .
   define variable tFree            AS DECIMAL     .
   define variable tGR              AS DECIMAL     .
   define variable tVarience        AS DECIMAL     .
   define variable tTol            AS DECIMAL      .
   define variable tCash            AS DECIMAL     .
   define variable tCredit          AS DECIMAL     .
   define variable tCheque          AS DECIMAL     .
   define variable tCollection      AS DECIMAL     .
   define variable tExpenses        AS DECIMAL     .
   define variable tCashIn          AS DECIMAL     .
   define variable tRealizedCheques AS DECIMAL     .
   define variable tIncome          AS DECIMAL     .
   DEFINE VARIABLE cnt AS INTEGER     NO-UNDO.

    fromDate = calendr:VALUE.
    ToDate   = calendrTo:VALUE.
    RUN ttBindMonth.
/*     RUN ttBindCommon. */
/*     RUN ttBindDB.     */

/*     FOR EACH tt-dates BY tt-dates.DATE.                                                          */
/*         tDATE = tt-dates.DATE.                                                                   */
/*                                                                                                  */
/*         FOR EACH tt-Veh.                                                                         */
/*                                                                                                  */
/*                                                                                                  */
/*             FOR EACH tt-bill WHERE tt-bill.DATE = tt-dates.DATE AND tt-bill.Vehicle = tt-Veh.Id. */
/*                 ACCUMULATE tt-bill.Tol (TOTAL).                                                  */
/*                 ACCUMULATE tt-bill.Varience (TOTAL).                                             */
/*             END.                                                                                 */
/*         END.                                                                                     */
/*                                                                                                  */
/*         CREATE tt-db.                                .                                           */
/*             tt-db.DATE              = tDATE              .                                       */
/*             tt-db.Area              = tArea              .                                       */
/*             tt-db.Expiery           = tExpiery           .                                       */
/*             tt-db.Damage            = tDamage            .                                       */
/*             tt-db.Free              = tFree              .                                       */
/*             tt-db.GR                = tGR                .                                       */
/*             tt-db.Varience          = tVarience          .                                       */
/*             tt-db.Tol               = tTol               .                                       */
/*             tt-db.Cash              = tCash              .                                       */
/*             tt-db.Credit            = tCredit            .                                       */
/*             tt-db.Cheque            = tCheque            .                                       */
/*             tt-db.Collection        = tCollection        .                                       */
/*             tt-db.Expenses          = tExpenses          .                                       */
/*             tt-db.CashIn            = tCashIn            .                                       */
/*             tt-db.RealizedCheques   = tRealizedCheques   .                                       */
/*             tt-db.Income            = tIncome            .                                       */
/*                                                                                                  */
/*     END.                                                                                         */
    OPEN QUERY brw FOR EACH tt-db BY DATE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame C-Win OCX.Change
PROCEDURE CtrlFrame.DTPicker.Change .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/

/* APPLY "CHOOSE":U TO btnView IN FRAME DEFAULT-FRAME. */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame C-Win OCX.Click
PROCEDURE CtrlFrame.DTPicker.Click .
/* APPLY "CHOOSE":U TO btnView IN FRAME DEFAULT-FRAME. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame-2 C-Win OCX.Change
PROCEDURE CtrlFrame-2.DTPicker.Change .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/

/* APPLY "CHOOSE":U TO btnView IN FRAME DEFAULT-FRAME. */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame-2 C-Win OCX.Click
PROCEDURE CtrlFrame-2.DTPicker.Click .
/* APPLY "CHOOSE":U TO btnView IN FRAME DEFAULT-FRAME. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME filArea
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filArea C-Win
ON ENTRY OF filArea IN FRAME charDEFAULT-FRAME /* Area */
DO:
  
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filArea C-Win
ON LEAVE OF filArea IN FRAME charDEFAULT-FRAME /* Area */
DO:
  
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filArea C-Win
ON VALUE-CHANGED OF filArea IN FRAME charDEFAULT-FRAME /* Area */
DO:
  
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME filBill
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filBill C-Win
ON ENTRY OF filBill IN FRAME charDEFAULT-FRAME /* Bill */
DO:
  
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filBill C-Win
ON LEAVE OF filBill IN FRAME charDEFAULT-FRAME /* Bill */
DO:
  
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filBill C-Win
ON VALUE-CHANGED OF filBill IN FRAME charDEFAULT-FRAME /* Bill */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME filDates
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filDates C-Win
ON ENTRY OF filDates IN FRAME charDEFAULT-FRAME /* Dates */
DO:
  
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filDates C-Win
ON LEAVE OF filDates IN FRAME charDEFAULT-FRAME /* Dates */
DO:
  
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filDates C-Win
ON VALUE-CHANGED OF filDates IN FRAME charDEFAULT-FRAME /* Dates */
DO:
  
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME filPayments
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filPayments C-Win
ON ENTRY OF filPayments IN FRAME charDEFAULT-FRAME /* Payments */
DO:
  
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filPayments C-Win
ON LEAVE OF filPayments IN FRAME charDEFAULT-FRAME /* Payments */
DO:
  
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filPayments C-Win
ON VALUE-CHANGED OF filPayments IN FRAME charDEFAULT-FRAME /* Payments */
DO:
  
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME filReceipts
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filReceipts C-Win
ON ENTRY OF filReceipts IN FRAME charDEFAULT-FRAME /* Receipts */
DO:
  
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filReceipts C-Win
ON LEAVE OF filReceipts IN FRAME charDEFAULT-FRAME /* Receipts */
DO:
  
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filReceipts C-Win
ON VALUE-CHANGED OF filReceipts IN FRAME charDEFAULT-FRAME /* Receipts */
DO:
  
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME filVeh
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filVeh C-Win
ON ENTRY OF filVeh IN FRAME charDEFAULT-FRAME /* Veh */
DO:
  
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filVeh C-Win
ON LEAVE OF filVeh IN FRAME charDEFAULT-FRAME /* Veh */
DO:
  
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filVeh C-Win
ON VALUE-CHANGED OF filVeh IN FRAME charDEFAULT-FRAME /* Veh */
DO:
  
    ASSIGN {&SELF-NAME}.
END.

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

  calendr = chCtrlFrame:DTPicker.
  calendr:ENABLED = TRUE.
  calendr:VALUE = TODAY - 10.

  calendrTo = chCtrlFrame-2:DTPicker.
  calendrTo:ENABLED = TRUE.
  calendrTo:VALUE = TODAY - 10.

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

OCXFile = SEARCH( "Dashboard.wrx":U ).
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
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "Dashboard.wrx":U SKIP(1)
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
  DISPLAY filBill filDates filVeh filArea filReceipts filPayments 
      WITH FRAME charDEFAULT-FRAME IN WINDOW C-Win.
  ENABLE btnView filBill filDates filVeh filArea filReceipts filPayments brw 
      WITH FRAME charDEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-charDEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ttBindCommon C-Win 
PROCEDURE ttBindCommon :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*     FOR EACH tt-bill.                                                */
/*         FIND FIRST tt-area WHERE tt-area.Id = tt-bill.Area NO-ERROR. */
/*         IF NOT AVAILABLE tt-area THEN                                */
/*         DO:                                                          */
/*             FIND FIRST area WHERE area.ID = tt-bill.Area NO-ERROR.   */
/*             IF AVAILABLE area THEN                                   */
/*             DO:                                                      */
/*                 CREATE tt-area.                                      */
/*                    tt-area.Id              = area.ID         .       */
/*                    tt-area.Code            = area.areaCode    .      */
/*                    tt-area.Description     = area.descrip  .         */
/*             END.                                                     */
/*                                                                      */
/*             filArea = filArea + 1.                                   */
/*         END.                                                         */
/*     END.                                                             */

    FOR EACH tt-bill.
        FOR EACH recipts WHERE recipts.bill# = tt-bill.bill# NO-LOCK,
            EACH itms WHERE recipts.item# =  itms.itmID NO-LOCK .
        CREATE tt-receipts.
            tt-receipts.bill#       = recipts.bill# * itms.unitPriceS  .
            tt-receipts.Expiery     = recipts.expP * itms.unitPriceS   .
            tt-receipts.Damage      = recipts.damP * itms.unitPriceS    .
            IF  recipts.ItmDiscount = 100 THEN
                tt-receipts.Free        = recipts.pieses * itms.unitPriceS  .
            ELSE 
                tt-receipts.Free = 0.
            tt-receipts.GR          = recipts.GRST * itms.unitPriceS    .

            filReceipts = filReceipts + 1.
        END.
    END.

    FOR EACH tt-bill.
        FOR EACH Payments WHERE Payments.bill# = tt-bill.bill# AND Payments.stat = YES.
            CREATE tt-payments.
               tt-payments.bill#          = Payments.bill#       .
               tt-payments.datePay        = Payments.datePay        .
               tt-payments.PayMethod      = Payments.PayMethod   .
               tt-payments.Amount         = Payments.Amount      .

               filPayments = filPayments + 1.
        END.
    END.

    DISPLAY filArea filReceipts filPayments WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ttBindCustom C-Win 
PROCEDURE ttBindCustom :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    FOR EACH bills WHERE bills.bilDate >= fromDate AND bills.bilDate <= toDate NO-LOCK,
        EACH area WHERE area.ID = bills.areaCode NO-LOCK,
        EACH vehical WHERE vehical.ID = bills.vehNo NO-LOCK.
        CREATE tt-bill.
           tt-bill.bill#     = bills.bill#.
           tt-bill.Area      = bills.areaCode.
           tt-bill.Date      = bills.bilDate     .
           tt-bill.Tol       = bills.tol         .
           tt-bill.Varience  = bills.varience    .
           tt-bill.Vehicle   = bills.vehNo.

       FIND FIRST tt-db WHERE tt-db.DATE = bills.bilDate AND tt-db.Vehicle = vehical.veh# 
/*            AND tt-db.Area = area.areaCode */
           NO-ERROR.
       IF NOT AVAILABLE tt-db THEN
       DO:
           CREATE tt-db.
            tt-db.DATE = bills.bilDate .
            tt-db.Vehicle = vehical.veh# .
/*             tt-db.Area = area.areaCode. */
       END.

       FIND FIRST tt-dates WHERE tt-dates.DATE = bills.bilDate NO-ERROR.
       IF NOT AVAILABLE tt-dates THEN
       DO:
           CREATE tt-dates.
               tt-dates.DATE = bills.bilDate.
       END.

       FIND FIRST tt-Veh WHERE tt-Veh.Id = bills.vehNo NO-ERROR.
       IF NOT AVAILABLE tt-Veh THEN
       DO:
           CREATE tt-Veh.
               tt-Veh.Id = vehical.ID.
               tt-Veh.veh# = vehical.veh#.
       END.
    END.

    FOR EACH tt-bill.
        filBill = filBill + 1.
    END.
    FOR EACH tt-dates.
        filDates = filDates + 1.
    END.
    FOR EACH tt-Veh.
        filVeh = filVeh + 1.
    END.

    DISPLAY filBill filDates filVeh WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ttBindDB C-Win 
PROCEDURE ttBindDB :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FOR EACH tt-db.
    FOR EACH tt-bill WHERE tt-bill.DATE = tt-db.DATE,
        EACH tt-area WHERE tt-area.Id = tt-bill.Area 
/*             AND tt-area.CODE = tt-db.Area */
        .
        ACCUMULATE tt-bill.Tol (TOTAL).
        ACCUMULATE tt-bill.Varience (TOTAL).
    END.

    ASSIGN 
        tt-db.Varience = ACCUM TOTAL tt-bill.Varience
        tt-db.Tol = ACCUM TOTAL tt-bill.Tol
        .

/*     MESSAGE ACCUM TOTAL tt-bill.Tol VIEW-AS ALERT-BOX INFO BUTTONS OK.      */
/*     MESSAGE ACCUM TOTAL tt-bill.Varience VIEW-AS ALERT-BOX INFO BUTTONS OK. */
END.

FOR EACH tt-receipts,
    EACH tt-bill WHERE tt-bill.bill# = tt-receipts.bill#,
    EACH tt-area WHERE tt-area.Id = tt-bill.Area
    .
    FIND FIRST tt-db WHERE tt-db.DATE = tt-bill.DATE 
/*             AND tt-db.Area = tt-area.CODE */
        NO-ERROR.
    IF AVAILABLE tt-db THEN
    DO:
        tt-db.Expiery = tt-db.Expiery + (tt-receipts.Expiery).
        tt-db.Damage = tt-db.Damage + (tt-receipts.Damage).
        tt-db.Free = tt-db.Free + (tt-receipts.Free).
        tt-db.GR = tt-db.GR + (tt-receipts.GR).
    END.
END.




END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ttBindMonth C-Win 
PROCEDURE ttBindMonth :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    FOR EACH bills WHERE month(bills.bilDate) = month(fromDate) NO-LOCK,
        EACH area WHERE area.ID = bills.areaCode NO-LOCK,
        EACH vehical WHERE vehical.ID = bills.vehNo NO-LOCK.
        CREATE tt-bill.
           tt-bill.bill#     = bills.bill#.
           tt-bill.Area      = bills.areaCode.
           tt-bill.Date      = bills.bilDate     .
           tt-bill.Tol       = bills.tol         .
           tt-bill.Varience  = bills.varience    .
           tt-bill.Vehicle   = bills.vehNo.

       FIND FIRST tt-db WHERE tt-db.DATE = bills.bilDate AND tt-db.Vehicle = vehical.veh# 
/*            AND tt-db.Area = area.areaCode */
           NO-ERROR.
       IF NOT AVAILABLE tt-db THEN
       DO:
           CREATE tt-db.
            tt-db.DATE = bills.bilDate .
            tt-db.Vehicle = vehical.veh# .
/*             tt-db.Area = area.areaCode. */
       END.

       FIND FIRST tt-dates WHERE tt-dates.DATE = bills.bilDate NO-ERROR.
       IF NOT AVAILABLE tt-dates THEN
       DO:
           CREATE tt-dates.
               tt-dates.DATE = bills.bilDate.
       END.

       FIND FIRST tt-Veh WHERE tt-Veh.Id = bills.vehNo NO-ERROR.
       IF NOT AVAILABLE tt-Veh THEN
       DO:
           CREATE tt-Veh.
               tt-Veh.Id = vehical.ID.
               tt-Veh.veh# = vehical.veh#.
       END.
    END.

    FOR EACH tt-bill.
        filBill = filBill + 1.
    END.
    FOR EACH tt-dates.
        filDates = filDates + 1.
    END.
    FOR EACH tt-Veh.
        filVeh = filVeh + 1.
    END.

    DISPLAY filBill filDates filVeh WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

