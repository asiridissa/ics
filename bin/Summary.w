&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
DEFINE SHARED VARIABLE session_User AS CHARACTER.

DEFINE VARIABLE calendrFrom AS COM-HANDLE   NO-UNDO.
DEFINE VARIABLE calendrTo AS COM-HANDLE   NO-UNDO.
DEFINE VARIABLE dateFrom AS DATE        NO-UNDO.
DEFINE VARIABLE dateTo AS DATE        NO-UNDO.


DEFINE TEMP-TABLE tt-db
    FIELD DATE            AS DATE                 
    FIELD VehId            AS INT
/*     FIELD Vehicle            AS CHAR */
    FIELD Expiery         AS DECIMAL                 
    FIELD Damage          AS DECIMAL                 
    FIELD Free            AS DECIMAL                 
    FIELD GR              AS DECIMAL                 
    FIELD Varience        AS DECIMAL                 
    FIELD Tol            AS DECIMAL         
              
    FIELD Cash            AS DECIMAL                 
    FIELD Credit          AS DECIMAL                 
    FIELD Cheque          AS DECIMAL                 
    FIELD ChequesIn          AS DECIMAL                 
/*     FIELD Collection      AS DECIMAL */

    FIELD Income          AS DECIMAL                 
    FIELD Expenses        AS DECIMAL                 
/*     FIELD CashIn          AS DECIMAL */
    FIELD RealizedCheques AS DECIMAL                 
    FIELD GrossIncome          AS DECIMAL                 
    .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BROWSE-3

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-db

/* Definitions for BROWSE BROWSE-3                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-3 DATE Expiery Damage Free GR Varience Tol Cash Credit ChequesIn Income Expenses GrossIncome   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-3   
&Scoped-define SELF-NAME BROWSE-3
&Scoped-define QUERY-STRING-BROWSE-3 FOR EACH tt-db
&Scoped-define OPEN-QUERY-BROWSE-3 OPEN QUERY {&SELF-NAME} FOR EACH tt-db.
&Scoped-define TABLES-IN-QUERY-BROWSE-3 tt-db
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-3 tt-db


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BROWSE-3}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-4 radTimePeriod btnView btnPrint ~
BROWSE-3 
&Scoped-Define DISPLAYED-OBJECTS radTimePeriod 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD zero C-Win 
FUNCTION zero RETURNS CHARACTER
    
  ( val AS DECIMAL)  FORWARD.

/* _UIB-CODE-BLOCK-END */
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
DEFINE BUTTON btnPrint 
     LABEL "Print" 
     SIZE 14 BY 1.

DEFINE BUTTON btnView 
     LABEL "View" 
     SIZE 14 BY 1.

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
     SIZE 143.43 BY 1.65.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL  GROUP-BOX  ROUNDED 
     SIZE 38 BY 1.65.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-3 FOR 
      tt-db SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-3 C-Win _FREEFORM
  QUERY BROWSE-3 DISPLAY
      DATE              FORMAT "99-99-9999"                 width 10    LABEL "Date"
      Expiery         FORMAT ">,>>>,>>9.99" width 7 LABEL "Exp"
      Damage          FORMAT ">,>>>,>>9.99" width 7 LABEL "Dam"
      Free            FORMAT ">,>>>,>>9.99" width 7 
      GR              FORMAT ">,>>>,>>9.99" width 7                                 
      Varience        FORMAT "->,>>9.99" width 7    LABEL "Vari"
      Tol             FORMAT ">,>>>,>>9.99" width 10 LABEL "Bills"
      Cash            FORMAT ">,>>>,>>9.99" width 10
      Credit          FORMAT ">,>>>,>>9.99" width 10
      ChequesIn          FORMAT ">,>>>,>>9.99" width 10 LABEL "Cheque"
      Income          FORMAT ">,>>>,>>9.99" width 12
      Expenses        FORMAT ">,>>>,>>9.99" width 12 LABEL "Expense"
      GrossIncome        FORMAT ">,>>>,>>9.99" width 12 LABEL "Balance"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 144 BY 20.58 ROW-HEIGHT-CHARS .58 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     radTimePeriod AT ROW 1.54 COL 65 NO-LABEL WIDGET-ID 264
     btnView AT ROW 1.54 COL 112 WIDGET-ID 14
     btnPrint AT ROW 1.54 COL 129 WIDGET-ID 234
     BROWSE-3 AT ROW 3.15 COL 1.14 WIDGET-ID 200
     "To :" VIEW-AS TEXT
          SIZE 4 BY .62 AT ROW 1.62 COL 34 WIDGET-ID 18
     "From :" VIEW-AS TEXT
          SIZE 6 BY .62 AT ROW 1.65 COL 3.29 WIDGET-ID 16
     RECT-1 AT ROW 1.23 COL 1.57 WIDGET-ID 260
     RECT-4 AT ROW 1.23 COL 107 WIDGET-ID 276
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 144.29 BY 22.77 WIDGET-ID 100.


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
         COLUMN             = 1.29
         ROW                = 2.65
         HEIGHT             = 22.77
         WIDTH              = 144.29
         MAX-HEIGHT         = 22.77
         MAX-WIDTH          = 144.29
         VIRTUAL-HEIGHT     = 22.77
         VIRTUAL-WIDTH      = 144.29
         RESIZE             = yes
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
/* BROWSE-TAB BROWSE-3 btnPrint DEFAULT-FRAME */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-3
/* Query rebuild information for BROWSE BROWSE-3
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-db.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-3 */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame-2 ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 1.62
       COLUMN          = 38
       HEIGHT          = .81
       WIDTH           = 23
       WIDGET-ID       = 10
       HIDDEN          = no
       SENSITIVE       = yes.

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 1.65
       COLUMN          = 9.29
       HEIGHT          = .81
       WIDTH           = 23
       WIDGET-ID       = 8
       HIDDEN          = no
       SENSITIVE       = yes.
/* CtrlFrame-2 OCXINFO:CREATE-CONTROL from: {20DD1B9E-87C4-11D1-8BE3-0000F8754DA1} type: DTPickerTo */
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {20DD1B9E-87C4-11D1-8BE3-0000F8754DA1} type: DTPickerFrom */
      CtrlFrame-2:MOVE-AFTER(btnPrint:HANDLE IN FRAME DEFAULT-FRAME).
      CtrlFrame:MOVE-AFTER(CtrlFrame-2).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Summary */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
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


&Scoped-define SELF-NAME btnPrint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPrint C-Win
ON CHOOSE OF btnPrint IN FRAME DEFAULT-FRAME /* Print */
DO:
    DEFINE VARIABLE veh AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE period AS CHARACTER   NO-UNDO.

    FIND FIRST tt-db NO-LOCK NO-ERROR.
    IF NOT AVAILABLE tt-db THEN
        DO:
            MESSAGE "No records available to print." VIEW-AS ALERT-BOX INFO BUTTONS OK.
            RETURN.
        END.
    RELEASE tt-db.

    {&SELF-NAME}:LABEL = "Working..".

    MESSAGE "Confirm to print?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE yn AS LOGICAL.
    IF yn = YES THEN
    DO:
        DEFINE VARIABLE tempCount AS INTEGER     NO-UNDO.

        OUTPUT TO VALUE("print\Summary.txt").

        APPLY "VALUE-CHANGED":U TO radTimePeriod IN FRAME DEFAULT-FRAME. 

            CASE radTimePeriod:
                WHEN "Custom" THEN
                    period = "From : " + string(calendrFrom:VALUE,"99/99/9999") + "  To : " + string(calendrTo:VALUE,"99/99/9999").
                WHEN "Daily" THEN
                    period = "Date : " + STRING((calendrFrom:VALUE),"99/99/9999").
                WHEN "Monthly" THEN
                    period = "Month : " + STRING(MONTH(calendrFrom:VALUE)) + " - " + STRING(YEAR(calendrFrom:VALUE)).
                WHEN "Yearly" THEN
                    period = "Year : " + STRING(YEAR(calendrFrom:VALUE)).
            END CASE.

            PUT UNFORMAT "|" + period + "|||||||By user : " + session_User SKIP.
            PUT UNFORMAT "|Date|Expiery|Damage|Free|GR|Varience|Bill Tol|Cash|Credit|Cheque|Income|Expenses|Balance|" SKIP.

            tempCount = 1.

            FOR EACH tt-db.
               PUT UNFORMAT STRING( tempCount) + "|" .
               PUT UNFORMAT STRING( DATE        ) + "|" .
               PUT UNFORMAT STRING(zero(Expiery     )) + "|" .
               PUT UNFORMAT STRING(zero(Damage      )) + "|" .
               PUT UNFORMAT STRING(zero(Free        )) + "|" .
               PUT UNFORMAT STRING(zero(GR          )) + "|".
               PUT UNFORMAT STRING(zero(Varience    )) + "|".
               PUT UNFORMAT STRING(zero(Tol         )) + "|".
               PUT UNFORMAT STRING(zero(Cash        )) + "|".
               PUT UNFORMAT STRING(zero(Credit      )) + "|".
               PUT UNFORMAT STRING(zero(ChequesIn   )) + "|".
               PUT UNFORMAT STRING(zero(Income      )) + "|".
               PUT UNFORMAT STRING(zero(Expenses    )) + "|".
               PUT UNFORMAT STRING(zero(GrossIncome )) + "|"SKIP.
               tempCount = tempCount + 1.

               accumulate Expiery      (total).
               accumulate Damage       (total).
               accumulate Free         (total).
               accumulate GR           (total).
               accumulate Varience     (total).
               accumulate Tol          (total).
               accumulate Cash         (total).
               accumulate Credit       (total).
               accumulate ChequesIn    (total).
               accumulate Income       (total).
               accumulate Expenses     (total).
               accumulate GrossIncome  (total).
            END.

            PUT UNFORMAT "~n".

            PUT UNFORMAT "|Total|" .
            PUT UNFORMAT STRING(accum total Expiery     ) + "|" .   
            PUT UNFORMAT STRING(accum total Damage      ) + "|" .   
            PUT UNFORMAT STRING(accum total Free        ) + "|" .   
            PUT UNFORMAT STRING(accum total GR          ) + "|".    
            PUT UNFORMAT STRING(accum total Varience    ) + "|".    
            PUT UNFORMAT STRING(accum total Tol         ) + "|".    
            PUT UNFORMAT STRING(accum total Cash        ) + "|".    
            PUT UNFORMAT STRING(accum total Credit      ) + "|".    
            PUT UNFORMAT STRING(accum total ChequesIn   ) + "|".    
            PUT UNFORMAT STRING(accum total Income      ) + "|".    
            PUT UNFORMAT STRING(accum total Expenses    ) + "|".    
            PUT UNFORMAT STRING(accum total GrossIncome ) + "|"SKIP.

        OUTPUT CLOSE.


        DOS SILENT START VALUE("print\Summary.bat").

        DOS SILENT START excel VALUE("print\Summary.xlsx").
    END.

    {&SELF-NAME}:LABEL = "Print".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnView
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnView C-Win
ON CHOOSE OF btnView IN FRAME DEFAULT-FRAME /* View */
DO:
    {&SELF-NAME}:LABEL = "Working".
    dateFrom = calendrFrom:VALUE.
    dateTo = calendrTo:VALUE.

    FOR EACH tt-db.
        DELETE tt-db.
    END.

    RUN ttBind.
    {&SELF-NAME}:LABEL = "View".

    OPEN QUERY BROWSE-3 FOR EACH tt-db BY tt-db.DATE DESC.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame C-Win OCX.Change
PROCEDURE CtrlFrame.DTPickerFrom.Change .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/

dateFrom = calendrFrom:VALUE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME radTimePeriod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL radTimePeriod C-Win
ON VALUE-CHANGED OF radTimePeriod IN FRAME DEFAULT-FRAME
DO:
  ASSIGN {&SELF-NAME}.
  IF {&SELF-NAME} = "Custom" THEN
      calendrTo:ENABLED = TRUE.
  ELSE
      calendrTo:ENABLED = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-3
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

  calendrFrom = chCtrlFrame:DTPickerFrom.
  calendrFrom:ENABLED = TRUE.
/*   calendrFrom:VALUE = 9/25/2015. */

  calendrTo = chCtrlFrame-2:DTPickerTo.
  calendrTo:ENABLED = FALSE.
/*   calendrTo:VALUE = 10/1/2015. */


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

OCXFile = SEARCH( "Summary.wrx":U ).
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
ELSE MESSAGE "Summary.wrx":U SKIP(1)
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
  DISPLAY radTimePeriod 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-1 RECT-4 radTimePeriod btnView btnPrint BROWSE-3 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ttBind C-Win 
PROCEDURE ttBind :
DEFINE VARIABLE bilCnt AS INTEGER     NO-UNDO.
DEFINE VARIABLE lineCnt AS INTEGER     NO-UNDO.

IF radTimePeriod = "Daily" THEN
    dateTo = dateFrom.
IF radTimePeriod = "Monthly" THEN
DO:
    dateFrom = DATE(MONTH(dateFrom),01,YEAR(dateFrom)).
    dateTo   = ADD-INTERVAL(dateFrom,1,"months") - 1.
END.
IF radTimePeriod = "Yearly" THEN
DO:
    dateFrom = DATE(1,1,YEAR(dateFrom)).
    dateTo   = ADD-INTERVAL(dateFrom,1,"years") - 1.
END.

FOR EACH bills WHERE bills.bilDate >= dateFrom AND bills.bilDate <= dateTo BY bills.bilDate DESC.
    FIND FIRST tt-db WHERE tt-db.DATE = bills.bilDate /*AND tt-db.VehId = bills.vehNo*/ NO-ERROR.
    IF NOT AVAILABLE tt-db THEN
    DO:
        CREATE tt-db.
            tt-db.VehId = bills.vehNo.
            tt-db.DATE = bills.bilDate.
            tt-db.Varience = tt-db.Varience + bills.Varience.
            tt-db.Tol      = tt-db.Tol + bills.Tol.
    END.

    FOR EACH recipts WHERE recipts.bill# = bills.bill#.
        DEFINE VARIABLE price AS DECIMAL     NO-UNDO.

        FIND FIRST itms WHERE itms.itmID = recipts.item# NO-ERROR.
        IF AVAILABLE itms THEN.
        DO:
            price = itms.unitPriceS.
        END.
        RELEASE itms.

        IF recipts.ItmDiscount = 100 THEN
            tt-db.Free = tt-db.Free + (recipts.pieses * price).
        ELSE
            ACCUMULATE recipts.pieses * price (TOTAL).

        ACCUMULATE recipts.damP * price (TOTAL COUNT).
        ACCUMULATE recipts.expP * price (TOTAL).
        ACCUMULATE recipts.GRRD * price (TOTAL).
        ACCUMULATE recipts.GRST * price (TOTAL).
    END.

    tt-db.Damage    = ACCUM TOTAL recipts.damP * price.
    tt-db.Expiery    = ACCUM TOTAL recipts.expP * price.
    tt-db.GR   = ACCUM TOTAL recipts.GRST * price.

    RELEASE tt-db.

    lineCnt = lineCnt + ACCUM COUNT recipts.damP * price.

    ACCUMULATE bills.bilDate (COUNT).

END.

bilCnt = ACCUM COUNT bills.bilDate.

FOR EACH tt-db.
    FOR EACH Expense WHERE DATE(Expense.DATE) = tt-db.DATE.
      ACCUMULATE Expense.Amount (TOTAL).
    END.
    tt-db.Expense = ACCUM TOTAL Expense.Amount.

    FOR EACH Payments WHERE date(Payments.datePay) = tt-db.DATE AND Payments.stat = YES.
        IF Payments.PayMethod = "Cash" THEN tt-db.Cash = tt-db.Cash + Payments.Amount.
        IF Payments.PayMethod = "Cheque" THEN tt-db.Cheque = tt-db.Cheque + Payments.Amount.
        IF Payments.PayMethod = "Credit" THEN tt-db.Credit = tt-db.Credit + Payments.Amount.
    END.

    FOR EACH cheques WHERE date(cheques.crDate) = tt-db.DATE.
        ACCUMULATE cheques.amount (TOTAL).
    END.

    tt-db.ChequesIn = ACCUM TOTAL cheques.amount.

    tt-db.Income = tt-db.Cash + tt-db.Cheque + tt-db.ChequesIn.
    tt-db.GrossIncome = tt-db.Income - tt-db.Expense.

END.


/* filBills = bilCnt.                                   */
/* filLines = lineCnt.                                  */
/*                                                      */
/* DISPLAY  filBills filLines WITH FRAME {&FRAME-NAME}. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION zero C-Win 
FUNCTION zero RETURNS CHARACTER
    
  ( val AS DECIMAL) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF  val = 0 THEN RETURN "-".
ELSE RETURN string(val).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

