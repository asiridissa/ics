&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
CREATE WIDGET-POOL.

DEFINE SHARED VARIABLE session_User AS CHARACTER.

DEFINE VARIABLE tempDate AS DATE        NO-UNDO.
DEFINE VARIABLE tempDateTo AS DATE        NO-UNDO.
DEFINE VARIABLE calendr AS COM-HANDLE   NO-UNDO.
DEFINE VARIABLE calendrTo AS COM-HANDLE   NO-UNDO.

DEFINE VARIABLE tmpID AS INTEGER     NO-UNDO INIT 1.
DEFINE VARIABLE tmpCash AS DECIMAL     NO-UNDO .
DEFINE VARIABLE tmpCheque AS DECIMAL     NO-UNDO .
DEFINE VARIABLE tmpCredit AS DECIMAL     NO-UNDO .
DEFINE VARIABLE tmpVarience AS DECIMAL     NO-UNDO .
DEFINE VARIABLE tmpGR AS DECIMAL     NO-UNDO .
DEFINE VARIABLE tmpDmg AS DECIMAL     NO-UNDO .
DEFINE VARIABLE tmpExp AS DECIMAL     NO-UNDO .
DEFINE VARIABLE tmpFIssu AS DECIMAL     NO-UNDO .
DEFINE VARIABLE tmpUnitPrice AS DECIMAL     NO-UNDO .
DEFINE VARIABLE FilterType AS CHAR     NO-UNDO .
DEFINE VARIABLE FilterFrom AS CHAR     NO-UNDO .
DEFINE VARIABLE FilterTo AS CHAR     NO-UNDO .

DEFINE TEMP-TABLE tt-dailyReport
    FIELD ID        AS INT     
    FIELD bill#        AS INT     
    FIELD BillNo    AS CHAR                 
    FIELD Customer  AS CHAR                 
    FIELD TolSale   AS DECIMAL                 
    FIELD TolValue  AS DECIMAL                 
    FIELD Varience  AS DECIMAL                 
    FIELD Ex        AS DECIMAL                 
    FIELD Dmg       AS DECIMAL                 
    FIELD Discount  AS DECIMAL                 
    FIELD DiscountAmount  AS DECIMAL                 
    FIELD Cash      AS DECIMAL                 
    FIELD Cheque    AS DECIMAL                 
    FIELD Credit    AS DECIMAL                 
    FIELD Gr        AS DECIMAL                 
    FIELD FIsu     AS DECIMAL                 
    FIELD Unpaid     AS DECIMAL                 
    .

DEFINE TEMP-TABLE tt-transactionHistory
    FIELD bill#        AS INT     
    FIELD TolSale   AS DECIMAL                 
    FIELD Varience  AS DECIMAL                 
    FIELD Ex        AS DECIMAL                 
    FIELD Dmg       AS DECIMAL                 
    FIELD Discount  AS DECIMAL                 
    FIELD DiscountAmount  AS DECIMAL                 
    FIELD Cash      AS DECIMAL                 
    FIELD Cheque    AS DECIMAL                 
    FIELD Credit    AS DECIMAL                 
    FIELD Gr        AS DECIMAL                 
    FIELD FIsu     AS DECIMAL     
    FIELD Unpaid     AS DECIMAL                 
    .
DEFINE TEMP-TABLE tt-transactionLastDay
    FIELD bill#        AS INT     
    FIELD TolSale   AS DECIMAL                 
    FIELD Varience  AS DECIMAL                 
    FIELD Ex        AS DECIMAL                 
    FIELD Dmg       AS DECIMAL                 
    FIELD Discount  AS DECIMAL                 
    FIELD DiscountAmount  AS DECIMAL                 
    FIELD Cash      AS DECIMAL                 
    FIELD Cheque    AS DECIMAL                 
    FIELD Credit    AS DECIMAL                 
    FIELD Gr        AS DECIMAL                 
    FIELD FIsu     AS DECIMAL                 
    .

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
&Scoped-define INTERNAL-TABLES tt-dailyReport

/* Definitions for BROWSE brw                                           */
&Scoped-define FIELDS-IN-QUERY-brw /* ID */ BillNo Customer TolSale Varience Ex Dmg Gr FIsu DiscountAmount Cash Cheque Credit Unpaid /* itmName */ /* Weight */ /* /* PriceP */ */ /* BSC */ /* BSP */ /* LDC */ /* LDP */ /* ULC */ /* ULP */   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brw   
&Scoped-define SELF-NAME brw
&Scoped-define QUERY-STRING-brw FOR EACH tt-dailyReport BY BillNo
&Scoped-define OPEN-QUERY-brw OPEN QUERY brw FOR EACH tt-dailyReport BY BillNo.
&Scoped-define TABLES-IN-QUERY-brw tt-dailyReport
&Scoped-define FIRST-TABLE-IN-QUERY-brw tt-dailyReport


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-brw}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-4 filTotal filTotalFissu ~
filTotalCash radTimePeriod cmbVeh btnView filTotalDisc filTotalExp ~
filTotalCredit filValue filTotalDam filTotalCheque btnPrint filToCollect ~
filTotalGr filTotalVarience brw 
&Scoped-Define DISPLAYED-OBJECTS filTotal filTotalFissu filTotalCash ~
radTimePeriod cmbVeh filTotalDisc filTotalExp filTotalCredit filValue ~
filTotalDam filTotalCheque filToCollect filTotalGr filTotalVarience 

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
DEFINE BUTTON btnPrint 
     LABEL "Print" 
     SIZE 14 BY 1.

DEFINE BUTTON btnView 
     LABEL "View" 
     SIZE 14 BY 1.

DEFINE VARIABLE cmbVeh AS INTEGER FORMAT "->>>>9":U INITIAL 0 
     LABEL "Vehicle" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEM-PAIRS "--Select Here--",0,
                     "All Vehicals",-1
     DROP-DOWN-LIST
     SIZE 23.72 BY 1 NO-UNDO.

DEFINE VARIABLE filToCollect AS DECIMAL FORMAT ">>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Credit" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88
     BGCOLOR 15 FGCOLOR 12 FONT 10 NO-UNDO.

DEFINE VARIABLE filTotal AS DECIMAL FORMAT "->>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Sale" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88
     BGCOLOR 0 FGCOLOR 14  NO-UNDO.

DEFINE VARIABLE filTotalCash AS DECIMAL FORMAT ">>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Cash" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88
     BGCOLOR 0 FGCOLOR 14  NO-UNDO.

DEFINE VARIABLE filTotalCheque AS DECIMAL FORMAT ">>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Cheques" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88
     BGCOLOR 0 FGCOLOR 14  NO-UNDO.

DEFINE VARIABLE filTotalCredit AS DECIMAL FORMAT ">>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Debit" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88
     BGCOLOR 0 FGCOLOR 14  NO-UNDO.

DEFINE VARIABLE filTotalDam AS DECIMAL FORMAT ">>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Dam" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88
     BGCOLOR 0 FGCOLOR 14  NO-UNDO.

DEFINE VARIABLE filTotalDisc AS DECIMAL FORMAT ">>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Disc" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88
     BGCOLOR 0 FGCOLOR 14  NO-UNDO.

DEFINE VARIABLE filTotalExp AS DECIMAL FORMAT ">>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Exp" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88
     BGCOLOR 0 FGCOLOR 14  NO-UNDO.

DEFINE VARIABLE filTotalFissu AS DECIMAL FORMAT ">>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Free" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88
     BGCOLOR 0 FGCOLOR 14  NO-UNDO.

DEFINE VARIABLE filTotalGr AS DECIMAL FORMAT ">>>,>>>,>>9.99":U INITIAL 0 
     LABEL "GR" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88
     BGCOLOR 0 FGCOLOR 14  NO-UNDO.

DEFINE VARIABLE filTotalVarience AS DECIMAL FORMAT "->>,>>>,>>9.99":U INITIAL 0 
     LABEL "Varience" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88
     BGCOLOR 15 FGCOLOR 12  NO-UNDO.

DEFINE VARIABLE filValue AS DECIMAL FORMAT "->>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Value" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88
     BGCOLOR 0 FGCOLOR 14  NO-UNDO.

DEFINE VARIABLE radTimePeriod AS CHARACTER INITIAL "Daily" 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Daily", "Daily",
"Monthly", "Monthly",
"Yearly", "Yearly",
"Custom", "Custom"
     SIZE 10.43 BY 2.96 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL  GROUP-BOX  ROUNDED 
     SIZE 64 BY 3.12.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL  GROUP-BOX  ROUNDED 
     SIZE 15.86 BY 3.12.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brw FOR 
      tt-dailyReport SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brw
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brw C-Win _FREEFORM
  QUERY brw DISPLAY
      /*       ID       FORMAT ">>9" LABEL "No." */
    BillNo   FORMAT "X(20)":U  WIDTH 8
    Customer FORMAT "X(50)":U WIDTH 35
    TolSale  FORMAT "->,>>>,>>9.99" LABEL "Total Sale"
        Varience  FORMAT "->,>>9.99" LABEL "Varience" WIDTH 8
    Ex       FORMAT ">>,>>9.99"  LABEL "Expiry"
    Dmg      FORMAT ">>,>>9.99"  LABEL "Damage"
    Gr       FORMAT ">>,>>9.99" LABEL "G/Return"
    FIsu     FORMAT ">>,>>9.99" LABEL "F/Issue"
    DiscountAmount FORMAT ">>,>>9.99" LABEL "Discount"
    Cash     FORMAT ">,>>>,>>9.99" WIDTH 8
    Cheque   FORMAT ">,>>>,>>9.99" WIDTH 8
    Credit   FORMAT ">,>>>,>>9.99" LABEL "Debit" WIDTH 8
    Unpaid   FORMAT ">,>>>,>>9.99" LABEL "Credit" COLUMN-FGCOLOR 12
    /* itmName FORMAT "X(38)":U LABEL "Item Name" */
    /* Weight  FORMAT ">>>9.999"                  */
    /* /* PriceP */                               */
    /* BSC    FORMAT ">>>9"                       */
    /* BSP    FORMAT ">>>9" COLUMN-BGCOLOR 16     */
    /* LDC    FORMAT ">>>9"                       */
    /* LDP    FORMAT ">>>9" COLUMN-BGCOLOR 16     */
    /* ULC    FORMAT ">>>9"                       */
    /* ULP    FORMAT ">>>9" COLUMN-BGCOLOR 16     */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 143 BY 23
         FONT 10
         TITLE "Daily Report" ROW-HEIGHT-CHARS .55 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     filTotal AT ROW 1.04 COL 73.29 COLON-ALIGNED WIDGET-ID 236
     filTotalFissu AT ROW 1.04 COL 98.72 COLON-ALIGNED WIDGET-ID 256
     filTotalCash AT ROW 1.04 COL 126 COLON-ALIGNED WIDGET-ID 250
     radTimePeriod AT ROW 1.31 COL 37.57 NO-LABEL WIDGET-ID 264
     cmbVeh AT ROW 1.46 COL 6.86 COLON-ALIGNED WIDGET-ID 100
     btnView AT ROW 1.62 COL 50.57 WIDGET-ID 2
     filTotalDisc AT ROW 1.88 COL 73.29 COLON-ALIGNED WIDGET-ID 242
     filTotalExp AT ROW 1.88 COL 98.72 COLON-ALIGNED WIDGET-ID 238
     filTotalCredit AT ROW 1.88 COL 126 COLON-ALIGNED WIDGET-ID 248
     filValue AT ROW 2.73 COL 73.29 COLON-ALIGNED WIDGET-ID 278
     filTotalDam AT ROW 2.73 COL 98.72 COLON-ALIGNED WIDGET-ID 240
     filTotalCheque AT ROW 2.73 COL 126 COLON-ALIGNED WIDGET-ID 246
     btnPrint AT ROW 2.85 COL 50.57 WIDGET-ID 234
     filToCollect AT ROW 3.58 COL 73.29 COLON-ALIGNED WIDGET-ID 280
     filTotalGr AT ROW 3.58 COL 98.72 COLON-ALIGNED WIDGET-ID 258
     filTotalVarience AT ROW 3.58 COL 126 COLON-ALIGNED WIDGET-ID 282
     brw AT ROW 4.5 COL 1.43 WIDGET-ID 200
     "From:" VIEW-AS TEXT
          SIZE 5 BY .62 AT ROW 2.5 COL 3.43 WIDGET-ID 254
     "To:" VIEW-AS TEXT
          SIZE 3.14 BY .62 AT ROW 3.42 COL 5.57 WIDGET-ID 274
     RECT-1 AT ROW 1.23 COL 1.57 WIDGET-ID 260
     RECT-4 AT ROW 1.23 COL 49.57 WIDGET-ID 276
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
         TITLE              = "Error"
         COLUMN             = 1.57
         ROW                = 1.19
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
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB brw filTotalVarience DEFAULT-FRAME */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brw
/* Query rebuild information for BROWSE brw
     _START_FREEFORM
OPEN QUERY brw FOR EACH tt-dailyReport BY BillNo.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE brw */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 2.42
       COLUMN          = 8.86
       HEIGHT          = .81
       WIDTH           = 23.72
       WIDGET-ID       = 232
       HIDDEN          = no
       SENSITIVE       = yes.

CREATE CONTROL-FRAME CtrlFrame-2 ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 3.35
       COLUMN          = 8.86
       HEIGHT          = .81
       WIDTH           = 23.72
       WIDGET-ID       = 270
       HIDDEN          = no
       SENSITIVE       = yes.
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {20DD1B9E-87C4-11D1-8BE3-0000F8754DA1} type: DTPicker */
/* CtrlFrame-2 OCXINFO:CREATE-CONTROL from: {20DD1B9E-87C4-11D1-8BE3-0000F8754DA1} type: DTPicker */
      CtrlFrame:MOVE-AFTER(filTotalCredit:HANDLE IN FRAME DEFAULT-FRAME).
      CtrlFrame-2:MOVE-AFTER(btnPrint:HANDLE IN FRAME DEFAULT-FRAME).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Error */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit.
  IF THIS-PROCEDURE:PERSISTENT THEN  */ RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Error */
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

    FIND FIRST tt-dailyReport NO-LOCK NO-ERROR.
    IF NOT AVAILABLE tt-dailyReport THEN
        DO:
            MESSAGE "No records available to print." VIEW-AS ALERT-BOX INFO BUTTONS OK.
            RETURN.
        END.
    RELEASE tt-dailyReport.

    {&SELF-NAME}:LABEL = "Working..".

    FIND FIRST vehical WHERE vehical.ID = cmbVeh NO-ERROR.
        IF AVAILABLE vehical THEN veh = vehical.veh# + " " + vehical.descrip.
        IF NOT AVAILABLE vehical THEN  veh = "All Vehicals".
    RELEASE vehical.

    MESSAGE "Conferm to print?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE yn AS LOGICAL.
    IF yn = YES THEN
    DO:
        DEFINE VARIABLE tempCount AS INTEGER     NO-UNDO.
    
        OUTPUT TO VALUE("E:\ICS\bin\print\TransactionReport.txt").
            
            CASE radTimePeriod:
                WHEN "Custom" THEN
                    period = "From : " + string(calendr:VALUE,"99/99/9999") + "  To : " + string(calendrTo:VALUE,"99/99/9999").
                WHEN "Daily" THEN
                DO:
                    period = "Date : " + STRING((calendr:VALUE),"99/99/9999").
                    RUN DailyTransHistory.
                END.
                WHEN "Monthly" THEN
                    period = "Month : " + STRING(MONTH(calendr:VALUE)) + " - " + STRING(YEAR(calendr:VALUE)).
                WHEN "Yearly" THEN
                    period = "Year : " + STRING(YEAR(calendr:VALUE)).
            END CASE.
            
            PUT UNFORMAT "||" + period + "|||Vehical : " + veh + "||||By user : " + session_User SKIP. 
            PUT UNFORMAT "NO|Bill No|Customer|Tol Sale|Varience|Expiries|Damages|Discount|GR|F/Issu|Cash|Cheques|Debit|Credits" SKIP. 
                
            tempCount = 1.
        
            FOR EACH tt-dailyReport.
               PUT UNFORMAT STRING( tempCount) + "|" .
               PUT UNFORMAT STRING( BillNo   ) + "|" .
               PUT UNFORMAT STRING( Customer ) + "|" .
               PUT UNFORMAT STRING( TolSale  ) + "|" .
               PUT UNFORMAT STRING( Varience  ) + "|" .
               PUT UNFORMAT STRING( Ex       ) + "|".
               PUT UNFORMAT STRING( Dmg      ) + "|".
               PUT UNFORMAT STRING( Discount ) + "|".
               PUT UNFORMAT STRING( Gr       ) + "|".
               PUT UNFORMAT STRING( FIsu     ) + "|".
               PUT UNFORMAT STRING( Cash     ) + "|".
               PUT UNFORMAT STRING( Cheque   ) + "|".
               PUT UNFORMAT STRING( Credit   ) + "|".
               PUT UNFORMAT STRING( Unpaid     ) + "|" SKIP.
               tempCount = tempCount + 1.
            END.    
            PUT UNFORMAT "~n".
            PUT UNFORMAT "||Total|"
                 + string( filTotal) + "|"
                + string( filTotalVarience) + "|"
                 + string( filTotalExp) + "|"
                 + string( filTotalDam) + "|"
                 + string( filTotalDisc) + "|"
                 + string( filTotalGr) + "|"
                 + string( filTotalFissu) + "|"
                 + string( filTotalCash) + "|"
                 + string( filTotalCheque) + "|"
                 + string( filTotalCredit) + "|" 
                 + string( filToCollect) + "|" 
                SKIP .
/*             PUT UNFORMAT "||Value|" + STRING(filValue)SKIP. */

            FOR EACH tt-transactionHistory.
               ACCUMULATE tt-transactionHistory.TolSale  (total).
               ACCUMULATE tt-transactionHistory.Varience  (total).
               ACCUMULATE tt-transactionHistory.Ex       (total).
               ACCUMULATE tt-transactionHistory.Dmg      (total).
               ACCUMULATE tt-transactionHistory.Discount (total).
               ACCUMULATE tt-transactionHistory.Cash     (total).
               ACCUMULATE tt-transactionHistory.Cheque   (total).
               ACCUMULATE tt-transactionHistory.Credit   (total).
               ACCUMULATE tt-transactionHistory.Gr       (total).
               ACCUMULATE tt-transactionHistory.FIsu     (total).
               ACCUMULATE tt-transactionHistory.Unpaid     (total).
            END.

            PUT UNFORMAT "||Upto Date Total|"                     
                + string(ACCUMULATE total tt-transactionHistory.TolSale )  + "|"              
                + string(ACCUMULATE total tt-transactionHistory.Varience )  + "|"              
                + string(ACCUMULATE total tt-transactionHistory.Ex      )  + "|"           
                + string(ACCUMULATE total tt-transactionHistory.Dmg     )  + "|"           
                + string(ACCUMULATE total tt-transactionHistory.Discount)  + "|"          
                + string(ACCUMULATE total tt-transactionHistory.Gr      )  + "|"            
                + string(ACCUMULATE total tt-transactionHistory.FIsu    )  + "|" 
                + string(ACCUMULATE total tt-transactionHistory.Cash    )  + "|"          
                + string(ACCUMULATE total tt-transactionHistory.Cheque  )  + "|"        
                + string(ACCUMULATE total tt-transactionHistory.Credit  )  + "|"         
                + string(ACCUMULATE total tt-transactionHistory.Unpaid  )  SKIP.        

        OUTPUT CLOSE.
        
        DOS SILENT START VALUE("E:\ICS\bin\print\TransactionReport.bat").
        DOS SILENT START excel VALUE("E:\ICS\bin\print\TransactionReport.xlsx").
    END.

    {&SELF-NAME}:LABEL = "Print".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnView
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnView C-Win
ON CHOOSE OF btnView IN FRAME DEFAULT-FRAME /* View */
DO:
    IF cmbVeh = 0 THEN
    DO:
        MESSAGE "Select Vehical first" VIEW-AS ALERT-BOX WARNING BUTTONS OK TITLE "Inventry Control Syatem".
        RETURN.
    END.

    filTotal       = 0.
    filTotalExp    = 0.
    filTotalDam    = 0.
    filTotalDisc   = 0.
    filTotalCheque = 0.
    filTotalCredit = 0.
    filTotalCash   = 0.
    filTotalFissu  = 0.
    filTotalGr     = 0.
    filValue       = 0.
    filToCollect   = 0.
    filTotalVarience = 0.

    tempDate = calendr:VALUE.
    tempDateTo = calendrTo:VALUE.
    
    {&SELF-NAME}:LABEL = "Working..".

    CASE radTimePeriod:
        WHEN "Daily" THEN
            RUN dailyTrans.
        WHEN "Monthly" THEN
            RUN MonthlyTrans.
        WHEN "Yearly" THEN
            RUN YearlyTrans.
        WHEN "Custom" THEN
            RUN CustomTrans.
    END CASE.
   OPEN QUERY brw FOR EACH tt-dailyReport BY tt-dailyReport.BillNo.
        IF AVAILABLE tt-dailyReport THEN
        DO:
            filTotal       = 0.
            filTotalExp    = 0.
            filTotalDam    = 0.
            filTotalDisc   = 0.
            filTotalCheque = 0.
            filTotalCredit = 0.
            filTotalCash   = 0.
            filTotalFissu  = 0.
            filTotalGr     = 0.
            filValue       = 0.
            filToCollect   = 0.
            filTotalVarience = 0.

            FOR EACH tt-dailyReport.
                filTotal        = filTotal       + tt-dailyReport.TolSale.
                filValue        = filValue       + tt-dailyReport.TolValue.
                filTotalExp     = filTotalExp    + tt-dailyReport.Ex.
                filTotalDam     = filTotalDam    + tt-dailyReport.Dmg.
                filTotalDisc    = filTotalDisc   + tt-dailyReport.DiscountAmount.
                filTotalCheque  = filTotalCheque + tt-dailyReport.Cheque.
                filTotalCredit  = filTotalCredit + tt-dailyReport.Credit.
                filTotalCash    = filTotalCash   + tt-dailyReport.Cash.
                filTotalGr      = filTotalGr     + tt-dailyReport.Gr.
                filTotalFissu   = filTotalFissu  + tt-dailyReport.FIsu.
                filToCollect    = filToCollect   + tt-dailyReport.Unpaid.
                filTotalVarience = filTotalVarience + tt-dailyReport.Varience.
            END.
        END.
        ELSE MESSAGE "No records to show." VIEW-AS ALERT-BOX INFO BUTTONS OK.

        DISPLAY filToCollect filTotal filValue filTotalDam filTotalExp filTotalDisc
            filTotalCash filTotalCredit filTotalCheque
            filTotalGr filTotalFissu filTotalVarience WITH FRAME {&FRAME-NAME}.

        {&SELF-NAME}:LABEL = "View".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cmbVeh
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbVeh C-Win
ON VALUE-CHANGED OF cmbVeh IN FRAME DEFAULT-FRAME /* Vehicle */
DO:
    ASSIGN {&SELF-NAME}.
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
/*                                                     */
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


&Scoped-define SELF-NAME radTimePeriod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL radTimePeriod C-Win
ON VALUE-CHANGED OF radTimePeriod IN FRAME DEFAULT-FRAME
DO:
  ASSIGN {&SELF-NAME}.
  IF {&SELF-NAME} = "Custom" THEN
      calendrTo:ENABLED = TRUE.
  ELSE
      calendrTo:ENABLED = FALSE.
/*   APPLY "CHOOSE":U TO btnView. */
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
  RUN vehloader.
  
  DEFINE SHARED VARIABLE session_Path AS CHAR.
  DEFINE SHARED VARIABLE session_icon AS CHAR.
  {&WINDOW-NAME}:TITLE = session_Path.
  {&WINDOW-NAME}:LOAD-ICON(session_icon).

  calendr = chCtrlFrame:DTPicker.
  calendrTo = chCtrlFrame-2:DTPicker.
  calendr:ENABLED = TRUE.
  calendr:VALUE = TODAY - 1.
  calendrTo:ENABLED = FALSE.
  calendrTo:VALUE = TODAY - 1.

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

OCXFile = SEARCH( "FinancialState.wrx":U ).
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
ELSE MESSAGE "FinancialState.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Custom C-Win 
PROCEDURE Custom :
EMPTY TEMP-TABLE tt-dailyReport.
    IF cmbVeh = -1 THEN
    DO:
    FOR EACH bills WHERE bills.bilDate >= tempDate AND bills.bilDate <= tempDateTo BY bills.BillNo.
        tmpGR = 0.
        tmpDmg = 0.
        tmpExp = 0.
        tmpFIssu = 0.
        tmpCash   = 0.   
        tmpCheque = 0.
        tmpCredit = 0.
        tmpUnitPrice = 0.

        FOR EACH recipts WHERE recipts.bill# = bills.bill# .
            FIND FIRST itms WHERE itms.itmID = recipts.item#.
                tmpUnitPrice = itms.unitPriceS.
            RELEASE itms.
            tmpGR = tmpGR + (recipts.GRRD * tmpUnitPrice).
            tmpDmg = tmpDmg + (recipts.damP * tmpUnitPrice).
            tmpExp = tmpExp + (recipts.expP * tmpUnitPrice).
            IF recipts.ItmDiscount = 100.00 THEN
                tmpFIssu = tmpFIssu + (recipts.pieses * tmpUnitPrice).
        END.

        FOR EACH Payments WHERE Payments.bill# = bills.bill#  .
            IF Payments.PayMethod = "Cash" THEN
                tmpCash = tmpCash + Payments.Amount.
            IF Payments.PayMethod = "Credit" THEN
                tmpCredit = tmpCredit + Payments.Amount.
            IF Payments.PayMethod = "Cheque" AND Payments.stat = YES THEN
                tmpCheque = tmpCheque + Payments.Amount.
        END.

        FIND FIRST area WHERE area.ID = bills.areaCode.

        CREATE tt-dailyReport.
        tt-dailyReport.ID       = tmpID                .
        tt-dailyReport.bill#    = bills.bill#          .
        tt-dailyReport.BillNo   = bills.BillNo         .
        tt-dailyReport.Customer = bills.cusName + " - " + area.descrip       .   
        tt-dailyReport.TolSale  = bills.tol            .   
        tt-dailyReport.TolValue = bills.tol + bills.discountedAmount         .   
        tt-dailyReport.Ex       = tmpExp               .
        tt-dailyReport.Dmg      = tmpDmg               .
        tt-dailyReport.Discount = bills.discountedAmount   .
        tt-dailyReport.Cash     = tmpCash              .
        tt-dailyReport.Cheque   = tmpCheque            .
        tt-dailyReport.Credit   = tmpCredit            .
        tt-dailyReport.Gr       = tmpGR                .
        tt-dailyReport.FIsu     = tmpFIssu             .
        tt-dailyReport.DiscountAmount = bills.discountedAmount.
        
        tmpID = tmpID + 1.
    END.
    END.
    ELSE
    DO:
    FOR EACH bills WHERE bills.bilDate >= tempDate AND bills.bilDate <= tempDateTo AND bills.vehNo = cmbVeh BY bills.BillNo.
        tmpGR = 0.
        tmpDmg = 0.
        tmpExp = 0.
        tmpFIssu = 0.
        tmpCash   = 0.   
        tmpCheque = 0.
        tmpCredit = 0.
        tmpUnitPrice = 0.

        FOR EACH recipts WHERE recipts.bill# = bills.bill# .
            FIND FIRST itms WHERE itms.itmID = recipts.item#.
                tmpUnitPrice = itms.unitPriceS.
            RELEASE itms.
            tmpGR = tmpGR + (recipts.GRRD * tmpUnitPrice).
            tmpDmg = tmpDmg + (recipts.damP * tmpUnitPrice).
            tmpExp = tmpExp + (recipts.expP * tmpUnitPrice).
            IF recipts.ItmDiscount = 100.00 THEN
                tmpFIssu = tmpFIssu + (recipts.pieses * tmpUnitPrice).
        END.

        FOR EACH Payments WHERE Payments.bill# = bills.bill#  .
            IF Payments.PayMethod = "Cash" THEN
                tmpCash = tmpCash + Payments.Amount.
            IF Payments.PayMethod = "Credit" THEN
                tmpCredit = tmpCredit + Payments.Amount.
            IF Payments.PayMethod = "Cheque" AND Payments.stat = YES THEN
                tmpCheque = tmpCheque + Payments.Amount.
        END.

        FIND FIRST area WHERE area.ID = bills.areaCode.

        CREATE tt-dailyReport.
        tt-dailyReport.ID       = tmpID                .
        tt-dailyReport.bill#    = bills.bill#          .
        tt-dailyReport.BillNo   = bills.BillNo         .
        tt-dailyReport.Customer = bills.cusName + " - " + area.descrip       .   
        tt-dailyReport.TolSale  = bills.tol            .   
        tt-dailyReport.TolValue = bills.tol + bills.discountedAmount         .   
        tt-dailyReport.Ex       = tmpExp               .
        tt-dailyReport.Dmg      = tmpDmg               .
        tt-dailyReport.Discount = bills.discountedAmount   .
        tt-dailyReport.Cash     = tmpCash              .
        tt-dailyReport.Cheque   = tmpCheque            .
        tt-dailyReport.Credit   = tmpCredit            .
        tt-dailyReport.Gr       = tmpGR                .
        tt-dailyReport.FIsu     = tmpFIssu             .
        tt-dailyReport.DiscountAmount = bills.discountedAmount.
        
        tmpID = tmpID + 1.
    END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CustomTrans C-Win 
PROCEDURE CustomTrans :
EMPTY TEMP-TABLE tt-dailyReport.

    FOR EACH Payments WHERE DATE(Payments.datePay) >= tempDate AND DATE(Payments.datePay) <= tempDateTo  AND Payments.stat = YES.
        tmpGR = 0.
        tmpDmg = 0.
        tmpExp = 0.
        tmpFIssu = 0.
        tmpCash   = 0.
        tmpCheque = 0.
        tmpCredit = 0.
        tmpUnitPrice = 0.
        FIND FIRST tt-dailyReport WHERE tt-dailyReport.bill# = Payments.bill# NO-ERROR.
        IF NOT AVAILABLE tt-dailyReport THEN
        DO:
            FIND FIRST bills WHERE bills.bill# = Payments.bill# NO-ERROR.
            IF AVAILABLE bills THEN
            DO:
                FIND FIRST area WHERE area.ID = bills.areaCode NO-ERROR.
                IF AVAILABLE area THEN
                DO:
                    CREATE tt-dailyReport.
                    tt-dailyReport.ID     = tmpID.
                    tt-dailyReport.bill#  = Payments.bill#.
                    tt-dailyReport.BillNo   = bills.BillNo         .              
                    tt-dailyReport.Customer = bills.cusName + " - " + area.areaCode.
                    tt-dailyReport.TolSale  = bills.tol            .              
                    tt-dailyReport.TolValue = bills.tol + bills.discountedAmount  .
                    tt-dailyReport.Varience = bills.varience         .   
                    tt-dailyReport.Unpaid   = bills.tol - bills.paidAmount.
        
                    FOR EACH recipts WHERE recipts.bill# = bills.bill# .           
                        FIND FIRST itms WHERE itms.itmID = recipts.item#.          
                            tmpUnitPrice = itms.unitPriceS.                        
                        RELEASE itms.                                              
                        tmpGR = tmpGR + (recipts.GRRD * tmpUnitPrice).      
                        tmpDmg = tmpDmg + (recipts.damP * tmpUnitPrice).           
                        tmpExp = tmpExp + (recipts.expP * tmpUnitPrice).           
                        IF recipts.ItmDiscount = 100.00 THEN                       
                            tmpFIssu = tmpFIssu + (recipts.pieses * tmpUnitPrice). 
                    END.                                             
                    
                    tt-dailyReport.Ex       = tmpExp               .       
                    tt-dailyReport.Dmg      = tmpDmg               .       
                    tt-dailyReport.Discount = bills.discountedAmount   .   
                    tt-dailyReport.Gr       = tmpGR                .       
                    tt-dailyReport.FIsu     = tmpFIssu             .       
                    tt-dailyReport.DiscountAmount = bills.discountedAmount.

                    CASE Payments.PayMethod:
                        WHEN "Cash" THEN tt-dailyReport.Cash = Payments.Amount.
                        WHEN "Credit" THEN tt-dailyReport.Credit = Payments.Amount.
                        WHEN "Cheque" THEN tt-dailyReport.Cheque = Payments.Amount.
                    END CASE.
                    tmpID = tmpID + 1.
                END.
                RELEASE area.
            END.
            RELEASE bills.
        END.
        ELSE
        DO:
            CASE Payments.PayMethod:
                WHEN "Cash" THEN tt-dailyReport.Cash = tt-dailyReport.Cash + Payments.Amount.
                WHEN "Credit" THEN tt-dailyReport.Credit = tt-dailyReport.Credit + Payments.Amount.
                WHEN "Cheque" THEN tt-dailyReport.Cheque = tt-dailyReport.Cheque + Payments.Amount.
            END CASE.
        END.
    END.

    IF cmbVeh <> -1 THEN
    DO:
        FOR EACH tt-dailyReport.
            FIND FIRST bills WHERE tt-dailyReport.bill# = bills.bill# NO-ERROR.
            IF AVAILABLE bills THEN
            DO:
                IF bills.vehNo <> cmbVeh THEN DELETE tt-dailyReport.
            END.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Daily C-Win 
PROCEDURE Daily :
EMPTY TEMP-TABLE tt-dailyReport.
    IF cmbVeh = -1 THEN
    DO:
        FOR EACH bills WHERE bills.bilDate = tempDate BY bills.BillNo.
        tmpGR = 0.
        tmpDmg = 0.
        tmpExp = 0.
        tmpFIssu = 0.
        tmpCash   = 0.   
        tmpCheque = 0.
        tmpCredit = 0.
        tmpUnitPrice = 0.

        FOR EACH recipts WHERE recipts.bill# = bills.bill# .
            FIND FIRST itms WHERE itms.itmID = recipts.item#.
                tmpUnitPrice = itms.unitPriceS.
            RELEASE itms.
            tmpGR = tmpGR + (recipts.GRRD * tmpUnitPrice).
            tmpDmg = tmpDmg + (recipts.damP * tmpUnitPrice).
            tmpExp = tmpExp + (recipts.expP * tmpUnitPrice).
            IF recipts.ItmDiscount = 100.00 THEN
                tmpFIssu = tmpFIssu + (recipts.pieses * tmpUnitPrice).
        END.

        FOR EACH Payments WHERE Payments.bill# = bills.bill#  .
            IF Payments.PayMethod = "Cash" THEN
                tmpCash = tmpCash + Payments.Amount.
            IF Payments.PayMethod = "Credit" THEN
                tmpCredit = tmpCredit + Payments.Amount.
            IF Payments.PayMethod = "Cheque" AND Payments.stat = YES THEN
                tmpCheque = tmpCheque + Payments.Amount.
        END.

        FIND FIRST area WHERE area.ID = bills.areaCode.

        CREATE tt-dailyReport.
        tt-dailyReport.ID       = tmpID                .
        tt-dailyReport.bill#    = bills.bill#          .
        tt-dailyReport.BillNo   = bills.BillNo         .
        tt-dailyReport.Customer = bills.cusName + " - " + area.descrip       .   
        tt-dailyReport.TolSale  = bills.tol            .   
        tt-dailyReport.TolValue = bills.tol + bills.discountedAmount         .   
        tt-dailyReport.Ex       = tmpExp               .
        tt-dailyReport.Dmg      = tmpDmg               .
        tt-dailyReport.Discount = bills.discountedAmount   .
        tt-dailyReport.Cash     = tmpCash              .
        tt-dailyReport.Cheque   = tmpCheque            .
        tt-dailyReport.Credit   = tmpCredit            .
        tt-dailyReport.Gr       = tmpGR                .
        tt-dailyReport.FIsu     = tmpFIssu             .
        tt-dailyReport.DiscountAmount = bills.discountedAmount.
        
        tmpID = tmpID + 1.
        END.
    END.
    ELSE
    DO:
        FOR EACH bills WHERE bills.bilDate = tempDate  AND bills.vehNo = cmbVeh  BY bills.BillNo.
        tmpGR = 0.
        tmpDmg = 0.
        tmpExp = 0.
        tmpFIssu = 0.
        tmpCash   = 0.   
        tmpCheque = 0.
        tmpCredit = 0.
        tmpUnitPrice = 0.

        FOR EACH recipts WHERE recipts.bill# = bills.bill# .
            FIND FIRST itms WHERE itms.itmID = recipts.item#.
                tmpUnitPrice = itms.unitPriceS.
            RELEASE itms.
            tmpGR = tmpGR + (recipts.GRRD * tmpUnitPrice).
            tmpDmg = tmpDmg + (recipts.damP * tmpUnitPrice).
            tmpExp = tmpExp + (recipts.expP * tmpUnitPrice).
            IF recipts.ItmDiscount = 100.00 THEN
                tmpFIssu = tmpFIssu + (recipts.pieses * tmpUnitPrice).
        END.

        FOR EACH Payments WHERE Payments.bill# = bills.bill#  .
            IF Payments.PayMethod = "Cash" THEN
                tmpCash = tmpCash + Payments.Amount.
            IF Payments.PayMethod = "Credit" THEN
                tmpCredit = tmpCredit + Payments.Amount.
            IF Payments.PayMethod = "Cheque" AND Payments.stat = YES THEN
                tmpCheque = tmpCheque + Payments.Amount.
        END.

        FIND FIRST area WHERE area.ID = bills.areaCode.

        CREATE tt-dailyReport.
        tt-dailyReport.ID       = tmpID                .
        tt-dailyReport.bill#    = bills.bill#          .
        tt-dailyReport.BillNo   = bills.BillNo         .
        tt-dailyReport.Customer = bills.cusName + " - " + area.descrip       .   
        tt-dailyReport.TolSale  = bills.tol            .   
        tt-dailyReport.TolValue = bills.tol + bills.discountedAmount         .   
        tt-dailyReport.Ex       = tmpExp               .
        tt-dailyReport.Dmg      = tmpDmg               .
        tt-dailyReport.Discount = bills.discountedAmount   .
        tt-dailyReport.Cash     = tmpCash              .
        tt-dailyReport.Cheque   = tmpCheque            .
        tt-dailyReport.Credit   = tmpCredit            .
        tt-dailyReport.Gr       = tmpGR                .
        tt-dailyReport.FIsu     = tmpFIssu             .
        tt-dailyReport.DiscountAmount = bills.discountedAmount.
        
        tmpID = tmpID + 1.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dailyTrans C-Win 
PROCEDURE dailyTrans :
EMPTY TEMP-TABLE tt-dailyReport.
    FOR EACH Payments WHERE DATE(Payments.datePay) = DATE(tempDate) AND Payments.stat = YES.
        tmpGR = 0.
        tmpDmg = 0.
        tmpExp = 0.
        tmpFIssu = 0.
        tmpCash   = 0.
        tmpCheque = 0.
        tmpCredit = 0.
        tmpUnitPrice = 0.
        FIND FIRST tt-dailyReport WHERE tt-dailyReport.bill# = Payments.bill# NO-ERROR.
        IF NOT AVAILABLE tt-dailyReport THEN
        DO:
            FIND FIRST bills WHERE bills.bill# = Payments.bill# NO-ERROR.
            IF AVAILABLE bills THEN
            DO:
                FIND FIRST area WHERE area.ID = bills.areaCode NO-ERROR.
                IF AVAILABLE area THEN
                DO:
                    CREATE tt-dailyReport.
                    tt-dailyReport.ID     = tmpID.
                    tt-dailyReport.bill#  = Payments.bill#.
                    tt-dailyReport.BillNo   = bills.BillNo         .              
                    tt-dailyReport.Customer = bills.cusName + " - " + area.areaCode.
                    tt-dailyReport.TolSale  = bills.tol            .              
                    tt-dailyReport.TolValue = bills.tol + bills.discountedAmount  .
                    tt-dailyReport.Varience = bills.varience         .   
                    tt-dailyReport.Unpaid   = bills.tol - bills.paidAmount.
        
                    FOR EACH recipts WHERE recipts.bill# = bills.bill# .   
                        FIND FIRST itms WHERE itms.itmID = recipts.item#.   
                            tmpUnitPrice = itms.unitPriceS.                        
                        RELEASE itms.                                              
                        tmpGR = tmpGR + (recipts.GRRD * tmpUnitPrice).      
                        tmpDmg = tmpDmg + (recipts.damP * tmpUnitPrice).           
                        tmpExp = tmpExp + (recipts.expP * tmpUnitPrice).           
                        IF recipts.ItmDiscount = 100.00 THEN                       
                            tmpFIssu = tmpFIssu + (recipts.pieses * tmpUnitPrice). 
                    END.                                             
                    
                    tt-dailyReport.Ex       = tmpExp               .       
                    tt-dailyReport.Dmg      = tmpDmg               .       
                    tt-dailyReport.Discount = bills.discountedAmount   .   
                    tt-dailyReport.Gr       = tmpGR                .       
                    tt-dailyReport.FIsu     = tmpFIssu             .       
                    tt-dailyReport.DiscountAmount = bills.discountedAmount.

                    CASE Payments.PayMethod:
                        WHEN "Cash" THEN tt-dailyReport.Cash = Payments.Amount.
                        WHEN "Credit" THEN tt-dailyReport.Credit = Payments.Amount.
                        WHEN "Cheque" THEN tt-dailyReport.Cheque = Payments.Amount.
                    END CASE.
                    tmpID = tmpID + 1.
                END.
                RELEASE area.
            END.
            RELEASE bills.
        END.
        ELSE
        DO:
            CASE Payments.PayMethod:
                WHEN "Cash" THEN tt-dailyReport.Cash = tt-dailyReport.Cash + Payments.Amount.
                WHEN "Credit" THEN tt-dailyReport.Credit = tt-dailyReport.Credit + Payments.Amount.
                WHEN "Cheque" THEN tt-dailyReport.Cheque = tt-dailyReport.Cheque + Payments.Amount.
            END CASE.
        END.
    END.

    IF cmbVeh <> -1 THEN
    DO:
        FOR EACH tt-dailyReport.
            FIND FIRST bills WHERE tt-dailyReport.bill# = bills.bill# NO-ERROR.
            IF AVAILABLE bills THEN
            DO:
                IF bills.vehNo <> cmbVeh THEN DELETE tt-dailyReport.
            END.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DailyTransHistory C-Win 
PROCEDURE DailyTransHistory :
EMPTY TEMP-TABLE tt-transactionHistory.
    EMPTY TEMP-TABLE tt-transactionLastDay.

    DEFINE VARIABLE lastDate AS DATE        NO-UNDO INIT 1/1/2013.

    FOR EACH Payments WHERE DATE(Payments.datePay) >= DATE(MONTH(tempDate),1,YEAR(tempDate)) 
        AND DATE(Payments.datePay) <= tempDate
        AND Payments.stat = YES.

        tmpGR = 0.
        tmpDmg = 0.
        tmpExp = 0.
        tmpFIssu = 0.
        tmpCash   = 0.
        tmpCheque = 0.
        tmpCredit = 0.
        tmpUnitPrice = 0.
        FIND FIRST tt-transactionHistory WHERE tt-transactionHistory.bill# = Payments.bill# NO-ERROR.
        IF NOT AVAILABLE tt-transactionHistory THEN
        DO:
            FIND FIRST bills WHERE bills.bill# = Payments.bill# NO-ERROR.
            IF AVAILABLE bills THEN
            DO:
                FIND FIRST area WHERE area.ID = bills.areaCode NO-ERROR.
                IF AVAILABLE area THEN
                DO:
                    CREATE tt-transactionHistory.
                    tt-transactionHistory.bill#  = Payments.bill#.
                    tt-transactionHistory.TolSale  = bills.tol            .              
                    tt-transactionHistory.Varience  = bills.varience            .              
        
                    FOR EACH recipts WHERE recipts.bill# = bills.bill# .           
                        FIND FIRST itms WHERE itms.itmID = recipts.item#.          
                            tmpUnitPrice = itms.unitPriceS.                        
                        RELEASE itms.                                              
                        tmpGR = tmpGR + (recipts.GRRD * tmpUnitPrice).      
                        tmpDmg = tmpDmg + (recipts.damP * tmpUnitPrice).           
                        tmpExp = tmpExp + (recipts.expP * tmpUnitPrice).           
                        IF recipts.ItmDiscount = 100.00 THEN                       
                            tmpFIssu = tmpFIssu + (recipts.pieses * tmpUnitPrice). 
                    END.                                             
                    
                    tt-transactionHistory.Ex       = tmpExp               .       
                    tt-transactionHistory.Dmg      = tmpDmg               .       
                    tt-transactionHistory.Discount = bills.discountedAmount   .   
                    tt-transactionHistory.Gr       = tmpGR                .       
                    tt-transactionHistory.FIsu     = tmpFIssu             .       
                    tt-transactionHistory.DiscountAmount = bills.discountedAmount.
                    tt-transactionHistory.Unpaid = bills.tol - bills.paidAmount.

                    CASE Payments.PayMethod:
                        WHEN "Cash" THEN tt-transactionHistory.Cash = Payments.Amount.
                        WHEN "Credit" THEN tt-transactionHistory.Credit = Payments.Amount.
                        WHEN "Cheque" THEN tt-transactionHistory.Cheque = Payments.Amount.
                    END CASE.
                END.
                RELEASE area.
            END.
            RELEASE bills.
        END.
        ELSE
        DO:
            CASE Payments.PayMethod:
                WHEN "Cash" THEN tt-transactionHistory.Cash = tt-transactionHistory.Cash + Payments.Amount.
                WHEN "Credit" THEN tt-transactionHistory.Credit = tt-transactionHistory.Credit + Payments.Amount.
                WHEN "Cheque" THEN tt-transactionHistory.Cheque = tt-transactionHistory.Cheque + Payments.Amount.
            END CASE.
        END.

        IF date(Payments.datePay) > lastDate THEN lastDate = date(Payments.datePay).
    END.

    IF cmbVeh <> -1 THEN
    DO:
        FOR EACH tt-transactionHistory.
            FIND FIRST bills WHERE tt-transactionHistory.bill# = bills.bill# NO-ERROR.
            IF AVAILABLE bills THEN
            DO:
                IF bills.vehNo <> cmbVeh THEN DELETE tt-transactionHistory.
            END.
        END.
    END.

    


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
  DISPLAY filTotal filTotalFissu filTotalCash radTimePeriod cmbVeh filTotalDisc 
          filTotalExp filTotalCredit filValue filTotalDam filTotalCheque 
          filToCollect filTotalGr filTotalVarience 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-1 RECT-4 filTotal filTotalFissu filTotalCash radTimePeriod cmbVeh 
         btnView filTotalDisc filTotalExp filTotalCredit filValue filTotalDam 
         filTotalCheque btnPrint filToCollect filTotalGr filTotalVarience brw 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Monthly C-Win 
PROCEDURE Monthly :
EMPTY TEMP-TABLE tt-dailyReport.
    IF cmbVeh = -1 THEN
    DO:
    FOR EACH bills WHERE MONTH(bills.bilDate) = MONTH(tempDate) BY bills.BillNo.
        tmpGR = 0.
        tmpDmg = 0.
        tmpExp = 0.
        tmpFIssu = 0.
        tmpCash   = 0.   
        tmpCheque = 0.
        tmpCredit = 0.
        tmpUnitPrice = 0.

        FOR EACH recipts WHERE recipts.bill# = bills.bill# .
            FIND FIRST itms WHERE itms.itmID = recipts.item#.
                tmpUnitPrice = itms.unitPriceS.
            RELEASE itms.
            tmpGR = tmpGR + (recipts.GRRD * tmpUnitPrice).
            tmpDmg = tmpDmg + (recipts.damP * tmpUnitPrice).
            tmpExp = tmpExp + (recipts.expP * tmpUnitPrice).
            IF recipts.ItmDiscount = 100.00 THEN
                tmpFIssu = tmpFIssu + (recipts.pieses * tmpUnitPrice).
        END.

        FOR EACH Payments WHERE Payments.bill# = bills.bill#  .
            IF Payments.PayMethod = "Cash" THEN
                tmpCash = tmpCash + Payments.Amount.
            IF Payments.PayMethod = "Credit" THEN
                tmpCredit = tmpCredit + Payments.Amount.
            IF Payments.PayMethod = "Cheque" AND Payments.stat = YES THEN
                tmpCheque = tmpCheque + Payments.Amount.
        END.

        FIND FIRST area WHERE area.ID = bills.areaCode.

        CREATE tt-dailyReport.
        tt-dailyReport.ID       = tmpID                .
        tt-dailyReport.bill#    = bills.bill#          .
        tt-dailyReport.BillNo   = bills.BillNo         .
        tt-dailyReport.Customer = bills.cusName + " - " + area.descrip       .   
        tt-dailyReport.TolSale  = bills.tol            .   
        tt-dailyReport.TolValue = bills.tol + bills.discountedAmount         .   
        tt-dailyReport.Varience = bills.varience         .   
        tt-dailyReport.Ex       = tmpExp               .
        tt-dailyReport.Dmg      = tmpDmg               .
        tt-dailyReport.Discount = bills.discountedAmount   .
        tt-dailyReport.Cash     = tmpCash              .
        tt-dailyReport.Cheque   = tmpCheque            .
        tt-dailyReport.Credit   = tmpCredit            .
        tt-dailyReport.Gr       = tmpGR                .
        tt-dailyReport.FIsu     = tmpFIssu             .
        tt-dailyReport.DiscountAmount = bills.discountedAmount.
        
        tmpID = tmpID + 1.
    END.
    END.
    ELSE
    DO:
    FOR EACH bills WHERE MONTH(bills.bilDate) = MONTH(tempDate)  AND bills.vehNo = cmbVeh  BY bills.BillNo.
        tmpGR = 0.
        tmpDmg = 0.
        tmpExp = 0.
        tmpFIssu = 0.
        tmpCash   = 0.   
        tmpCheque = 0.
        tmpCredit = 0.
        tmpUnitPrice = 0.

        FOR EACH recipts WHERE recipts.bill# = bills.bill# .
            FIND FIRST itms WHERE itms.itmID = recipts.item#.
                tmpUnitPrice = itms.unitPriceS.
            RELEASE itms.
            tmpGR = tmpGR + (recipts.GRRD * tmpUnitPrice).
            tmpDmg = tmpDmg + (recipts.damP * tmpUnitPrice).
            tmpExp = tmpExp + (recipts.expP * tmpUnitPrice).
            IF recipts.ItmDiscount = 100.00 THEN
                tmpFIssu = tmpFIssu + (recipts.pieses * tmpUnitPrice).
        END.

        FOR EACH Payments WHERE Payments.bill# = bills.bill#  .
            IF Payments.PayMethod = "Cash" THEN
                tmpCash = tmpCash + Payments.Amount.
            IF Payments.PayMethod = "Credit" THEN
                tmpCredit = tmpCredit + Payments.Amount.
            IF Payments.PayMethod = "Cheque" AND Payments.stat = YES THEN
                tmpCheque = tmpCheque + Payments.Amount.
        END.

        FIND FIRST area WHERE area.ID = bills.areaCode.

        CREATE tt-dailyReport.
        tt-dailyReport.ID       = tmpID                .
        tt-dailyReport.bill#    = bills.bill#          .
        tt-dailyReport.BillNo   = bills.BillNo         .
        tt-dailyReport.Customer = bills.cusName + " - " + area.descrip       .   
        tt-dailyReport.TolSale  = bills.tol            .   
        tt-dailyReport.TolValue = bills.tol + bills.discountedAmount         .   
        tt-dailyReport.Varience = bills.varience         .   
        tt-dailyReport.Ex       = tmpExp               .
        tt-dailyReport.Dmg      = tmpDmg               .
        tt-dailyReport.Discount = bills.discountedAmount   .
        tt-dailyReport.Cash     = tmpCash              .
        tt-dailyReport.Cheque   = tmpCheque            .
        tt-dailyReport.Credit   = tmpCredit            .
        tt-dailyReport.Gr       = tmpGR                .
        tt-dailyReport.FIsu     = tmpFIssu             .
        tt-dailyReport.DiscountAmount = bills.discountedAmount.
        
        tmpID = tmpID + 1.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MonthlyTrans C-Win 
PROCEDURE MonthlyTrans :
EMPTY TEMP-TABLE tt-dailyReport.

    FOR EACH Payments WHERE month(DATE(Payments.datePay)) = month(DATE(tempDate)) AND Payments.stat = YES.
        tmpGR = 0.
        tmpDmg = 0.
        tmpExp = 0.
        tmpFIssu = 0.
        tmpCash   = 0.
        tmpCheque = 0.
        tmpCredit = 0.
        tmpUnitPrice = 0.
        FIND FIRST tt-dailyReport WHERE tt-dailyReport.bill# = Payments.bill# NO-ERROR.
        IF NOT AVAILABLE tt-dailyReport THEN
        DO:
            FIND FIRST bills WHERE bills.bill# = Payments.bill# NO-ERROR.
            IF AVAILABLE bills THEN
            DO:
                FIND FIRST area WHERE area.ID = bills.areaCode NO-ERROR.
                IF AVAILABLE area THEN
                DO:
                    CREATE tt-dailyReport.
                    tt-dailyReport.ID     = tmpID.
                    tt-dailyReport.bill#  = Payments.bill#.
                    tt-dailyReport.BillNo   = bills.BillNo         .              
                    tt-dailyReport.Customer = bills.cusName + " - " + area.areaCode.
                    tt-dailyReport.TolSale  = bills.tol            .              
                    tt-dailyReport.TolValue = bills.tol + bills.discountedAmount  .
                    tt-dailyReport.Varience = bills.varience         .   
                    tt-dailyReport.Unpaid   = bills.tol - bills.paidAmount.
        
                    FOR EACH recipts WHERE recipts.bill# = bills.bill# .           
                        FIND FIRST itms WHERE itms.itmID = recipts.item#.          
                            tmpUnitPrice = itms.unitPriceS.                        
                        RELEASE itms.                                              
                        tmpGR = tmpGR + (recipts.GRRD * tmpUnitPrice).      
                        tmpDmg = tmpDmg + (recipts.damP * tmpUnitPrice).           
                        tmpExp = tmpExp + (recipts.expP * tmpUnitPrice).           
                        IF recipts.ItmDiscount = 100.00 THEN                       
                            tmpFIssu = tmpFIssu + (recipts.pieses * tmpUnitPrice). 
                    END.                                             
                    
                    tt-dailyReport.Ex       = tmpExp               .       
                    tt-dailyReport.Dmg      = tmpDmg               .       
                    tt-dailyReport.Discount = bills.discountedAmount   .   
                    tt-dailyReport.Gr       = tmpGR                .       
                    tt-dailyReport.FIsu     = tmpFIssu             .       
                    tt-dailyReport.DiscountAmount = bills.discountedAmount.

                    CASE Payments.PayMethod:
                        WHEN "Cash" THEN tt-dailyReport.Cash = Payments.Amount.
                        WHEN "Credit" THEN tt-dailyReport.Credit = Payments.Amount.
                        WHEN "Cheque" THEN tt-dailyReport.Cheque = Payments.Amount.
                    END CASE.
                    tmpID = tmpID + 1.
                END.
                RELEASE area.
            END.
            RELEASE bills.
        END.
        ELSE
        DO:
            CASE Payments.PayMethod:
                WHEN "Cash" THEN tt-dailyReport.Cash = tt-dailyReport.Cash + Payments.Amount.
                WHEN "Credit" THEN tt-dailyReport.Credit = tt-dailyReport.Credit + Payments.Amount.
                WHEN "Cheque" THEN tt-dailyReport.Cheque = tt-dailyReport.Cheque + Payments.Amount.
            END CASE.
        END.
    END.

    IF cmbVeh <> -1 THEN
    DO:
        FOR EACH tt-dailyReport.
            FIND FIRST bills WHERE tt-dailyReport.bill# = bills.bill# NO-ERROR.
            IF AVAILABLE bills THEN
            DO:
                IF bills.vehNo <> cmbVeh THEN DELETE tt-dailyReport.
            END.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE vehloader C-Win 
PROCEDURE vehloader :
FOR EACH vehical.
    cmbVeh:ADD-LAST(veh# + " - " + descrip,ID) IN FRAME DEFAULT-FRAME.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Yearly C-Win 
PROCEDURE Yearly :
EMPTY TEMP-TABLE tt-dailyReport.
    IF cmbVeh = -1 THEN
    DO:
    FOR EACH bills WHERE YEAR(bills.bilDate) = YEAR(tempDate) BY bills.BillNo.
    tmpGR = 0.
        tmpDmg = 0.
        tmpExp = 0.
        tmpFIssu = 0.
        tmpCash   = 0.   
        tmpCheque = 0.
        tmpCredit = 0.
        tmpUnitPrice = 0.

        FOR EACH recipts WHERE recipts.bill# = bills.bill# .
            FIND FIRST itms WHERE itms.itmID = recipts.item#.
                tmpUnitPrice = itms.unitPriceS.
            RELEASE itms.
            tmpGR = tmpGR + (recipts.GRRD * tmpUnitPrice).
            tmpDmg = tmpDmg + (recipts.damP * tmpUnitPrice).
            tmpExp = tmpExp + (recipts.expP * tmpUnitPrice).
            IF recipts.ItmDiscount = 100.00 THEN
                tmpFIssu = tmpFIssu + (recipts.pieses * tmpUnitPrice).
        END.

        FOR EACH Payments WHERE Payments.bill# = bills.bill#  .
            IF Payments.PayMethod = "Cash" THEN
                tmpCash = tmpCash + Payments.Amount.
            IF Payments.PayMethod = "Credit" THEN
                tmpCredit = tmpCredit + Payments.Amount.
            IF Payments.PayMethod = "Cheque" AND Payments.stat = YES THEN
                tmpCheque = tmpCheque + Payments.Amount.
        END.

        FIND FIRST area WHERE area.ID = bills.areaCode.

        CREATE tt-dailyReport.
        tt-dailyReport.ID       = tmpID                .
        tt-dailyReport.bill#    = bills.bill#          .
        tt-dailyReport.BillNo   = bills.BillNo         .
        tt-dailyReport.Customer = bills.cusName + " - " + area.descrip       .   
        tt-dailyReport.TolSale  = bills.tol            .   
        tt-dailyReport.TolValue = bills.tol + bills.discountedAmount         .   
        tt-dailyReport.Varience = bills.varience         .   
        tt-dailyReport.Ex       = tmpExp               .
        tt-dailyReport.Dmg      = tmpDmg               .
        tt-dailyReport.Discount = bills.discountedAmount   .
        tt-dailyReport.Cash     = tmpCash              .
        tt-dailyReport.Cheque   = tmpCheque            .
        tt-dailyReport.Credit   = tmpCredit            .
        tt-dailyReport.Gr       = tmpGR                .
        tt-dailyReport.FIsu     = tmpFIssu             .
        tt-dailyReport.DiscountAmount = bills.discountedAmount.
        
        tmpID = tmpID + 1.
    END.
    END.
    ELSE
    DO:
    FOR EACH bills WHERE YEAR(bills.bilDate) = YEAR(tempDate) AND bills.vehNo = cmbVeh  BY bills.BillNo.
    tmpGR = 0.
        tmpDmg = 0.
        tmpExp = 0.
        tmpFIssu = 0.
        tmpCash   = 0.   
        tmpCheque = 0.
        tmpCredit = 0.
        tmpUnitPrice = 0.

        FOR EACH recipts WHERE recipts.bill# = bills.bill# .
            FIND FIRST itms WHERE itms.itmID = recipts.item#.
                tmpUnitPrice = itms.unitPriceS.
            RELEASE itms.
            tmpGR = tmpGR + (recipts.GRRD * tmpUnitPrice).
            tmpDmg = tmpDmg + (recipts.damP * tmpUnitPrice).
            tmpExp = tmpExp + (recipts.expP * tmpUnitPrice).
            IF recipts.ItmDiscount = 100.00 THEN
                tmpFIssu = tmpFIssu + (recipts.pieses * tmpUnitPrice).
        END.

        FOR EACH Payments WHERE Payments.bill# = bills.bill#  .
            IF Payments.PayMethod = "Cash" THEN
                tmpCash = tmpCash + Payments.Amount.
            IF Payments.PayMethod = "Credit" THEN
                tmpCredit = tmpCredit + Payments.Amount.
            IF Payments.PayMethod = "Cheque" AND Payments.stat = YES THEN
                tmpCheque = tmpCheque + Payments.Amount.
        END.

        FIND FIRST area WHERE area.ID = bills.areaCode.

        CREATE tt-dailyReport.
        tt-dailyReport.ID       = tmpID                .
        tt-dailyReport.bill#    = bills.bill#          .
        tt-dailyReport.BillNo   = bills.BillNo         .
        tt-dailyReport.Customer = bills.cusName + " - " + area.descrip       .   
        tt-dailyReport.TolSale  = bills.tol            .   
        tt-dailyReport.TolValue = bills.tol + bills.discountedAmount         .   
        tt-dailyReport.Varience = bills.varience         .   
        tt-dailyReport.Ex       = tmpExp               .
        tt-dailyReport.Dmg      = tmpDmg               .
        tt-dailyReport.Discount = bills.discountedAmount   .
        tt-dailyReport.Cash     = tmpCash              .
        tt-dailyReport.Cheque   = tmpCheque            .
        tt-dailyReport.Credit   = tmpCredit            .
        tt-dailyReport.Gr       = tmpGR                .
        tt-dailyReport.FIsu     = tmpFIssu             .
        tt-dailyReport.DiscountAmount = bills.discountedAmount.
        
        tmpID = tmpID + 1.
    END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE YearlyTrans C-Win 
PROCEDURE YearlyTrans :
EMPTY TEMP-TABLE tt-dailyReport.

    FOR EACH Payments WHERE YEAR(DATE(Payments.datePay)) = YEAR(DATE(tempDate)) AND Payments.stat = YES.
        tmpGR = 0.
        tmpDmg = 0.
        tmpExp = 0.
        tmpFIssu = 0.
        tmpCash   = 0.
        tmpCheque = 0.
        tmpCredit = 0.
        tmpUnitPrice = 0.
        FIND FIRST tt-dailyReport WHERE tt-dailyReport.bill# = Payments.bill# NO-ERROR.
        IF NOT AVAILABLE tt-dailyReport THEN
        DO:
            FIND FIRST bills WHERE bills.bill# = Payments.bill# NO-ERROR.
            IF AVAILABLE bills THEN
            DO:
                FIND FIRST area WHERE area.ID = bills.areaCode NO-ERROR.
                IF AVAILABLE area THEN
                DO:
                    CREATE tt-dailyReport.
                    tt-dailyReport.ID     = tmpID.
                    tt-dailyReport.bill#  = Payments.bill#.
                    tt-dailyReport.BillNo   = bills.BillNo         .              
                    tt-dailyReport.Customer = bills.cusName + " - " + area.areaCode.
                    tt-dailyReport.TolSale  = bills.tol            .              
                    tt-dailyReport.TolValue = bills.tol + bills.discountedAmount  .
                    tt-dailyReport.Varience = bills.varience         .   
                    tt-dailyReport.Unpaid   = bills.tol - bills.paidAmount.
        
                    FOR EACH recipts WHERE recipts.bill# = bills.bill# .           
                        FIND FIRST itms WHERE itms.itmID = recipts.item#.          
                            tmpUnitPrice = itms.unitPriceS.                        
                        RELEASE itms.                                              
                        tmpGR = tmpGR + (recipts.GRRD * tmpUnitPrice).      
                        tmpDmg = tmpDmg + (recipts.damP * tmpUnitPrice).           
                        tmpExp = tmpExp + (recipts.expP * tmpUnitPrice).           
                        IF recipts.ItmDiscount = 100.00 THEN                       
                            tmpFIssu = tmpFIssu + (recipts.pieses * tmpUnitPrice). 
                    END.                                             
                    
                    tt-dailyReport.Ex       = tmpExp               .       
                    tt-dailyReport.Dmg      = tmpDmg               .       
                    tt-dailyReport.Discount = bills.discountedAmount   .   
                    tt-dailyReport.Gr       = tmpGR                .       
                    tt-dailyReport.FIsu     = tmpFIssu             .       
                    tt-dailyReport.DiscountAmount = bills.discountedAmount.

                    CASE Payments.PayMethod:
                        WHEN "Cash" THEN tt-dailyReport.Cash = Payments.Amount.
                        WHEN "Credit" THEN tt-dailyReport.Credit = Payments.Amount.
                        WHEN "Cheque" THEN tt-dailyReport.Cheque = Payments.Amount.
                    END CASE.
                    tmpID = tmpID + 1.
                END.
                RELEASE area.
            END.
            RELEASE bills.
        END.
        ELSE
        DO:
            CASE Payments.PayMethod:
                WHEN "Cash" THEN tt-dailyReport.Cash = tt-dailyReport.Cash + Payments.Amount.
                WHEN "Credit" THEN tt-dailyReport.Credit = tt-dailyReport.Credit + Payments.Amount.
                WHEN "Cheque" THEN tt-dailyReport.Cheque = tt-dailyReport.Cheque + Payments.Amount.
            END CASE.
        END.
    END.

    IF cmbVeh <> -1 THEN
    DO:
        FOR EACH tt-dailyReport.
            FIND FIRST bills WHERE tt-dailyReport.bill# = bills.bill# NO-ERROR.
            IF AVAILABLE bills THEN
            DO:
                IF bills.vehNo <> cmbVeh THEN DELETE tt-dailyReport.
            END.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

