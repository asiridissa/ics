&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
CREATE WIDGET-POOL.

DEFINE SHARED VARIABLE session_UsersName AS CHARACTER.
DEFINE VARIABLE calendr AS COM-HANDLE   NO-UNDO.
DEFINE VARIABLE calendrTo AS COM-HANDLE   NO-UNDO.
DEFINE VARIABLE fromD AS DATE      NO-UNDO.
DEFINE VARIABLE toD AS DATE        NO-UNDO.
DEFINE VARIABLE tols AS DECIMAL     NO-UNDO.
DEFINE VARIABLE paids AS DECIMAL     NO-UNDO.
DEFINE VARIABLE cnt AS DECIMAL     NO-UNDO.

DEFINE TEMP-TABLE tt-areas
    FIELD ID        AS INT     
    FIELD areaCode  AS CHAR
    FIELD Descrip   AS CHAR 
    FIELD tick      AS LOGICAL
    .

DEFINE TEMP-TABLE tt-bills LIKE bills.

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
&Scoped-define INTERNAL-TABLES tt-bills tt-areas

/* Definitions for BROWSE brw                                           */
&Scoped-define FIELDS-IN-QUERY-brw tt-bills.BillNo tt-bills.bilDate tt-areas.descrip tt-bills.cusName tt-bills.tol tt-bills.paidAmount tt-bills.tol - tt-bills.paidAmount TODAY - tt-bills.bilDate   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brw   
&Scoped-define SELF-NAME brw
&Scoped-define QUERY-STRING-brw FOR EACH tt-bills       WHERE tt-bills.tol <> tt-bills.paidAmount NO-LOCK, ~
             EACH tt-areas WHERE tt-areas.ID = tt-bills.areaCode NO-LOCK     BY tt-bills.bilDate INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-brw OPEN QUERY {&SELF-NAME} FOR EACH tt-bills       WHERE tt-bills.tol <> tt-bills.paidAmount NO-LOCK, ~
             EACH tt-areas WHERE tt-areas.ID = tt-bills.areaCode NO-LOCK     BY tt-bills.bilDate INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-brw tt-bills tt-areas
&Scoped-define FIRST-TABLE-IN-QUERY-brw tt-bills
&Scoped-define SECOND-TABLE-IN-QUERY-brw tt-areas


/* Definitions for BROWSE brw2                                          */
&Scoped-define FIELDS-IN-QUERY-brw2 tt-areas.tick tt-areas.areaCode tt-areas.Descrip   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brw2 tt-areas.tick   
&Scoped-define ENABLED-TABLES-IN-QUERY-brw2 tt-areas
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-brw2 tt-areas
&Scoped-define SELF-NAME brw2
&Scoped-define QUERY-STRING-brw2 FOR EACH tt-areas
&Scoped-define OPEN-QUERY-brw2 OPEN QUERY brw2 FOR EACH tt-areas.
&Scoped-define TABLES-IN-QUERY-brw2 tt-areas
&Scoped-define FIRST-TABLE-IN-QUERY-brw2 tt-areas


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-brw2}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RADIO-SET-2 filBillCount RADIO-SET-1 btnView ~
btnPrint filTotal filPaid filCollection btnClose RECT-1 RECT-2 RECT-3 brw ~
RECT-4 
&Scoped-Define DISPLAYED-OBJECTS RADIO-SET-2 filBillCount RADIO-SET-1 ~
filTotal filPaid filCollection 

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
DEFINE BUTTON btnClose 
     LABEL "Close" 
     SIZE 14 BY 1.

DEFINE BUTTON btnPrint 
     LABEL "Print" 
     SIZE 14 BY 1.

DEFINE BUTTON btnView 
     LABEL "View / Refresh" 
     SIZE 14 BY 1.

DEFINE VARIABLE filBillCount AS INTEGER FORMAT ">>>>>>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88
     BGCOLOR 15 FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE filCollection AS DECIMAL FORMAT ">>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Tol Collection" 
     VIEW-AS FILL-IN 
     SIZE 22 BY .88
     BGCOLOR 15 FGCOLOR 12  NO-UNDO.

DEFINE VARIABLE filPaid AS DECIMAL FORMAT ">>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Tol Paid" 
     VIEW-AS FILL-IN 
     SIZE 22 BY .88
     BGCOLOR 15 FGCOLOR 2  NO-UNDO.

DEFINE VARIABLE filTotal AS DECIMAL FORMAT ">>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Tol Sale" 
     VIEW-AS FILL-IN 
     SIZE 22 BY .88
     BGCOLOR 15 FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE RADIO-SET-1 AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "All", 0,
"None", -1,
"Custom", 1
     SIZE 33.14 BY 1.08 NO-UNDO.

DEFINE VARIABLE RADIO-SET-2 AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "All the time", 1,
"Custom", 2
     SIZE 27 BY 1.08 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL  GROUP-BOX  ROUNDED 
     SIZE 36 BY 4.04.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL  GROUP-BOX  ROUNDED 
     SIZE 36 BY 2.15.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL  GROUP-BOX  ROUNDED 
     SIZE 47 BY 4.04.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL  GROUP-BOX  ROUNDED 
     SIZE 47 BY 2.15.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brw FOR 
      tt-bills, 
      tt-areas SCROLLING.

DEFINE QUERY brw2 FOR 
      tt-areas SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brw
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brw C-Win _FREEFORM
  QUERY brw NO-LOCK DISPLAY
      tt-bills.BillNo FORMAT "x(20)":U WIDTH 10
    tt-bills.bilDate FORMAT "99-99-9999":U WIDTH 10
    tt-areas.descrip COLUMN-LABEL "Area" FORMAT "x(30)":U WIDTH 25
    tt-bills.cusName COLUMN-LABEL "Customer" FORMAT "x(60)":U WIDTH 35
    tt-bills.tol COLUMN-LABEL "Bill Total" FORMAT ">,>>>,>>>,>>9.99":U
             WIDTH 16
    tt-bills.paidAmount COLUMN-LABEL "Paid" FORMAT ">>>,>>>,>>9.99":U
             WIDTH 14
    tt-bills.tol - tt-bills.paidAmount COLUMN-LABEL "Collection" FORMAT ">,>>>,>>>,>>9.99":U
            WIDTH 16
      TODAY  - tt-bills.bilDate COLUMN-LABEL "Days" FORMAT ">>9":U
            WIDTH 4 COLUMN-FGCOLOR 12 LABEL-FGCOLOR 12
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS NO-TAB-STOP SIZE 143 BY 18.85
         FONT 9
         TITLE "Collection Report" ROW-HEIGHT-CHARS .55 FIT-LAST-COLUMN.

DEFINE BROWSE brw2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brw2 C-Win _FREEFORM
  QUERY brw2 DISPLAY
      tt-areas.tick    COLUMN-LABEL "" FORMAT "yes/no":U WIDTH 3 VIEW-AS TOGGLE-BOX 
 tt-areas.areaCode WIDTH 5 LABEL "Code" FORMAT "X(5)"
 tt-areas.Descrip WIDTH 42.5 LABEL "Area" FORMAT "X(40)"
 ENABLE tt-areas.tick
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS NO-TAB-STOP SIZE 55 BY 7.12
         FONT 10
         TITLE "Select Areas" ROW-HEIGHT-CHARS .55 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     RADIO-SET-2 AT ROW 4.65 COL 65.29 NO-LABEL WIDGET-ID 312
     filBillCount AT ROW 5.77 COL 97 COLON-ALIGNED NO-LABEL WIDGET-ID 306 NO-TAB-STOP 
     RADIO-SET-1 AT ROW 2.08 COL 60.86 NO-LABEL WIDGET-ID 280
     btnView AT ROW 2.08 COL 99.29 WIDGET-ID 2
     btnPrint AT ROW 2.08 COL 113.72 WIDGET-ID 234
     filTotal AT ROW 4.69 COL 118 COLON-ALIGNED WIDGET-ID 236 NO-TAB-STOP 
     filPaid AT ROW 5.77 COL 118 COLON-ALIGNED WIDGET-ID 278 NO-TAB-STOP 
     filCollection AT ROW 6.85 COL 118 COLON-ALIGNED WIDGET-ID 290 NO-TAB-STOP 
     btnClose AT ROW 2.08 COL 128.14 WIDGET-ID 304
     brw2 AT ROW 1.15 COL 1.43 WIDGET-ID 300
     brw AT ROW 8.62 COL 1.43 WIDGET-ID 200
     "No. of Bills" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 5.12 COL 99 WIDGET-ID 308
     " Period" VIEW-AS TEXT
          SIZE 6 BY .58 AT ROW 3.96 COL 60.14 WIDGET-ID 298
     "From:" VIEW-AS TEXT
          SIZE 5 BY .62 AT ROW 6.04 COL 62.43 WIDGET-ID 254
     " Areas" VIEW-AS TEXT
          SIZE 5 BY .58 AT ROW 1.27 COL 60.14 WIDGET-ID 284
     "To:" VIEW-AS TEXT
          SIZE 2.72 BY .62 AT ROW 7.04 COL 64.57 WIDGET-ID 274
     " Totals" VIEW-AS TEXT
          SIZE 5.57 BY .58 AT ROW 3.96 COL 98.29 WIDGET-ID 310
     RECT-1 AT ROW 4.23 COL 59 WIDGET-ID 294
     RECT-2 AT ROW 1.54 COL 59 WIDGET-ID 296
     RECT-3 AT ROW 4.23 COL 97 WIDGET-ID 300
     RECT-4 AT ROW 1.54 COL 97 WIDGET-ID 302
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 143.86 BY 26.46
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
         TITLE              = "Collection Report"
         COLUMN             = 1.57
         ROW                = 1.04
         HEIGHT             = 26.46
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
   FRAME-NAME Custom                                                    */
/* BROWSE-TAB brw2 RECT-2 DEFAULT-FRAME */
/* BROWSE-TAB brw RECT-3 DEFAULT-FRAME */
/* SETTINGS FOR BROWSE brw2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       filBillCount:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       filCollection:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       filPaid:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       filTotal:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brw
/* Query rebuild information for BROWSE brw
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-bills
      WHERE tt-bills.tol <> tt-bills.paidAmount NO-LOCK,
      EACH tt-areas WHERE tt-areas.ID = tt-bills.areaCode NO-LOCK
    BY tt-bills.bilDate INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _TblOptList       = ","
     _OrdList          = "ics.bills.bilDate|yes"
     _Where[1]         = "bills.tol <> bills.paidAmount"
     _JoinCode[2]      = "area.ID = bills.areaCode"
     _Query            is NOT OPENED
*/  /* BROWSE brw */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brw2
/* Query rebuild information for BROWSE brw2
     _START_FREEFORM
OPEN QUERY brw2 FOR EACH tt-areas.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE brw2 */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 6.04
       COLUMN          = 67.43
       HEIGHT          = .81
       WIDTH           = 23.72
       WIDGET-ID       = 232
       HIDDEN          = no
       SENSITIVE       = yes.

CREATE CONTROL-FRAME CtrlFrame-2 ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 7.04
       COLUMN          = 67.29
       HEIGHT          = .81
       WIDTH           = 23.72
       WIDGET-ID       = 270
       HIDDEN          = no
       SENSITIVE       = yes.
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {20DD1B9E-87C4-11D1-8BE3-0000F8754DA1} type: DTPicker */
/* CtrlFrame-2 OCXINFO:CREATE-CONTROL from: {20DD1B9E-87C4-11D1-8BE3-0000F8754DA1} type: DTPicker */
      CtrlFrame:MOVE-AFTER(RADIO-SET-1:HANDLE IN FRAME DEFAULT-FRAME).
      CtrlFrame-2:MOVE-AFTER(CtrlFrame).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Collection Report */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit.
  IF THIS-PROCEDURE:PERSISTENT THEN  */ RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Collection Report */
DO:
  /* This event will close the window and terminate the procedure.  */
  MESSAGE "Conferm to close?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE yn AS LOGICAL.
    IF yn = YES THEN
    DO:
      APPLY "CLOSE":U TO THIS-PROCEDURE.
      RETURN NO-APPLY.
    END.
    ELSE
        RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnClose
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnClose C-Win
ON CHOOSE OF btnClose IN FRAME DEFAULT-FRAME /* Close */
DO:
APPLY "WINDOW-CLOSE":U TO {&WINDOW-NAME}. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPrint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPrint C-Win
ON CHOOSE OF btnPrint IN FRAME DEFAULT-FRAME /* Print */
DO:
    DEFINE VARIABLE tempTol AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE tempPaid AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE tempCnt AS INT     NO-UNDO.
    DEFINE VARIABLE week1 AS INT     NO-UNDO.
    DEFINE VARIABLE week2 AS INT     NO-UNDO.
    DEFINE VARIABLE week3 AS INT     NO-UNDO.
    DEFINE VARIABLE weeks AS INT     NO-UNDO.
    DEFINE VARIABLE week1Tol AS INT     NO-UNDO.
    DEFINE VARIABLE week2Tol AS INT     NO-UNDO.
    DEFINE VARIABLE week3Tol AS INT     NO-UNDO.
    DEFINE VARIABLE weeksTol AS INT     NO-UNDO.

    DEFINE VARIABLE chartString AS CHARACTER   NO-UNDO.

    IF filBillCount = 0 THEN
    DO:
        MESSAGE "No records available to print." VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN.
    END.

    fromD = calendr:VALUE.
    toD = calendrTo:VALUE.

    IF fromD > toD OR fromD > TODAY OR toD > TODAY THEN
    DO:
        MESSAGE "Invalid time period." SKIP
            "Dates can not be future dates and," SKIP
            "<From> date cannot be newer than <To> date."
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
        RETURN.
    END.

    MESSAGE "Conferm to print?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE yn AS LOGICAL.
    IF yn = YES THEN
    DO:
        OUTPUT TO VALUE("E:\ICS\bin\print\Collection_Report.txt"). 

        PUT UNFORMAT "|||||From : " + STRING(fromD,"99/99/9999") + " To : "  + STRING(toD,"99/99/9999") +  "    By user : " + session_UsersName SKIP. 
        PUT UNFORMAT "|No|Date|Bill No.|Customer|Bill Total|Paid|Collection|Days" SKIP. 
        chartString = "||||From : " + STRING(fromD,"99/99/9999") + " To : "  + STRING(toD,"99/99/9999") +  "    By user : " + session_UsersName + "~n"
            + "Code|Area|Bills|Sub-Total|Paid-Total|Collections|<=7|7<=14|14<=21|21<~n".


        FOR EACH tt-areas WHERE tt-areas.tick = TRUE.
            PUT UNFORMAT STRING(tt-areas.descrip ) + " (" + STRING(tt-areas.areaCode) + ")" SKIP.   
            chartString = chartString + tt-areas.areaCode + "|" + tt-areas.descrip + "|".
            week1 = 0.
            week2 = 0.
            week3 = 0.
            weeks = 0.
            FOR EACH tt-bills WHERE tt-bills.tol <> bills.paidAmount AND tt-areas.ID = tt-bills.areaCode AND tt-bills.bilDate >= fromD AND tt-bills.bilDate <= toD.
                
                ACCUMULATE tt-bills.tol (COUNT TOTAL).
                ACCUMULATE tt-bills.paidAmount (TOTAL).                

                PUT UNFORMAT "|" + STRING(ACCUMULATE COUNT tt-bills.tol) + "|" + STRING(tt-bills.bilDate) + "|" .
                PUT UNFORMAT STRING(tt-bills.BillNo   ) + "|" .
                PUT UNFORMAT STRING(tt-bills.cusName   ) + "|" .
                PUT UNFORMAT STRING(tt-bills.tol ) + "|" .
                PUT UNFORMAT STRING(tt-bills.paidAmount ) + "|" .
                PUT UNFORMAT STRING(tt-bills.tol - tt-bills.paidAmount  ) + "|".
                PUT UNFORMAT STRING(TODAY - tt-bills.bilDate  ) + "|" SKIP.

                IF (TODAY - tt-bills.bilDate) <= 7 THEN
                DO:
                    week1 = week1 + 1. 
                    week1Tol = week1Tol + 1. 
                END.
                ELSE IF (TODAY - tt-bills.bilDate) <= 14 THEN 
                DO: 
                    week2 = week2 + 1. 
                    week2Tol = week2Tol + 1. 
                END.
                ELSE IF (TODAY - tt-bills.bilDate) <= 21 THEN 
                DO: 
                    week3 = week3 + 1.
                    week3Tol = week3Tol + 1.
                END.
                ELSE 
                DO: 
                    weeks = weeks + 1.
                    weeksTol = weeksTol + 1.
                END.
                
            END.

            PUT UNFORMAT "|||Sub-Total :|" + STRING(ACCUMULATE COUNT tt-bills.tol) + "|" .         
            PUT UNFORMAT STRING(ACCUMULATE TOTAL tt-bills.tol) + "|". 
            PUT UNFORMAT STRING(ACCUMULATE TOTAL tt-bills.paidAmount) + "|".
            PUT UNFORMAT STRING((ACCUMULATE TOTAL tt-bills.tol) - (ACCUMULATE TOTAL tt-bills.paidAmount)) + "|" SKIP.

            chartString = chartString + STRING(ACCUMULATE COUNT tt-bills.tol) + "|" + STRING(ACCUMULATE TOTAL tt-bills.tol) + "|"
                 + STRING(ACCUMULATE TOTAL tt-bills.paidAmount) + "|" + STRING((ACCUMULATE TOTAL tt-bills.tol) - (ACCUMULATE TOTAL tt-bills.paidAmount)) + "|"
                + STRING(week1) + "|" + STRING(week2) + "|" + STRING(week3) + "|" + STRING(weeks) + "~n".

            tempTol = tempTol + DECIMAL(ACCUMULATE TOTAL tt-bills.tol).
            tempPaid = tempPaid + DECIMAL(ACCUMULATE TOTAL tt-bills.paidAmount).
            tempCnt = tempCnt + ACCUMULATE COUNT tt-bills.tol.
        END.

        PUT UNFORMAT " " SKIP "|||Total :|" + STRING(tempCnt) + "|" .         
        PUT UNFORMAT STRING(tempTol) + "|".
        PUT UNFORMAT STRING(tempPaid) + "|".
        PUT UNFORMAT STRING(tempTol - tempPaid) + "|" SKIP.

        chartString = chartString + "| Total|"  + STRING(tempCnt) + "|" + STRING(tempTol) + "|" + STRING(tempPaid) + "|" + STRING(tempTol - tempPaid) + "|"
            + STRING(week1Tol) + "|" + STRING(week2Tol) + "|" + STRING(week3Tol) + "|" + STRING(weeksTol) .

        OUTPUT CLOSE.                                                                                                                  

        OUTPUT TO VALUE("E:\ICS\bin\print\CollectionChart.txt").

        PUT UNFORMAT STRING(chartString).

        OUTPUT CLOSE.

        DOS SILENT START VALUE("E:\ICS\bin\print\CollectionReport.bat").       
        DOS SILENT START excel VALUE("E:\ICS\bin\print\CollectionReport.xlsx").

    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnView
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnView C-Win
ON CHOOSE OF btnView IN FRAME DEFAULT-FRAME /* View / Refresh */
DO:
    fromD = calendr:VALUE.
    toD = calendrTo:VALUE.

    IF fromD > toD OR fromD > TODAY OR toD > TODAY THEN
    DO:
        MESSAGE "Invalid time period." SKIP
            "Dates can not be future dates and" SKIP
            "<From> date cannot be newer than <To> date."
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
        RETURN.
    END.

    OPEN QUERY brw FOR EACH tt-bills
      WHERE tt-bills.tol <> tt-bills.paidAmount NO-LOCK,
      EACH tt-areas
      WHERE tt-areas.ID = tt-bills.areaCode
        AND tt-areas.tick = TRUE
        AND tt-bills.bilDate >= fromD AND tt-bills.bilDate <= toD
        NO-LOCK
    BY tt-bills.areaCode BY tt-bills.bilDate.

        tols = 0.
        paids = 0.
        cnt = 0.
    FOR EACH bills
      WHERE bills.tol <> bills.paidAmount NO-LOCK,
        EACH tt-areas
      WHERE tt-areas.ID = bills.areaCode
        AND tt-areas.tick = TRUE
        AND bills.bilDate >= fromD AND bills.bilDate <= toD NO-LOCK
      BY bills.bilDate.
        tols = tols + bills.tol.
        paids = paids +  bills.paidAmount.
        cnt = cnt + 1.
    END.

    filPaid = paids.
    filTotal = tols.
    filCollection = tols - paids.
    filBillCount = cnt.

    DISPLAY filPaid filTotal filCollection filBillCount WITH FRAME {&FRAME-NAME}.

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


&Scoped-define SELF-NAME filBillCount
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filBillCount C-Win
ON LEAVE OF filBillCount IN FRAME DEFAULT-FRAME
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RADIO-SET-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RADIO-SET-1 C-Win
ON VALUE-CHANGED OF RADIO-SET-1 IN FRAME DEFAULT-FRAME
DO:
    ASSIGN {&SELF-NAME}.
    CASE {&SELF-NAME}:
        WHEN 0 THEN
        DO:
            FOR EACH tt-areas.
                tick = TRUE.
            END.
            DISABLE brw2 WITH FRAME {&FRAME-NAME}.
        END.
        WHEN -1 THEN
        DO:
            FOR EACH tt-areas.
                tick = FALSE.
            END.
            DISABLE brw2 WITH FRAME {&FRAME-NAME}.
        END.
        OTHERWISE
        DO:
            ENABLE brw2 WITH FRAME {&FRAME-NAME}.
        END.
    END CASE.
    OPEN QUERY brw2 FOR EACH tt-areas.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RADIO-SET-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RADIO-SET-2 C-Win
ON VALUE-CHANGED OF RADIO-SET-2 IN FRAME DEFAULT-FRAME
DO:
    ASSIGN {&SELF-NAME}.
    IF {&SELF-NAME} = 1 THEN
    DO:
        calendr:ENABLED = FALSE.
        calendrTo:ENABLED = FALSE.
        calendr:VALUE = 01/01/2013.
        calendrTo:VALUE = TODAY.
    END.
    ELSE
    DO:
        calendr:ENABLED = TRUE.
        calendrTo:ENABLED = TRUE.
        calendr:VALUE = TODAY - 30.
        calendrTo:VALUE = TODAY.
    END.
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
  RUN loadAreas.
  RUN loadBills.
  
  DEFINE SHARED VARIABLE session_Path AS CHAR.
  DEFINE SHARED VARIABLE session_icon AS CHAR.
  {&WINDOW-NAME}:TITLE = session_Path.
  {&WINDOW-NAME}:LOAD-ICON(session_icon).

  calendr = chCtrlFrame:DTPicker.
  calendrTo = chCtrlFrame-2:DTPicker.
  calendr:ENABLED = FALSE.
  calendr:VALUE = 01/01/2013.
  calendrTo:ENABLED = FALSE.
  calendrTo:VALUE = TODAY.

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

OCXFile = SEARCH( "CollectionReport.wrx":U ).
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
ELSE MESSAGE "CollectionReport.wrx":U SKIP(1)
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
  DISPLAY RADIO-SET-2 filBillCount RADIO-SET-1 filTotal filPaid filCollection 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RADIO-SET-2 filBillCount RADIO-SET-1 btnView btnPrint filTotal filPaid 
         filCollection btnClose RECT-1 RECT-2 RECT-3 brw RECT-4 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE loadAreas C-Win 
PROCEDURE loadAreas :
FOR EACH area.
    CREATE tt-areas.
    tt-areas.ID       =   area.ID        .
    tt-areas.areaCode =   area.areaCode  .
    tt-areas.Descrip  =   area.descrip   .
    tt-areas.tick     =   TRUE          .
/*     CREATE tt-area.                       */
/*     tt-area.ID       =   area.ID        . */
/*     tt-area.areaCode =   area.areaCode  . */
/*     tt-area.Descrip  =   area.descrip   . */
END.

OPEN QUERY brw2 FOR EACH tt-areas.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE loadBills C-Win 
PROCEDURE loadBills :
EMPTY TEMP-TABLE tt-bills.
FOR EACH bills.

    CREATE tt-bills.
    IF bills.tol > 0 AND bills.tol <> bills.paidAmount THEN
    BUFFER-COPY bills TO tt-bills.
/*      tt-bills.areaCode         = bills.areaCode           . */
/*      tt-bills.bilDate          = bills.bilDate            . */
/*      tt-bills.bill#            = bills.bill#              . */
/*      tt-bills.BillNo           = bills.BillNo             . */
/*      tt-bills.cusID            = bills.cusID              . */
/*      tt-bills.cusName          = bills.cusName            . */
/*      tt-bills.discountedAmount = bills.discountedAmount   . */
/*      tt-bills.discountRate     = bills.discountRate       . */
/*      tt-bills.empCode          = bills.empCode            . */
/*      tt-bills.paidAmount       = bills.paidAmount         . */
/*      tt-bills.tol              = bills.tol                . */
/*      tt-bills.varience         = bills.varience           . */
/*      tt-bills.vehNo            = bills.vehNo              . */
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

