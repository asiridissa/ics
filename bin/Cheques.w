&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          ics              PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
CREATE WIDGET-POOL.

DEFINE SHARED VARIABLE session_Window AS INT.
DEFINE VARIABLE addModify AS CHARACTER   NO-UNDO.
DEFINE VARIABLE calendr AS COM-HANDLE   NO-UNDO.
DEFINE VARIABLE calendr2 AS COM-HANDLE   NO-UNDO.
DEFINE VARIABLE calendr3 AS COM-HANDLE   NO-UNDO.
    DEFINE VARIABLE tempStatus AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE tempDate AS DATE        NO-UNDO.

DEFINE TEMP-TABLE tt-chqBills
    FIELD bill#        AS INTEGER LABEL "   Bill #        "
    FIELD billNo        AS INTEGER LABEL "   Bill No        "
    FIELD billDate     AS DATE    LABEL " Bill Date "  
    FIELD tol          AS DECIMAL LABEL "       Total        "
    FIELD creditAmount AS DECIMAL LABEL "       To Collect   "
    FIELD debitAmount  AS DECIMAL LABEL "   Pay"
    .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME brwCheques

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES cheques customer tt-chqBills

/* Definitions for BROWSE brwCheques                                    */
&Scoped-define FIELDS-IN-QUERY-brwCheques cheques.cusName cheques.chqNo ~
cheques.amount cheques.chqDate cheques.bankName cheques.Branch cheques.stat ~
cheques.crDate customer.CusArea 
&Scoped-define ENABLED-FIELDS-IN-QUERY-brwCheques 
&Scoped-define QUERY-STRING-brwCheques FOR EACH cheques NO-LOCK, ~
      EACH customer WHERE customer.cusID = cheques.cusID NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-brwCheques OPEN QUERY brwCheques FOR EACH cheques NO-LOCK, ~
      EACH customer WHERE customer.cusID = cheques.cusID NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-brwCheques cheques customer
&Scoped-define FIRST-TABLE-IN-QUERY-brwCheques cheques
&Scoped-define SECOND-TABLE-IN-QUERY-brwCheques customer


/* Definitions for BROWSE brwCreditAmounts                              */
&Scoped-define FIELDS-IN-QUERY-brwCreditAmounts tt-chqBills.billNo tt-chqBills.billDate tt-chqBills.tol tt-chqBills.creditAmount tt-chqBills.debitAmount   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brwCreditAmounts   
&Scoped-define SELF-NAME brwCreditAmounts
&Scoped-define QUERY-STRING-brwCreditAmounts FOR EACH tt-chqBills BY tt-chqBills.billDate
&Scoped-define OPEN-QUERY-brwCreditAmounts OPEN QUERY brwCreditAmounts FOR EACH tt-chqBills BY tt-chqBills.billDate.
&Scoped-define TABLES-IN-QUERY-brwCreditAmounts tt-chqBills
&Scoped-define FIRST-TABLE-IN-QUERY-brwCreditAmounts tt-chqBills


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-brwCheques}~
    ~{&OPEN-QUERY-brwCreditAmounts}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS radFilter brwCheques brwCreditAmounts btnGo ~
filSearch btnAdd btnSearch btnMod btnClose RECT-5 RECT-6 RECT-21 
&Scoped-Define DISPLAYED-OBJECTS filBIllId radFilter filID filBIll# cmbArea ~
cmbCus filChqNo filToCollect filChqAmount filSearch filUnDedicated filBank ~
cmbSearchCol filAmount cmbSearchArea filBranch cmbSearchTime radStat 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlFrame-2 AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame-2 AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE CtrlFrame-4 AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame-4 AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnAdd 
     LABEL "Add" 
     SIZE 15 BY 1.

DEFINE BUTTON btnCancel 
     LABEL "Cancel" 
     SIZE 15 BY 1.

DEFINE BUTTON btnClose 
     LABEL "Close" 
     SIZE 15 BY 1.

DEFINE BUTTON btnDel 
     LABEL "Delete" 
     SIZE 15 BY 1.

DEFINE BUTTON btnDoPay 
     LABEL "Select Bill" 
     SIZE 15 BY 1.

DEFINE BUTTON btnGo 
     LABEL "Go" 
     SIZE 5.86 BY 1.

DEFINE BUTTON btnMod 
     LABEL "Modify / Pay" 
     SIZE 15 BY 1.

DEFINE BUTTON btnPay 
     LABEL "Allocate amount" 
     SIZE 15 BY 1.

DEFINE BUTTON btnReturned 
     LABEL "Return" 
     SIZE 15 BY 1.

DEFINE BUTTON btnSave 
     LABEL "Save" 
     SIZE 15 BY 1.

DEFINE BUTTON btnSearch 
     LABEL "Search" 
     SIZE 8 BY 1.

DEFINE VARIABLE cmbArea AS INTEGER FORMAT ">>>>9":U INITIAL 0 
     LABEL "Area" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEM-PAIRS "--Select Here--",0
     DROP-DOWN-LIST
     SIZE 35 BY 1 NO-UNDO.

DEFINE VARIABLE cmbCus AS INTEGER FORMAT ">>>>>>9":U INITIAL 0 
     LABEL "Customer" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "--Select Here--",0
     DROP-DOWN-LIST
     SIZE 35 BY 1 NO-UNDO.

DEFINE VARIABLE cmbSearchArea AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Area" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEM-PAIRS "All",0
     DROP-DOWN-LIST
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE cmbSearchCol AS CHARACTER FORMAT "X(20)":U INITIAL "Customer" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEMS "Customer","Cheque No" 
     DROP-DOWN-LIST
     SIZE 13.14 BY 1 NO-UNDO.

DEFINE VARIABLE cmbSearchTime AS INTEGER FORMAT ">>>,>>9":U INITIAL 0 
     LABEL "Within Last" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEM-PAIRS "All Time",0,
                     "1 Month",1,
                     "2 Months",2,
                     "3 Months",3,
                     "6 Months",6,
                     "1 Year",12,
                     "2 Years",24,
                     "3 Years",36,
                     "4 Years",48,
                     "5 Years",72
     DROP-DOWN-LIST
     SIZE 12.14 BY 1 NO-UNDO.

DEFINE VARIABLE filAmount AS DECIMAL FORMAT ">>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Pay" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88
     BGCOLOR 15 FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE filBank AS CHARACTER FORMAT "X(50)":U 
     LABEL "Bank" 
     VIEW-AS FILL-IN 
     SIZE 35 BY .88
     BGCOLOR 15 FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE filBIll# AS INTEGER FORMAT ">>>>>>>>>9":U INITIAL 0 
     LABEL "Bill No" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88
     BGCOLOR 7 FGCOLOR 11  NO-UNDO.

DEFINE VARIABLE filBIllId AS INTEGER FORMAT ">>>>>>>9":U INITIAL 0 
     LABEL "ID" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88
     BGCOLOR 7 FGCOLOR 11  NO-UNDO.

DEFINE VARIABLE filBranch AS CHARACTER FORMAT "X(50)":U 
     LABEL "Branch" 
     VIEW-AS FILL-IN 
     SIZE 35 BY .88
     BGCOLOR 15 FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE filChqAmount AS DECIMAL FORMAT ">>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Amount" 
     VIEW-AS FILL-IN 
     SIZE 23 BY .88
     BGCOLOR 15 FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE filChqNo AS INT64 FORMAT ">>>>>>>>>>>>>>9":U INITIAL 0 
     LABEL "Cheque No" 
     VIEW-AS FILL-IN 
     SIZE 23 BY .88
     BGCOLOR 15 FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE filID AS INTEGER FORMAT ">>>>>>>>>9":U INITIAL 0 
     LABEL "ID" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88
     BGCOLOR 7 FGCOLOR 11  NO-UNDO.

DEFINE VARIABLE filSearch AS CHARACTER FORMAT "X(256)":U 
     LABEL "Search" 
     VIEW-AS FILL-IN 
     SIZE 25 BY .88 NO-UNDO.

DEFINE VARIABLE filToCollect AS DECIMAL FORMAT ">>>,>>>,>>9.99":U INITIAL 0 
     LABEL "To Collect" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88
     BGCOLOR 15 FGCOLOR 12  NO-UNDO.

DEFINE VARIABLE filUnDedicated AS DECIMAL FORMAT ">>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Available" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88
     BGCOLOR 15 FGCOLOR 2  NO-UNDO.

DEFINE VARIABLE radFilter AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "All", 1,
"Today", 2,
"Chq Date :", 3
     SIZE 23.57 BY 1 NO-UNDO.

DEFINE VARIABLE radStat AS CHARACTER INITIAL "P" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Pending", "P",
"Banked", "B",
"Cleared", "C",
"Bounced", "R"
     SIZE 36.14 BY .73 NO-UNDO.

DEFINE RECTANGLE RECT-21
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL  GROUP-BOX  ROUNDED 
     SIZE 30 BY 3.62.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL  GROUP-BOX  ROUNDED 
     SIZE 95.43 BY 7.81.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL  GROUP-BOX  ROUNDED 
     SIZE 48.29 BY 7.81.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brwCheques FOR 
      cheques, 
      customer SCROLLING.

DEFINE QUERY brwCreditAmounts FOR 
      tt-chqBills SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brwCheques
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brwCheques C-Win _STRUCTURED
  QUERY brwCheques NO-LOCK DISPLAY
      cheques.cusName COLUMN-LABEL "                                    Customer" FORMAT "x(100)":U
            WIDTH 40
      cheques.chqNo COLUMN-LABEL "         Cheque No" FORMAT ">>>>>>>>>>>>>>9":U
      cheques.amount COLUMN-LABEL "         Amount" FORMAT ">>>,>>>,>>>,>>9.99":U
      cheques.chqDate COLUMN-LABEL " Cheq Date" FORMAT "99/99/9999":U
      cheques.bankName COLUMN-LABEL "        Bank" FORMAT "x(200)":U
            WIDTH 20
      cheques.Branch COLUMN-LABEL "      Branch" FORMAT "x(200)":U
            WIDTH 25
      cheques.stat COLUMN-LABEL "Stat" FORMAT "x(1)":U COLUMN-FGCOLOR 4 COLUMN-BGCOLOR 15
      cheques.crDate COLUMN-LABEL "      Date" FORMAT "99/99/9999":U
      customer.CusArea COLUMN-LABEL "  Area" FORMAT "x(100)":U
            WIDTH 5
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 144 BY 15.35
         FONT 10
         TITLE "Cheque Details" ROW-HEIGHT-CHARS .55 FIT-LAST-COLUMN.

DEFINE BROWSE brwCreditAmounts
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brwCreditAmounts C-Win _FREEFORM
  QUERY brwCreditAmounts DISPLAY
      tt-chqBills.billNo FORMAT ">>>>>>>9":U
 tt-chqBills.billDate FORMAT "99/99/9999":U
 tt-chqBills.tol FORMAT ">>>,>>>,>>>,>>9.99":U
 tt-chqBills.creditAmount FORMAT ">>>,>>>,>>>,>>9.99":U
 tt-chqBills.debitAmount FORMAT ">>>,>>>,>>>,>>9.99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 69 BY 7.77
         FONT 10
         TITLE "Pending Bills of the Customer" ROW-HEIGHT-CHARS .55 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     filBIllId AT ROW 18 COL 58.43 COLON-ALIGNED WIDGET-ID 178 NO-TAB-STOP 
     radFilter AT ROW 1.23 COL 7.43 NO-LABEL WIDGET-ID 128
     brwCheques AT ROW 2.35 COL 1.14 WIDGET-ID 200
     brwCreditAmounts AT ROW 17.81 COL 76 WIDGET-ID 300
     filID AT ROW 18.04 COL 10.86 COLON-ALIGNED WIDGET-ID 2
     filBIll# AT ROW 18.96 COL 58.43 COLON-ALIGNED WIDGET-ID 108
     cmbArea AT ROW 18.96 COL 10.86 COLON-ALIGNED WIDGET-ID 80
     cmbCus AT ROW 19.92 COL 10.86 COLON-ALIGNED WIDGET-ID 92
     btnGo AT ROW 1.23 COL 48.29 WIDGET-ID 166 NO-TAB-STOP 
     filChqNo AT ROW 20.88 COL 10.86 COLON-ALIGNED WIDGET-ID 94
     filToCollect AT ROW 19.96 COL 58.43 COLON-ALIGNED WIDGET-ID 114
     filChqAmount AT ROW 21.85 COL 10.86 COLON-ALIGNED WIDGET-ID 96
     filSearch AT ROW 1.23 COL 60.57 COLON-ALIGNED WIDGET-ID 160 NO-TAB-STOP 
     filUnDedicated AT ROW 20.96 COL 58.43 COLON-ALIGNED WIDGET-ID 122
     filBank AT ROW 22.81 COL 10.86 COLON-ALIGNED WIDGET-ID 98
     cmbSearchCol AT ROW 1.23 COL 85.86 COLON-ALIGNED NO-LABEL WIDGET-ID 136 NO-TAB-STOP 
     filAmount AT ROW 23.31 COL 57.43 COLON-ALIGNED WIDGET-ID 116
     cmbSearchArea AT ROW 1.23 COL 103.86 COLON-ALIGNED WIDGET-ID 156 NO-TAB-STOP 
     filBranch AT ROW 23.77 COL 10.86 COLON-ALIGNED WIDGET-ID 100
     btnDoPay AT ROW 22.19 COL 59.57 WIDGET-ID 120
     cmbSearchTime AT ROW 1.23 COL 122 COLON-ALIGNED WIDGET-ID 158 NO-TAB-STOP 
     btnPay AT ROW 24.35 COL 59.57 WIDGET-ID 118
     radStat AT ROW 24.73 COL 12.14 NO-LABEL WIDGET-ID 102
     btnAdd AT ROW 26.12 COL 12.86 WIDGET-ID 30
     btnSearch AT ROW 1.19 COL 136.43 WIDGET-ID 134 NO-TAB-STOP 
     btnMod AT ROW 26.12 COL 29.86 WIDGET-ID 32
     btnDel AT ROW 26.12 COL 46.86 WIDGET-ID 34
     btnReturned AT ROW 26.12 COL 63.86 WIDGET-ID 162
     btnSave AT ROW 26.12 COL 84.86 WIDGET-ID 38
     btnCancel AT ROW 26.12 COL 101.86 WIDGET-ID 36
     btnClose AT ROW 26.12 COL 118.86 WIDGET-ID 40
     "View :" VIEW-AS TEXT
          SIZE 5.29 BY 1 AT ROW 1.23 COL 1.72 WIDGET-ID 132
     "Status:" VIEW-AS TEXT
          SIZE 5.57 BY .62 AT ROW 24.77 COL 6.29 WIDGET-ID 106
     RECT-5 AT ROW 17.81 COL 49.57 WIDGET-ID 168
     RECT-6 AT ROW 17.81 COL 1.57 WIDGET-ID 174
     RECT-21 AT ROW 22 COL 49.57 WIDGET-ID 180
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 144.29 BY 26.54
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
         TITLE              = "Cheques"
         COLUMN             = 1.57
         ROW                = 1.19
         HEIGHT             = 26.54
         WIDTH              = 144.29
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
/* BROWSE-TAB brwCheques radFilter DEFAULT-FRAME */
/* BROWSE-TAB brwCreditAmounts brwCheques DEFAULT-FRAME */
ASSIGN 
       customer.CusArea:VISIBLE IN BROWSE brwCheques = FALSE.

/* SETTINGS FOR BUTTON btnCancel IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnDel IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnDoPay IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnPay IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnReturned IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnSave IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX cmbArea IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX cmbCus IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX cmbSearchArea IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX cmbSearchCol IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX cmbSearchTime IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN filAmount IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN filBank IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN filBIll# IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN filBIllId IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       filBIllId:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN filBranch IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN filChqAmount IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN filChqNo IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN filID IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN filToCollect IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN filUnDedicated IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET radStat IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brwCheques
/* Query rebuild information for BROWSE brwCheques
     _TblList          = "ics.cheques,ics.customer WHERE ics.cheques ..."
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _TblOptList       = ","
     _JoinCode[2]      = "customer.cusID = cheques.cusID"
     _FldNameList[1]   > ics.cheques.cusName
"cheques.cusName" "                                    Customer" ? "character" ? ? ? ? ? ? no ? no no "40" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ics.cheques.chqNo
"cheques.chqNo" "         Cheque No" ? "int64" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ics.cheques.amount
"cheques.amount" "         Amount" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ics.cheques.chqDate
"cheques.chqDate" " Cheq Date" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > ics.cheques.bankName
"cheques.bankName" "        Bank" ? "character" ? ? ? ? ? ? no ? no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > ics.cheques.Branch
"cheques.Branch" "      Branch" ? "character" ? ? ? ? ? ? no ? no no "25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > ics.cheques.stat
"cheques.stat" "Stat" "x(1)" "character" 15 4 ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > ics.cheques.crDate
"cheques.crDate" "      Date" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > ics.customer.CusArea
"customer.CusArea" "  Area" ? "character" ? ? ? ? ? ? no ? no no "5" no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE brwCheques */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brwCreditAmounts
/* Query rebuild information for BROWSE brwCreditAmounts
     _START_FREEFORM
OPEN QUERY brwCreditAmounts FOR EACH tt-chqBills BY tt-chqBills.billDate.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE brwCreditAmounts */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame-4 ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 1.31
       COLUMN          = 31.57
       HEIGHT          = .88
       WIDTH           = 16.14
       WIDGET-ID       = 164
       HIDDEN          = no
       SENSITIVE       = yes.

CREATE CONTROL-FRAME CtrlFrame-2 ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 18.08
       COLUMN          = 27
       HEIGHT          = .81
       WIDTH           = 20.86
       WIDGET-ID       = 72
       HIDDEN          = no
       SENSITIVE       = yes.
/* CtrlFrame-4 OCXINFO:CREATE-CONTROL from: {20DD1B9E-87C4-11D1-8BE3-0000F8754DA1} type: DTPicker */
/* CtrlFrame-2 OCXINFO:CREATE-CONTROL from: {20DD1B9E-87C4-11D1-8BE3-0000F8754DA1} type: DTPicker */
      CtrlFrame-4:MOVE-AFTER(brwCreditAmounts:HANDLE IN FRAME DEFAULT-FRAME).
      CtrlFrame-2:MOVE-AFTER(CtrlFrame-4).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Cheques */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit.
  IF THIS-PROCEDURE:PERSISTENT THEN  */ RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Cheques */
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


&Scoped-define BROWSE-NAME brwCheques
&Scoped-define SELF-NAME brwCheques
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brwCheques C-Win
ON VALUE-CHANGED OF brwCheques IN FRAME DEFAULT-FRAME /* Cheque Details */
DO:
  DEFINE VARIABLE TempCusArea AS CHARACTER   NO-UNDO.
  RUN cusLoaderAll.
  IF AVAILABLE cheques THEN
    DO:
            ASSIGN
            filChqAmount     = cheques.amount   
            filBank          = cheques.bankName 
            filBranch        = cheques.Branch   
            calendr:VALUE    = cheques.chqDate  
            filChqNo         = cheques.chqNo
            filChqAmount     = cheques.amount
            cmbCus           = cheques.cusID    
            filID            = cheques.ID       
            radStat          = cheques.stat 
            .
            FIND FIRST customer WHERE customer.cusID = cheques.cusID.
            IF AVAILABLE customer THEN
                TempCusArea = customer.CusArea.
            RELEASE customer.

            FIND FIRST area WHERE area.descrip = TempCusArea.
            IF AVAILABLE area THEN
                cmbArea = area.ID.
            RELEASE area.

            DISPLAY filChqAmount cmbArea cmbCus filChqNo filChqAmount filID filBank filBranch radStat WITH FRAME {&FRAME-NAME}.
    END.
    ELSE IF NOT AVAILABLE cheques THEN MESSAGE "No records to show." VIEW-AS ALERT-BOX INFO BUTTONS OK .
    RUN billsBrowserUpdate.
    tempStatus = radStat.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brwCreditAmounts
&Scoped-define SELF-NAME brwCreditAmounts
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brwCreditAmounts C-Win
ON VALUE-CHANGED OF brwCreditAmounts IN FRAME DEFAULT-FRAME /* Pending Bills of the Customer */
DO:
  IF AVAILABLE tt-chqBills THEN
  DO:
      filBIll# = tt-chqBills.billNo.
      filToCollect = tt-chqBills.creditAmount.
      filAmount = tt-chqBills.debitAmount.
      filBIllId = tt-chqBills.bill#.
  END.
  DISPLAY filBIll# filBIllId filAmount filToCollect WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAdd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAdd C-Win
ON CHOOSE OF btnAdd IN FRAME DEFAULT-FRAME /* Add */
DO:
    ASSIGN
      cmbArea  = 0
      cmbCus   = 0
      filChqNo = 0
      filID    = 0
      filBank  = ""
      filBranch= ""
      radStat  = "P"
      filChqAmount = 0
      .
      calendr:VALUE = STRING(TODAY,"99/99/9999").
    

  FIND FIRST paramtrs WHERE NAME = "chqID".
    filID = INT(val) + 1.
  RELEASE paramtrs.

  DISPLAY filChqAmount cmbArea cmbCus filChqNo filID filBank filBranch radStat WITH FRAME {&FRAME-NAME}.
  
  calendr:ENABLED = TRUE.
  ENABLE filChqAmount btnCancel btnSave cmbArea cmbCus filChqNo filID filChqAmount filBank filBranch radStat WITH FRAME {&FRAME-NAME}.
  DISABLE brwCheques btnAdd btnClose btnDel btnMod WITH FRAME {&FRAME-NAME}.

  addModify = "add".
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCancel C-Win
ON CHOOSE OF btnCancel IN FRAME DEFAULT-FRAME /* Cancel */
DO:

    MESSAGE "Conferm to cancel." VIEW-AS ALERT-BOX WARNING BUTTONS YES-NO UPDATE yn AS LOGICAL.
    IF yn =YES THEN
    DO:
        ASSIGN
        cmbArea  = 0
        cmbCus   = 0
        filChqNo = 0
        filID    = 0
        filBank  = ""
        filBranch= ""
        radStat  = "P"
        filChqAmount = 0
            .
        calendr:VALUE = STRING(TODAY,"99/99/9999").

        OPEN QUERY brwCheques FOR
              EACH ics.cheques NO-LOCK,
              EACH ics.customer WHERE customer.cusID = cheques.cusID NO-LOCK INDEXED-REPOSITION.
        APPLY "VALUE-CHANGED":U TO brwCheques.
        
  DISPLAY cmbArea cmbCus filChqNo filID filBank filBranch radStat WITH FRAME {&FRAME-NAME}.
  
  calendr:ENABLED = FALSE.
  DISABLE filAmount btnPay btnDoPay btnReturned  filChqAmount btnDoPay btnCancel btnSave cmbArea cmbCus filChqAmount filChqNo filID filBank filBranch radStat WITH FRAME {&FRAME-NAME}.
  ENABLE brwCreditAmounts brwCheques btnAdd btnClose btnDel btnMod WITH FRAME {&FRAME-NAME}.
    END.
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


&Scoped-define SELF-NAME btnDel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDel C-Win
ON CHOOSE OF btnDel IN FRAME DEFAULT-FRAME /* Delete */
DO:
  IF filID <> 0 THEN
  DO:
      MESSAGE "No records to Delete." VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      RETURN.
  END.
  IF filChqAmount <> filUnDedicated THEN
  DO:
      MESSAGE "Check in use. Can not be deleted." VIEW-AS ALERT-BOX WARNING BUTTONS OK.
  END.

  MESSAGE "Conferm to delete the record?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE yn AS LOGICAL.
  IF yn = YES THEN
  DO:
      FIND FIRST cheques WHERE cheques.ID = filID EXCLUSIVE-LOCK NO-ERROR.
      IF AVAILABLE cheques THEN
        DELETE cheques.
      ELSE
          MESSAGE "No Records to Delete." VIEW-AS ALERT-BOX ERROR BUTTONS OK .
      
      OPEN QUERY brwCheques FOR
          EACH ics.cheques NO-LOCK,
          EACH ics.customer WHERE customer.cusID = cheques.cusID NO-LOCK INDEXED-REPOSITION.
      APPLY "VALUE-CHANGED":U TO brwCheques.

      IF NOT ERROR-STATUS:ERROR THEN
          MESSAGE "Record successfully deleted." VIEW-AS ALERT-BOX INFO BUTTONS OK.
          
  END.
  
            
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDoPay
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDoPay C-Win
ON CHOOSE OF btnDoPay IN FRAME DEFAULT-FRAME /* Select Bill */
DO:
    
        ENABLE filAmount btnPay WITH FRAME {&FRAME-NAME}.
        DISABLE btnDoPay brwCreditAmounts WITH FRAME {&FRAME-NAME}.
        DISPLAY filAmount btnDoPay btnPay WITH FRAME {&FRAME-NAME}.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnGo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnGo C-Win
ON CHOOSE OF btnGo IN FRAME DEFAULT-FRAME /* Go */
DO:
  APPLY "VALUE-CHANGED":U TO radFilter IN FRAME DEFAULT-FRAME. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnMod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnMod C-Win
ON CHOOSE OF btnMod IN FRAME DEFAULT-FRAME /* Modify / Pay */
DO:
  IF filID <> 0 THEN
  DO:
      ENABLE btnReturned filChqAmount btnDoPay btnCancel btnSave cmbArea filChqAmount cmbCus filChqNo filID filBank filBranch radStat WITH FRAME {&FRAME-NAME}.
      DISABLE filChqNo brwCheques btnAdd btnClose btnDel btnMod WITH FRAME {&FRAME-NAME}.
      addModify = "modify".
      calendr:ENABLED = TRUE.
  END.
  ELSE
      MESSAGE "No records to Modify." VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  IF filChqAmount <> filUnDedicated THEN DISABLE filChqAmount WITH FRAME {&FRAME-NAME}.
  IF radStat = "R" THEN DISABLE btnDoPay btnReturned WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPay
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPay C-Win
ON CHOOSE OF btnPay IN FRAME DEFAULT-FRAME /* Allocate amount */
DO:
  IF filAmount > filUnDedicated THEN
  DO:
      MESSAGE "Paying Amount too Large." VIEW-AS ALERT-BOX INFO BUTTONS OK.
      filAmount = 0.
      DISPLAY filAmount WITH FRAME DEFAULT-FRAME.
      RETURN.
  END.
  MESSAGE "Confrm to Pay this Bill?" VIEW-AS ALERT-BOX INFO BUTTONS YES-NO UPDATE yn AS LOGICAL.
  IF yn = YES THEN
  DO:
      FIND FIRST tt-chqBills WHERE tt-chqBills.bill# = filBIllId.
      DO:
          IF tt-chqBills.creditAmount >= filAmount THEN
              tt-chqBills.debitAmount = filAmount.
          ELSE
          DO:
              MESSAGE "Too large amount to pay." VIEW-AS ALERT-BOX INFO BUTTONS OK.
              RETURN.
          END.
      END.
      RELEASE tt-chqBills.

      IF (filUnDedicated - filAmount) >= 0 THEN
      DO:
        filUnDedicated = filUnDedicated - filAmount.
        DISPLAY filUnDedicated WITH FRAME {&FRAME-NAME}.
      END.
      ELSE
      DO:
        MESSAGE "Too large value to Pay." VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN.
      END.

      OPEN QUERY brwCreditAmounts FOR EACH tt-chqBills BY tt-chqBills.billDate.
      APPLY "VALUE-CHANGED":U TO brwCreditAmounts IN FRAME {&FRAME-NAME}.
  END.
  DISABLE {&SELF-NAME} WITH FRAME {&FRAME-NAME}.
  ENABLE btnDoPay brwCreditAmounts WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnReturned
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnReturned C-Win
ON CHOOSE OF btnReturned IN FRAME DEFAULT-FRAME /* Return */
DO:
  IF filID <> 0 THEN
  DO:
      MESSAGE "Conferm to Return this Cheque?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE yn AS LOGICAL.
      IF yn = YES THEN
      DO:
          FOR EACH billChqAssoc WHERE billChqAssoc.chq# = filChqNo.
              FIND FIRST bills WHERE bills.bill# = billChqAssoc.bill# EXCLUSIVE-LOCK NO-ERROR.
              IF AVAILABLE bills THEN
                  bills.paidAmount = bills.paidAmount - billChqAssoc.amount.
              RELEASE bills.

              FOR EACH Payments WHERE Payments.PayMethod = "Cheque" AND Payments.bill# = billChqAssoc.bill# AND Payments.Amount = billChqAssoc.amount.
                  Payments.stat = FALSE.
              END.

              DELETE billChqAssoc.
          END.

          FIND FIRST cheques WHERE cheques.chqNo = filChqNo.
              cheques.dedicatedAmount = 0.
              cheques.stat = "R".
          RELEASE cheques.
          
          OPEN QUERY brwCheques FOR
          EACH ics.cheques NO-LOCK,
          EACH ics.customer WHERE customer.cusID = cheques.cusID NO-LOCK INDEXED-REPOSITION.

          IF NOT ERROR-STATUS:ERROR THEN
              MESSAGE "Cheque successfully Returned." VIEW-AS ALERT-BOX INFO BUTTONS OK.
              
      END.
  END.
  ELSE
      MESSAGE "No Cheques to Return." VIEW-AS ALERT-BOX ERROR BUTTONS OK.

  DISABLE btnReturned btnDoPay btnCancel btnSave cmbArea filChqAmount cmbCus filChqNo filID filBank filBranch radStat WITH FRAME {&FRAME-NAME}.
      ENABLE brwCheques btnAdd btnClose btnDel btnMod WITH FRAME {&FRAME-NAME}.

      APPLY "VALUE-CHANGED":U TO brwCheques.
            
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSave C-Win
ON CHOOSE OF btnSave IN FRAME DEFAULT-FRAME /* Save */
DO:
    DEFINE VARIABLE tempPayID AS INTEGER     NO-UNDO.
    
    IF cmbArea = 0 THEN
    DO:
      MESSAGE "Area cannot be a Blank." VIEW-AS ALERT-BOX WARNING BUTTONS OK.
      RETURN.
    END.
    IF cmbCus = 0 THEN
    DO:
      MESSAGE "Customer cannot be a Blank." VIEW-AS ALERT-BOX WARNING BUTTONS OK.
      RETURN.
    END.
    IF filChqNo = 0 THEN
    DO:
      MESSAGE "Cheque No cannot be a Blank." VIEW-AS ALERT-BOX WARNING BUTTONS OK.
      RETURN.
    END.
    IF filChqAmount = 0 THEN
    DO:
      MESSAGE "Amount cannot be a Blank." VIEW-AS ALERT-BOX WARNING BUTTONS OK.
      RETURN.
    END.
    IF filBank = "" THEN
    DO:
      MESSAGE "Bank cannot be a Blank." VIEW-AS ALERT-BOX WARNING BUTTONS OK.
      RETURN.
    END.
    IF filBranch = "" THEN
    DO:
      MESSAGE "Branch cannot be a Blank." VIEW-AS ALERT-BOX WARNING BUTTONS OK.
      RETURN.
    END.
    
    
    MESSAGE "Conferm to save the record?" VIEW-AS ALERT-BOX INFO BUTTONS YES-NO UPDATE yn AS LOGICAL.
    IF yn = YES THEN
    DO:
        IF addModify = "add" THEN
        DO:
            FIND FIRST cheques WHERE cheques.chqNo = filChqNo NO-ERROR.
            IF AVAILABLE cheques THEN
            DO:
                MESSAGE "Cheque Number already excists." VIEW-AS ALERT-BOX WARNING BUTTONS OK.
                RETURN.
            END.
            RELEASE cheques.

            CREATE cheques.
            FIND FIRST customer WHERE customer.cusID = cmbCus.
            cheques.cusName = customer.cusName.
            cheques.amount   = filChqAmount. 
            cheques.bankName = filBank      .
            cheques.Branch   = filBranch   .
            cheques.chqDate  = calendr:VALUE .
            cheques.chqNo    = filChqNo        .
            cheques.crDate   = TODAY          .
            cheques.cusID    = cmbCus           .
            cheques.ID       = filID             .
            cheques.stat     = radStat            .
            
            FIND FIRST paramtrs WHERE NAME = "chqID" EXCLUSIVE-LOCK NO-ERROR.
                val = STRING(filID).
            RELEASE paramtrs.
    
            IF NOT ERROR-STATUS:ERROR THEN
                MESSAGE "Record created successfully." VIEW-AS ALERT-BOX INFO BUTTONS OK.
        
        END.
        ELSE IF addModify = "modify" THEN
        DO:
            FIND FIRST cheques WHERE cheques.ID = filID.
            IF AVAILABLE cheques THEN
            DO:
                ASSIGN
                cheques.amount   = filChqAmount 
                cheques.bankName = filBank 
                cheques.Branch   = filBranch 
                cheques.chqDate  = calendr:VALUE 
                cheques.chqNo    = filChqNo 
                cheques.crDate   = TODAY 
                cheques.cusID    = cmbCus 
                cheques.ID       = filID 
                cheques.stat     = radStat 
                .
        
                FOR EACH tt-chqBills WHERE tt-chqBills.debitAmount > 0.
                    FIND FIRST paramtrs WHERE NAME = "PaymentID" EXCLUSIVE-LOCK NO-ERROR.
                       tempPayID = INT(val).
                       val = STRING(tempPayID + 1).
                    RELEASE paramtrs.
        
                    CREATE Payments.
                     Payments.bill#       = tt-chqBills.bill#.
                     Payments.Amount      = tt-chqBills.debitAmount.
                     Payments.CusID       = cmbCus.
                     Payments.date        = TODAY.
                     Payments.PaymentID   = tempPayID + 1.
                     Payments.PayMethod   = "Cheque".
                     Payments.stat        = YES.

                     cheques.dedicatedAmount = cheques.dedicatedAmount + tt-chqBills.debitAmount.
    
                    CREATE billChqAssoc.
                    ASSIGN
                        billChqAssoc.chq# = filChqNo
                        billChqAssoc.bill# = filBIll#
                        billChqAssoc.amount = tt-chqBills.debitAmount.
        
                    FIND FIRST bills WHERE bills.bill# = tt-chqBills.bill#.
                        bills.paidAmount = bills.paidAmount + debitAmount.
                    RELEASE bills.
                END.
        
                IF NOT ERROR-STATUS:ERROR THEN
                    MESSAGE "Record successfully Modified." VIEW-AS ALERT-BOX INFO BUTTONS OK.
            END.
        END.
    
        DISABLE filAmount btnReturned btnDoPay btnCancel btnSave cmbArea filChqAmount cmbCus filChqNo filID filBank filBranch radStat WITH FRAME {&FRAME-NAME}.
        ENABLE brwCreditAmounts brwCheques btnAdd btnClose btnDel btnMod WITH FRAME {&FRAME-NAME}.
        calendr:ENABLED = FALSE.
        
        OPEN QUERY brwCheques FOR
              EACH ics.cheques NO-LOCK,
              EACH ics.customer WHERE customer.cusID = cheques.cusID NO-LOCK INDEXED-REPOSITION.
        APPLY "VALUE-CHANGED":U TO brwCheques.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSearch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSearch C-Win
ON CHOOSE OF btnSearch IN FRAME DEFAULT-FRAME /* Search */
DO:
    RUN finderBills.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cmbArea
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbArea C-Win
ON VALUE-CHANGED OF cmbArea IN FRAME DEFAULT-FRAME /* Area */
DO:
  ASSIGN {&SELF-NAME}.
  RUN cusLoader.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cmbCus
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbCus C-Win
ON ENTRY OF cmbCus IN FRAME DEFAULT-FRAME /* Customer */
DO:
/*   ASSIGN cmbArea. */
/*   ASSIGN cmbCus.  */
/*   RUN cusLoader.  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbCus C-Win
ON VALUE-CHANGED OF cmbCus IN FRAME DEFAULT-FRAME /* Customer */
DO:
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cmbSearchArea
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbSearchArea C-Win
ON LEAVE OF cmbSearchArea IN FRAME DEFAULT-FRAME /* Area */
DO:
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbSearchArea C-Win
ON VALUE-CHANGED OF cmbSearchArea IN FRAME DEFAULT-FRAME /* Area */
DO:
  ASSIGN {&SELF-NAME}.
  RUN finderBills.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cmbSearchCol
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbSearchCol C-Win
ON VALUE-CHANGED OF cmbSearchCol IN FRAME DEFAULT-FRAME
DO:
  ASSIGN {&SELF-NAME}.
    IF {&SELF-NAME} = "Cheque No" THEN DISABLE cmbSearchTime cmbSearchArea WITH FRAME {&FRAME-NAME}.
    IF {&SELF-NAME} <> "Cheque No" THEN ENABLE cmbSearchTime cmbSearchArea WITH FRAME {&FRAME-NAME}.
  RUN finderBills.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cmbSearchTime
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbSearchTime C-Win
ON VALUE-CHANGED OF cmbSearchTime IN FRAME DEFAULT-FRAME /* Within Last */
DO:
    ASSIGN {&SELF-NAME}.
    RUN finderBills.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame-2 C-Win OCX.Click
PROCEDURE CtrlFrame-2.DTPicker.Click .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/

calendr:VALUE = STRING(TODAY,"99/99/9999").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame-4 C-Win OCX.Change
PROCEDURE CtrlFrame-4.DTPicker.Change .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/
/* tempdate = calendr3:VALUE.                                                                         */
/* /* OPEN QUERY brwCheques FOR EACH ics.cheques WHERE cheques.chqDate = tempdate NO-LOCK,         */ */
/* /*           EACH ics.customer WHERE customer.cusID = cheques.cusID NO-LOCK INDEXED-REPOSITION. */ */
/* APPLY "VALUE-CHANGED":U TO radFilter IN FRAME DEFAULT-FRAME.                                       */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame-4 C-Win OCX.Click
PROCEDURE CtrlFrame-4.DTPicker.Click .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME filAmount
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filAmount C-Win
ON LEAVE OF filAmount IN FRAME DEFAULT-FRAME /* Pay */
DO:
  ASSIGN {&SELF-NAME}.
  END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME filBank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filBank C-Win
ON LEAVE OF filBank IN FRAME DEFAULT-FRAME /* Bank */
DO:
  
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME filBranch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filBranch C-Win
ON LEAVE OF filBranch IN FRAME DEFAULT-FRAME /* Branch */
DO:
  
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME filChqAmount
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filChqAmount C-Win
ON LEAVE OF filChqAmount IN FRAME DEFAULT-FRAME /* Amount */
DO:
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME filChqNo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filChqNo C-Win
ON LEAVE OF filChqNo IN FRAME DEFAULT-FRAME /* Cheque No */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME filSearch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filSearch C-Win
ON VALUE-CHANGED OF filSearch IN FRAME DEFAULT-FRAME /* Search */
DO:
  ASSIGN {&SELF-NAME}.
  RUN finderBills.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME filUnDedicated
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filUnDedicated C-Win
ON LEAVE OF filUnDedicated IN FRAME DEFAULT-FRAME /* Available */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME radFilter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL radFilter C-Win
ON VALUE-CHANGED OF radFilter IN FRAME DEFAULT-FRAME
DO:

    ASSIGN {&SELF-NAME}.
    tempdate = TODAY.
    
    
    IF {&SELF-NAME} = 1 THEN
    DO:
        calendr3:ENABLED = FALSE.
        OPEN QUERY brwCheques FOR EACH ics.cheques NO-LOCK,
          EACH ics.customer WHERE customer.cusID = cheques.cusID NO-LOCK INDEXED-REPOSITION.
    END.
    IF {&SELF-NAME} = 2 THEN
    DO:
        calendr3:ENABLED = FALSE.
        OPEN QUERY brwCheques FOR EACH ics.cheques WHERE cheques.chqDate = TODAY NO-LOCK,
          EACH ics.customer WHERE customer.cusID = cheques.cusID NO-LOCK INDEXED-REPOSITION.
    END.
    IF {&SELF-NAME} = 3 THEN
    DO:
        tempdate = calendr3:VALUE.
        calendr3:ENABLED = TRUE.
        OPEN QUERY brwCheques FOR EACH ics.cheques WHERE cheques.chqDate = tempdate NO-LOCK,
          EACH ics.customer WHERE customer.cusID = cheques.cusID NO-LOCK INDEXED-REPOSITION.
    END.
    
    APPLY "VALUE-CHANGED":U TO brwCheques.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME radStat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL radStat C-Win
ON VALUE-CHANGED OF radStat IN FRAME DEFAULT-FRAME
DO:
    
    ASSIGN {&SELF-NAME}.
    IF {&SELF-NAME} = "R" THEN
    DO:
        MESSAGE "Select Button Return." VIEW-AS ALERT-BOX WARNING BUTTONS OK.
        {&SELF-NAME} = tempStatus.
        DISPLAY {&SELF-NAME} WITH FRAME {&FRAME-NAME}.
    END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brwCheques
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
  calendr = chCtrlFrame-2:DTPicker.
  calendr:ENABLED = FALSE.
  calendr3 = chCtrlFrame-4:DTPicker.
  calendr3:ENABLED = FALSE.
  calendr3:VALUE = TODAY.

  RUN areaLoader.
  RUN cusLoaderAll.
  RUN areaCodeLoader.

  OPEN QUERY brwCreditAmounts FOR EACH tt-chqBills.

  APPLY "VALUE-CHANGED":U TO brwCheques.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE areaCodeLoader C-Win 
PROCEDURE areaCodeLoader :
FOR EACH area BY area.ID.
  cmbSearchArea:ADD-LAST(area.areaCode,area.ID) IN FRAME {&FRAME-NAME}.
END.
DISPLAY cmbSearchArea WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE areaLoader C-Win 
PROCEDURE areaLoader :
FOR EACH area BY areaCode.
    cmbArea:ADD-LAST(areaCode + " - " + descrip,ID) IN FRAME DEFAULT-FRAME.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE billsBrowserUpdate C-Win 
PROCEDURE billsBrowserUpdate :
DEFINE VARIABLE tempUnDedicated AS DECIMAL     NO-UNDO.

EMPTY TEMP-TABLE tt-chqBills.

FOR EACH bills WHERE bills.cusID = cmbCus AND (bills.tol - bills.paidAmount) > 0.
    CREATE tt-chqBills.
         tt-chqBills.bill#        = bills.bill#.
         tt-chqBills.billNo        = bills.billNo.
         tt-chqBills.billDate     = bills.bilDate.
         tt-chqBills.creditAmount = (bills.tol - bills.paidAmount).
         tt-chqBills.tol          = bills.tol.
         tt-chqBills.debitAmount  = 0.00.
END.

tempUnDedicated = filChqAmount.         

FOR EACH billChqAssoc WHERE billChqAssoc.chq# = filChqNo.
    tempUnDedicated = tempUnDedicated - billChqAssoc.amount.
END.

IF tempUnDedicated >= 0 THEN filUnDedicated = tempUnDedicated.
ELSE filUnDedicated = 0.

DISPLAY filUnDedicated WITH FRAME {&FRAME-NAME}.

OPEN QUERY brwCreditAmounts FOR EACH tt-chqBills BY tt-chqBills.billDate.
APPLY "VALUE-CHANGED":U TO brwCreditAmounts IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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

OCXFile = SEARCH( "Cheques.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chCtrlFrame-2 = CtrlFrame-2:COM-HANDLE
    UIB_S = chCtrlFrame-2:LoadControls( OCXFile, "CtrlFrame-2":U)
    CtrlFrame-2:NAME = "CtrlFrame-2":U
    chCtrlFrame-4 = CtrlFrame-4:COM-HANDLE
    UIB_S = chCtrlFrame-4:LoadControls( OCXFile, "CtrlFrame-4":U)
    CtrlFrame-4:NAME = "CtrlFrame-4":U
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "Cheques.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cusLoader C-Win 
PROCEDURE cusLoader :
DEFINE VARIABLE areaCod AS CHARACTER     NO-UNDO.
DEFINE VARIABLE cmbList AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cnt     AS INTEGER     NO-UNDO.

DISPLAY WITH FRAME DEFAULT-FRAME.

cmbCus = 0.

IF cmbArea = 0 THEN
DO:
    MESSAGE "Select an Area first." VIEW-AS ALERT-BOX INFO BUTTONS OK.
    cmbCus:LIST-ITEM-PAIRS = "--Select Here--,0".
END.
ELSE
DO:
    FIND FIRST area WHERE ID = cmbArea EXCLUSIVE-LOCK NO-ERROR.
    areaCod = area.descrip.
    
    cmbCus:LIST-ITEM-PAIRS = "--Select Here--,0".
    
    FOR EACH customer WHERE CusArea = areaCod BY cusName.
        cmbCus:ADD-LAST(cusName,cusID) NO-ERROR.
        cnt = cnt + 1.
    END.
    IF cnt = 0 THEN
        MESSAGE "No Customers for this Area." VIEW-AS ALERT-BOX WARNING BUTTONS OK.
END.

DISPLAY cmbCus WITH FRAME DEFAULT-FRAME.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cusLoaderAll C-Win 
PROCEDURE cusLoaderAll :
DISPLAY WITH FRAME DEFAULT-FRAME.

cmbCus:LIST-ITEM-PAIRS = "--Select Here--,0" NO-ERROR. 
    
    FOR EACH customer BY cusName.
        cmbCus:ADD-LAST(cusName,cusID) NO-ERROR.
    END.

DISPLAY cmbCus WITH FRAME DEFAULT-FRAME.

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
  DISPLAY filBIllId radFilter filID filBIll# cmbArea cmbCus filChqNo 
          filToCollect filChqAmount filSearch filUnDedicated filBank 
          cmbSearchCol filAmount cmbSearchArea filBranch cmbSearchTime radStat 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE radFilter brwCheques brwCreditAmounts btnGo filSearch btnAdd btnSearch 
         btnMod btnClose RECT-5 RECT-6 RECT-21 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE finderBills C-Win 
PROCEDURE finderBills :
IF filSearch = "" AND cmbSearchCol = "Customer" THEN
DO:
   OPEN QUERY brwCheques FOR EACH ics.cheques NO-LOCK,
      EACH ics.customer WHERE customer.cusID = cheques.cusID NO-LOCK INDEXED-REPOSITION.
END.
IF filSearch <> "" AND cmbSearchCol = "Customer" THEN
DO:
   OPEN QUERY brwCheques FOR EACH ics.cheques WHERE cheques.cusName BEGINS filSearch NO-LOCK,
      EACH ics.customer WHERE customer.cusID = cheques.cusID NO-LOCK INDEXED-REPOSITION.
END.

cheques.cusName:SORT-ASCENDING IN BROWSE brwCheques = TRUE.

APPLY "VALUE-CHANGED":U TO brwCheques IN FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

