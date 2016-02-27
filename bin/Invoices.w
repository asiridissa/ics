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
DEFINE VARIABLE monthTag AS CHARACTER   NO-UNDO.


DEFINE VARIABLE calendr AS COM-HANDLE   NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME EFAULT-FRAME
&Scoped-define BROWSE-NAME brwItem

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES itms

/* Definitions for BROWSE brwItem                                       */
&Scoped-define FIELDS-IN-QUERY-brwItem itms.itmID itms.cat itms.itmName ~
itms.unitWeightKG itms.unitsPerCase itms.maxWeight itms.discountIN ~
itms.unitPriceS itms.casePriceS itms.unitPriceB itms.casePriceB ~
itms.noOfUnits itms.noOfCases 
&Scoped-define ENABLED-FIELDS-IN-QUERY-brwItem itms.cat 
&Scoped-define ENABLED-TABLES-IN-QUERY-brwItem itms
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-brwItem itms
&Scoped-define QUERY-STRING-brwItem FOR EACH itms ~
      WHERE itms.stat = yes NO-LOCK ~
    BY itms.SortID INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-brwItem OPEN QUERY brwItem FOR EACH itms ~
      WHERE itms.stat = yes NO-LOCK ~
    BY itms.SortID INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-brwItem itms
&Scoped-define FIRST-TABLE-IN-QUERY-brwItem itms


/* Definitions for FRAME EFAULT-FRAME                                   */
&Scoped-define OPEN-BROWSERS-IN-QUERY-EFAULT-FRAME ~
    ~{&OPEN-QUERY-brwItem}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS brwItem btnAdd btnDuplicate btnMod btnDel ~
filitmSearch btnClose RECT-15 RECT-16 RECT-17 RECT-18 RECT-19 
&Scoped-Define DISPLAYED-OBJECTS filitmName cmbCat filunitWeightKG ~
filunitsPerCase filnoOfUnits filnoOfCases filunitPriceS fillDiscount ~
filcasePriceS filkg filitmSearch filVarKg filID filmaxWeight filunitPriceB ~
filValue filValueS filcasePriceB filProfit 

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

DEFINE BUTTON btnDuplicate 
     LABEL "Duplicate" 
     SIZE 15 BY 1.

DEFINE BUTTON btnMod 
     LABEL "Modify" 
     SIZE 15 BY 1.

DEFINE BUTTON btnSave 
     LABEL "Save" 
     SIZE 15 BY 1.

DEFINE VARIABLE cmbCat AS CHARACTER FORMAT "X(256)":U 
     LABEL "Category" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "--Select Here--" 
     DROP-DOWN-LIST
     SIZE 26 BY 1
     BGCOLOR 15 FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE filcasePriceB AS DECIMAL FORMAT ">>>,>>9.99":U INITIAL 0 
     LABEL "C Price Buy (Rs.)" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .88
     BGCOLOR 7 FGCOLOR 11  NO-UNDO.

DEFINE VARIABLE filcasePriceS AS DECIMAL FORMAT ">>>,>>9.99":U INITIAL 0 
     LABEL "C Price Sell (Rs.)" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .88
     BGCOLOR 15 FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE filID AS INTEGER FORMAT "9999":U INITIAL 0 
     LABEL "Item ID" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88
     BGCOLOR 7 FGCOLOR 11  NO-UNDO.

DEFINE VARIABLE filitmName AS CHARACTER FORMAT "X(50)":U 
     LABEL "Item Name" 
     VIEW-AS FILL-IN 
     SIZE 35 BY .88
     BGCOLOR 15 FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE filitmSearch AS CHARACTER FORMAT "X(50)":U 
     LABEL "Search" 
     VIEW-AS FILL-IN 
     SIZE 35 BY .88
     BGCOLOR 15 FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE filkg AS DECIMAL FORMAT ">>,>>9.999":U INITIAL 0 
     LABEL "Tol Weight" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88
     BGCOLOR 5 FGCOLOR 15 FONT 10 NO-UNDO.

DEFINE VARIABLE fillDiscount AS DECIMAL FORMAT ">9.99":U INITIAL 0 
     LABEL "Discount (%)" 
     VIEW-AS FILL-IN 
     SIZE 9.43 BY .88
     BGCOLOR 15 FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE filmaxWeight AS DECIMAL FORMAT ">>,>>9.999":U INITIAL 0 
     LABEL "Max Satock (kg)" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88
     BGCOLOR 8 FGCOLOR 7  NO-UNDO.

DEFINE VARIABLE filnoOfCases AS INTEGER FORMAT ">,>>>,>>9":U INITIAL 0 
     LABEL "No of Cases" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .88
     BGCOLOR 15 FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE filnoOfUnits AS INTEGER FORMAT ">,>>>,>>9":U INITIAL 0 
     LABEL "No of Pieces" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .88
     BGCOLOR 15 FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE filProfit AS DECIMAL FORMAT "->>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Exp Profit" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .88
     BGCOLOR 5 FGCOLOR 15 FONT 10 NO-UNDO.

DEFINE VARIABLE filunitPriceB AS DECIMAL FORMAT ">,>>9.99":U INITIAL 0 
     LABEL "P Price Buy (Rs.)" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88
     BGCOLOR 7 FGCOLOR 11  NO-UNDO.

DEFINE VARIABLE filunitPriceS AS DECIMAL FORMAT ">,>>9.99":U INITIAL 0 
     LABEL "P Price Sell (Rs.)" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88
     BGCOLOR 15 FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE filunitsPerCase AS INTEGER FORMAT ">,>>9":U INITIAL 0 
     LABEL "Pieces per Case" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88
     BGCOLOR 15 FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE filunitWeightKG AS DECIMAL FORMAT ">>9.999":U INITIAL 0 
     LABEL "P Weight (kg)" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88
     BGCOLOR 15 FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE filValue AS DECIMAL FORMAT "->>>,>>>,>>9.99":U INITIAL 0 
     LABEL "B Value" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .88
     BGCOLOR 5 FGCOLOR 15 FONT 10 NO-UNDO.

DEFINE VARIABLE filValueS AS DECIMAL FORMAT "->>>,>>>,>>9.99":U INITIAL 0 
     LABEL "S Value" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .88
     BGCOLOR 5 FGCOLOR 15 FONT 10 NO-UNDO.

DEFINE VARIABLE filVarKg AS DECIMAL FORMAT ">,>>>,>>9.99":U INITIAL 0 
     LABEL "Stock" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88
     BGCOLOR 8 FGCOLOR 7 FONT 10 NO-UNDO.

DEFINE RECTANGLE RECT-15
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL  GROUP-BOX  ROUNDED 
     SIZE 47 BY 1.65.

DEFINE RECTANGLE RECT-16
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL  GROUP-BOX  ROUNDED 
     SIZE 47 BY 3.58.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL  GROUP-BOX  ROUNDED 
     SIZE 64.86 BY 5.31.

DEFINE RECTANGLE RECT-18
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL  GROUP-BOX  ROUNDED 
     SIZE 30.57 BY 5.31.

DEFINE RECTANGLE RECT-19
     EDGE-PIXELS 3 GRAPHIC-EDGE  NO-FILL  GROUP-BOX  ROUNDED 
     SIZE 143 BY 1.5.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brwItem FOR 
      itms SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brwItem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brwItem C-Win _STRUCTURED
  QUERY brwItem NO-LOCK DISPLAY
      itms.itmID COLUMN-LABEL "ID" FORMAT "99999":U
      itms.cat COLUMN-LABEL "   Category" FORMAT "x(12)":U WIDTH 10
      itms.itmName COLUMN-LABEL "                           Item Name" FORMAT "x(50)":U
            WIDTH 32 COLUMN-FGCOLOR 14 COLUMN-BGCOLOR 0
      itms.unitWeightKG COLUMN-LABEL "Weight  (kg)" FORMAT ">>,>>9.999":U
            WIDTH 9.43 COLUMN-FGCOLOR 14 COLUMN-BGCOLOR 0
      itms.unitsPerCase COLUMN-LABEL "Per C" FORMAT ">>>9":U WIDTH 5
      itms.maxWeight COLUMN-LABEL " Max Weight" FORMAT ">>,>>9.999":U
            WIDTH 10
      itms.discountIN COLUMN-LABEL "Discnt %" FORMAT ">>9.99":U
      itms.unitPriceS COLUMN-LABEL "Unit Sell" FORMAT ">>,>>9.99":U
            WIDTH 10 COLUMN-FGCOLOR 15 COLUMN-BGCOLOR 3
      itms.casePriceS COLUMN-LABEL "Case Sell" FORMAT ">>,>>9.99":U
            WIDTH 10 COLUMN-FGCOLOR 15 COLUMN-BGCOLOR 3
      itms.unitPriceB COLUMN-LABEL "Unit Buy" FORMAT ">>,>>9.99":U
            WIDTH 10
      itms.casePriceB COLUMN-LABEL "Case Buy" FORMAT ">>,>>9.99":U
            WIDTH 10
      itms.noOfUnits COLUMN-LABEL "Pieses" FORMAT ">>>9":U WIDTH 8
      itms.noOfCases COLUMN-LABEL "Cases" FORMAT ">,>>>,>>9":U
  ENABLE
      itms.cat
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 143.57 BY 18.81
         BGCOLOR 15 FGCOLOR 0 FONT 10
         TITLE BGCOLOR 15 FGCOLOR 0 "Invoices" ROW-HEIGHT-CHARS .55 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME EFAULT-FRAME
     brwItem AT ROW 1.08 COL 1.43 WIDGET-ID 200
     filitmName AT ROW 23.46 COL 10.29 COLON-ALIGNED WIDGET-ID 4
     cmbCat AT ROW 24.46 COL 10.29 COLON-ALIGNED WIDGET-ID 76
     filunitWeightKG AT ROW 20.58 COL 62.43 COLON-ALIGNED WIDGET-ID 12
     filunitsPerCase AT ROW 21.58 COL 62.43 COLON-ALIGNED WIDGET-ID 20
     filnoOfUnits AT ROW 23.54 COL 62.57 COLON-ALIGNED WIDGET-ID 14
     filnoOfCases AT ROW 24.54 COL 62.57 COLON-ALIGNED WIDGET-ID 16
     filunitPriceS AT ROW 20.58 COL 92.86 COLON-ALIGNED WIDGET-ID 6
     fillDiscount AT ROW 21.58 COL 92.86 COLON-ALIGNED WIDGET-ID 96
     filcasePriceS AT ROW 23.54 COL 92.86 COLON-ALIGNED WIDGET-ID 8
     btnAdd AT ROW 26.04 COL 3.14 WIDGET-ID 30
     btnDuplicate AT ROW 26.04 COL 18.57 WIDGET-ID 112
     btnMod AT ROW 26.04 COL 34 WIDGET-ID 32
     btnDel AT ROW 26.04 COL 49.29 WIDGET-ID 34
     btnSave AT ROW 26.04 COL 113.29 WIDGET-ID 38
     filkg AT ROW 20.58 COL 124 COLON-ALIGNED WIDGET-ID 86 NO-TAB-STOP 
     btnCancel AT ROW 26.04 COL 128.57 WIDGET-ID 36
     filitmSearch AT ROW 20.81 COL 10.43 COLON-ALIGNED WIDGET-ID 98 NO-TAB-STOP 
     filVarKg AT ROW 21.58 COL 124 COLON-ALIGNED WIDGET-ID 88 NO-TAB-STOP 
     filID AT ROW 22.5 COL 10.29 COLON-ALIGNED WIDGET-ID 2 NO-TAB-STOP 
     filmaxWeight AT ROW 22.58 COL 62.43 COLON-ALIGNED WIDGET-ID 22 NO-TAB-STOP 
     filunitPriceB AT ROW 22.58 COL 92.86 COLON-ALIGNED WIDGET-ID 80 NO-TAB-STOP 
     filValue AT ROW 22.58 COL 124 COLON-ALIGNED WIDGET-ID 90 NO-TAB-STOP 
     filValueS AT ROW 23.54 COL 124 COLON-ALIGNED WIDGET-ID 102 NO-TAB-STOP 
     filcasePriceB AT ROW 24.54 COL 92.86 COLON-ALIGNED WIDGET-ID 78 NO-TAB-STOP 
     filProfit AT ROW 24.54 COL 124.14 COLON-ALIGNED WIDGET-ID 92 NO-TAB-STOP 
     btnClose AT ROW 26.04 COL 98.14 WIDGET-ID 40 NO-TAB-STOP 
     "Version:" VIEW-AS TEXT
          SIZE 7.14 BY .81 AT ROW 22.42 COL 26.29 WIDGET-ID 116
     "*" VIEW-AS TEXT
          SIZE 5 BY .62 AT ROW 23.65 COL 73.86 WIDGET-ID 72
          FGCOLOR 12 
     "*" VIEW-AS TEXT
          SIZE 1.72 BY .62 AT ROW 20.73 COL 108.14 WIDGET-ID 82
          FGCOLOR 12 
     "*" VIEW-AS TEXT
          SIZE .86 BY .62 AT ROW 23.58 COL 47.57 WIDGET-ID 58
          FGCOLOR 12 
     "*" VIEW-AS TEXT
          SIZE 1.72 BY .62 AT ROW 21.69 COL 104.57 WIDGET-ID 68
          FGCOLOR 12 
     "*" VIEW-AS TEXT
          SIZE 1.43 BY .62 AT ROW 20.73 COL 77.57 WIDGET-ID 66
          FGCOLOR 12 
     "*" VIEW-AS TEXT
          SIZE 5 BY .62 AT ROW 24.62 COL 38.86 WIDGET-ID 60
          FGCOLOR 12 
     RECT-15 AT ROW 20.38 COL 1.72 WIDGET-ID 100
     RECT-16 AT ROW 22.12 COL 1.72 WIDGET-ID 104
     RECT-17 AT ROW 20.38 COL 49 WIDGET-ID 106
     RECT-18 AT ROW 20.38 COL 114 WIDGET-ID 108
     RECT-19 AT ROW 25.81 COL 1.72 WIDGET-ID 110
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
         TITLE              = "LMS - Invoice"
         COLUMN             = 1.57
         ROW                = 1.23
         HEIGHT             = 26.5
         WIDTH              = 144
         MAX-HEIGHT         = 27.77
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 27.77
         VIRTUAL-WIDTH      = 146.29
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
/* SETTINGS FOR FRAME EFAULT-FRAME
   FRAME-NAME Custom                                                    */
/* BROWSE-TAB brwItem 1 EFAULT-FRAME */
/* SETTINGS FOR BUTTON btnCancel IN FRAME EFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       btnClose:HIDDEN IN FRAME EFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON btnSave IN FRAME EFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX cmbCat IN FRAME EFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN filcasePriceB IN FRAME EFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN filcasePriceS IN FRAME EFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN filID IN FRAME EFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN filitmName IN FRAME EFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN filkg IN FRAME EFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       filkg:READ-ONLY IN FRAME EFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN fillDiscount IN FRAME EFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN filmaxWeight IN FRAME EFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN filnoOfCases IN FRAME EFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN filnoOfUnits IN FRAME EFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN filProfit IN FRAME EFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN filunitPriceB IN FRAME EFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       filunitPriceB:READ-ONLY IN FRAME EFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN filunitPriceS IN FRAME EFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN filunitsPerCase IN FRAME EFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN filunitWeightKG IN FRAME EFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN filValue IN FRAME EFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN filValueS IN FRAME EFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN filVarKg IN FRAME EFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brwItem
/* Query rebuild information for BROWSE brwItem
     _TblList          = "ics.itms"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "ics.itms.SortID|yes"
     _Where[1]         = "itms.stat = yes"
     _FldNameList[1]   > ics.itms.itmID
"itms.itmID" "ID" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ics.itms.cat
"itms.cat" "   Category" "x(12)" "character" ? ? ? ? ? ? yes ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ics.itms.itmName
"itms.itmName" "                           Item Name" ? "character" 0 14 ? ? ? ? no ? no no "32" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ics.itms.unitWeightKG
"itms.unitWeightKG" "Weight  (kg)" ? "decimal" 0 14 ? ? ? ? no ? no no "9.43" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > ics.itms.unitsPerCase
"itms.unitsPerCase" "Per C" ? "integer" ? ? ? ? ? ? no ? no no "5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > ics.itms.maxWeight
"itms.maxWeight" " Max Weight" ? "decimal" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > ics.itms.discountIN
"itms.discountIN" "Discnt %" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > ics.itms.unitPriceS
"itms.unitPriceS" "Unit Sell" ? "decimal" 3 15 ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > ics.itms.casePriceS
"itms.casePriceS" "Case Sell" ? "decimal" 3 15 ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > ics.itms.unitPriceB
"itms.unitPriceB" "Unit Buy" ? "decimal" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > ics.itms.casePriceB
"itms.casePriceB" "Case Buy" ? "decimal" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > ics.itms.noOfUnits
"itms.noOfUnits" "Pieses" ">>>9" "integer" ? ? ? ? ? ? no ? no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > ics.itms.noOfCases
"itms.noOfCases" "Cases" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE brwItem */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame-2 ASSIGN
       FRAME           = FRAME EFAULT-FRAME:HANDLE
       ROW             = 22.46
       COLUMN          = 34
       HEIGHT          = .81
       WIDTH           = 13
       WIDGET-ID       = 114
       HIDDEN          = no
       SENSITIVE       = yes.
/* CtrlFrame-2 OCXINFO:CREATE-CONTROL from: {20DD1B9E-87C4-11D1-8BE3-0000F8754DA1} type: DTPicker */
      CtrlFrame-2:MOVE-AFTER(btnCancel:HANDLE IN FRAME EFAULT-FRAME).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* LMS - Invoice */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. 
  IF THIS-PROCEDURE:PERSISTENT THEN  */ RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* LMS - Invoice */
DO:
  /* This event will close the window and terminate the procedure.  */
/*       session_Window = session_Window - 1. */
      APPLY "CLOSE":U TO THIS-PROCEDURE.
      RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brwItem
&Scoped-define SELF-NAME brwItem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brwItem C-Win
ON VALUE-CHANGED OF brwItem IN FRAME EFAULT-FRAME /* Invoices */
DO:
    IF AVAILABLE itms THEN
    DO:
    ASSIGN
      filcasePriceB    = casePriceB 
      filcasePriceS    = casePriceS 
      filunitPriceB    = unitPriceB 
      filunitPriceS    = unitPriceS 
      cmbCat           = cat          
      filID            = itmID        
      filitmName       = itmName      
      filmaxWeight     = maxWeight    
      filnoOfCases     = noOfCases    
      filnoOfUnits     = noOfUnits    
      filunitsPerCase  = unitsPerCase 
      filunitWeightKG  = unitWeightKG 
      fillDiscount     = discountIN 
/*       calendr:VALUE = string(SUBSTRING(itmName,LENGTH(itmName) - 10,LENGTH(itmName)),"99/99/9999"). */
/*     MESSAGE string(SUBSTRING(itmName,LENGTH(itmName) - 10,LENGTH(itmName)),"99/99/9999")            */
/*         VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                          */
      .
    RUN cntKg.
    DISPLAY fillDiscount filcasePriceB filcasePriceS filunitPriceB filunitPriceS cmbCat filID filitmName filmaxWeight filnoOfCases filnoOfUnits filunitsPerCase filunitWeightKG WITH FRAME EFAULT-FRAME.
    END.
    ELSE IF NOT AVAILABLE itms THEN
        MESSAGE "No records to show." VIEW-AS ALERT-BOX INFO BUTTONS OK .

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAdd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAdd C-Win
ON CHOOSE OF btnAdd IN FRAME EFAULT-FRAME /* Add */
DO:
    ASSIGN
      cmbCat          = "--Select Here--" 
      filcasePriceB   = 0
      filcasePriceS   = 0
      filunitPriceB   = 0
      filunitPriceS   = 0 
      filID           = 0
      filitmName      = "" 
      filmaxWeight    = 0 
      filnoOfCases    = 0 
      filnoOfUnits    = 0 
      filunitsPerCase = 0 
      filunitWeightKG = 0
      filkg           = 0
      filVarKg        = 0
      fillDiscount    = 0
        .
    calendr:VALUE = STRING(TODAY,"99/99/9999").
    calendr:ENABLED = TRUE.

  FIND FIRST paramtrs WHERE NAME = "lastItmID".
    filID = INT(val) + 1.
  RELEASE paramtrs.

  DISPLAY fillDiscount filkg filVarKg filcasePriceB filcasePriceS filunitPriceB filunitPriceS cmbCat filID filitmName filmaxWeight filnoOfCases filnoOfUnits filunitsPerCase filunitWeightKG WITH FRAME EFAULT-FRAME.

  ENABLE fillDiscount btnCancel btnSave filcasePriceB filcasePriceS filunitPriceB filunitPriceS cmbCat filID filitmName /*filmaxWeight*/ filnoOfCases filnoOfUnits filunitsPerCase filunitWeightKG WITH FRAME EFAULT-FRAME.
  DISABLE btnDuplicate brwItem btnAdd btnClose btnDel btnMod WITH FRAME EFAULT-FRAME.

  addModify = "add".
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCancel C-Win
ON CHOOSE OF btnCancel IN FRAME EFAULT-FRAME /* Cancel */
DO:

    MESSAGE "Conferm to cancel." VIEW-AS ALERT-BOX WARNING BUTTONS YES-NO UPDATE yn AS LOGICAL.
    IF yn =YES THEN
    DO:
        ASSIGN
        cmbCat          = "--Select Here--" 
        filcasePriceB   = 0
        filcasePriceS   = 0
        filunitPriceB   = 0
        filunitPriceS   = 0 
        filID           = 0
        filitmName      = "" 
        filmaxWeight    = 0 
        filnoOfCases    = 0 
        filnoOfUnits    = 0 
        filunitsPerCase = 0 
        filunitWeightKG = 0
        filkg           = 0
        filVarKg        = 0
        .

        OPEN QUERY brwItem FOR EACH itms WHERE itms.stat = yes NO-LOCK BY itms.SortID.
        APPLY "VALUE-CHANGED":U TO brwItem.
        
        calendr:ENABLED = FALSE.
        DISABLE fillDiscount btnCancel btnSave filcasePriceB filcasePriceS filunitPriceB filunitPriceS cmbCat filitmName filmaxWeight filnoOfCases filnoOfUnits filunitsPerCase filunitWeightKG
        WITH FRAME EFAULT-FRAME.
        ENABLE btnDuplicate brwItem btnAdd btnClose btnDel btnMod WITH FRAME EFAULT-FRAME.
        DISPLAY filkg filVarKg filcasePriceB filcasePriceS filunitPriceB filunitPriceS cmbCat filID filitmName filmaxWeight filnoOfCases filnoOfUnits filunitsPerCase filunitWeightKG WITH FRAME EFAULT-FRAME.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnClose
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnClose C-Win
ON CHOOSE OF btnClose IN FRAME EFAULT-FRAME /* Close */
DO:
  APPLY "WINDOW-CLOSE":U TO {&WINDOW-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDel C-Win
ON CHOOSE OF btnDel IN FRAME EFAULT-FRAME /* Delete */
DO:

/*     MESSAGE "This function is temporaray out of service" VIEW-AS ALERT-BOX INFO BUTTONS OK. */
  IF filitmName <> "" THEN
  DO:
      MESSAGE "Conferm to delete the record?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE yn AS LOGICAL.
      IF yn = YES THEN
      DO:
          FIND FIRST itms WHERE itmID = filID EXCLUSIVE-LOCK NO-ERROR.
          IF AVAILABLE itms THEN
/*             DELETE itms. */
              stat = NO.
          ELSE
              MESSAGE "No Records to Delete." VIEW-AS ALERT-BOX ERROR BUTTONS OK .
          RELEASE itms.

          IF NOT ERROR-STATUS:ERROR THEN
              MESSAGE "Record successfully deleted." VIEW-AS ALERT-BOX INFO BUTTONS OK.

          OPEN QUERY brwItem FOR EACH itms where stat = yes BY itms.SortID.
          APPLY "VALUE-CHANGED":U TO brwItem.
      END.
  END.
  ELSE
      MESSAGE "No records to Delete." VIEW-AS ALERT-BOX ERROR BUTTONS OK.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDuplicate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDuplicate C-Win
ON CHOOSE OF btnDuplicate IN FRAME EFAULT-FRAME /* Duplicate */
DO:
    MESSAGE "This function is temporaray out of service" VIEW-AS ALERT-BOX INFO BUTTONS OK.
/*   IF filitmName <> "" THEN                                                                                                                                                                        */
/*   DO:                                                                                                                                                                                             */
/*       ENABLE fillDiscount btnCancel btnSave filcasePriceB filcasePriceS filunitPriceB filunitPriceS cmbCat  filitmName /*filmaxWeight*/ filnoOfCases filnoOfUnits filunitsPerCase filunitWeightKG */
/*         WITH FRAME EFAULT-FRAME.                                                                                                                                                                  */
/*       DISABLE btnDuplicate brwItem btnAdd btnClose btnDel btnMod WITH FRAME EFAULT-FRAME.                                                                                                         */
/*       addModify = "add".                                                                                                                                                                          */
/*                                                                                                                                                                                                   */
/*       FIND FIRST paramtrs WHERE NAME = "lastItmID".                                                                                                                                               */
/*         filID = INT(val) + 1.                                                                                                                                                                     */
/*       RELEASE paramtrs.                                                                                                                                                                           */
/*                                                                                                                                                                                                   */
/*         filitmName = SUBSTRING(filitmName,1,LENGTH(filitmName) - 13).                                                                                                                             */
/*         filnoOfUnits = 0.                                                                                                                                                                         */
/*         filnoOfCases = 0.                                                                                                                                                                         */
/*         DISPLAY filitmName filnoOfUnits filnoOfCases filID WITH FRAME EFAULT-FRAME.                                                                                                               */
/*                                                                                                                                                                                                   */
/*         calendr:ENABLED = TRUE.                                                                                                                                                                   */
/*         calendr:VALUE = STRING(TODAY,"99/99/9999").                                                                                                                                               */
/*   END.                                                                                                                                                                                            */
/*   ELSE                                                                                                                                                                                            */
/*   DO:                                                                                                                                                                                             */
/*     MESSAGE "No records to Duplicate." VIEW-AS ALERT-BOX ERROR BUTTONS OK.                                                                                                                        */
/*   END.                                                                                                                                                                                            */

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnMod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnMod C-Win
ON CHOOSE OF btnMod IN FRAME EFAULT-FRAME /* Modify */
DO:

/*     MESSAGE "This function is temporaray out of service" VIEW-AS ALERT-BOX INFO BUTTONS OK. */
  IF filitmName <> "" THEN
  DO:
      ENABLE fillDiscount btnCancel btnSave filcasePriceB filcasePriceS filunitPriceB filunitPriceS cmbCat  filitmName /*filmaxWeight*/ filnoOfCases filnoOfUnits filunitsPerCase filunitWeightKG
        WITH FRAME EFAULT-FRAME.
      DISABLE btnDuplicate brwItem btnAdd btnClose btnDel btnMod WITH FRAME EFAULT-FRAME.
      addModify = "modify".
  END.
  ELSE
      MESSAGE "No records to Modify." VIEW-AS ALERT-BOX ERROR BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSave C-Win
ON CHOOSE OF btnSave IN FRAME EFAULT-FRAME /* Save */
DO:
  IF filitmName = "" THEN
  DO:
      MESSAGE "Item Name cannot be a blank." VIEW-AS ALERT-BOX INFO BUTTONS OK .
      RETURN.
  END.
  IF cmbCat = "--Select Here--" THEN
  DO:
      MESSAGE "Category cannot be a blank." VIEW-AS ALERT-BOX INFO BUTTONS OK .
      RETURN.
  END.
  IF filunitWeightKG = 0 THEN
  DO:
      MESSAGE "Piese Weight cannot be zero." VIEW-AS ALERT-BOX INFO BUTTONS OK .
      RETURN.
  END.
  IF filunitPriceB = 0 THEN
  DO:
      MESSAGE "Piese Price Buy cannot be zero." VIEW-AS ALERT-BOX INFO BUTTONS OK .
      RETURN.
  END.
  IF filunitPriceS = 0 THEN
  DO:
      MESSAGE "Piese Price Sell cannot be zero." VIEW-AS ALERT-BOX INFO BUTTONS OK .
      RETURN.
  END.
  IF filunitPriceS <> 0 AND filunitPriceS < filunitPriceB THEN
  DO:
      MESSAGE "Selling price is smaller than the Buying price." VIEW-AS ALERT-BOX INFO BUTTONS OK .
      RETURN.
  END.
  IF filcasePriceB = 0 AND filunitsPerCase > 0  THEN
  DO:
      MESSAGE "Case Price Buy cannot be zero." VIEW-AS ALERT-BOX INFO BUTTONS OK .
      RETURN.
  END.
  IF filcasePriceS = 0 AND filunitsPerCase > 0  THEN
  DO:
      MESSAGE "Case Price Sell cannot be zero." VIEW-AS ALERT-BOX INFO BUTTONS OK .
      RETURN.
  END.
  IF filnoOfCases > 0 AND filunitsPerCase = 0 THEN
  DO:
      MESSAGE "You must enter Pieses per Case before enter No of Cases." VIEW-AS ALERT-BOX ERROR BUTTONS OK .
      RETURN.
  END.

  MESSAGE "Conferm to save the record?" VIEW-AS ALERT-BOX INFO BUTTONS YES-NO UPDATE yn AS LOGICAL.
  IF yn = YES THEN
  DO:
        IF addModify = "add" THEN
        DO:
            CREATE itms.
            ASSIGN
            casePriceB   = filcasePriceB
            casePriceS   = filcasePriceS
            unitPriceB   = filunitPriceB
            unitPriceS   = filunitPriceS
            cat          = cmbCat
            itmID        = filID
            itmName      = filitmName + " - " + STRING(calendr:VALUE,"99/99/9999")
            maxWeight    = filmaxWeight
            noOfCases    = filnoOfCases
            noOfUnits    = filnoOfUnits
            unitsPerCase = filunitsPerCase
            unitWeightKG = filunitWeightKG
            discountIN   = fillDiscount
            crDate       = calendr:VALUE
            stockC       = filnoOfCases
            stockP       = filnoOfUnits.
            stat         = YES.
            FIND FIRST paramtrs WHERE paramtrs.name = "lastSortID".
                itms.SortID = INT(val) + 1.
                val = string(INT(val) + 1).
            RELEASE paramtrs.

            FIND FIRST paramtrs WHERE NAME = "lastItmID" EXCLUSIVE-LOCK NO-ERROR.
                val = STRING(filID).
            RELEASE paramtrs.
            IF NOT ERROR-STATUS:ERROR THEN
                MESSAGE "Record successfully created." VIEW-AS ALERT-BOX INFO BUTTONS OK.
        
        END.
        ELSE IF addModify = "modify" THEN
        DO:
            DEFINE VARIABLE tempStockP AS INTEGER     NO-UNDO.
            DEFINE VARIABLE tempStockC AS INTEGER     NO-UNDO.

            FIND FIRST itms WHERE itmID = filID.
            IF AVAILABLE itms THEN
            DO:
                tempStockC = noOfCases.
                tempStockP = noOfUnits.  
              
                IF filnoOfCases > tempStockC THEN stockC = stockC + (filnoOfCases - tempStockC).
                IF filnoOfCases < tempStockC AND (stockC - (tempStockC - filnoOfCases)) > 0 THEN stockC = stockC - (tempStockC - filnoOfCases).
                IF filnoOfCases < tempStockC AND (stockC - (tempStockC - filnoOfCases)) <= 0 THEN
                DO: 
                    MESSAGE "This will empty Pieces in your store." SKIP
                        "Are you sure you want to proceed?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE yn1 AS LOGICAL.
                    IF yn1 = TRUE THEN
                        stockC = 0.
                    IF yn1 = FALSE THEN
                        RETURN.
                END.
                IF filnoOfUnits > tempStockP THEN stockP = stockP + (filnoOfUnits - tempStockP).
                IF filnoOfUnits < tempStockP AND (stockP - (tempStockP - filnoOfUnits)) <= 0 THEN stockP = stockP - (tempStockP - filnoOfUnits).
                IF filnoOfUnits < tempStockP AND (stockP - (tempStockP - filnoOfUnits)) > 0 THEN
                DO: 
                    MESSAGE "This will empty Cases in your store." SKIP
                        "Are you sure you want to proceed?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE yn2 AS LOGICAL.
                    IF yn2 = TRUE THEN
                        stockP = 0.
                    IF yn2 = FALSE THEN
                        RETURN.
                END.
                
                casePriceB   = filcasePriceB.
                casePriceS   = filcasePriceS.
                unitPriceB   = filunitPriceB.
                unitPriceS   = filunitPriceS.
                cat          = cmbCat          .
                itmName      = filitmName      .
                maxWeight    = filmaxWeight    .
                noOfCases    = filnoOfCases    .
                noOfUnits    = filnoOfUnits    .
                unitsPerCase = filunitsPerCase .
                unitWeightKG = filunitWeightKG .
                discountIN   = fillDiscount.
                stat         = YES.
                 MESSAGE filunitWeightKG
                     VIEW-AS ALERT-BOX INFO BUTTONS OK.
                IF NOT ERROR-STATUS:ERROR THEN
                    MESSAGE "Record successfully Modified." VIEW-AS ALERT-BOX INFO BUTTONS OK.
            END.
        END.

        calendr:ENABLED = FALSE.
          DISABLE fillDiscount btnCancel btnSave filcasePriceB filcasePriceS filunitPriceB filunitPriceS cmbCat filitmName filmaxWeight filnoOfCases filnoOfUnits filunitsPerCase filunitWeightKG
              WITH FRAME EFAULT-FRAME.
          ENABLE btnDuplicate brwItem btnAdd btnClose btnDel btnMod WITH FRAME EFAULT-FRAME.
        
          OPEN QUERY brwItem for EACH ics.itms WHERE itms.stat = yes NO-LOCK BY itms.SortID.
          APPLY "VALUE-CHANGED":U TO brwItem.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cmbCat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbCat C-Win
ON VALUE-CHANGED OF cmbCat IN FRAME EFAULT-FRAME /* Category */
DO:
  ASSIGN {&SELF-NAME}.
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


&Scoped-define SELF-NAME filcasePriceB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filcasePriceB C-Win
ON LEAVE OF filcasePriceB IN FRAME EFAULT-FRAME /* C Price Buy (Rs.) */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME filcasePriceS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filcasePriceS C-Win
ON LEAVE OF filcasePriceS IN FRAME EFAULT-FRAME /* C Price Sell (Rs.) */
DO:
  ASSIGN {&SELF-NAME}.
  filunitPriceS = {&SELF-NAME} / filunitsPerCase.

  DISPLAY filunitPriceS WITH FRAME {&FRAME-NAME}.

  APPLY "LEAVE":U TO filunitPriceS.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME filID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filID C-Win
ON LEAVE OF filID IN FRAME EFAULT-FRAME /* Item ID */
DO:
  ASSIGN filID.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME filitmName
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filitmName C-Win
ON LEAVE OF filitmName IN FRAME EFAULT-FRAME /* Item Name */
DO:
  ASSIGN {&SELF-NAME}.
  {&SELF-NAME} = CAPS(SUBSTRING({&SELF-NAME},1,1)) + SUBSTRING({&SELF-NAME},2).

  DISPLAY {&SELF-NAME} WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME filitmSearch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filitmSearch C-Win
ON LEAVE OF filitmSearch IN FRAME EFAULT-FRAME /* Search */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filitmSearch C-Win
ON VALUE-CHANGED OF filitmSearch IN FRAME EFAULT-FRAME /* Search */
DO:
  ASSIGN {&SELF-NAME}.
  OPEN QUERY brwItem FOR EACH ics.itms WHERE itms.itmName BEGINS {&SELF-NAME} and stat = yes NO-LOCK
    BY itms.SortID INDEXED-REPOSITION.
    APPLY "VALUE-CHANGED":U TO brwItem.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME filkg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filkg C-Win
ON LEAVE OF filkg IN FRAME EFAULT-FRAME /* Tol Weight */
DO:
  ASSIGN filmaxWeight.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fillDiscount
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fillDiscount C-Win
ON VALUE-CHANGED OF fillDiscount IN FRAME EFAULT-FRAME /* Discount (%) */
DO:
  ASSIGN {&SELF-NAME}.
  filunitPriceB = filunitPriceS * (1 - (fillDiscount / 100)).
  DISPLAY filunitPriceB WITH FRAME {&FRAME-NAME}.
  RUN cntKg.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME filmaxWeight
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filmaxWeight C-Win
ON LEAVE OF filmaxWeight IN FRAME EFAULT-FRAME /* Max Satock (kg) */
DO:
  ASSIGN filmaxWeight.
  RUN cntKg.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME filnoOfCases
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filnoOfCases C-Win
ON LEAVE OF filnoOfCases IN FRAME EFAULT-FRAME /* No of Cases */
DO:
  ASSIGN filnoOfCases.
  RUN cntKg.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME filnoOfUnits
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filnoOfUnits C-Win
ON LEAVE OF filnoOfUnits IN FRAME EFAULT-FRAME /* No of Pieces */
DO:
  ASSIGN filnoOfUnits.
  RUN cntKg.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME filProfit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filProfit C-Win
ON LEAVE OF filProfit IN FRAME EFAULT-FRAME /* Exp Profit */
DO:
    ASSIGN {&SELF-NAME}.

  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME filunitPriceB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filunitPriceB C-Win
ON LEAVE OF filunitPriceB IN FRAME EFAULT-FRAME /* P Price Buy (Rs.) */
DO:
  ASSIGN {&SELF-NAME}.
  RUN cntKg.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME filunitPriceS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filunitPriceS C-Win
ON LEAVE OF filunitPriceS IN FRAME EFAULT-FRAME /* P Price Sell (Rs.) */
DO:
    ASSIGN {&SELF-NAME}.
    RUN cntKg.
    APPLY "VALUE-CHANGED":U TO fillDiscount.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME filunitsPerCase
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filunitsPerCase C-Win
ON LEAVE OF filunitsPerCase IN FRAME EFAULT-FRAME /* Pieces per Case */
DO:
  ASSIGN filunitsPerCase.
  RUN cntKg.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME filunitWeightKG
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filunitWeightKG C-Win
ON LEAVE OF filunitWeightKG IN FRAME EFAULT-FRAME /* P Weight (kg) */
DO:
  ASSIGN filunitWeightKG.
  RUN cntKg.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME filValue
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filValue C-Win
ON LEAVE OF filValue IN FRAME EFAULT-FRAME /* B Value */
DO:
    ASSIGN {&SELF-NAME}.

  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME filValueS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filValueS C-Win
ON LEAVE OF filValueS IN FRAME EFAULT-FRAME /* S Value */
DO:
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME filVarKg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filVarKg C-Win
ON LEAVE OF filVarKg IN FRAME EFAULT-FRAME /* Stock */
DO:
    ASSIGN {&SELF-NAME}.

  
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

/*   session_Window = session_Window + 1. */
  FOR EACH itemCat.
      cmbCat:ADD-LAST(descrip).
  END.

  calendr = chCtrlFrame-2:DTPicker.
  calendr:ENABLED = FALSE.

  RUN monthView.

  APPLY "VALUE-CHANGED":U TO brwItem.
  APPLY "CHOOSE":U TO brwItem .

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cntKg C-Win 
PROCEDURE cntKg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

filkg = filunitWeightKG * ( filnoOfUnits + (filnoOfCases * filunitsPerCase) ).

filcasePriceB = filunitsPerCase * filunitPriceB.

filcasePriceS = filunitsPerCase * filunitPriceS.

filVarKg      = ( filkg / filmaxWeight) * 100.

filValue      = (filnoOfUnits * filunitPriceB) + (filnoOfCases * filcasePriceB).

filValueS      = (filnoOfUnits * filunitPriceS) + (filnoOfCases * filcasePriceS).

filProfit     = ((filnoOfUnits * filunitPriceS) + (filnoOfCases * filcasePriceS)) - filValue.

DISPLAY filkg filcasePriceB filcasePriceS filVarKg filValue filValueS filProfit WITH FRAME EFAULT-FRAME.

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

OCXFile = SEARCH( "Invoices.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chCtrlFrame-2 = CtrlFrame-2:COM-HANDLE
    UIB_S = chCtrlFrame-2:LoadControls( OCXFile, "CtrlFrame-2":U)
    CtrlFrame-2:NAME = "CtrlFrame-2":U
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "Invoices.wrx":U SKIP(1)
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
  DISPLAY filitmName cmbCat filunitWeightKG filunitsPerCase filnoOfUnits 
          filnoOfCases filunitPriceS fillDiscount filcasePriceS filkg 
          filitmSearch filVarKg filID filmaxWeight filunitPriceB filValue 
          filValueS filcasePriceB filProfit 
      WITH FRAME EFAULT-FRAME IN WINDOW C-Win.
  ENABLE brwItem btnAdd btnDuplicate btnMod btnDel filitmSearch btnClose 
         RECT-15 RECT-16 RECT-17 RECT-18 RECT-19 
      WITH FRAME EFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-EFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE monthView C-Win 
PROCEDURE monthView :
DEFINE VARIABLE ttMonth AS CHARACTER   NO-UNDO.
ttMonth = STRING(MONTH(TODAY)).
CASE ttMonth.
    WHEN "1" THEN
    DO:
        monthTag = "JAN".
    END.
    WHEN "2" THEN
    DO:
        monthTag = "FEB".
    END.
    WHEN "3" THEN
    DO:
        monthTag = "MAR".
    END.
    WHEN "4" THEN
    DO:
        monthTag = "APR".
    END.
    WHEN "5" THEN
    DO:
        monthTag = "MAY".
    END.
    WHEN "6" THEN
    DO:
        monthTag = "JUN".
    END.
    WHEN "7" THEN
    DO:
        monthTag = "JUL".
    END.
    WHEN "8" THEN
    DO:
        monthTag = "AUG".
    END.
    WHEN "9" THEN
    DO:
        monthTag = "SEP".
    END.
    WHEN "10" THEN
    DO:
        monthTag = "OCT".
    END.
    WHEN "11" THEN
    DO:
        monthTag = "NOV".
    END.
    WHEN "12" THEN
    DO:
        monthTag = "DEC".
    END.
END CASE.
monthTag = monthTag + "-" + STRING(DAY(TODAY)).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

