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
/* DEFINE SHARED VARIABLE session_UserType AS CHARACTER. */
/* Local Variable Definitions ---                                       */
DEFINE VARIABLE addModify AS CHARACTER   NO-UNDO.
DEFINE VARIABLE monthTag AS CHARACTER   NO-UNDO.

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
itms.unitWeightKG itms.unitsPerCase itms.unitPriceS itms.casePriceS ~
itms.stockP itms.stockC itms.noOfUnits itms.noOfCases 
&Scoped-define ENABLED-FIELDS-IN-QUERY-brwItem 
&Scoped-define QUERY-STRING-brwItem FOR EACH itms ~
      WHERE itms.stat = TRUE NO-LOCK ~
    BY itms.SortID INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-brwItem OPEN QUERY brwItem FOR EACH itms ~
      WHERE itms.stat = TRUE NO-LOCK ~
    BY itms.SortID INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-brwItem itms
&Scoped-define FIRST-TABLE-IN-QUERY-brwItem itms


/* Definitions for FRAME EFAULT-FRAME                                   */
&Scoped-define OPEN-BROWSERS-IN-QUERY-EFAULT-FRAME ~
    ~{&OPEN-QUERY-brwItem}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS brwItem btnMod btnClose filitmSearch RECT-16 ~
RECT-17 RECT-18 RECT-19 RECT-20 
&Scoped-Define DISPLAYED-OBJECTS filitmName cmbCat filID filitmSearch ~
filunitsPerCase filunitWeightKG filmaxWeight filnoOfUnits filnoOfCases ~
filunitPriceB filunitPriceS filcasePriceB filcasePriceS filkg filVarKg ~
filValue filProfit fillDiscount filValueS 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCancel 
     LABEL "Cancel" 
     SIZE 15 BY 1.

DEFINE BUTTON btnClose 
     LABEL "Close" 
     SIZE 15 BY 1.

DEFINE BUTTON btnMod 
     LABEL "Add to Store" 
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
     BGCOLOR 7 FGCOLOR 11  NO-UNDO.

DEFINE VARIABLE filID AS INTEGER FORMAT "9999":U INITIAL 0 
     LABEL "Item ID" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88
     BGCOLOR 7 FGCOLOR 11  NO-UNDO.

DEFINE VARIABLE filitmName AS CHARACTER FORMAT "X(50)":U 
     LABEL "Item Name" 
     VIEW-AS FILL-IN 
     SIZE 35 BY .88
     BGCOLOR 7 FGCOLOR 11  NO-UNDO.

DEFINE VARIABLE filitmSearch AS CHARACTER FORMAT "X(50)":U 
     LABEL "Search" 
     VIEW-AS FILL-IN 
     SIZE 35 BY .88
     BGCOLOR 15 FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE filkg AS DECIMAL FORMAT ">>,>>9.999":U INITIAL 0 
     LABEL "Total Weight" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88
     BGCOLOR 5 FGCOLOR 15 FONT 10 NO-UNDO.

DEFINE VARIABLE fillDiscount AS DECIMAL FORMAT ">9.99":U INITIAL 0 
     LABEL "Discount (%)" 
     VIEW-AS FILL-IN 
     SIZE 9.43 BY .88
     BGCOLOR 7 FGCOLOR 11  NO-UNDO.

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
     BGCOLOR 7 FGCOLOR 11  NO-UNDO.

DEFINE VARIABLE filunitsPerCase AS INTEGER FORMAT ">,>>9":U INITIAL 0 
     LABEL "Pieces per Case" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88
     BGCOLOR 7 FGCOLOR 11  NO-UNDO.

DEFINE VARIABLE filunitWeightKG AS DECIMAL FORMAT ">>,>>9.999":U INITIAL 0 
     LABEL "P Weight (kg)" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88
     BGCOLOR 7 FGCOLOR 11  NO-UNDO.

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

DEFINE RECTANGLE RECT-16
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL  GROUP-BOX  ROUNDED 
     SIZE 47 BY 1.65.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL  GROUP-BOX  ROUNDED 
     SIZE 47 BY 3.58.

DEFINE RECTANGLE RECT-18
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL  GROUP-BOX  ROUNDED 
     SIZE 64.86 BY 5.31.

DEFINE RECTANGLE RECT-19
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL  GROUP-BOX  ROUNDED 
     SIZE 28.43 BY 5.31.

DEFINE RECTANGLE RECT-20
     EDGE-PIXELS 3 GRAPHIC-EDGE  NO-FILL  GROUP-BOX  ROUNDED 
     SIZE 69 BY 1.5.

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
      itms.cat COLUMN-LABEL " Category" FORMAT "x(12)":U WIDTH 13
      itms.itmName FORMAT "x(50)":U WIDTH 40 COLUMN-FGCOLOR 11 COLUMN-BGCOLOR 7
      itms.unitWeightKG COLUMN-LABEL "Weight (kg)" FORMAT ">>,>>9.999":U
            WIDTH 11 COLUMN-FGCOLOR 11 COLUMN-BGCOLOR 7
      itms.unitsPerCase COLUMN-LABEL "Per Case" FORMAT ">>>9":U
            WIDTH 8
      itms.unitPriceS COLUMN-LABEL "Unit Sell" FORMAT ">>,>>9.99":U
            WIDTH 10
      itms.casePriceS COLUMN-LABEL "Case Sell" FORMAT ">>,>>9.99":U
            WIDTH 10
      itms.stockP COLUMN-LABEL "Stock P" FORMAT ">>>>9":U WIDTH 10
            COLUMN-FGCOLOR 14 COLUMN-BGCOLOR 0
      itms.stockC COLUMN-LABEL "Stock C" FORMAT ">>>>9":U WIDTH 10
            COLUMN-FGCOLOR 14 COLUMN-BGCOLOR 0
      itms.noOfUnits COLUMN-LABEL "Pieces In" FORMAT ">>>>9":U
      itms.noOfCases COLUMN-LABEL "Cases In" FORMAT ">>>>9":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 142.57 BY 18.81
         BGCOLOR 15 FGCOLOR 0 FONT 10
         TITLE BGCOLOR 15 FGCOLOR 0 "Items" ROW-HEIGHT-CHARS .55 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME EFAULT-FRAME
     brwItem AT ROW 1.08 COL 1.43 WIDGET-ID 200
     filitmName AT ROW 23.35 COL 11.43 COLON-ALIGNED WIDGET-ID 4
     cmbCat AT ROW 24.31 COL 11.72 COLON-ALIGNED WIDGET-ID 76
     filID AT ROW 22.38 COL 11.43 COLON-ALIGNED WIDGET-ID 2 NO-TAB-STOP 
     btnSave AT ROW 26.08 COL 56.72 WIDGET-ID 38
     btnCancel AT ROW 26.08 COL 73.72 WIDGET-ID 36
     btnMod AT ROW 26.08 COL 39.72 WIDGET-ID 32
     btnClose AT ROW 26.08 COL 90.72 WIDGET-ID 40
     filitmSearch AT ROW 20.77 COL 11.43 COLON-ALIGNED WIDGET-ID 98
     filunitsPerCase AT ROW 20.58 COL 63 COLON-ALIGNED WIDGET-ID 124
     filunitWeightKG AT ROW 21.58 COL 63 COLON-ALIGNED WIDGET-ID 126
     filmaxWeight AT ROW 22.58 COL 63 COLON-ALIGNED WIDGET-ID 112
     filnoOfUnits AT ROW 23.54 COL 63.14 COLON-ALIGNED WIDGET-ID 116
     filnoOfCases AT ROW 24.54 COL 63.14 COLON-ALIGNED WIDGET-ID 114
     filunitPriceB AT ROW 22.58 COL 92.57 COLON-ALIGNED WIDGET-ID 120 NO-TAB-STOP 
     filunitPriceS AT ROW 20.58 COL 92.57 COLON-ALIGNED WIDGET-ID 122
     filcasePriceB AT ROW 24.54 COL 92.57 COLON-ALIGNED WIDGET-ID 104 NO-TAB-STOP 
     filcasePriceS AT ROW 23.54 COL 92.57 COLON-ALIGNED WIDGET-ID 106 NO-TAB-STOP 
     filkg AT ROW 20.58 COL 126.57 COLON-ALIGNED WIDGET-ID 108 NO-TAB-STOP 
     filVarKg AT ROW 21.58 COL 126.57 COLON-ALIGNED WIDGET-ID 132 NO-TAB-STOP 
     filValue AT ROW 22.58 COL 122.57 COLON-ALIGNED WIDGET-ID 128 NO-TAB-STOP 
     filProfit AT ROW 24.54 COL 122.57 COLON-ALIGNED WIDGET-ID 118 NO-TAB-STOP 
     fillDiscount AT ROW 21.58 COL 92.57 COLON-ALIGNED WIDGET-ID 110
     filValueS AT ROW 23.54 COL 122.57 COLON-ALIGNED WIDGET-ID 130 NO-TAB-STOP 
     RECT-16 AT ROW 20.38 COL 2.43 WIDGET-ID 134
     RECT-17 AT ROW 22.12 COL 2.43 WIDGET-ID 136
     RECT-18 AT ROW 20.35 COL 49.72 WIDGET-ID 138
     RECT-19 AT ROW 20.35 COL 114.86 WIDGET-ID 140
     RECT-20 AT ROW 25.81 COL 38 WIDGET-ID 142
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 143.43 BY 26.54
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
         TITLE              = "LMS - Items"
         COLUMN             = 1.29
         ROW                = 1
         HEIGHT             = 26.54
         WIDTH              = 143.43
         MAX-HEIGHT         = 26.54
         MAX-WIDTH          = 144.14
         VIRTUAL-HEIGHT     = 26.54
         VIRTUAL-WIDTH      = 144.14
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
     _Where[1]         = "itms.stat = TRUE"
     _FldNameList[1]   > ics.itms.itmID
"itms.itmID" "ID" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ics.itms.cat
"itms.cat" " Category" "x(12)" "character" ? ? ? ? ? ? no ? no no "13" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ics.itms.itmName
"itms.itmName" ? ? "character" 7 11 ? ? ? ? no ? no no "40" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ics.itms.unitWeightKG
"itms.unitWeightKG" "Weight (kg)" ? "decimal" 7 11 ? ? ? ? no ? no no "11" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > ics.itms.unitsPerCase
"itms.unitsPerCase" "Per Case" ? "integer" ? ? ? ? ? ? no ? no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > ics.itms.unitPriceS
"itms.unitPriceS" "Unit Sell" ? "decimal" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > ics.itms.casePriceS
"itms.casePriceS" "Case Sell" ? "decimal" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > ics.itms.stockP
"itms.stockP" "Stock P" ? "integer" 0 14 ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > ics.itms.stockC
"itms.stockC" "Stock C" ? "integer" 0 14 ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > ics.itms.noOfUnits
"itms.noOfUnits" "Pieces In" ">>>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > ics.itms.noOfCases
"itms.noOfCases" "Cases In" ">>>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE brwItem */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* LMS - Items */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. 
  IF THIS-PROCEDURE:PERSISTENT THEN  */ RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* LMS - Items */
DO:
  /* This event will close the window and terminate the procedure.  */
  MESSAGE "Confrm to close the window?" VIEW-AS ALERT-BOX INFO BUTTONS YES-NO UPDATE yn AS LOGICAL.
  IF yn = YES THEN
    DO:
/*       session_Window = session_Window - 1. */
      APPLY "CLOSE":U TO THIS-PROCEDURE.
      RETURN NO-APPLY.
    END.
  ELSE
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brwItem
&Scoped-define SELF-NAME brwItem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brwItem C-Win
ON VALUE-CHANGED OF brwItem IN FRAME EFAULT-FRAME /* Items */
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
      filnoOfCases     = stockC    
      filnoOfUnits     = stockP    
      filunitsPerCase  = unitsPerCase 
      filunitWeightKG  = unitWeightKG 
      fillDiscount     = discountIN 
      .
    RUN cntKg.
    DISPLAY fillDiscount filcasePriceB filcasePriceS filunitPriceB filunitPriceS cmbCat filID filitmName filmaxWeight filnoOfCases filnoOfUnits filunitsPerCase filunitWeightKG WITH FRAME EFAULT-FRAME.
    END.
    ELSE IF NOT AVAILABLE itms THEN
        MESSAGE "No records to show." VIEW-AS ALERT-BOX INFO BUTTONS OK .

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
        
        DISABLE fillDiscount btnCancel btnSave filcasePriceB filcasePriceS filunitPriceB filunitPriceS cmbCat filitmName filmaxWeight filnoOfCases filnoOfUnits filunitsPerCase filunitWeightKG
        WITH FRAME EFAULT-FRAME.
        ENABLE brwItem btnClose btnMod WITH FRAME EFAULT-FRAME.
        DISPLAY filkg filVarKg filcasePriceB filcasePriceS filunitPriceB filunitPriceS cmbCat filID filitmName filmaxWeight filnoOfCases filnoOfUnits filunitsPerCase filunitWeightKG WITH FRAME EFAULT-FRAME.
    END.
    filnoOfUnits:LABEL = "No of Pieces".
  filnoOfCases:LABEL = "No of Cases".
/*     RUN permissions. */
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


&Scoped-define SELF-NAME btnMod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnMod C-Win
ON CHOOSE OF btnMod IN FRAME EFAULT-FRAME /* Add to Store */
DO:
  IF filitmName <> "" THEN
  DO:
      filnoOfUnits = 0.
      filnoOfCases = 0.
      ENABLE btnCancel btnSave filnoOfCases filnoOfUnits WITH FRAME EFAULT-FRAME.
      DISABLE brwItem  btnClose  btnMod WITH FRAME EFAULT-FRAME.
      DISPLAY filnoOfUnits filnoOfCases WITH FRAME {&FRAME-NAME}.
  END.
  ELSE
      MESSAGE "No records to Modify." VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  filnoOfUnits:LABEL = "Add " + filnoOfUnits:LABEL.
  filnoOfCases:LABEL = "Add " + filnoOfCases:LABEL.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSave C-Win
ON CHOOSE OF btnSave IN FRAME EFAULT-FRAME /* Save */
DO:
  IF NOT filnoOfCases > 0 AND NOT filnoOfUnits > 0 THEN
  DO:
      MESSAGE "You must enter No of Pieses or  No of Cases to add to store." VIEW-AS ALERT-BOX ERROR BUTTONS OK .
      RETURN.
  END.

  MESSAGE "Conferm to save the record?" VIEW-AS ALERT-BOX INFO BUTTONS YES-NO UPDATE yn AS LOGICAL.
  IF yn = YES THEN
  DO:
         FIND FIRST itms WHERE itms.itmID = filID.
            itms.noOfUnits = itms.noOfUnits + filnoOfUnits.
            itms.noOfCases = itms.noOfCases + filnoOfCases.
            itms.stockC    = itms.stockC + filnoOfCases.
            itms.stockP    = itms.stockP + filnoOfUnits.
         RELEASE itms.
     
         DISABLE fillDiscount btnCancel btnSave filcasePriceB filcasePriceS filunitPriceB filunitPriceS cmbCat filitmName filmaxWeight filnoOfCases filnoOfUnits filunitsPerCase filunitWeightKG
           WITH FRAME EFAULT-FRAME.
         ENABLE brwItem btnClose btnMod WITH FRAME EFAULT-FRAME.
     
         OPEN QUERY brwItem FOR EACH itms WHERE itms.stat = yes NO-LOCK BY itms.SortID.
         APPLY "VALUE-CHANGED":U TO brwItem.
         IF NOT ERROR-STATUS:ERROR THEN
             MESSAGE "Record successfully created." VIEW-AS ALERT-BOX INFO BUTTONS OK.
  END.
  filnoOfUnits:LABEL = "No of Pieces".
  filnoOfCases:LABEL = "No of Cases".

/*   RUN permissions. */
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
ON LEAVE OF filkg IN FRAME EFAULT-FRAME /* Total Weight */
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
  ASSIGN {&SELF-NAME}.
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

  session_Window = session_Window + 1.
  RUN permissions.

  FOR EACH itemCat.
      cmbCat:ADD-LAST(descrip).
  END.

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

filProfit     = ((filnoOfUnits * filunitPriceS) + (filnoOfCases * filcasePriceS)) - filValue.

DISPLAY filkg filcasePriceB filcasePriceS filVarKg filValue filProfit WITH FRAME EFAULT-FRAME.

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
  DISPLAY filitmName cmbCat filID filitmSearch filunitsPerCase filunitWeightKG 
          filmaxWeight filnoOfUnits filnoOfCases filunitPriceB filunitPriceS 
          filcasePriceB filcasePriceS filkg filVarKg filValue filProfit 
          fillDiscount filValueS 
      WITH FRAME EFAULT-FRAME IN WINDOW C-Win.
  ENABLE brwItem btnMod btnClose filitmSearch RECT-16 RECT-17 RECT-18 RECT-19 
         RECT-20 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE permissions C-Win 
PROCEDURE permissions :
/* IF session_UserType <> "Administrator" OR session_UserType <> "Super Admin" THEN */
/* DO:                                                                              */
/*         DISABLE btnMod WITH FRAME {&FRAME-NAME}.                                 */
/*     DISPLAY btnMod WITH FRAME {&FRAME-NAME}.                                     */
/* END.                                                                             */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

