&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
CREATE WIDGET-POOL.

DEFINE VARIABLE addModifyItem AS CHARACTER   NO-UNDO.
DEFINE VARIABLE addModifyTable AS CHARACTER   NO-UNDO.
DEFINE SHARED VARIABLE session_User AS CHARACTER.
/* DEFINE VARIABLE session_User AS CHARACTER INIT "asiri". */
DEFINE SHARED VARIABLE session_Window AS INT.

DEFINE VARIABLE isRefresh AS LOGICAL   NO-UNDO INIT FALSE.

DEFINE VARIABLE calendr AS COM-HANDLE   NO-UNDO.

DEFINE VARIABLE tempIDtt-ldunld AS INTEGER     NO-UNDO.
DEFINE VARIABLE tempID AS INTEGER     NO-UNDO.

DEFINE TEMP-TABLE tt-ldunld
 FIELDS ID       AS INT
 FIELDS SortID   AS INT
 FIELDS vehNo    AS INT
 FIELDS itmID    AS INT
 FIELDS itmName  AS CHAR
 FIELDS Weight   AS DEC
 FIELDS PriceP   AS DEC
 FIELDS PerCase  AS INT
 FIELDS BSC      AS INT
 FIELDS BSP      AS INT
 FIELDS GRRD      AS INT
 FIELDS GRST      AS INT
 FIELDS LDC      AS INT
 FIELDS LDP      AS INT
 FIELDS ULC      AS INT
 FIELDS ULP      AS INT
 FIELDS RDC      AS INT
 FIELDS RDP      AS INT
 FIELDS TolP     AS INT
 FIELDS TOlC     AS INT
 FIELDS Excess   AS INT
 FIELDS BilP     AS INT
 FIELDS Short    AS INT
 FIELDS Amount   AS DEC
 .

DEFINE TEMP-TABLE tt-itms LIKE itms.
DEFINE TEMP-TABLE tt-recipts LIKE recipts.
DEFINE TEMP-TABLE tt-bills LIKE bills.
DEFINE TEMP-TABLE tt-lorryStock LIKE lorryStock.

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
&Scoped-define INTERNAL-TABLES tt-ldunld

/* Definitions for BROWSE brw                                           */
&Scoped-define FIELDS-IN-QUERY-brw /* ID */ /* vehNo */ /* itmID */ itmName Weight /* PriceP */ BSC BSP GRRD GRST LDC LDP ULC ULP RDC RDP Excess SHORT /* Amount */ /*COLUMN-FGCOLOR 1*/ /*COLUMN-FGCOLOR 9 */ /*COLUMN-FGCOLOR 1 */ /*COLUMN-FGCOLOR 9 */ /*COLUMN-FGCOLOR 1 */ /*COLUMN-FGCOLOR 9 */ /*COLUMN-FGCOLOR 1 */ /*COLUMN-FGCOLOR 9 */ /*COLUMN-FGCOLOR 2 */ /*COLUMN-FGCOLOR 12 */   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brw   
&Scoped-define SELF-NAME brw
&Scoped-define QUERY-STRING-brw FOR EACH tt-ldunld by tt-ldunld.SortID
&Scoped-define OPEN-QUERY-brw OPEN QUERY brw FOR EACH tt-ldunld by tt-ldunld.SortID.
&Scoped-define TABLES-IN-QUERY-brw tt-ldunld
&Scoped-define FIRST-TABLE-IN-QUERY-brw tt-ldunld


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-brw}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS cmbVeh btnModifyTable btnDeleteTable ~
filExcessShortP btnBSSave btnAddTable RECT-1 RECT-2 RECT-3 RECT-4 RECT-5 
&Scoped-Define DISPLAYED-OBJECTS cmbVeh filLDC filLDP filULC filULP ~
filLastWorkingDay cmbName filStockP filLorriesP filBSC filBSP filTolRDP ~
filBillP filRecipt# filTolLDC filTolLDP filKg filUnitPrice filExcessShortP ~
filRDC filRDP filCasePrice filPerCase filGRRD filGRST 

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
DEFINE BUTTON btnAddTable 
     LABEL "Add Table" 
     SIZE 14 BY 1.

DEFINE BUTTON btnBSSave 
     LABEL "BS Save" 
     SIZE 14 BY 1.

DEFINE BUTTON btnCancel 
     LABEL "Cancel Item" 
     SIZE 14 BY 1.

DEFINE BUTTON btnCancelTable 
     LABEL "Cancel Table" 
     SIZE 14 BY 1.

DEFINE BUTTON btnDeleteTable 
     LABEL "Print Table" 
     SIZE 14 BY 1.

DEFINE BUTTON btnModifyItem 
     LABEL "Modify Item" 
     SIZE 14 BY 1.

DEFINE BUTTON btnModifyTable 
     LABEL "View Table" 
     SIZE 14 BY 1.

DEFINE BUTTON btnRfresh 
     LABEL "Refresh" 
     SIZE 9 BY 2 TOOLTIP "Refresh & save BS, Billed amounts for the day".

DEFINE BUTTON btnSave 
     LABEL "Save Item" 
     SIZE 14 BY 1.

DEFINE BUTTON btnSaveTable 
     LABEL "Save Table" 
     SIZE 14 BY 1.

DEFINE VARIABLE cmbName AS CHARACTER FORMAT "X(32)":U 
     LABEL "Name" 
     VIEW-AS COMBO-BOX INNER-LINES 30
     LIST-ITEM-PAIRS "--Select Here--","0"
     DROP-DOWN-LIST
     SIZE 35 BY 1 NO-UNDO.

DEFINE VARIABLE cmbVeh AS INTEGER FORMAT ">>>>9":U INITIAL 0 
     LABEL "Vehical" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEM-PAIRS "--Select Here--",0
     DROP-DOWN-LIST
     SIZE 23.72 BY 1 NO-UNDO.

DEFINE VARIABLE filBillP AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "Billed P" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .85
     BGCOLOR 5 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE filBSC AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "BSC" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .85
     BGCOLOR 0 FGCOLOR 11  NO-UNDO.

DEFINE VARIABLE filBSP AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "BSP" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .85
     BGCOLOR 0 FGCOLOR 11  NO-UNDO.

DEFINE VARIABLE filCasePrice AS DECIMAL FORMAT ">,>>>,>>9.99":U INITIAL 0 
     LABEL "C Price" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .85
     BGCOLOR 7 FGCOLOR 11  NO-UNDO.

DEFINE VARIABLE filExcessShortP AS INTEGER FORMAT "->>>9":U INITIAL 0 
     LABEL "Variance" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .85
     BGCOLOR 7 FGCOLOR 15 FONT 6 NO-UNDO.

DEFINE VARIABLE filGRRD AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "GRP RD" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .85 TOOLTIP "Road distributed good return Qty"
     BGCOLOR 0 FGCOLOR 11  NO-UNDO.

DEFINE VARIABLE filGRST AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "GRP Stock" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .85
     BGCOLOR 7 FGCOLOR 11  NO-UNDO.

DEFINE VARIABLE filKg AS DECIMAL FORMAT ">>9.999":U INITIAL 0 
     LABEL "Weight" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .85
     BGCOLOR 7 FGCOLOR 11  NO-UNDO.

DEFINE VARIABLE filLastWorkingDay AS DATE FORMAT "99/99/9999":U 
     LABEL "Last Working ( BS Saved ) Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1
     BGCOLOR 4 FGCOLOR 14  NO-UNDO.

DEFINE VARIABLE filLDC AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "LDC" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .85
     BGCOLOR 15 FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE filLDP AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "LDP" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .85
     BGCOLOR 15 FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE filLorriesP AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "Lorries P" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .88
     BGCOLOR 7 FGCOLOR 11  NO-UNDO.

DEFINE VARIABLE filPerCase AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Per C" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .85
     BGCOLOR 7 FGCOLOR 11  NO-UNDO.

DEFINE VARIABLE filRDC AS INTEGER FORMAT "->>>9":U INITIAL 0 
     LABEL "RDC" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .85
     BGCOLOR 0 FGCOLOR 11  NO-UNDO.

DEFINE VARIABLE filRDP AS INTEGER FORMAT "->>>9":U INITIAL 0 
     LABEL "RDP" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .85
     BGCOLOR 0 FGCOLOR 11  NO-UNDO.

DEFINE VARIABLE filRecipt# AS INTEGER FORMAT ">,>>>,>>>,>>>9":U INITIAL 0 
     LABEL "ID" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .85
     BGCOLOR 7 FGCOLOR 11  NO-UNDO.

DEFINE VARIABLE filStockP AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "Stock P" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .85
     BGCOLOR 7 FGCOLOR 11  NO-UNDO.

DEFINE VARIABLE filTolLDC AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "Tol LDC" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .85
     BGCOLOR 0 FGCOLOR 11  NO-UNDO.

DEFINE VARIABLE filTolLDP AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "Tol LDP" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .85
     BGCOLOR 0 FGCOLOR 11  NO-UNDO.

DEFINE VARIABLE filTolRDP AS INTEGER FORMAT "->>>9":U INITIAL 0 
     LABEL "Tol RDP" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .85
     BGCOLOR 0 FGCOLOR 11  NO-UNDO.

DEFINE VARIABLE filULC AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "ULC" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .85
     BGCOLOR 15 FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE filULP AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "ULP" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .85
     BGCOLOR 15 FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE filUnitPrice AS DECIMAL FORMAT ">,>>>,>>9.99":U INITIAL 0 
     LABEL "P Price" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .85
     BGCOLOR 7 FGCOLOR 11  NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL  GROUP-BOX  ROUNDED 
     SIZE 44 BY 16.08.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL  GROUP-BOX  ROUNDED 
     SIZE 44 BY 1.88.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL  GROUP-BOX  ROUNDED 
     SIZE 44 BY 2.92.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL  GROUP-BOX  ROUNDED 
     SIZE 44 BY 4.46.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL  GROUP-BOX  ROUNDED 
     SIZE 44 BY 1.35.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brw FOR 
      tt-ldunld SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brw
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brw C-Win _FREEFORM
  QUERY brw DISPLAY
      /*   ID */
/* vehNo */
/* itmID */
itmName FORMAT "X(40)":U LABEL "Item Name"
Weight  FORMAT ">9.999" LABEL "Wgt"
/* PriceP */
BSC    FORMAT ">>>9"
BSP    FORMAT ">>>9" COLUMN-BGCOLOR 16
GRRD    FORMAT ">>>9" LABEL "GRR"
GRST    FORMAT ">>>9" LABEL "GRS" COLUMN-BGCOLOR 16
LDC    FORMAT ">>>9" 
LDP    FORMAT ">>>9" COLUMN-BGCOLOR 16
ULC    FORMAT ">>>9" 
ULP    FORMAT ">>>9" COLUMN-BGCOLOR 16
RDC    FORMAT "->>>9"
RDP    FORMAT "->>>9"COLUMN-BGCOLOR 16
BilP   FORMAT "->>>9" LABEL "Bil"
Excess FORMAT ">>>9" LABEL "Exs" COLUMN-FGCOLOR 2
SHORT  FORMAT ">>>9" LABEL "Sht" COLUMN-FGCOLOR 12
/* Amount FORMAT ">,>>>,>>9.99" */
/*COLUMN-FGCOLOR 1*/                                  
/*COLUMN-FGCOLOR 9 COLUMN-BGCOLOR 16 */                
/*COLUMN-FGCOLOR 1 */                
/*COLUMN-FGCOLOR 9 COLUMN-BGCOLOR 16 */                
/*COLUMN-FGCOLOR 1 */                
/*COLUMN-FGCOLOR 9 COLUMN-BGCOLOR 16 */            
/*COLUMN-FGCOLOR 1                   */          
/*COLUMN-FGCOLOR 9 COLUMN-BGCOLOR 16 */        
/*COLUMN-FGCOLOR 2                   */      
/*COLUMN-FGCOLOR 12                  */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 98.86 BY 26.46
         FONT 10
         TITLE "Loading Unloading" ROW-HEIGHT-CHARS .54 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnRfresh AT ROW 1.54 COL 34.86 WIDGET-ID 248 NO-TAB-STOP 
     cmbVeh AT ROW 1.58 COL 7.57 COLON-ALIGNED WIDGET-ID 84
     btnModifyTable AT ROW 6.15 COL 16.43 WIDGET-ID 218
     btnDeleteTable AT ROW 6.15 COL 30.72 WIDGET-ID 220 NO-TAB-STOP 
     brw AT ROW 1 COL 46.14 WIDGET-ID 300
     btnModifyItem AT ROW 9.5 COL 2.14 WIDGET-ID 208
     filLDC AT ROW 20.46 COL 11.43 COLON-ALIGNED WIDGET-ID 180
     filLDP AT ROW 20.46 COL 28 COLON-ALIGNED WIDGET-ID 168
     filULC AT ROW 22.92 COL 11.43 COLON-ALIGNED WIDGET-ID 184
     filULP AT ROW 22.92 COL 28 COLON-ALIGNED WIDGET-ID 182
     btnSave AT ROW 9.5 COL 16.43 WIDGET-ID 212
     btnCancel AT ROW 9.5 COL 30.72 WIDGET-ID 214
     filLastWorkingDay AT ROW 4.38 COL 25.86 COLON-ALIGNED WIDGET-ID 206 NO-TAB-STOP 
     cmbName AT ROW 13 COL 7.57 COLON-ALIGNED WIDGET-ID 54 NO-TAB-STOP 
     filStockP AT ROW 14.42 COL 32.43 COLON-ALIGNED WIDGET-ID 158 NO-TAB-STOP 
     filLorriesP AT ROW 15.65 COL 32.43 COLON-ALIGNED WIDGET-ID 176 NO-TAB-STOP 
     filBSC AT ROW 19.31 COL 11.43 COLON-ALIGNED WIDGET-ID 166 NO-TAB-STOP 
     btnSaveTable AT ROW 7.42 COL 2.14 WIDGET-ID 224 NO-TAB-STOP 
     filBSP AT ROW 19.31 COL 28 COLON-ALIGNED WIDGET-ID 164 NO-TAB-STOP 
     filTolRDP AT ROW 25.65 COL 7.57 COLON-ALIGNED WIDGET-ID 228 NO-TAB-STOP 
     btnCancelTable AT ROW 7.42 COL 16.43 WIDGET-ID 222 NO-TAB-STOP 
     filBillP AT ROW 25.65 COL 21.57 COLON-ALIGNED WIDGET-ID 200 NO-TAB-STOP 
     filRecipt# AT ROW 11.81 COL 7.57 COLON-ALIGNED WIDGET-ID 4 NO-TAB-STOP 
     filTolLDC AT ROW 21.69 COL 11.43 COLON-ALIGNED WIDGET-ID 194 NO-TAB-STOP 
     filTolLDP AT ROW 21.69 COL 28 COLON-ALIGNED WIDGET-ID 196 NO-TAB-STOP 
     filKg AT ROW 14.42 COL 7.57 COLON-ALIGNED WIDGET-ID 8 NO-TAB-STOP 
     filUnitPrice AT ROW 15.65 COL 7.57 COLON-ALIGNED WIDGET-ID 58 NO-TAB-STOP 
     filExcessShortP AT ROW 25.65 COL 36.57 COLON-ALIGNED WIDGET-ID 204 NO-TAB-STOP 
     filRDC AT ROW 24.15 COL 11.43 COLON-ALIGNED WIDGET-ID 188 NO-TAB-STOP 
     filRDP AT ROW 24.15 COL 28 COLON-ALIGNED WIDGET-ID 186 NO-TAB-STOP 
     filCasePrice AT ROW 16.88 COL 7.57 COLON-ALIGNED WIDGET-ID 64 NO-TAB-STOP 
     filPerCase AT ROW 16.88 COL 32.57 COLON-ALIGNED WIDGET-ID 162 NO-TAB-STOP 
     btnBSSave AT ROW 7.46 COL 30.72 WIDGET-ID 246 NO-TAB-STOP 
     btnAddTable AT ROW 6.15 COL 2.14 WIDGET-ID 252
     filGRRD AT ROW 18.23 COL 28 COLON-ALIGNED WIDGET-ID 254
     filGRST AT ROW 18.23 COL 11.29 COLON-ALIGNED WIDGET-ID 256
     "Date:" VIEW-AS TEXT
          SIZE 4.57 BY .58 AT ROW 2.85 COL 4.86 WIDGET-ID 234
     RECT-1 AT ROW 11.23 COL 1.43 WIDGET-ID 238
     RECT-2 AT ROW 9.08 COL 1.43 WIDGET-ID 240
     RECT-3 AT ROW 5.85 COL 1.43 WIDGET-ID 242
     RECT-4 AT ROW 1.12 COL 1.43 WIDGET-ID 244
     RECT-5 AT ROW 4.23 COL 1.43 WIDGET-ID 250
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
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
         TITLE              = "Loading Unloading"
         COLUMN             = 1.72
         ROW                = 1.04
         HEIGHT             = 26.46
         WIDTH              = 144
         MAX-HEIGHT         = 26.46
         MAX-WIDTH          = 144
         VIRTUAL-HEIGHT     = 26.46
         VIRTUAL-WIDTH      = 144
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
   FRAME-NAME Size-to-Fit Custom                                        */
/* BROWSE-TAB brw btnDeleteTable DEFAULT-FRAME */
ASSIGN 
       FRAME DEFAULT-FRAME:SCROLLABLE       = FALSE.

/* SETTINGS FOR BROWSE brw IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnCancel IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnCancelTable IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnModifyItem IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnRfresh IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnSave IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnSaveTable IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX cmbName IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN filBillP IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN filBSC IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN filBSP IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN filCasePrice IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN filGRRD IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN filGRST IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN filKg IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN filLastWorkingDay IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       filLastWorkingDay:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN filLDC IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN filLDP IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN filLorriesP IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN filPerCase IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN filRDC IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN filRDP IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN filRecipt# IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN filStockP IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN filTolLDC IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN filTolLDP IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN filTolRDP IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN filULC IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN filULP IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN filUnitPrice IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brw
/* Query rebuild information for BROWSE brw
     _START_FREEFORM
OPEN QUERY brw FOR EACH tt-ldunld by tt-ldunld.SortID.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE brw */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 2.73
       COLUMN          = 9.57
       HEIGHT          = .81
       WIDTH           = 23.72
       WIDGET-ID       = 232
       HIDDEN          = no
       SENSITIVE       = yes.
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {20DD1B9E-87C4-11D1-8BE3-0000F8754DA1} type: DTPicker */
      CtrlFrame:MOVE-AFTER(cmbVeh:HANDLE IN FRAME DEFAULT-FRAME).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Loading Unloading */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. 
  IF THIS-PROCEDURE:PERSISTENT THEN  */ RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Loading Unloading */
DO:
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


&Scoped-define BROWSE-NAME brw
&Scoped-define SELF-NAME brw
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brw C-Win
ON VALUE-CHANGED OF brw IN FRAME DEFAULT-FRAME /* Loading Unloading */
DO:
  RUN itemLoader.
    IF AVAILABLE tt-ldunld THEN     
    DO:

        FIND FIRST itms WHERE itms.itmID = tt-ldunld.itmID NO-LOCK NO-ERROR.

        filRecipt#      = tt-ldunld.ID     .
        filKg           = tt-ldunld.Weight .   
        filPerCase      = itms.unitsPerCase.
        filUnitPrice    = tt-ldunld.PriceP   .       
        filCasePrice    = itms.casePriceS   .
        filBSP          = tt-ldunld.BSP    .   
        filBSC          = tt-ldunld.BSC    .
        filGRRD         = tt-ldunld.GRRD    .
        filGRST         = tt-ldunld.GRST    .
        filLDC          = tt-ldunld.LDC    .   
        filLDP          = tt-ldunld.LDP    .   
        filULC          = tt-ldunld.ULC    .   
        filULP          = tt-ldunld.ULP    .   
        filRDC          = tt-ldunld.RDC    .       
        filRDP          = tt-ldunld.RDP    .  
        cmbName         = string(tt-ldunld.itmID) .
        filStockP       = itms.stockP + (itms.stockC * itms.unitsPerCase).
        filBillP        = tt-ldunld.BilP. 
        filLorriesP     = tt-ldunld.BSP + tt-ldunld.BSP.

        
        IF tt-ldunld.Short = 0 THEN
        DO:
            filExcessShortP = Excess .
            filExcessShortP:BGCOLOR IN FRAME {&FRAME-NAME} = 2 .
        END.
        ELSE IF tt-ldunld.Excess = 0 THEN
        DO:
            filExcessShortP = tt-ldunld.Short.
            filExcessShortP:BGCOLOR IN FRAME {&FRAME-NAME} = 12 .
        END.
        IF tt-ldunld.SHORT = 0 AND tt-ldunld.Excess = 0 THEN
        DO:
            filExcessShortP = 0.
            filExcessShortP:BGCOLOR IN FRAME {&FRAME-NAME} = 7.
        END.

        filTolLDP = tt-ldunld.GRRD + tt-ldunld.BSP + tt-ldunld.LDP.
        filTolLDC = tt-ldunld.BSC + tt-ldunld.LDC.
        filTolRDP = tt-ldunld.RDP + (tt-ldunld.RDC * itms.unitsPerCase).
    END.

DISPLAY filTolLDP filTolLDC filTolRDP filRecipt#      
        filKg        
        filPerCase   
        filUnitPrice 
        filCasePrice 
        filBSP       
        filBSC 
        filGRRD
        filGRST
        filLDC       
        filLDP       
        filULC       
        filULP       
        filRDC       
        filRDP       
        cmbName    
        filStockP  
        filBillP   
        filLorriesP
        filExcessShortP
    WITH FRAME DEFAULT-FRAME.



END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAddTable
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAddTable C-Win
ON CHOOSE OF btnAddTable IN FRAME DEFAULT-FRAME /* Add Table */
DO:
    DEFINE VARIABLE tim AS INTEGER     NO-UNDO .

    DEFINE VARIABLE tempDate AS DATE        NO-UNDO.
    tempDate = calendr:VALUE.

    IF cmbVeh = 0 THEN
    DO:
        MESSAGE "Select Vehical first." VIEW-AS ALERT-BOX WARNING BUTTONS OK.
        RETURN.
    END.
    IF tempDate > TODAY THEN
    DO:
        MESSAGE "Date cannot be a future date." VIEW-AS ALERT-BOX WARNING BUTTONS OK.
        RETURN.
    END.
    FIND FIRST lorryStock WHERE lorryStock.crDate = tempDate AND lorryStock.VehID = cmbVeh NO-LOCK NO-ERROR.
    IF AVAILABLE lorryStock THEN
    DO:
        MESSAGE "You already entered Loading Unloading for " + STRING(DATE(tempDate)) + "." VIEW-AS ALERT-BOX WARNING BUTTONS OK.
        RETURN.
    END.

  
    MESSAGE "This process should be done after entering all Bill's data to the system for the relevent Date" SKIP
      "You cannot enter bills after saving daysale report." SKIP 
      "Conferm to Add new Day Sale record?"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE yn AS LOGICAL.
  
    IF yn = YES THEN 
    DO:
      tim = TIME.

      RUN populateTableFast.

      APPLY "VALUE-CHANGED":U TO brw.
      APPLY "ENTRY":U TO brw IN FRAME DEFAULT-FRAME.


      calendr:ENABLED = FALSE.
      ENABLE brw btnCancelTable btnSaveTable btnModifyItem WITH FRAME {&FRAME-NAME}.
      DISABLE cmbVeh btnAddTable btnDeleteTable btnModifyTable WITH FRAME {&FRAME-NAME}.

      MESSAGE "Query compleated in " + STRING(TIME - tim,"HH:MM:SS") + "." VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnBSSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnBSSave C-Win
ON CHOOSE OF btnBSSave IN FRAME DEFAULT-FRAME /* BS Save */
DO:
    DEFINE VARIABLE tempDate AS DATE        NO-UNDO.
    tempDate = calendr:VALUE.
    
    IF NOT AVAILABLE tt-ldunld THEN
    DO:
        MESSAGE "Select records first" VIEW-AS ALERT-BOX WARNING BUTTONS OK.
        RETURN.
    END.

    FIND FIRST BSSave WHERE BSSave.datez = tempDate AND BSSave.vehNo = cmbVeh NO-ERROR. 
    IF AVAILABLE BSSave THEN
    DO:
        MESSAGE "BS Already Saved !" VIEW-AS ALERT-BOX WARNING BUTTONS OK.
        RETURN.
    END.
    RELEASE BSSave.

    MESSAGE "Confirm to" SKIP
            "1. Save the Table " SKIP
            "2. Set the Balance Stock and" SKIP
            "3. Save Today as the Last working date for selected vehicle?" SKIP
            "   You cannot change this data again."
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE yn AS LOGICAL.
    
    IF yn THEN
    DO:
        RUN saveTable.
    
        FOR EACH tt-ldunld.
            FIND FIRST itms WHERE itms.itmID = tt-ldunld.itmID EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE itms THEN 
            DO:
                DEFINE VARIABLE stock AS INTEGER     NO-UNDO.
                DEFINE VARIABLE loading AS INTEGER     NO-UNDO.
                DEFINE VARIABLE newStock AS INTEGER     NO-UNDO.

                stock = (itms.stockC * itms.unitsPerCase) + itms.stockP.
                loading = (tt-ldunld.LDC * itms.unitsPerCase) + tt-ldunld.LDP.
                newStock = stock - loading.

                    itms.stockP = newStock MODULO itms.unitsPerCase.
                    itms.stockC = TRUNCATE(newStock / itms.unitsPerCase , 0).
/*                     itms.stockP = ( itms.unitsPerCase - ((tt-ldunld.LDP - itms.stockP ) MODULO itms.unitsPerCase ) ) + tt-ldunld.GRRD */
/*                     itms.stockC = itms.stockC - (TRUNCATE((tt-ldunld.LDP - itms.stockP ) / itms.unitsPerCase , 0) + 1)                */
/*                     itms.stockP = ( itms.stockP + tt-ldunld.GRRD ) - tt-ldunld.LDP */
/*                     itms.stockC = itms.stockC + tt-ldunld.GRRD                     */
            END.
            RELEASE itms.
        END.
    
        FIND FIRST paramtrs WHERE paramtrs.name = "lastWorkingDay" EXCLUSIVE-LOCK NO-ERROR.
        paramtrs.val = STRING(tempDate).
        RELEASE paramtrs.
    
        CREATE BSSave.
        BSSave.datez = tempDate.
        BSSave.vehNo = cmbVeh.
        BSSave.BSCalculated = YES.
    
        IF NOT ERROR-STATUS:ERROR THEN
        DO:
            RUN ttBind.
            RUN LastWrkDate.
            MESSAGE "BS successfully calculated." SKIP
                "Your Last working date is " +  STRING(tempDate)
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
        END.
        DISABLE btnSaveTable WITH FRAM {&FRAME-NAME}.
    END.
            
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCancel C-Win
ON CHOOSE OF btnCancel IN FRAME DEFAULT-FRAME /* Cancel Item */
DO:
  MESSAGE "Conferm to cancel?" SKIP
      "Unsaved changes will be lost."
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE yn AS LOGICAL.

  IF yn = NO THEN
  DO:
      RETURN.
  END.

  OPEN QUERY brw FOR EACH tt-ldunld by tt-ldunld.SortID.
        APPLY "VALUE-CHANGED":U TO brw IN FRAME DEFAULT-FRAME.

  DISABLE  cmbName btnCancel btnSave cmbName filLDC filLDP filULC filULP WITH FRAME {&FRAME-NAME}.
  ENABLE btnDeleteTable btnBSSave brw btnSaveTable btnCancelTable   btnModifyItem WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCancelTable
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCancelTable C-Win
ON CHOOSE OF btnCancelTable IN FRAME DEFAULT-FRAME /* Cancel Table */
DO:
    MESSAGE "Conferm to cancel?" SKIP
      "Unsaved changes will be lost."
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE yn AS LOGICAL.
    
    IF yn THEN
    DO:
        cmbVeh = 0.
        calendr:VALUE = TODAY.
        filBillP          = 0.
        filBSC            = 0.
        filBSP            = 0.
        filCasePrice      = 0.
        filExcessShortP   = 0.
        filKg             = 0.
        filLDC            = 0.
        filLDP            = 0.
        filLorriesP       = 0.
        filPerCase        = 0.
        filRDC            = 0.
        filRDP            = 0.
        filRecipt#        = 0.
        filStockP         = 0.
        filTolLDC         = 0.
        filTolLDP         = 0.
        filTolRDP         = 0.
        filULC            = 0.
        filULP            = 0.
        filUnitPrice      = 0.
        cmbName           = "0".
        
        EMPTY TEMP-TABLE tt-ldunld.
        OPEN QUERY brw FOR EACH tt-ldunld by tt-ldunld.SortID.
        
        calendr:ENABLED = TRUE.
        DISABLE btnRfresh brw btnModifyItem btnCancelTable btnSaveTable WITH FRAME {&FRAME-NAME}.
        ENABLE cmbVeh btnAddTable btnDeleteTable btnModifyTable WITH FRAME {&FRAME-NAME}.
        DISPLAY cmbVeh
            filBillP        
            filBSC         
            filBSP         
            filCasePrice   
            filExcessShortP
            filKg          
            filLDC         
            filLDP         
            filLorriesP    
            filPerCase     
            filRDC         
            filRDP         
            filRecipt#     
            filStockP      
            filTolLDC      
            filTolLDP      
            filTolRDP      
            filULC         
            filULP         
            filUnitPrice   
            cmbName        
            WITH FRAME DEFAULT-FRAME.
        RUN ttBind.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDeleteTable
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDeleteTable C-Win
ON CHOOSE OF btnDeleteTable IN FRAME DEFAULT-FRAME /* Print Table */
DO:
  IF cmbVeh = 0 THEN
    DO:
        MESSAGE "Select Vehical first." VIEW-AS ALERT-BOX WARNING BUTTONS OK.
        RETURN.
    END.
    IF calendr:VALUE > TODAY THEN
    DO:
        MESSAGE "Date cannot be a future date." VIEW-AS ALERT-BOX WARNING BUTTONS OK.
        RETURN.
    END.

    IF NOT AVAILABLE tt-ldunld THEN
    DO:
        MESSAGE "Select data first." VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN.
    END.


    MESSAGE "Conferm to print Day Sale Report?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE yn AS LOGICAL.
    
    IF yn = YES THEN
    DO:
        RUN printTextFast.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnModifyItem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnModifyItem C-Win
ON CHOOSE OF btnModifyItem IN FRAME DEFAULT-FRAME /* Modify Item */
DO:
    DEFINE VARIABLE tempDate AS DATE        NO-UNDO.
    tempDate = calendr:VALUE.

    FIND FIRST BSSave WHERE BSSave.datez = tempDate AND BSSave.vehNo = cmbVeh NO-LOCK NO-ERROR.
    IF AVAILABLE BSSave AND BSSave.BSCalculated THEN
    DO:
        MESSAGE "BS has been saved and cannot be modified now." VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.
    ELSE
    DO:
        ENABLE  btnCancel btnSave cmbName filLDC filLDP filULC filULP WITH FRAME {&FRAME-NAME}.
        DISABLE btnDeleteTable btnBSSave brw cmbName btnSaveTable btnCancelTable btnModifyItem WITH FRAME {&FRAME-NAME}.
    END.
    RELEASE BSSave.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnModifyTable
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnModifyTable C-Win
ON CHOOSE OF btnModifyTable IN FRAME DEFAULT-FRAME /* View Table */
DO:
    IF cmbVeh = 0 THEN
    DO:
        MESSAGE "Select Vehical first." VIEW-AS ALERT-BOX WARNING BUTTONS OK.
        RETURN.
    END.
    IF calendr:VALUE > TODAY THEN
    DO:
        MESSAGE "Date cannot be a future date." VIEW-AS ALERT-BOX WARNING BUTTONS OK.
        RETURN.
    END.

    DEFINE VARIABLE tempDate AS DATE     NO-UNDO.
        tempDate = date(calendr:VALUE).
    
        EMPTY TEMP-TABLE tt-ldunld.
        FOR EACH lorryStock WHERE lorryStock.VehID = cmbVeh AND lorryStock.crDate = tempDate.
            FIND FIRST itms WHERE itms.itmID = lorryStock.itmID NO-LOCK NO-ERROR.
                CREATE tt-ldunld.
                tt-ldunld.ID      = lorryStock.ID      .
                tt-ldunld.vehNo   = lorryStock.VehID   .
                tt-ldunld.itmID   = lorryStock.itmID   .
                tt-ldunld.itmName = lorryStock.itmName .
                tt-ldunld.Weight  = lorryStock.Weight  .
                tt-ldunld.PriceP  = itms.unitPriceS    .
                tt-ldunld.BSC     = lorryStock.BSC     .
                tt-ldunld.BSP     = lorryStock.BSP     .
                tt-ldunld.GRRD     = lorryStock.GRRD     .
                tt-ldunld.GRST     = lorryStock.GRST     .
                tt-ldunld.LDC     = lorryStock.LDC     .
                tt-ldunld.LDP     = lorryStock.LDP     .
                tt-ldunld.ULC     = lorryStock.ULC     .
                tt-ldunld.ULP     = lorryStock.ULP     .
                tt-ldunld.RDC     = lorryStock.RDC     .
                tt-ldunld.RDP     = lorryStock.RDP     .
                tt-ldunld.TolP    = lorryStock.TolP    .
                tt-ldunld.TOlC    = lorryStock.TOlC    .
                tt-ldunld.BilP    = lorryStock.billedP .
                tt-ldunld.Excess  = lorryStock.Excess  .
                tt-ldunld.Short   = lorryStock.Short   .
                tt-ldunld.SortID  = itms.SortID        .
                tt-ldunld.PerCase = itms.unitsPerCase  .
            RELEASE itms.
        END.
    
        OPEN QUERY brw FOR EACH tt-ldunld BY tt-ldunld.SortID.

    APPLY "VALUE-CHANGED":U TO brw IN FRAME DEFAULT-FRAME.
    APPLY "ENTRY":U TO brw IN FRAME DEFAULT-FRAME.

    calendr:ENABLED = FALSE.
    ENABLE btnRfresh btnModifyItem brw btnCancelTable btnDeleteTable WITH FRAME {&FRAME-NAME}.
    DISABLE cmbVeh btnSaveTable btnAddTable btnModifyTable WITH FRAME {&FRAME-NAME}.

    IF NOT AVAILABLE tt-ldunld THEN
    DO:
        calendr:ENABLED = TRUE.
        ENABLE btnAddTable btnDeleteTable btnModifyTable cmbVeh WITH FRAME DEFAULT-FRAME.
        DISABLE btnRfresh brw btnCancelTable btnSaveTable btnModifyItem WITH FRAME DEFAULT-FRAME.
        MESSAGE "No records to show." VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRfresh
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRfresh C-Win
ON CHOOSE OF btnRfresh IN FRAME DEFAULT-FRAME /* Refresh */
DO:
  DEFINE VARIABLE tempDate AS DATE        NO-UNDO.
  DEFINE VARIABLE tim AS INTEGER     NO-UNDO.
  tempDate = calendr:VALUE.

    IF cmbVeh = 0 THEN
    DO:
      MESSAGE "Select Vehical first." VIEW-AS ALERT-BOX WARNING BUTTONS OK.
      RETURN.
    END.
    IF tempDate > TODAY THEN
    DO:
      MESSAGE "Date cannot be a future date." VIEW-AS ALERT-BOX WARNING BUTTONS OK.
      RETURN.
    END.
    
    FIND FIRST BSSave WHERE BSSave.datez = tempDate AND BSSave.vehNo = cmbVeh NO-LOCK NO-ERROR.
    IF AVAILABLE BSSave AND BSSave.BSCalculated THEN
    DO:
        MESSAGE "BS has been saved and cannot be modified now." VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.
    ELSE
    DO:
        tim = TIME.
/*         RUN refreshTable. */
        RUN refreshTableFast.
        
        APPLY "VALUE-CHANGED":U TO brw.
        APPLY "ENTRY":U TO brw IN FRAME DEFAULT-FRAME.
        
        isRefresh = TRUE.
        
        ENABLE btnSaveTable WITH FRAME {&FRAME-NAME}.
        MESSAGE "Query compleated in " + STRING(TIME - tim,"HH:MM:SS") + "." VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSave C-Win
ON CHOOSE OF btnSave IN FRAME DEFAULT-FRAME /* Save Item */
DO:
    FIND FIRST tt-ldunld WHERE tt-ldunld.itmID = INT(cmbName) NO-LOCK NO-ERROR.
    IF AVAILABLE tt-ldunld AND addModifyItem = "Add" THEN
    DO:
        MESSAGE "Item already exist." VIEW-AS ALERT-BOX WARNING BUTTONS OK.
        RETURN.
    END.

    IF (filLDP + (filLDC * filPerCase)) > filStockP THEN
    DO:
        MESSAGE "Loaded qty (LDP + LDC) is greater than Stock qty (Stock P)." VIEW-AS ALERT-BOX WARNING BUTTONS OK.
        RETURN.
    END.

    MESSAGE "Conferm to save the record?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE yn AS LOGICAL.
    IF yn = YES THEN
    DO:
            FIND FIRST tt-ldunld WHERE tt-ldunld.ID = filRecipt# EXCLUSIVE-LOCK.
            tt-ldunld.BSC     = filBSC.
            tt-ldunld.BSP     = filBSP.
            tt-ldunld.LDC     = filLDC.
            tt-ldunld.LDP     = filLDP.
            tt-ldunld.ULC     = filULC.
            tt-ldunld.ULP     = filULP.
            tt-ldunld.RDC     = filRDC.
            tt-ldunld.RDP     = filRDP.
    
            IF filTolRDP < filBillP THEN
            DO:
                tt-ldunld.Excess  = filExcessShortP.
                tt-ldunld.Short   = 0.
            END.
            IF filTolRDP > filBillP THEN
            DO:
                tt-ldunld.Short   = filExcessShortP.
                tt-ldunld.Excess  = 0.
            END.
            IF filTolRDP = filBillP THEN
            DO:
                tt-ldunld.Excess  = 0.
                tt-ldunld.Short   = 0.
            END.

            RELEASE tt-ldunld.
    
      OPEN QUERY brw FOR EACH tt-ldunld by tt-ldunld.SortID.
        APPLY "VALUE-CHANGED":U TO brw IN FRAME DEFAULT-FRAME.
    
      DISABLE btnCancel btnSave filLDC filLDP filULC filULP WITH FRAME {&FRAME-NAME}.
      ENABLE btnDeleteTable btnBSSave brw btnSaveTable btnCancelTable btnModifyItem WITH FRAME {&FRAME-NAME}.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSaveTable
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSaveTable C-Win
ON CHOOSE OF btnSaveTable IN FRAME DEFAULT-FRAME /* Save Table */
DO:
    MESSAGE "Confirm to Save the Table?" SKIP
            "You can change this data again."
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE yn AS LOGICAL.

    IF yn THEN
    DO:
        RUN saveTable.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cmbName
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbName C-Win
ON LEAVE OF cmbName IN FRAME DEFAULT-FRAME /* Name */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbName C-Win
ON VALUE-CHANGED OF cmbName IN FRAME DEFAULT-FRAME /* Name */
DO:
    ASSIGN {&SELF-NAME}.
    
/*     DEFINE VARIABLE BilP AS INTEGER     NO-UNDO.  */
/*     DEFINE VARIABLE Bil_Date AS DATE     NO-UNDO. */
/*     Bil_Date = calendr:VALUE.                     */

/*     FIND FIRST itms WHERE itms.itmID = INT(cmbName) NO-ERROR. */
/*     IF AVAILABLE itms THEN                                    */
/*     DO:                                                       */
/*         filKg        = unitWeightKG.                          */
/*         filUnitPrice = unitPriceS.                            */
/*         filCasePrice = casePriceS.                            */
/*         filPerCase   = unitsPerCase.                          */
/*         filStockP    = stockP + (stockC * filPerCase).        */
/*     END.                                                      */
/*     ELSE IF NOT AVAILABLE itms THEN                           */
/*     DO:                                                       */
/*         filKg        = 0.                                     */
/*         filUnitPrice = 0.                                     */
/*         filCasePrice = 0.                                     */
/*         filPerCase   = 0.                                     */
/*         filStockP    = 0.                                     */
/*         filBSP       = 0.                                     */
/*         filBSC       = 0.                                     */
/*         filBillP     = 0.                                     */
/*     END.                                                      */
/*     RELEASE itms.                                             */
    
/*     FOR EACH bills WHERE bilDate = Bil_Date .                                              */
/*         FOR EACH recipts WHERE item# = INT({&SELF-NAME}) AND recipts.bill# = bills.bill# . */
/*             BilP = BilP + (pieses + (cases * filPerCase)).                                 */
/*         END.                                                                               */
/*     END.                                                                                   */

    
/*     filBillP     = BilP. */
/*     RUN autoCal. */
    
/*     DISPLAY filLorriesP filPerCase filStockP filKg filUnitPrice filCasePrice filBSP filBSC filBillP WITH FRAME {&FRAME-NAME}. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cmbVeh
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbVeh C-Win
ON LEAVE OF cmbVeh IN FRAME DEFAULT-FRAME /* Vehical */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbVeh C-Win
ON VALUE-CHANGED OF cmbVeh IN FRAME DEFAULT-FRAME /* Vehical */
DO:
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame C-Win OCX.Change
PROCEDURE CtrlFrame.DTPicker.Change .
IF cmbVeh = 0 THEN
    DO:
        MESSAGE "Select Vehical First." VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN.
    END.

/*   RUN QueryLDUNLD. */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME filBillP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filBillP C-Win
ON LEAVE OF filBillP IN FRAME DEFAULT-FRAME /* Billed P */
DO:
    ASSIGN {&SELF-NAME}.
/*     RUN calAmount. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filBillP C-Win
ON VALUE-CHANGED OF filBillP IN FRAME DEFAULT-FRAME /* Billed P */
DO:
  
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME filBSC
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filBSC C-Win
ON LEAVE OF filBSC IN FRAME DEFAULT-FRAME /* BSC */
DO:
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME filBSP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filBSP C-Win
ON LEAVE OF filBSP IN FRAME DEFAULT-FRAME /* BSP */
DO:
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME filCasePrice
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filCasePrice C-Win
ON LEAVE OF filCasePrice IN FRAME DEFAULT-FRAME /* C Price */
DO:
  
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME filExcessShortP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filExcessShortP C-Win
ON LEAVE OF filExcessShortP IN FRAME DEFAULT-FRAME /* Variance */
DO:
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filExcessShortP C-Win
ON LEFT-MOUSE-CLICK OF filExcessShortP IN FRAME DEFAULT-FRAME /* Variance */
DO:
  MESSAGE {&SELF-NAME}
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME filGRRD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filGRRD C-Win
ON LEAVE OF filGRRD IN FRAME DEFAULT-FRAME /* GRP RD */
DO:
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filGRRD C-Win
ON VALUE-CHANGED OF filGRRD IN FRAME DEFAULT-FRAME /* GRP RD */
DO:
  ASSIGN {&SELF-NAME}.
  RUN autoCal.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME filGRST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filGRST C-Win
ON LEAVE OF filGRST IN FRAME DEFAULT-FRAME /* GRP Stock */
DO:
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filGRST C-Win
ON VALUE-CHANGED OF filGRST IN FRAME DEFAULT-FRAME /* GRP Stock */
DO:
  ASSIGN {&SELF-NAME}.
  RUN autoCal.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME filKg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filKg C-Win
ON LEAVE OF filKg IN FRAME DEFAULT-FRAME /* Weight */
DO:
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME filLDC
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filLDC C-Win
ON LEAVE OF filLDC IN FRAME DEFAULT-FRAME /* LDC */
DO:
    ASSIGN {&SELF-NAME}.
  RUN autoCal.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filLDC C-Win
ON VALUE-CHANGED OF filLDC IN FRAME DEFAULT-FRAME /* LDC */
DO:
  ASSIGN {&SELF-NAME}.
  RUN checkAvailability.
  RUN autoCal.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME filLDP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filLDP C-Win
ON LEAVE OF filLDP IN FRAME DEFAULT-FRAME /* LDP */
DO:
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filLDP C-Win
ON VALUE-CHANGED OF filLDP IN FRAME DEFAULT-FRAME /* LDP */
DO:
  ASSIGN {&SELF-NAME}.
  RUN checkAvailability.
  RUN autoCal.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME filLorriesP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filLorriesP C-Win
ON LEAVE OF filLorriesP IN FRAME DEFAULT-FRAME /* Lorries P */
DO:
    ASSIGN {&SELF-NAME}.
    RUN calAmount.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME filPerCase
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filPerCase C-Win
ON LEAVE OF filPerCase IN FRAME DEFAULT-FRAME /* Per C */
DO:
  
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME filRDC
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filRDC C-Win
ON LEAVE OF filRDC IN FRAME DEFAULT-FRAME /* RDC */
DO:
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME filRDP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filRDP C-Win
ON LEAVE OF filRDP IN FRAME DEFAULT-FRAME /* RDP */
DO:
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME filRecipt#
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filRecipt# C-Win
ON LEAVE OF filRecipt# IN FRAME DEFAULT-FRAME /* ID */
DO:
  
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME filStockP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filStockP C-Win
ON LEAVE OF filStockP IN FRAME DEFAULT-FRAME /* Stock P */
DO:
    ASSIGN {&SELF-NAME}.
    RUN calAmount.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filStockP C-Win
ON VALUE-CHANGED OF filStockP IN FRAME DEFAULT-FRAME /* Stock P */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME filTolLDC
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filTolLDC C-Win
ON LEAVE OF filTolLDC IN FRAME DEFAULT-FRAME /* Tol LDC */
DO:
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME filTolLDP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filTolLDP C-Win
ON LEAVE OF filTolLDP IN FRAME DEFAULT-FRAME /* Tol LDP */
DO:
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME filTolRDP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filTolRDP C-Win
ON LEAVE OF filTolRDP IN FRAME DEFAULT-FRAME /* Tol RDP */
DO:
    ASSIGN {&SELF-NAME}.
/*     RUN calAmount. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filTolRDP C-Win
ON VALUE-CHANGED OF filTolRDP IN FRAME DEFAULT-FRAME /* Tol RDP */
DO:
   ASSIGN {&SELF-NAME}.
/*     RUN calAmount. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME filULC
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filULC C-Win
ON LEAVE OF filULC IN FRAME DEFAULT-FRAME /* ULC */
DO:
    ASSIGN {&SELF-NAME}.
    RUN checkAvailability.
  RUN autoCal.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filULC C-Win
ON VALUE-CHANGED OF filULC IN FRAME DEFAULT-FRAME /* ULC */
DO:
  ASSIGN {&SELF-NAME}.
  RUN autoCal.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME filULP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filULP C-Win
ON LEAVE OF filULP IN FRAME DEFAULT-FRAME /* ULP */
DO:
    ASSIGN {&SELF-NAME}.
    RUN checkAvailability.
  RUN autoCal.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filULP C-Win
ON VALUE-CHANGED OF filULP IN FRAME DEFAULT-FRAME /* ULP */
DO:
  ASSIGN {&SELF-NAME}.
  RUN autoCal.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME filUnitPrice
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filUnitPrice C-Win
ON LEAVE OF filUnitPrice IN FRAME DEFAULT-FRAME /* P Price */
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

  calendr = chCtrlFrame:DTPicker.
  calendr:ENABLED = TRUE.
  calendr:VALUE = TODAY - 1.

  RUN vehLoader.
  RUN itemLoader.
  RUN LastWrkDate.
  RUN ttBind.
  
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE autoCal C-Win 
PROCEDURE autoCal :
DEFINE VARIABLE tolRDP AS INTEGER     NO-UNDO.
filTolLDP = filGRRD + filBSP + filLDP.
filTolLDC = filBSC + filLDC.

tolRDP = (filTolLDP + (filTolLDC * filPerCase)) - (filULP + (filULC * filPerCase)).

filTolRDP = tolRDP.

filRDP = filTolRDP MODULO filPerCase.

filRDC = ROUND((filTolRDP - filRDP) / filPerCase,0).

RUN ExcessShort.

DISPLAY filTolLDP filTolLDC filRDP filRDC filTolRDP WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE checkAvailability C-Win 
PROCEDURE checkAvailability :
DEFINE VARIABLE tempQty AS INTEGER     NO-UNDO.

IF filStockP < (filLDP + (filLDC * filPerCase))  THEN
DO:
    MESSAGE "Loading Value exceeded the Stock amount !" VIEW-AS ALERT-BOX WARNING BUTTONS OK.
    filLDP = 0.
    filLDC = 0.
    filULP = 0.
    filULC = 0.
    DISPLAY filLDP
            filLDC
            filULP
            filULC WITH FRAME DEFAULT-FRAME.
    RETURN.
END.

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

OCXFile = SEARCH( "DaySale.wrx":U ).
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
ELSE MESSAGE "DaySale.wrx":U SKIP(1)
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
  DISPLAY cmbVeh filLDC filLDP filULC filULP filLastWorkingDay cmbName filStockP 
          filLorriesP filBSC filBSP filTolRDP filBillP filRecipt# filTolLDC 
          filTolLDP filKg filUnitPrice filExcessShortP filRDC filRDP 
          filCasePrice filPerCase filGRRD filGRST 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE cmbVeh btnModifyTable btnDeleteTable filExcessShortP btnBSSave 
         btnAddTable RECT-1 RECT-2 RECT-3 RECT-4 RECT-5 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExcessShort C-Win 
PROCEDURE ExcessShort :
filExcessShortP:LABEL IN FRAME {&FRAME-NAME} = "Varience".
filExcessShortP = 0.
filExcessShortP :BGCOLOR IN FRAME {&FRAME-NAME}= 15 .

IF filTolRDP = filBillP THEN
DO:
    filExcessShortP = 0.
END.
ELSE IF filTolRDP < filBillP THEN
DO:
    filExcessShortP:LABEL = "Excess P".
    filExcessShortP:BGCOLOR IN FRAME {&FRAME-NAME} = 2 .
    filExcessShortP = filBillP - filTolRDP.
END.
ELSE IF filTolRDP > filBillP THEN
DO:
    filExcessShortP:LABEL = "Short P".
    filExcessShortP:BGCOLOR IN FRAME {&FRAME-NAME} = 12 .
    filExcessShortP = filTolRDP - filBillP.
END.

DISPLAY filExcessShortP WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE itemLoader C-Win 
PROCEDURE itemLoader :
FOR EACH itms BY itms.itmName.
    cmbName:ADD-LAST(itms.itmName + " - " + STRING(unitWeightKG,">>9.999") + " kg",STRING(itmID)) IN FRAME DEFAULT-FRAME.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LastWrkDate C-Win 
PROCEDURE LastWrkDate :
FIND FIRST paramtrs WHERE paramtrs.NAME = "lastWorkingDay".
    filLastWorkingDay = DATE(paramtrs.val).
RELEASE paramtrs.

DISPLAY filLastWorkingDay WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE populateTable C-Win 
PROCEDURE populateTable :
DEFINE VARIABLE cnt AS INTEGER     NO-UNDO.
DEFINE VARIABLE tempBSC AS INTEGER     NO-UNDO.
DEFINE VARIABLE tempBSP AS INTEGER     NO-UNDO.
DEFINE VARIABLE Bil_Date AS DATE     NO-UNDO.
DEFINE VARIABLE tempDate AS DATE     NO-UNDO INIT 01/01/2013.
DEFINE VARIABLE lastBSDate AS DATE     NO-UNDO.

Bil_Date = calendr:VALUE.
lastBSDate = Bil_Date - 1.
            
        FOR EACH lorryStock.
            IF lorryStock.crDate > tempDate AND lorryStock.crDate < Bil_Date THEN
                lastBSDate = lorryStock.crDate.
        END.

        FIND FIRST paramtrs WHERE paramtrs.NAME = "lastLorryStockID".
        IF AVAILABLE paramtrs THEN
        DO:
            tempIDtt-ldunld = INT(paramtrs.val).

            EMPTY TEMP-TABLE tt-ldunld. 

            FOR EACH itms BY itms.SortID.
                cnt = 0.   

                FOR EACH bills WHERE bilDate = Bil_Date AND bills.vehNo = cmbVeh.
                    FOR EACH recipts WHERE recipts.item# = itms.itmID AND recipts.bill# = bills.bill#.
                        IF AVAILABLE recipts THEN
                        DO:
                            cnt = cnt + recipts.pieses + (recipts.cases * itms.unitsPerCase).
                        END.
                    END.
                END.

                CREATE tt-ldunld.
                tt-ldunld.ID      = tempIDtt-ldunld + 1.
                tt-ldunld.vehNo   = cmbVeh.
                tt-ldunld.itmID   = itms.itmID.
                tt-ldunld.itmName = itms.itmName.
                tt-ldunld.Weight  = itms.unitWeightKG.
                tt-ldunld.PriceP  = itms.unitPriceS.
                tt-ldunld.PerCase = itms.unitsPerCase.
                tt-ldunld.SortID  = itms.SortID.


                FIND FIRST lorryStock WHERE lorryStock.itmID = itms.itmID AND lorryStock.VehID = cmbVeh AND lorryStock.crDate = lastBSDate NO-LOCK NO-ERROR.
                IF AVAILABLE lorryStock THEN
                DO:
                    tt-ldunld.BSC     = lorryStock.BSC.
                    tt-ldunld.BSP     = lorryStock.BSp.
                END.
                ELSE
                DO:
                    tt-ldunld.BSC     = 0.
                    tt-ldunld.BSP     = 0.
                END.

                RELEASE lorryStock.

                tt-ldunld.LDC     = 0.
                tt-ldunld.LDP     = 0.
                tt-ldunld.ULC     = 0.
                tt-ldunld.ULP     = 0.
                tt-ldunld.RDC     = tt-ldunld.BSC.
                tt-ldunld.RDP     = tt-ldunld.BSP.
                tt-ldunld.BilP    = cnt.
            
                IF (tt-ldunld.BSP + (tt-ldunld.BSC * itms.unitsPerCase)) < cnt THEN
                    tt-ldunld.Excess  = cnt - (tt-ldunld.BSP + (tt-ldunld.BSC * itms.unitsPerCase)).
                ELSE
                DO:
                    tt-ldunld.Short   = (tt-ldunld.BSP + (tt-ldunld.BSC * itms.unitsPerCase)) - cnt.
                    tt-ldunld.Amount  = itms.unitPriceS * cnt.
                END.

                tempIDtt-ldunld = tempIDtt-ldunld + 1.

                filLorriesP  = tt-ldunld.BSP + (tt-ldunld.BSC * itms.unitsPerCase).
            
            END.
        END.
      
        OPEN QUERY brw FOR EACH tt-ldunld by tt-ldunld.SortID.
        APPLY "VALUE-CHANGED":U TO brw IN FRAME DEFAULT-FRAME.
    
      DISPLAY filLorriesP WITH FRAME DEFAULT-FRAME.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE populateTableFast C-Win 
PROCEDURE populateTableFast :
DEFINE VARIABLE cnt AS INTEGER     NO-UNDO.
DEFINE VARIABLE cntGRRD AS INTEGER     NO-UNDO.
DEFINE VARIABLE cntGRST AS INTEGER     NO-UNDO.
DEFINE VARIABLE tempBSC AS INTEGER     NO-UNDO.
DEFINE VARIABLE tempBSP AS INTEGER     NO-UNDO.
DEFINE VARIABLE Bil_Date AS DATE     NO-UNDO.
DEFINE VARIABLE tempDate AS DATE     NO-UNDO INIT 01/01/2013.
DEFINE VARIABLE lastBSDate AS DATE     NO-UNDO.

    Bil_Date = calendr:VALUE.
        
    FOR EACH tt-lorryStock WHERE tt-lorryStock.VehID = cmbVeh AND tt-lorryStock.crDate < Bil_Date.
        ACCUMULATE tt-lorryStock.crDate (MAX).
    END.

    lastBSDate = ACCUM MAX tt-lorryStock.crDate.

    FIND FIRST paramtrs WHERE paramtrs.NAME = "lastLorryStockID".
    IF AVAILABLE paramtrs THEN
    DO:
        tempIDtt-ldunld = INT(paramtrs.val).
    
        EMPTY TEMP-TABLE tt-ldunld. 
    
        FOR EACH tt-itms BY tt-itms.SortID.
            cnt = 0.   
            cntGRRD = 0.
            cntGRST = 0.
    
            FOR EACH tt-bills WHERE bilDate = Bil_Date AND tt-bills.vehNo = cmbVeh.
                FOR EACH tt-recipts WHERE tt-recipts.item# = tt-itms.itmID AND tt-recipts.bill# = tt-bills.bill#.
                    IF AVAILABLE tt-recipts THEN
                    DO:
                        cnt = cnt + tt-recipts.pieses + (tt-recipts.cases * tt-itms.unitsPerCase).
                        cntGRRD = cntGRRD + tt-recipts.GRRD.
                        cntGRST = cntGRST + tt-recipts.GRST.
                    END.
                END.
            END.
    
            CREATE tt-ldunld.
            tt-ldunld.ID      = tempIDtt-ldunld + 1.
            tt-ldunld.vehNo   = cmbVeh.
            tt-ldunld.itmID   = tt-itms.itmID.
            tt-ldunld.itmName = tt-itms.itmName.
            tt-ldunld.Weight  = tt-itms.unitWeightKG.
            tt-ldunld.PriceP  = tt-itms.unitPriceS.
            tt-ldunld.PerCase = tt-itms.unitsPerCase.
            tt-ldunld.SortID  = tt-itms.SortID.
    
            FIND FIRST tt-lorryStock WHERE tt-lorryStock.itmID = tt-itms.itmID AND tt-lorryStock.VehID = cmbVeh AND tt-lorryStock.crDate = lastBSDate NO-LOCK NO-ERROR.
                IF AVAILABLE tt-lorryStock THEN
                DO:
                    tt-ldunld.BSC     = tt-lorryStock.ULC.
                    tt-ldunld.BSP     = tt-lorryStock.ULP.
                END.
                ELSE
                DO:
                    tt-ldunld.BSC     = 0.
                    tt-ldunld.BSP     = 0.
                END.
            RELEASE lorryStock.

            tt-ldunld.GRRD    = cntGRRD.
            tt-ldunld.GRST    = cntGRST.
    
            tt-ldunld.LDC     = 0.
            tt-ldunld.LDP     = 0.
            tt-ldunld.ULC     = tt-ldunld.BSC.
            tt-ldunld.ULP     = tt-ldunld.BSP + cntGRRD.
            tt-ldunld.RDC     = 0.
            tt-ldunld.RDP     = 0.
            tt-ldunld.BilP    = cnt.
            tt-ldunld.Excess  = cnt.
            tt-ldunld.Short   = 0.
            tt-ldunld.Amount  = tt-itms.unitPriceS * cnt.
        
            tempIDtt-ldunld = tempIDtt-ldunld + 1.
    
            filLorriesP  = cntGRRD + tt-ldunld.BSP + (tt-ldunld.BSC * tt-itms.unitsPerCase).
        
        END.
    END.
    
    OPEN QUERY brw FOR EACH tt-ldunld by tt-ldunld.SortID.
    APPLY "VALUE-CHANGED":U TO brw IN FRAME DEFAULT-FRAME.
    
    DISPLAY filLorriesP WITH FRAME DEFAULT-FRAME.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE printHtml C-Win 
PROCEDURE printHtml :
OUTPUT TO VALUE("E:\ICS\bin\Report.html").

    PUT UNFORMAT "<html>".
    PUT UNFORMAT "<head>".
    PUT UNFORMAT "<script></script>"   .
    PUT UNFORMAT "</head>".
    PUT UNFORMAT "<body >".
    PUT UNFORMAT "<divclass=Section1>".
    PUT UNFORMAT "<div style='border:1px solid #999999;'></div>".
    PUT UNFORMAT "<table border='0' padding='1' margin='1' style='border:1px solid #cccccc;'>".
    PUT UNFORMAT "<tr><th>No</th><th>Item</th><th>Weight(kg)</th><th>BSC</th><th>BSP</th><th>LDC</th><th>LDP</th><th>ULC</th><th>ULP</th><th>RDC</th><th>RDP</th><th>TolP</th><th>TOlC</th><th>Excess</th><th>Short</th></tr>".

        FOR EACH tt-ldunld NO-LOCK. 
            PUT UNFORMAT "<tr><td>" STRING(tt-ldunld.ID)       + "</td>".
            PUT UNFORMAT      "<td>" string(   itmName) + "</td> ".
            PUT UNFORMAT      "<td>" string(   Weight,">>9.999" ) + "</td> ".
            PUT UNFORMAT      "<td>" string(   BSC    ) + "</td> ".
            PUT UNFORMAT      "<td>" string(   BSP    ) + "</td> ".
            PUT UNFORMAT      "<td>" string(   LDC    ) + "</td> ".
            PUT UNFORMAT      "<td>" string(   LDP    ) + "</td> ".
            PUT UNFORMAT      "<td>" string(   ULC    ) + "</td> ".
            PUT UNFORMAT      "<td>" string(   ULP    ) + "</td> ".
            PUT UNFORMAT      "<td>" string(   RDC    ) + "</td> ".
            PUT UNFORMAT      "<td>" string(   RDP    ) + "</td> ".
            PUT UNFORMAT      "<td>" string(   TolP   ) + "</td> ".
            PUT UNFORMAT      "<td>" string(   TOlC   ) + "</td> ".
            PUT UNFORMAT      "<td>" string(   Excess ) + "</td> ".
            PUT UNFORMAT      "<td>" string(   Short  ) + "</td> ".
            PUT UNFORMAT  "</tr>". 
        END.

    PUT UNFORMAT "</table>".

    PUT UNFORMAT "</div>".
    PUT UNFORMAT "</body>".
    PUT UNFORMAT "</html>".
    PUT UNFORMAT "".

    OUTPUT CLOSE.

    DOS SILENT START chrome VALUE("E:\ICS\bin\Report.html").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE printText C-Win 
PROCEDURE printText :
DEFINE VARIABLE tempDate AS DATE        NO-UNDO.
DEFINE VARIABLE tempVeh AS CHARACTER   NO-UNDO.
DEFINE VARIABLE tempUser AS CHARACTER   NO-UNDO.

DEFINE VARIABLE tempBilledC AS INTEGER     NO-UNDO.
DEFINE VARIABLE tempBilledP AS INTEGER     NO-UNDO.
DEFINE VARIABLE tempPerCase AS INTEGER     NO-UNDO.

tempDate = calendr:VALUE.
FIND FIRST vehical WHERE vehical.ID = cmbVeh.

tempVeh  = STRING(vehical.veh#).
tempUser = session_User.

OUTPUT TO VALUE("E:\ICS\bin\print\DaySale.txt").
          PUT UNFORMAT "User :|". 
          PUT UNFORMAT tempUser .
          PUT UNFORMAT "       Date : ".
          PUT UNFORMAT STRING(tempDate) + "  |".
          PUT UNFORMAT "Vehical :|".
          PUT UNFORMAT tempVeh + "  " + descrip SKIP. 
          PUT UNFORMAT "No|Item|Weight|Price|Units|BSC|BSP|LDC|LDP|ULC|ULP|RDC|RDP|BilC|BilP|Excess|Short" SKIP. 

      FOR EACH tt-ldunld NO-LOCK.
          tempBilledP = 0. 
          tempBilledC = 0.

          FOR EACH tt-bills WHERE tt-bills.bilDate = tempDate.
              FOR EACH tt-recipts WHERE tt-recipts.bill# = tt-bills.bill# AND tt-recipts.item# = tt-ldunld.itmID .
                tempBilledP = tempBilledP + tt-recipts.pieses . 
                tempBilledC = tempBilledC + tt-recipts.cases.
              END.
          END.

          IF ( tt-ldunld.BSC + tt-ldunld.BSP + tt-ldunld.LDC + tt-ldunld.LDP + tt-ldunld.ULC + tt-ldunld.ULP + tt-ldunld.RDC +
              tt-ldunld.RDP + tempBilledC + tempBilledP + tt-ldunld.Excess + tt-ldunld.SHORT ) > 0 THEN
          DO:
              FIND FIRST tt-itms WHERE tt-itms.itmID = tt-ldunld.itmID NO-LOCK NO-ERROR.
                tempPerCase = tt-itms.unitsPerCase.
              RELEASE itms.
    
              PUT UNFORMAT STRING( tt-ldunld.ID) + "|".
              PUT UNFORMAT string( tt-ldunld.itmName) + "|"  .
              PUT UNFORMAT string( tt-ldunld.Weight,">>9.999") + "|" . 
              PUT UNFORMAT string( tt-ldunld.PriceP,">,>>9.99") + "|"   . 
              PUT UNFORMAT string( tempPerCase,">>9") + "|"   . 
              PUT UNFORMAT string( tt-ldunld.BSC   ) + "|"   . 
              PUT UNFORMAT string( tt-ldunld.BSP   ) + "|"   . 
              PUT UNFORMAT string( tt-ldunld.LDC   ) + "|"   . 
              PUT UNFORMAT string( tt-ldunld.LDP   ) + "|"   . 
              PUT UNFORMAT string( tt-ldunld.ULC   ) + "|"   . 
              PUT UNFORMAT string( tt-ldunld.ULP   ) + "|"   . 
              PUT UNFORMAT string( tt-ldunld.RDC   ) + "|"   . 
              PUT UNFORMAT string( tt-ldunld.RDP   ) + "|"   . 
              PUT UNFORMAT string( tempBilledC  ) + "|"   . 
              PUT UNFORMAT string( tempBilledP  ) + "|"   . 
              PUT UNFORMAT string( tt-ldunld.Excess) + "|"   . 
              PUT UNFORMAT string( tt-ldunld.Short )   SKIP. 
          END.
      END.

    DEFINE VARIABLE tonnage AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE tolVal AS DECIMAL     NO-UNDO.
    FOR EACH tt-itms.
        tonnage = tonnage + ((tt-itms.stockP + (tt-itms.stockC * tt-itms.unitsPerCase)) * tt-itms.unitWeightKG).
        tolVal  = tolVal + ((tt-itms.stockP + (tt-itms.stockC * tt-itms.unitsPerCase)) * tt-itms.unitPriceS).
    END.
      PUT UNFORMAT " " SKIP.
      PUT UNFORMAT SKIP "|Tonnage : " + STRING( tonnage,">>>,>>>,>>>,>>9.999") + " kg" SKIP. 
      PUT UNFORMAT "|Value   : Rs." + STRING( tolVal,">>>,>>>,>>>,>>9.99")  SKIP. 

  OUTPUT CLOSE.

  DOS SILENT START VALUE("E:\ICS\bin\print\DaySale.bat").
  DOS SILENT START excel VALUE("E:\ICS\bin\print\DaySale.xlsm").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE printTextFast C-Win 
PROCEDURE printTextFast :
DEFINE VARIABLE tempDate AS DATE        NO-UNDO.
DEFINE VARIABLE tempVeh AS CHARACTER   NO-UNDO.
DEFINE VARIABLE tempUser AS CHARACTER   NO-UNDO.
DEFINE VARIABLE tempNo AS INT   NO-UNDO.
DEFINE VARIABLE tempVal AS DECIMAL   NO-UNDO INIT 0.
DEFINE VARIABLE tempTon AS DECIMAL   NO-UNDO INIT 0.

tempDate = calendr:VALUE.
FIND FIRST vehical WHERE vehical.ID = cmbVeh.

tempVeh  = STRING(vehical.veh#).
tempUser = session_User.

OUTPUT TO VALUE("E:\ICS\bin\print\DaySale.txt").
          PUT UNFORMAT " Date : " + STRING(tempDate) + "    Vehical : " + tempVeh + "  " + descrip + "    By User : " + tempUser SKIP. 
          PUT UNFORMAT "No|Item|Weight|Price|Units|BSC|BSP|LDC|LDP|ULC|ULP|RDC|RDP|BilP|Excs|Shrt" SKIP. 

          FOR EACH itms BY itms.SortID.
          FIND FIRST tt-ldunld WHERE tt-ldunld.itmID = itms.itmID NO-LOCK NO-ERROR.
              IF AVAILABLE tt-ldunld AND (tt-ldunld.BSC   +
                  tt-ldunld.BSP   +
                  tt-ldunld.LDC   +
                  tt-ldunld.LDP   +
                  tt-ldunld.ULC   +
                  tt-ldunld.ULP   +
                  tt-ldunld.RDC   +
                  tt-ldunld.RDP   +
                  tt-ldunld.BilP
                  ) > 0 THEN
              DO:
                  tempNo = tempNo + 1.
                  PUT UNFORMAT STRING( tempNo) + "|".                   
                  PUT UNFORMAT string( tt-ldunld.itmName) + "|"  .            
                  PUT UNFORMAT string( tt-ldunld.Weight,">>9.999") + "|" .    
                  PUT UNFORMAT string( tt-ldunld.PriceP,">,>>9.99") + "|"   . 
                  PUT UNFORMAT string( tt-ldunld.PerCase,">>9") + "|"   .           
                  PUT UNFORMAT string( tt-ldunld.BSC   ) + "|"   .            
                  PUT UNFORMAT string( tt-ldunld.BSP   ) + "|"   .            
                  PUT UNFORMAT string( tt-ldunld.LDC   ) + "|"   .            
                  PUT UNFORMAT string( tt-ldunld.LDP   ) + "|"   .            
                  PUT UNFORMAT string( tt-ldunld.ULC   ) + "|"   .            
                  PUT UNFORMAT string( tt-ldunld.ULP   ) + "|"   .            
                  PUT UNFORMAT string( tt-ldunld.RDC   ) + "|"   .            
                  PUT UNFORMAT string( tt-ldunld.RDP   ) + "|"   .            
                  PUT UNFORMAT string( tt-ldunld.BilP  ) + "|"   .               
                  PUT UNFORMAT string( tt-ldunld.Excess) + "|"   .            
                  PUT UNFORMAT string( tt-ldunld.Short )   SKIP.   
                  tempVal = tempVal + (tt-ldunld.BilP * tt-ldunld.PriceP).
                  tempTon = tempTon + (tt-ldunld.BilP * tt-ldunld.Weight).
              END.
        END.

    DEFINE VARIABLE tonnage AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE tolVal AS DECIMAL     NO-UNDO.
    FOR EACH tt-itms.
        tonnage = tonnage + ((tt-itms.stockP + (tt-itms.stockC * tt-itms.unitsPerCase)) * tt-itms.unitWeightKG).
        tolVal  = tolVal + ((tt-itms.stockP + (tt-itms.stockC * tt-itms.unitsPerCase)) * tt-itms.unitPriceS).
    END.
      PUT UNFORMAT " " SKIP.
      PUT UNFORMAT "|Sales Value (Rs.)   : " + STRING( tempVal,">>>,>>>,>>>,>>9.99").
      PUT UNFORMAT "|Total Value (Rs.)   : " + STRING( tolVal,">>>,>>>,>>>,>>9.99") SKIP.
      PUT UNFORMAT "|Sales Tonnage (kg)  : " + STRING( tempTon,">>>,>>>,>>>,>>9.999").
      PUT UNFORMAT "|Total Tonnage (kg)  : " + STRING( tonnage,">>>,>>>,>>>,>>9.999") SKIP.

  OUTPUT CLOSE.

  DOS SILENT START VALUE("E:\ICS\bin\print\DaySale.bat").
  DOS SILENT START excel VALUE("E:\ICS\bin\print\DaySale.xlsm").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE QueryLDUNLD C-Win 
PROCEDURE QueryLDUNLD :
DEFINE VARIABLE tempDate AS DATE     NO-UNDO.
        tempDate = date(calendr:VALUE).
    
        EMPTY TEMP-TABLE tt-ldunld.
        FOR EACH lorryStock WHERE lorryStock.VehID = cmbVeh AND lorryStock.crDate = tempDate.
            FIND FIRST itms WHERE itms.itmID = lorryStock.itmID NO-LOCK NO-ERROR.
                CREATE tt-ldunld.
                tt-ldunld.ID      = lorryStock.ID      .
                tt-ldunld.vehNo   = lorryStock.VehID   .
                tt-ldunld.itmID   = lorryStock.itmID   .
                tt-ldunld.itmName = lorryStock.itmName .
                tt-ldunld.Weight  = lorryStock.Weight  .
                tt-ldunld.PriceP  = ics.itms.unitPriceS.
                tt-ldunld.BSC     = lorryStock.BSC     .
                tt-ldunld.BSP     = lorryStock.BSP     .
                tt-ldunld.LDC     = lorryStock.LDC     .
                tt-ldunld.LDP     = lorryStock.LDP     .
                tt-ldunld.ULC     = lorryStock.ULC     .
                tt-ldunld.ULP     = lorryStock.ULP     .
                tt-ldunld.RDC     = lorryStock.RDC     .
                tt-ldunld.RDP     = lorryStock.RDP     .
                tt-ldunld.TolP    = lorryStock.TolP    .
                tt-ldunld.TOlC    = lorryStock.TOlC    .
                tt-ldunld.BilP    = lorryStock.billedP .
                tt-ldunld.Excess  = lorryStock.Excess  .
                tt-ldunld.Short   = lorryStock.Short   .
                tt-ldunld.sortId  = lorryStock.SortID   .
            RELEASE itms.
        END.
    
        OPEN QUERY brw FOR EACH tt-ldunld by tt-ldunld.SortID.

        ENABLE brw btnCancelTable btnDeleteTable WITH FRAME {&FRAME-NAME}.
        DISABLE btnSaveTable btnAddTable btnModifyTable WITH FRAME {&FRAME-NAME}.
    
/*         IF NOT AVAILABLE tt-ldunld THEN                                              */
/*         DO:                                                                          */
/*             DISABLE brw btnCancelTable btnDeleteTable WITH FRAME DEFAULT-FRAME.      */
/*             ENABLE btnSaveTable btnAddTable btnModifyTable WITH FRAME DEFAULT-FRAME. */
/*             MESSAGE "No records to show." VIEW-AS ALERT-BOX INFO BUTTONS OK.         */
/*                                                                                      */
/*         END.                                                                         */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE refreshTable C-Win 
PROCEDURE refreshTable :
DEFINE VARIABLE cnt AS INTEGER     NO-UNDO.
DEFINE VARIABLE tempBSC AS INTEGER     NO-UNDO.
DEFINE VARIABLE tempBSP AS INTEGER     NO-UNDO.
DEFINE VARIABLE Bil_Date AS DATE     NO-UNDO.
DEFINE VARIABLE tempDate AS DATE     NO-UNDO INIT 01/01/2013.
DEFINE VARIABLE lastBSDate AS DATE     NO-UNDO.

Bil_Date = calendr:VALUE.
lastBSDate = Bil_Date - 1.
            
        FOR EACH lorryStock WHERE vehical.ID = cmbVeh.
            IF lorryStock.crDate > tempDate AND lorryStock.crDate < Bil_Date THEN lastBSDate = lorryStock.crDate.
        END.

        FOR EACH tt-ldunld.

            cnt = 0.

            FOR EACH bills WHERE bilDate = Bil_Date AND bills.vehNo = cmbVeh.
                FOR EACH recipts WHERE recipts.item# = tt-ldunld.itmID AND recipts.bill# = bills.bill#.
                    IF AVAILABLE recipts THEN
                    DO:
                        cnt = cnt + recipts.pieses + (recipts.cases * tt-ldunld.PerCase).
                    END.
                END.
            END.
            
            
            FIND FIRST lorryStock WHERE lorryStock.itmID = tt-ldunld.itmID AND lorryStock.VehID = cmbVeh AND lorryStock.crDate = lastBSDate NO-LOCK. 
                IF AVAILABLE lorryStock THEN
                DO:
                    tt-ldunld.BSC     = lorryStock.ULC.
                    tt-ldunld.BSP     = lorryStock.ULP.
                    tt-ldunld.RDC     = (lorryStock.ULC + tt-ldunld.LDC) - tt-ldunld.ULC.
                    tt-ldunld.RDP     = (lorryStock.ULP + tt-ldunld.LDP) - tt-ldunld.ULP.
                END.
                ELSE
                DO:
                    tt-ldunld.BSC     = 0.
                    tt-ldunld.BSP     = 0.
                END.    
            RELEASE lorryStock.

            tt-ldunld.BilP    = cnt.

            IF (tt-ldunld.BSP + (tt-ldunld.BSC * itms.unitsPerCase)) < cnt THEN                                                                                  
                tt-ldunld.Excess  = cnt - (tt-ldunld.BSP + (tt-ldunld.BSC * tt-ldunld.PerCase)).
            ELSE
            DO:
                tt-ldunld.Short   = (tt-ldunld.BSP + (tt-ldunld.BSC * tt-ldunld.PerCase)) - cnt.
                tt-ldunld.Amount  = tt-ldunld.PerCase * cnt.
            END.     

            filLorriesP  = tt-ldunld.BSP + (tt-ldunld.BSC * tt-ldunld.PerCase).

        END.
        
        OPEN QUERY brw FOR EACH tt-ldunld by tt-ldunld.SortID.
        APPLY "VALUE-CHANGED":U TO brw IN FRAME DEFAULT-FRAME.
    
        DISPLAY filLorriesP WITH FRAME DEFAULT-FRAME.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE refreshTableFast C-Win 
PROCEDURE refreshTableFast :
DEFINE VARIABLE cnt         AS INTEGER NO-UNDO.
DEFINE VARIABLE cntGRRD AS INTEGER     NO-UNDO.
DEFINE VARIABLE cntGRST AS INTEGER     NO-UNDO.
DEFINE VARIABLE tempBSC     AS INTEGER NO-UNDO.
DEFINE VARIABLE tempBSP     AS INTEGER NO-UNDO.
DEFINE VARIABLE Bil_Date    AS DATE    NO-UNDO.
DEFINE VARIABLE tempDate    AS DATE    NO-UNDO INIT 01/01/2013.
DEFINE VARIABLE lastBSDate  AS DATE    NO-UNDO.

Bil_Date = calendr:VALUE.
            
        FOR EACH tt-lorryStock WHERE tt-lorryStock.VehID = cmbVeh AND tt-lorryStock.crDate < Bil_Date.
            ACCUMULATE tt-lorryStock.crDate (MAX).
        END.

        lastBSDate = ACCUM MAX tt-lorryStock.crDate.

        FOR EACH tt-ldunld.

            cnt = 0.
            cntGRRD = 0.
            cntGRST = 0.

            FOR EACH tt-bills WHERE tt-bills.bilDate = Bil_Date AND tt-bills.vehNo = cmbVeh.
                FOR EACH tt-recipts WHERE tt-recipts.item# = tt-ldunld.itmID AND tt-recipts.bill# = tt-bills.bill#.
                    IF AVAILABLE tt-recipts THEN
                    DO:
                        cnt = cnt + tt-recipts.pieses + (tt-recipts.cases * tt-ldunld.PerCase).
                        cntGRRD = cntGRRD + tt-recipts.GRRD.
                        cntGRST = cntGRST + tt-recipts.GRST.
                    END.
                END.
            END.
            
            
            FIND FIRST tt-lorryStock WHERE tt-lorryStock.itmID = tt-ldunld.itmID AND tt-lorryStock.VehID = cmbVeh AND tt-lorryStock.crDate = lastBSDate NO-ERROR. 
                IF AVAILABLE tt-lorryStock THEN
                DO:
                    tt-ldunld.BSC     = tt-lorryStock.ULC.
                    tt-ldunld.BSP     = tt-lorryStock.ULP.
                    tt-ldunld.RDC     = (tt-lorryStock.ULC + tt-ldunld.LDC) - tt-ldunld.ULC.
                    tt-ldunld.RDP     = (tt-lorryStock.ULP + tt-ldunld.LDP) - tt-ldunld.ULP.
                END.
                ELSE
                DO:
                    tt-ldunld.BSC     = 0.
                    tt-ldunld.BSP     = 0.
                    tt-ldunld.RDC     = 0. 
                    tt-ldunld.RDP     = 0. 
                END.    
            RELEASE tt-lorryStock.

            tt-ldunld.GRRD    = cntGRRD.
            tt-ldunld.GRST    = cntGRST.

            tt-ldunld.BilP    = cnt.

            IF (tt-ldunld.RDP + (tt-ldunld.RDC * tt-ldunld.PerCase)) < cnt THEN                                                                                  
                tt-ldunld.Excess  = cnt - (tt-ldunld.RDP + (tt-ldunld.RDC * tt-ldunld.PerCase)).
            ELSE IF (tt-ldunld.RDP + (tt-ldunld.RDC * tt-ldunld.PerCase)) > cnt THEN
                tt-ldunld.Short   = (tt-ldunld.RDP + (tt-ldunld.RDC * tt-ldunld.PerCase)) - cnt.

            tt-ldunld.Amount  = tt-ldunld.PerCase * cnt.

            filLorriesP  = cntGRRD + tt-ldunld.BSP + (tt-ldunld.BSC * tt-ldunld.PerCase).

        END.
        
        OPEN QUERY brw FOR EACH tt-ldunld by tt-ldunld.SortID.
        APPLY "VALUE-CHANGED":U TO brw IN FRAME DEFAULT-FRAME.
    
        DISPLAY filLorriesP WITH FRAME DEFAULT-FRAME.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saveTable C-Win 
PROCEDURE saveTable :
DEFINE VARIABLE tempDate AS DATE NO-UNDO.
tempDate = calendr:VALUE .

    

    IF NOT isRefresh THEN
    DO:
        FOR EACH lorryStock WHERE lorryStock.CrDate = tempDate AND lorryStock.VehID = tt-ldunld.vehNo.
            DELETE lorryStock.
        END.
    
        FIND FIRST paramtrs WHERE paramtrs.NAME = "lastLorryStockID".
        tempID = INT(paramtrs.val).
    
            FOR EACH tt-ldunld.
               CREATE lorryStock.
                   lorryStock.ID      = tempID + 1       . 
                   lorryStock.VehID   = tt-ldunld.vehNo  . 
                   lorryStock.itmID   = tt-ldunld.itmID  . 
                   lorryStock.itmName = tt-ldunld.itmName. 
                   lorryStock.SortId  = tt-ldunld.SortId .
                   lorryStock.weight  = tt-ldunld.Weight . 
                   lorryStock.BSC     = tt-ldunld.BSC    . 
                   lorryStock.BSP     = tt-ldunld.BSP    .
                   lorryStock.GRRD    = tt-ldunld.GRRD   . 
                   lorryStock.GRST    = tt-ldunld.GRST   .
                   lorryStock.LDC     = tt-ldunld.LDC    . 
                   lorryStock.LDP     = tt-ldunld.LDP    . 
                   lorryStock.ULC     = tt-ldunld.ULC    . 
                   lorryStock.ULP     = tt-ldunld.ULP    . 
                   lorryStock.RDC     = tt-ldunld.RDC    . 
                   lorryStock.RDP     = tt-ldunld.RDP    . 
                   lorryStock.TolP    = tt-ldunld.TolP   . 
                   lorryStock.TolC    = tt-ldunld.TOlC   .
                   lorryStock.billedP = tt-ldunld.BilP   .
                   lorryStock.Excess  = tt-ldunld.Excess . 
                   lorryStock.Short   = tt-ldunld.Short  . 
                   lorryStock.CrDate  = calendr:VALUE    . 
               tempID = tempID + 1.
    
        END.

        paramtrs.val = STRING(tempID).
        RELEASE paramtrs.
    END.
    ELSE
    DO:
        FOR EACH tt-ldunld .
            FIND FIRST lorryStock WHERE tt-ldunld.ID = lorryStock.ID.
            IF AVAILABLE lorryStock THEN
            DO:
                ASSIGN
                     lorryStock.BSC     =   tt-ldunld.BSC    
                     lorryStock.BSP     =   tt-ldunld.BSP
                     lorryStock.GRRD    =   tt-ldunld.GRRD 
                     lorryStock.GRST    =   tt-ldunld.GRST
                     lorryStock.LDC     =   tt-ldunld.LDC    
                     lorryStock.LDP     =   tt-ldunld.LDP    
                     lorryStock.ULC     =   tt-ldunld.ULC    
                     lorryStock.ULP     =   tt-ldunld.ULP    
                     lorryStock.RDC     =   tt-ldunld.RDC    
                     lorryStock.RDP     =   tt-ldunld.RDP    
                     lorryStock.TolP    =   tt-ldunld.TolP   
                     lorryStock.TOlC    =   tt-ldunld.TOlC   
                     lorryStock.billedP =   tt-ldunld.BilP   
                     lorryStock.Excess  =   tt-ldunld.Excess 
                     lorryStock.Short   =   tt-ldunld.Short  .
            END.
            RELEASE lorryStock.

        END.
    
        isRefresh = FALSE.
    END.
    
    RUN ttBind.

/*     OPEN QUERY brw FOR EACH tt-ldunld by tt-ldunld.SortID. */
    
    IF NOT ERROR-STATUS:ERROR THEN
    DO:
        MESSAGE "Records successfully saved." VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.
    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE test C-Win 
PROCEDURE test :
DEFINE VARIABLE tempDate AS DATE        NO-UNDO.
DEFINE VARIABLE cnt AS INTEGER     NO-UNDO.
tempDate = calendr:VALUE.

FOR EACH ics.recipts NO-LOCK,
      EACH ics.bills 
    WHERE bills.bill# = recipts.bill# 
    AND bills.vehNo = cmbVeh
    AND bills.bilDate = tempDate
    NO-LOCK .
    cnt = cnt + 1 .
END.
MESSAGE cnt VIEW-AS ALERT-BOX INFO BUTTONS OK.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ttBind C-Win 
PROCEDURE ttBind :
EMPTY TEMP-TABLE tt-bills.
EMPTY TEMP-TABLE tt-recipts.
EMPTY TEMP-TABLE tt-itms.
EMPTY TEMP-TABLE tt-lorryStock.

FOR EACH bills.
    CREATE tt-bills.
     tt-bills.bill#   = bills.bill#. 
     tt-bills.bilDate = bills.bilDate. 
     tt-bills.BillNo  = bills.BillNo. 
     tt-bills.vehNo   = bills.vehNo.
     tt-bills.bilDate = bills.bilDate.
END.

FOR EACH recipts.
    CREATE tt-recipts.
     tt-recipts.bill#       =   recipts.bill#       .
     tt-recipts.cases       =   recipts.cases       .
     tt-recipts.damageC     =   recipts.damageC     .
     tt-recipts.damP        =   recipts.damP        .
     tt-recipts.expC        =   recipts.expC        .
     tt-recipts.expP        =   recipts.expP        .
     tt-recipts.GRRD        =   recipts.GRRD .
     tt-recipts.GRST        =   recipts.GRST .
     tt-recipts.item#       =   recipts.item#       .
     tt-recipts.pieses      =   recipts.pieses      .
     tt-recipts.reciptID    =   recipts.reciptID    .
END.

FOR EACH itms.
    CREATE tt-itms.
     tt-itms.itmID         = itms.itmID         .
     tt-itms.itmName       = itms.itmName       .
     tt-itms.SortID        = itms.SortID        .
     tt-itms.unitWeightKG  = itms.unitWeightKG  .
     tt-itms.unitsPerCase  = itms.unitsPerCase  .
     tt-itms.unitPriceS    = itms.unitPriceS    .
     tt-itms.stockC        = itms.stockC        .
     tt-itms.stockP        = itms.stockP        .
END.

FOR EACH lorryStock.
    CREATE tt-lorryStock.
     tt-lorryStock.ID      = lorryStock.ID      .
     tt-lorryStock.itmID   = lorryStock.itmID   .
     tt-lorryStock.crDate  = lorryStock.crDate  .
     tt-lorryStock.VehID   = lorryStock.VehID   .
     tt-lorryStock.billedC = lorryStock.billedC .
     tt-lorryStock.billedP = lorryStock.billedP .
     tt-lorryStock.BSC     = lorryStock.BSC     .
     tt-lorryStock.BSP     = lorryStock.BSP     .
     tt-lorryStock.GRRD    = lorryStock.GRRD    .
     tt-lorryStock.GRST    = lorryStock.GRST    .
     tt-lorryStock.Excess  = lorryStock.Excess  .
     tt-lorryStock.LDC     = lorryStock.LDC     .
     tt-lorryStock.LDP     = lorryStock.LDP     .
     tt-lorryStock.RDP     = lorryStock.RDP     .
     tt-lorryStock.RDC     = lorryStock.RDC     .
     tt-lorryStock.Short   = lorryStock.Short   .
     tt-lorryStock.TolC    = lorryStock.TolC    .
     tt-lorryStock.TolP    = lorryStock.TolP    .
     tt-lorryStock.ULC     = lorryStock.ULC     .
     tt-lorryStock.ULP     = lorryStock.ULP     .
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE vehLoader C-Win 
PROCEDURE vehLoader :
FOR EACH vehical.
    cmbVeh:ADD-LAST(veh# + " - " + descrip,ID) IN FRAME {&FRAME-NAME}.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

