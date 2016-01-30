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
DEFINE VARIABLE ModifyVal AS CHARACTER   NO-UNDO.
DEFINE VARIABLE ModifyCnt AS INTEGER   NO-UNDO.

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
&Scoped-define INTERNAL-TABLES customer

/* Definitions for BROWSE arbrw                                         */
&Scoped-define FIELDS-IN-QUERY-arbrw customer.cusID customer.CusArea ~
customer.cusName customer.cusType customer.paymentMethod customer.phoneNo ~
customer.descrip 
&Scoped-define ENABLED-FIELDS-IN-QUERY-arbrw 
&Scoped-define QUERY-STRING-arbrw FOR EACH customer NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-arbrw OPEN QUERY arbrw FOR EACH customer NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-arbrw customer
&Scoped-define FIRST-TABLE-IN-QUERY-arbrw customer


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-arbrw}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS arbrw btnAdd filSearch btnModify btnDelete ~
RECT-20 RECT-21 RECT-22 
&Scoped-Define DISPLAYED-OBJECTS filSearch cmbArea filCode filDescr filID ~
cmbCusCat cmbPayMethod filPhone 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

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

DEFINE BUTTON btnSave 
     LABEL "Save" 
     SIZE 14 BY 1.

DEFINE VARIABLE cmbArea AS CHARACTER FORMAT "X(30)":U 
     LABEL "Area" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "--Select Here--" 
     DROP-DOWN-LIST
     SIZE 35 BY 1 NO-UNDO.

DEFINE VARIABLE cmbCusCat AS CHARACTER FORMAT "X(30)":U 
     LABEL "Category" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "--Select Here--","Grocery","Pharmacy","SMMT","Hotel","Wholesale","Stores","Semi Wholesale","Bookshop","Other" 
     DROP-DOWN-LIST
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE cmbPayMethod AS CHARACTER FORMAT "X(30)":U 
     LABEL "Payment Method" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "--Select Here--","Credit","Cash","Cheque","Other" 
     DROP-DOWN-LIST
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE filDescr AS CHARACTER 
     VIEW-AS EDITOR MAX-CHARS 100
     SIZE 55 BY 2.15
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE filCode AS CHARACTER FORMAT "X(50)":U 
     LABEL "Customer Name" 
     VIEW-AS FILL-IN 
     SIZE 55 BY .88
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE filID AS INTEGER FORMAT ">>>,>>9":U INITIAL 0 
     LABEL "ID" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88
     BGCOLOR 7 FGCOLOR 11  NO-UNDO.

DEFINE VARIABLE filPhone AS INT64 FORMAT "9999999999":U INITIAL 0 
     LABEL "Phone" 
     VIEW-AS FILL-IN 
     SIZE 20 BY .88
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE filSearch AS CHARACTER FORMAT "X(256)":U 
     LABEL "Search" 
     VIEW-AS FILL-IN 
     SIZE 35 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-20
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL  GROUP-BOX  ROUNDED 
     SIZE 102 BY 1.38.

DEFINE RECTANGLE RECT-21
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL  GROUP-BOX  ROUNDED 
     SIZE 131 BY 5.65.

DEFINE RECTANGLE RECT-22
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL  GROUP-BOX  ROUNDED 
     SIZE 45 BY 1.62.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY arbrw FOR 
      customer SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE arbrw
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS arbrw C-Win _STRUCTURED
  QUERY arbrw NO-LOCK DISPLAY
      customer.cusID COLUMN-LABEL "ID" FORMAT ">,>>>,>>9":U
      customer.CusArea COLUMN-LABEL "                                  Area" FORMAT "x(100)":U
            WIDTH 20
      customer.cusName COLUMN-LABEL "                                  Name" FORMAT "X(50)":U
            WIDTH 30
      customer.cusType FORMAT "x(20)":U
      customer.paymentMethod FORMAT "x(20)":U
      customer.phoneNo FORMAT "9999999999":U
      customer.descrip COLUMN-LABEL "                                     Description" FORMAT "x(100)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 143.72 BY 18.81
         BGCOLOR 15 FGCOLOR 4 FONT 10 ROW-HEIGHT-CHARS .55 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     arbrw AT ROW 1.08 COL 1.14 WIDGET-ID 200
     btnAdd AT ROW 26.12 COL 22.14 WIDGET-ID 6
     filSearch AT ROW 20.58 COL 98.72 COLON-ALIGNED WIDGET-ID 70 NO-TAB-STOP 
     btnModify AT ROW 26.12 COL 36.43 WIDGET-ID 8
     btnDelete AT ROW 26.12 COL 50.86 WIDGET-ID 10
     cmbArea AT ROW 21.31 COL 23 COLON-ALIGNED WIDGET-ID 50
     filCode AT ROW 22.38 COL 23 COLON-ALIGNED WIDGET-ID 52
     filDescr AT ROW 23.42 COL 25 NO-LABEL WIDGET-ID 26
     filID AT ROW 20.31 COL 23 COLON-ALIGNED WIDGET-ID 16 NO-TAB-STOP 
     cmbCusCat AT ROW 22.35 COL 98.72 COLON-ALIGNED WIDGET-ID 58
     cmbPayMethod AT ROW 23.42 COL 98.72 COLON-ALIGNED WIDGET-ID 60
     filPhone AT ROW 24.5 COL 98.72 COLON-ALIGNED WIDGET-ID 62
     btnSave AT ROW 26.12 COL 87.14 WIDGET-ID 12
     btnCancel AT ROW 26.12 COL 101.43 WIDGET-ID 14
     "Description :" VIEW-AS TEXT
          SIZE 10.29 BY .88 AT ROW 23.15 COL 14.57 WIDGET-ID 28
     "*" VIEW-AS TEXT
          SIZE 2.43 BY .62 AT ROW 23.42 COL 81.14 WIDGET-ID 54
          FGCOLOR 12 
     "*" VIEW-AS TEXT
          SIZE .86 BY .62 AT ROW 22.35 COL 81.14 WIDGET-ID 56
          FGCOLOR 12 
     "*" VIEW-AS TEXT
          SIZE 5 BY .62 AT ROW 22.31 COL 121.14 WIDGET-ID 64
          FGCOLOR 12 
     "*" VIEW-AS TEXT
          SIZE 5 BY .62 AT ROW 23.54 COL 121.14 WIDGET-ID 66
          FGCOLOR 12 
     "*" VIEW-AS TEXT
          SIZE 5 BY .62 AT ROW 21.27 COL 60.86 WIDGET-ID 68
          FGCOLOR 12 
     RECT-20 AT ROW 25.92 COL 17.57 WIDGET-ID 72
     RECT-21 AT ROW 20.12 COL 7 WIDGET-ID 74
     RECT-22 AT ROW 20.31 COL 92 WIDGET-ID 76
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 144.14 BY 26.42
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
         TITLE              = "LMS - Customers"
         COLUMN             = 1.14
         ROW                = 1.27
         HEIGHT             = 26.54
         WIDTH              = 144.14
         MAX-HEIGHT         = 30.08
         MAX-WIDTH          = 144.14
         VIRTUAL-HEIGHT     = 30.08
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
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME Custom                                                    */
/* BROWSE-TAB arbrw 1 DEFAULT-FRAME */
/* SETTINGS FOR BUTTON btnCancel IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnSave IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX cmbArea IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX cmbCusCat IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX cmbPayMethod IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN filCode IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR EDITOR filDescr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       filDescr:AUTO-INDENT IN FRAME DEFAULT-FRAME      = TRUE.

/* SETTINGS FOR FILL-IN filID IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN filPhone IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE arbrw
/* Query rebuild information for BROWSE arbrw
     _TblList          = "ics.customer"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > ics.customer.cusID
"customer.cusID" "ID" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ics.customer.CusArea
"customer.CusArea" "                                  Area" ? "character" ? ? ? ? ? ? no ? no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ics.customer.cusName
"customer.cusName" "                                  Name" ? "character" ? ? ? ? ? ? no ? no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   = ics.customer.cusType
     _FldNameList[5]   = ics.customer.paymentMethod
     _FldNameList[6]   > ics.customer.phoneNo
"customer.phoneNo" ? "9999999999" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > ics.customer.descrip
"customer.descrip" "                                     Description" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE arbrw */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* LMS - Customers */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. 
  IF THIS-PROCEDURE:PERSISTENT THEN  */ RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* LMS - Customers */
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
ON VALUE-CHANGED OF arbrw IN FRAME DEFAULT-FRAME
DO:
    IF AVAILABLE customer THEN
    DO:
        ASSIGN
           filID        = cusID           
           filCode      = cusName         
           filDescr     = customer.descrip
           cmbArea      = customer.CusArea
           cmbPayMethod = customer.paymentMethod 
           filPhone     = customer.phoneNo      
           cmbCusCat    = customer.cusType        
           filDescr     = customer.descrip
            .
    END.
    DISPLAY  cmbCusCat cmbPayMethod filPhone filCode filDescr filID cmbArea WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAdd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAdd C-Win
ON CHOOSE OF btnAdd IN FRAME DEFAULT-FRAME /* Add */
DO:
  ENABLE filCode filDescr cmbPayMethod filPhone cmbCusCat cmbArea btnSave btnCancel WITH FRAME {&FRAME-NAME}.
  DISABLE btnAdd btnDelete btnModify arbrw WITH FRAME {&FRAME-NAME}.
  
  FIND FIRST paramtrs WHERE NAME = "lastCustomerID".
  IF AVAILABLE paramtrs THEN
      filID    = INT(val) + 1.

  filCode    = "".
  filDescr   = "".
  cmbArea    = "--Select Here--".
  cmbCusCat = "--Select Here--".
  cmbPayMethod = "--Select Here--".
  filPhone = 0.

  addModify  = "add".

  DISPLAY filCode filDescr filID cmbArea cmbCusCat cmbPayMethod filPhone WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCancel C-Win
ON CHOOSE OF btnCancel IN FRAME DEFAULT-FRAME /* Cancel */
DO:
  MESSAGE "Conferm to cancel?" VIEW-AS ALERT-BOX INFO BUTTONS YES-NO UPDATE yn AS LOGICAL.
  IF yn = YES THEN
  DO:
      DISABLE cmbPayMethod filPhone cmbCusCat filCode filDescr cmbArea cmbArea btnSave btnCancel WITH FRAME {&FRAME-NAME}.
      ENABLE btnAdd btnDelete btnModify arbrw WITH FRAME {&FRAME-NAME}.
    
      OPEN QUERY arbrw FOR EACH customer BY cusID.
      APPLY "VALUE-CHANGED":U TO arbrw.
    
      DISPLAY cmbArea cmbCusCat cmbPayMethod filPhone WITH FRAME {&FRAME-NAME}.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDelete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDelete C-Win
ON CHOOSE OF btnDelete IN FRAME DEFAULT-FRAME /* Delete */
DO:
    IF filCode = "" THEN
        MESSAGE "No records to delete." VIEW-AS ALERT-BOX WARNING BUTTONS OK .
    ELSE
    DO:
        MESSAGE "Conferm to save the record?" VIEW-AS ALERT-BOX INFO BUTTONS YES-NO UPDATE yn AS LOGICAL.
        IF yn THEN
        DO:
            FIND FIRST customer WHERE cusID = filID.
            IF AVAILABLE customer THEN
                DELETE customer.
    
            OPEN QUERY arbrw FOR EACH customer BY cusID.
            APPLY "VALUE-CHANGED":U TO arbrw.
        END.
    END.
    

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnModify
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnModify C-Win
ON CHOOSE OF btnModify IN FRAME DEFAULT-FRAME /* Modify */
DO:
  ENABLE filCode filDescr cmbArea cmbPayMethod filPhone cmbCusCat btnSave btnCancel WITH FRAME {&FRAME-NAME}.
  DISABLE btnAdd btnDelete btnModify arbrw WITH FRAME {&FRAME-NAME}.
  addModify = "modify".
  ModifyVal = filCode.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSave C-Win
ON CHOOSE OF btnSave IN FRAME DEFAULT-FRAME /* Save */
DO:
    IF filCode = "" THEN
    DO:
        MESSAGE "Customer Name cannot be a blank." VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN .
    END.
    IF LENGTH(filCode) < 5 THEN
    DO:
        MESSAGE "Customer Name must have atleast 5 characters." VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN .
    END.
    IF cmbArea = "--Select Here--" THEN
    DO:
        MESSAGE "Area cannot be a blank." VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN .
    END.
    IF cmbCusCat = "--Select Here--" THEN
    DO:
        MESSAGE "Category cannot be a blank." VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN .
    END.
    IF cmbPayMethod = "--Select Here--" THEN
    DO:
        MESSAGE "Payment Method cannot be a blank." VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN .
    END.
    IF (filPhone <> 0 AND filPhone < 0112000000) OR filPhone > 0999999999 THEN
    DO:
        MESSAGE "Invalid Phone Number." VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN .
    END.

/*add*******************************************************************************************/

    IF addModify = "add" THEN
    DO:
        FIND FIRST customer WHERE cusName = filCode AND CusArea = cmbArea EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE customer THEN
            DO:
                MESSAGE "Customer already exists." VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                RETURN.
            END.
            ELSE IF NOT AVAILABLE customer THEN
            DO:
                MESSAGE "Conferm to save record?" VIEW-AS ALERT-BOX INFO BUTTONS YES-NO UPDATE yn AS LOGICAL.
                IF yn = TRUE THEN
                DO:
                    CREATE customer.
                            cusID            = filID                .
                            cusName          = filCode              .
                            customer.descrip = filDescr             .
                            customer.CusArea = cmbArea              .
                            customer.paymentMethod = cmbPayMethod   .
                            customer.phoneNo = int(filPhone)             .
                            customer.cusType = cmbCusCat            .
                            
                    FIND FIRST paramtrs WHERE paramtrs.NAME = "lastCustomerID".
                    IF AVAILABLE paramtrs THEN
                        val    = STRING(INT(val) + 1).
                    RELEASE paramtrs. 
                END.
                ELSE
                    RETURN.
            END.

        RELEASE customer.
    END.
/*modify*****************************************************************************************************/
    ELSE IF addModify = "modify" THEN

    DO:
/*modify******************************************************validation Begins*******************************************************/
        


/*modify******************************************************validation Ends*******************************************************/
    MESSAGE "Conferm to save record?" VIEW-AS ALERT-BOX INFO BUTTONS YES-NO UPDATE yn1 AS LOGICAL.
        IF yn1 = TRUE THEN
        DO:
            FOR EACH customer WHERE cusName = filCode AND cusID <> filID AND CusArea = cmbArea.
                ModifyCnt = ModifyCnt + 1.
            END.

            IF ModifyCnt >= 1 THEN
            DO:
                MESSAGE "Customer already exists." VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                ModifyCnt = 0.
                APPLY "ENTRY":U TO filCode.
                RETURN.
            END.
            ELSE IF ModifyCnt = 0 THEN
            DO:
                FIND FIRST customer WHERE cusID = filID EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE customer THEN
                DO:
                        cusName          = filCode.
                        customer.descrip = filDescr.
                        customer.CusArea = cmbArea  .
                        customer.paymentMethod = cmbPayMethod   .
                            customer.phoneNo = int(filPhone)             .
                            customer.cusType = cmbCusCat            .
                RELEASE customer.
                END.
            END.
        END.
        
    END.


    DISABLE cmbPayMethod filPhone cmbCusCat cmbArea filCode filDescr btnSave btnCancel WITH FRAME {&FRAME-NAME}.
    ENABLE btnAdd btnDelete btnModify arbrw WITH FRAME {&FRAME-NAME}.

    DISPLAY cmbArea WITH FRAME {&FRAME-NAME}.

    OPEN QUERY arbrw FOR EACH customer BY cusID.
    APPLY "VALUE-CHANGED":U TO arbrw.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cmbArea
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbArea C-Win
ON VALUE-CHANGED OF cmbArea IN FRAME DEFAULT-FRAME /* Area */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cmbCusCat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbCusCat C-Win
ON VALUE-CHANGED OF cmbCusCat IN FRAME DEFAULT-FRAME /* Category */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cmbPayMethod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbPayMethod C-Win
ON VALUE-CHANGED OF cmbPayMethod IN FRAME DEFAULT-FRAME /* Payment Method */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME filCode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filCode C-Win
ON ANY-KEY OF filCode IN FRAME DEFAULT-FRAME /* Customer Name */
DO:
  ASSIGN {&SELF-NAME}.
  {&SELF-NAME} = CAPS(SUBSTRING({&SELF-NAME},1,1)) + SUBSTRING({&SELF-NAME},2).

  DISPLAY {&SELF-NAME} WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filCode C-Win
ON LEAVE OF filCode IN FRAME DEFAULT-FRAME /* Customer Name */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filCode C-Win
ON RETURN OF filCode IN FRAME DEFAULT-FRAME /* Customer Name */
DO:
  
    APPLY "CHOOSE":U TO btnSave.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME filDescr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filDescr C-Win
ON LEAVE OF filDescr IN FRAME DEFAULT-FRAME
DO:
  ASSIGN {&SELF-NAME}.
  {&SELF-NAME} = CAPS(SUBSTRING({&SELF-NAME},1,1)) + SUBSTRING({&SELF-NAME},2).

  DISPLAY {&SELF-NAME} WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME filID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filID C-Win
ON LEAVE OF filID IN FRAME DEFAULT-FRAME /* ID */
DO:
  ASSIGN {&SELF-NAME}.
  

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


&Scoped-define SELF-NAME filPhone
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filPhone C-Win
ON ANY-KEY OF filPhone IN FRAME DEFAULT-FRAME /* Phone */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filPhone C-Win
ON LEAVE OF filPhone IN FRAME DEFAULT-FRAME /* Phone */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filPhone C-Win
ON RETURN OF filPhone IN FRAME DEFAULT-FRAME /* Phone */
DO:
  
    APPLY "CHOOSE":U TO btnSave.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME filSearch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filSearch C-Win
ON VALUE-CHANGED OF filSearch IN FRAME DEFAULT-FRAME /* Search */
DO:
  ASSIGN {&SELF-NAME}.
  OPEN QUERY arbrw FOR EACH ics.customer
      WHERE customer.cusName BEGINS {&SELF-NAME}
      NO-LOCK INDEXED-REPOSITION.
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
  FOR EACH area NO-LOCK .
      cmbArea:ADD-LAST(descrip).
  END.

  APPLY "VALUE-CHANGED":U TO arbrw.

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
  DISPLAY filSearch cmbArea filCode filDescr filID cmbCusCat cmbPayMethod 
          filPhone 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE arbrw btnAdd filSearch btnModify btnDelete RECT-20 RECT-21 RECT-22 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

