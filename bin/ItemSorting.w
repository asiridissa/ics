&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE SHARED VARIABLE session_Window AS INT.

/* Local Variable Definitions ---                                       */

DEFINE TEMP-TABLE ttItems LIKE itms.

      DEFINE VARIABLE currentID AS INTEGER     NO-UNDO.

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
&Scoped-define INTERNAL-TABLES ttItems

/* Definitions for BROWSE brw                                           */
&Scoped-define FIELDS-IN-QUERY-brw itmName SortID unitWeightKG unitPriceS itmID   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brw   
&Scoped-define SELF-NAME brw
&Scoped-define QUERY-STRING-brw FOR EACH ttItems BY SortID
&Scoped-define OPEN-QUERY-brw OPEN QUERY brw FOR EACH ttItems BY SortID.
&Scoped-define TABLES-IN-QUERY-brw ttItems
&Scoped-define FIRST-TABLE-IN-QUERY-brw ttItems


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-brw}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-18 RECT-19 brw btnFIRST btnUP btnDOWN ~
btnLAST filNo btnSet btnSave btnCancel btnClose 
&Scoped-Define DISPLAYED-OBJECTS filNo 

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
     SIZE 10 BY 3.

DEFINE BUTTON btnClose 
     LABEL "Close" 
     SIZE 10 BY 3.

DEFINE BUTTON btnDOWN 
     LABEL "DOWN" 
     SIZE 10 BY 3.

DEFINE BUTTON btnFIRST 
     LABEL "FIRST" 
     SIZE 10 BY 3.

DEFINE BUTTON btnLAST 
     LABEL "LAST" 
     SIZE 10 BY 3.

DEFINE BUTTON btnSave 
     LABEL "Save" 
     SIZE 10 BY 3.

DEFINE BUTTON btnSet 
     LABEL "Set" 
     SIZE 10 BY 1.

DEFINE BUTTON btnUP 
     LABEL "UP" 
     SIZE 10 BY 3.

DEFINE VARIABLE filNo AS INTEGER FORMAT ">>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-18
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL  GROUP-BOX  ROUNDED 
     SIZE 11.72 BY 13.19.

DEFINE RECTANGLE RECT-19
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL  GROUP-BOX  ROUNDED 
     SIZE 11.72 BY 9.65.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brw FOR 
      ttItems SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brw
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brw C-Win _FREEFORM
  QUERY brw NO-LOCK DISPLAY
      itmName FORMAT "x(50)":U 
          SortID FORMAT ">>>9":U
      unitWeightKG FORMAT ">>,>>9.999":U
      unitPriceS FORMAT ">>,>>9.99":U
itmID FORMAT "99999":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 92 BY 26.19
         FONT 10
         TITLE "Item Sorting" ROW-HEIGHT-CHARS .46 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     brw AT ROW 1.15 COL 1.43 WIDGET-ID 200
     btnFIRST AT ROW 1.81 COL 95.57 WIDGET-ID 4
     btnUP AT ROW 5 COL 95.57 WIDGET-ID 2
     btnDOWN AT ROW 8.23 COL 95.57 WIDGET-ID 6
     btnLAST AT ROW 11.46 COL 95.57 WIDGET-ID 8
     filNo AT ROW 15 COL 94.72 COLON-ALIGNED NO-LABEL WIDGET-ID 20
     btnSet AT ROW 16.04 COL 95.57 WIDGET-ID 22
     btnSave AT ROW 17.65 COL 95.57 WIDGET-ID 10
     btnCancel AT ROW 20.69 COL 95.57 WIDGET-ID 12
     btnClose AT ROW 23.77 COL 95.57 WIDGET-ID 14
     RECT-18 AT ROW 1.54 COL 94.72 WIDGET-ID 16
     RECT-19 AT ROW 17.35 COL 94.72 WIDGET-ID 18
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 106.14 BY 26.54
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
         TITLE              = "Item Sort"
         COLUMN             = 37.72
         ROW                = 1.15
         HEIGHT             = 26.54
         WIDTH              = 106.14
         MAX-HEIGHT         = 26.54
         MAX-WIDTH          = 154.43
         VIRTUAL-HEIGHT     = 26.54
         VIRTUAL-WIDTH      = 154.43
         CONTROL-BOX        = no
         MIN-BUTTON         = no
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
/* BROWSE-TAB brw RECT-19 DEFAULT-FRAME */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brw
/* Query rebuild information for BROWSE brw
     _START_FREEFORM
OPEN QUERY brw FOR EACH ttItems BY SortID.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE brw */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Item Sort */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. 
  IF THIS-PROCEDURE:PERSISTENT THEN  */ RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Item Sort */
DO:
  /* This event will close the window and terminate the procedure.  */
    MESSAGE "Conferm to close?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE yn AS LOGICAL.
    IF yn = YES THEN
    DO:
      APPLY "CLOSE":U TO THIS-PROCEDURE.
      RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brw
&Scoped-define SELF-NAME brw
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brw C-Win
ON VALUE-CHANGED OF brw IN FRAME DEFAULT-FRAME /* Item Sorting */
DO:
  filNo = ttItems.SortID.
  currentID = SortID.
  DISPLAY filNo WITH FRAME DEFAULT-FRAME.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCancel C-Win
ON CHOOSE OF btnCancel IN FRAME DEFAULT-FRAME /* Cancel */
DO:
    MESSAGE "Conferm to cancel?" SKIP
        "Yous unsaved changes will be lost." VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE yn AS LOGICAL.
    IF yn = YES THEN
    DO:
        RUN ttLoader.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnClose
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnClose C-Win
ON CHOOSE OF btnClose IN FRAME DEFAULT-FRAME /* Close */
DO:
  APPLY "WINDOW-CLOSE":U TO C-Win.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDOWN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDOWN C-Win
ON CHOOSE OF btnDOWN IN FRAME DEFAULT-FRAME /* DOWN */
DO:
  DEFINE VARIABLE tempSortID AS INTEGER     NO-UNDO.
   DEFINE VARIABLE tempMaxSortID AS INTEGER     NO-UNDO.
   DEFINE VARIABLE tempNextSortID AS INTEGER     NO-UNDO.
   tempSortID = 0.
   tempMaxSortID = 0.

   FOR EACH ttItems.
      IF ttItems.SortID > tempMaxSortID THEN
          tempMaxSortID = ttItems.SortID.
      END.

   APPLY "VALUE-CHANGED":U TO brw .
   
   IF AVAILABLE ttItems AND ttItems.SortID <> tempMaxSortID THEN
    DO:
      tempSortID = ttItems.SortID.
      ttItems.SortID = 0.

      FIND FIRST ttItems WHERE ttItems.SortID = tempSortID + 1.
      DO:
          ttItems.SortID = tempSortID.
      END.
      RELEASE ttItems.

      FIND FIRST ttItems WHERE ttItems.SortID = 0.
      DO:
          ttItems.SortID = tempSortID + 1.
      END.
      RELEASE ttItems.

    END.
    OPEN QUERY brw FOR EACH ttItems BY ttItems.SortID.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnFIRST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFIRST C-Win
ON CHOOSE OF btnFIRST IN FRAME DEFAULT-FRAME /* FIRST */
DO:
   DEFINE VARIABLE tempSortID AS INTEGER     NO-UNDO.
   tempSortID = 0.
  IF AVAILABLE ttItems THEN
  DO:
      tempSortID = ttItems.SortID.
      ttItems.SortID = 0.
      FOR EACH ttItems WHERE ttItems.SortID < tempSortID.
        ttItems.SortID = ttItems.SortID + 1.
      END.
  END.

  OPEN QUERY brw FOR EACH ttItems BY ttItems.SortID.
  PUT CURSOR ROW 5.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLAST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLAST C-Win
ON CHOOSE OF btnLAST IN FRAME DEFAULT-FRAME /* LAST */
DO:
  DEFINE VARIABLE tempSortID AS INTEGER     NO-UNDO.
  DEFINE VARIABLE tempMaxSortID AS INTEGER     NO-UNDO.
   tempSortID = 0.
   tempMaxSortID = 0.

   IF AVAILABLE ttItems THEN
   DO:
    FOR EACH ttItems.
    IF ttItems.SortID > tempMaxSortID THEN
        tempMaxSortID = ttItems.SortID.
    END.
    
    APPLY "VALUE-CHANGED":U TO brw.
    tempSortID = ttItems.SortID.

    FOR EACH ttItems WHERE ttItems.SortID > tempSortID.
        ttItems.SortID = ttItems.SortID - 1.
    END.

    APPLY "VALUE-CHANGED":U TO brw.
    ttItems.SortID = tempMaxSortID.
   END.

  OPEN QUERY brw FOR EACH ttItems BY ttItems.SortID.
  PUT CURSOR ROW tempMaxSortID COLUMN ttItems.SortID.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSave C-Win
ON CHOOSE OF btnSave IN FRAME DEFAULT-FRAME /* Save */
DO:
    MESSAGE "Conferm to save records?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE yn AS LOGICAL.
    IF yn = YES THEN
    DO:
        FOR EACH ttItems.
            FIND FIRST itms WHERE itms.itmID = ttItems.itmID.
            itms.SortID       =  ttItems.SortID        .
        END.
        MESSAGE "Successfully saved." VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSet
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSet C-Win
ON CHOOSE OF btnSet IN FRAME DEFAULT-FRAME /* Set */
DO:
    FIND FIRST ttItems WHERE SortID = filNo NO-LOCK NO-ERROR.
      IF AVAILABLE ttItems THEN
      DO:
        MESSAGE "Conferm to change Sort ID?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE yn AS LOGICAL.
        IF yn THEN
        DO:
      
              IF filNo > currentID THEN
              DO:
                  FIND FIRST ttItems WHERE SortID = currentID.
                    SortID = 0.
                  RELEASE ttItems. 
                  FOR EACH ttItems WHERE SortID > currentID AND SortID <= filNo.
                    SortID = SortID - 1.
                  END.
                  FIND FIRST ttItems WHERE SortID = 0.
                    SortID = filNo.
                  RELEASE ttItems.
              END.
              ELSE
              DO:
                  /*Move Up*/
                  FIND FIRST ttItems WHERE SortID = currentID.
                    SortID = 0.
                  RELEASE ttItems.
                  FOR EACH ttItems WHERE SortID < currentID AND SortID >= filNo.
                    SortID = SortID + 1.
                  END.
                  FIND FIRST ttItems WHERE SortID = 0.
                    SortID = filNo.
                  RELEASE ttItems.
              END.
        
              OPEN QUERY brw FOR EACH ttItems BY SortID.
              APPLY "VALUE-CHANGED":U TO brw.
          END.
  END.
      IF NOT AVAILABLE ttItems THEN
          MESSAGE "No space for number " + string(filNo) + "." VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUP C-Win
ON CHOOSE OF btnUP IN FRAME DEFAULT-FRAME /* UP */
DO:
   DEFINE VARIABLE tempSortID AS INTEGER     NO-UNDO.
   DEFINE VARIABLE tempPrevSortID AS INTEGER     NO-UNDO.
   tempSortID = 0.
   IF AVAILABLE ttItems AND ttItems.SortID <> 1 THEN
    DO:
      tempSortID = ttItems.SortID.
      ttItems.SortID = 0.

      FIND FIRST ttItems WHERE ttItems.SortID = tempSortID - 1.
      DO:
          ttItems.SortID = tempSortID.
      END.
      RELEASE ttItems.

      FIND FIRST ttItems WHERE ttItems.SortID = 0.
      DO:
          ttItems.SortID = tempSortID - 1.
      END.
      RELEASE ttItems.

    END.
    
    OPEN QUERY brw FOR EACH ttItems BY ttItems.SortID.
    PUT CURSOR ROW 3 COLUMN ttItems.SortID.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME filNo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filNo C-Win
ON LEAVE OF filNo IN FRAME DEFAULT-FRAME
DO:
    ASSIGN {&SELF-NAME}.
  


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filNo C-Win
ON RETURN OF filNo IN FRAME DEFAULT-FRAME
DO:

    ASSIGN {&SELF-NAME}.
  APPLY "CHOOSE":U TO btnSet.
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

  RUN ttLoader.

  APPLY "VALUE-CHANGED":U TO brw.

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
  DISPLAY filNo 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-18 RECT-19 brw btnFIRST btnUP btnDOWN btnLAST filNo btnSet 
         btnSave btnCancel btnClose 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ttLoader C-Win 
PROCEDURE ttLoader :
EMPTY TEMP-TABLE ttItems.
    FOR EACH itms.
      CREATE ttItems.
      ttItems.BSC          =   itms.BSC            .
      ttItems.BSP          =   itms.BSP            .
      ttItems.casePriceB   =   itms.casePriceB     .
      ttItems.casePriceS   =   itms.casePriceS     .
      ttItems.cat          =   itms.cat            .
      ttItems.crDate       =   itms.crDate         .
      ttItems.discountIN   =   itms.discountIN     .
      ttItems.itmID        =   itms.itmID          .
      ttItems.itmName      =   itms.itmName        .
      ttItems.maxWeight    =   itms.maxWeight      .
      ttItems.noOfCases    =   itms.noOfCases      .
      ttItems.noOfUnits    =   itms.noOfUnits      .
      ttItems.SortID       =   itms.SortID         .
      ttItems.stockC       =   itms.stockC         .
      ttItems.stockP       =   itms.stockP         .
      ttItems.unitPriceB   =   itms.unitPriceB     .
      ttItems.unitPriceS   =   itms.unitPriceS     .
      ttItems.unitsPerCase =   itms.unitsPerCase   .
      ttItems.unitWeightKG =   itms.unitWeightKG   .
  END.

  OPEN QUERY brw FOR EACH ttItems BY ttItems.SortID.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

