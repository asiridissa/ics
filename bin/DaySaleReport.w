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

DEFINE TEMP-TABLE tt-lorryStock LIKE lorryStock.
DEFINE TEMP-TABLE tt-print LIKE tt-lorryStock.

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
&Scoped-define INTERNAL-TABLES tt-lorryStock

/* Definitions for BROWSE BROWSE-3                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-3 Id itmName weight BSC BSP GRRD GRST LDC LDP ULC ULP RDC RDP billedP Excess Short TolP   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-3   
&Scoped-define SELF-NAME BROWSE-3
&Scoped-define QUERY-STRING-BROWSE-3 FOR EACH tt-lorryStock
&Scoped-define OPEN-QUERY-BROWSE-3 OPEN QUERY {&SELF-NAME} FOR EACH tt-lorryStock.
&Scoped-define TABLES-IN-QUERY-BROWSE-3 tt-lorryStock
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-3 tt-lorryStock


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
      tt-lorryStock SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-3 C-Win _FREEFORM
  QUERY BROWSE-3 DISPLAY
      Id          width 3                   LABEL "#"
    itmName     width 35                  LABEL "Item"
    weight      width 6                   LABEL "Weight"
    BSC        FORMAT "->>,>>9" width 4   
    BSP        FORMAT "->>,>>9" width 6   
    GRRD       FORMAT "->>,>>9" width 4   LABEL "GRR"
    GRST       FORMAT "->>,>>9" width 4   LABEL "GRS" 
    LDC        FORMAT "->>,>>9" width 4   
    LDP        FORMAT "->>,>>9" width 6   
    ULC        FORMAT "->>,>>9" width 4   
    ULP        FORMAT "->>,>>9" width 6   
    RDC        FORMAT "->>,>>9" width 4   
    RDP        FORMAT "->>,>>9" width 6   
    billedP    FORMAT "->>,>>9" width 9   LABEL "Bill"
    Excess     FORMAT "->>,>>9" width 9   
    Short      FORMAT "->>,>>9" width 9   
    TolP       FORMAT "->>,>>9" width 4   LABEL "Gap" COLUMN-FGCOLOR 12
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
         ROW                = 2.69
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
ASSIGN 
       BROWSE-3:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE
       BROWSE-3:COLUMN-RESIZABLE IN FRAME DEFAULT-FRAME       = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-3
/* Query rebuild information for BROWSE BROWSE-3
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-lorryStock.
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
    

    FIND FIRST tt-lorryStock NO-LOCK NO-ERROR.
    IF NOT AVAILABLE tt-lorryStock THEN
        DO:
            MESSAGE "No records available to print." VIEW-AS ALERT-BOX INFO BUTTONS OK.
            RETURN.
        END.
    RELEASE tt-lorryStock.

    {&SELF-NAME}:LABEL = "Working..".

    RUN print.

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

    RUN ttSum.

    {&SELF-NAME}:LABEL = "View".

    OPEN QUERY BROWSE-3 FOR EACH tt-lorryStock.
    APPLY "VALUE-CHANGED":U TO BROWSE-3.
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

OCXFile = SEARCH( "DaySaleReport.wrx":U ).
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
ELSE MESSAGE "DaySaleReport.wrx":U SKIP(1)
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE print C-Win 
PROCEDURE print :
DEFINE VARIABLE veh AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE period AS CHARACTER   NO-UNDO.
    
    MESSAGE "Confirm to print?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE yn AS LOGICAL.
    IF yn = YES THEN
    DO:
        OUTPUT TO VALUE("print\DaySaleReport.txt").

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
            PUT UNFORMAT "#|Item|Weight|BSC|BSP|GRRD|GRST|LDC|LDP|ULC|ULP|RDC|RDP|Bill|Exess|Short|Gap|" SKIP.

            FOR EACH tt-lorryStock.
               PUT UNFORMAT STRING(     tt-lorryStock.ID           ) + "|" .
               PUT UNFORMAT STRING(     tt-lorryStock.itmName      ) + "|" .
               PUT UNFORMAT STRING(     tt-lorryStock.weight       ) + "|" .
               PUT UNFORMAT STRING(zero(tt-lorryStock.BSC     )) + "|".
               PUT UNFORMAT STRING(zero(tt-lorryStock.BSP     )) + "|".
               PUT UNFORMAT STRING(zero(tt-lorryStock.GRRD    )) + "|".
               PUT UNFORMAT STRING(zero(tt-lorryStock.GRST    )) + "|".
               PUT UNFORMAT STRING(zero(tt-lorryStock.LDC     )) + "|".
               PUT UNFORMAT STRING(zero(tt-lorryStock.LDP     )) + "|".
               PUT UNFORMAT STRING(zero(tt-lorryStock.ULC     )) + "|".
               PUT UNFORMAT STRING(zero(tt-lorryStock.ULP     )) + "|".
               PUT UNFORMAT STRING(zero(tt-lorryStock.RDC     )) + "|".
               PUT UNFORMAT STRING(zero(tt-lorryStock.RDP     )) + "|".
               PUT UNFORMAT STRING(zero(tt-lorryStock.billedP )) + "|".
               PUT UNFORMAT STRING(zero(tt-lorryStock.Excess  )) + "|".
               PUT UNFORMAT STRING(zero(tt-lorryStock.Short   )) + "|".
               PUT UNFORMAT STRING(zero(tt-lorryStock.TolP    )) + "|"SKIP.
            END.
        OUTPUT CLOSE.


        DOS SILENT START VALUE("print\DaySaleReport.bat").

        DOS SILENT START excel VALUE("print\DaySaleReport.xlsx").
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ttSum C-Win 
PROCEDURE ttSum :
DEFINE VARIABLE cnt AS INTEGER     NO-UNDO.

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
    
    EMPTY TEMP-TABLE tt-lorryStock NO-ERROR.
    
    FOR EACH lorryStock WHERE lorryStock.crDate >= dateFrom AND lorryStock.crDate <= dateTo NO-LOCK.
        FIND FIRST tt-lorryStock WHERE tt-lorryStock.itmID = lorryStock.itmID NO-ERROR.
        IF NOT AVAILABLE tt-lorryStock THEN
        DO:
            IF  (lorryStock.BSC     +
                 lorryStock.BSP     +
                 lorryStock.GRRD    +
                 lorryStock.GRST    +
                 lorryStock.LDC     +
                 lorryStock.LDP     +
                 lorryStock.ULC     +
                 lorryStock.ULP     +
                 lorryStock.RDC     +
                 lorryStock.RDP     +
                 lorryStock.TolP    +
                 lorryStock.TOlC    +
                 lorryStock.billedP +
                 lorryStock.Excess  +
                 lorryStock.Short   
                 ) <> 0 THEN
            DO:
            CREATE tt-lorryStock.
                     tt-lorryStock.VehID   = lorryStock.VehID  . 
                     tt-lorryStock.itmID   = lorryStock.itmID  . 
                     tt-lorryStock.itmName = lorryStock.itmName. 
                     tt-lorryStock.SortId  = lorryStock.SortId .
                     tt-lorryStock.weight  = lorryStock.Weight . 
                     tt-lorryStock.BSC     = lorryStock.BSC    . 
                     tt-lorryStock.BSP     = lorryStock.BSP    .
                     tt-lorryStock.GRRD    = lorryStock.GRRD   . 
                     tt-lorryStock.GRST    = lorryStock.GRST   .
                     tt-lorryStock.LDC     = lorryStock.LDC    . 
                     tt-lorryStock.LDP     = lorryStock.LDP    . 
                     tt-lorryStock.ULC     = lorryStock.ULC    . 
                     tt-lorryStock.ULP     = lorryStock.ULP    . 
                     tt-lorryStock.RDC     = lorryStock.RDC    . 
                     tt-lorryStock.RDP     = lorryStock.RDP    . 
                     tt-lorryStock.billedP = lorryStock.billedP   .
                     tt-lorryStock.Excess  = lorryStock.Excess . 
                     tt-lorryStock.Short   = lorryStock.Short  . 
                     tt-lorryStock.CrDate  = lorryStock.CrDate    .
    /*                  Excess SHORT Varience */
                     tt-lorryStock.TolP    = tt-lorryStock.Excess -  tt-lorryStock.Short  . 
            END.
        END.
        ELSE
        DO:
            ASSIGN
                    tt-lorryStock.BSC     =   tt-lorryStock.BSC      + lorryStock.BSC     
                    tt-lorryStock.BSP     =   tt-lorryStock.BSP      + lorryStock.BSP     
                    tt-lorryStock.GRRD    =   tt-lorryStock.GRRD     + lorryStock.GRRD    
                    tt-lorryStock.GRST    =   tt-lorryStock.GRST     + lorryStock.GRST    
                    tt-lorryStock.LDC     =   tt-lorryStock.LDC      + lorryStock.LDC     
                    tt-lorryStock.LDP     =   tt-lorryStock.LDP      + lorryStock.LDP     
                    tt-lorryStock.ULC     =   tt-lorryStock.ULC      + lorryStock.ULC     
                    tt-lorryStock.ULP     =   tt-lorryStock.ULP      + lorryStock.ULP     
                    tt-lorryStock.RDC     =   tt-lorryStock.RDC      + lorryStock.RDC     
                    tt-lorryStock.RDP     =   tt-lorryStock.RDP      + lorryStock.RDP     
                    tt-lorryStock.billedP =   tt-lorryStock.billedP  + lorryStock.billedP 
                    tt-lorryStock.Excess  =   tt-lorryStock.Excess   + lorryStock.Excess  
                    tt-lorryStock.Short   =   tt-lorryStock.Short    + lorryStock.Short  
                /*                  Excess SHORT Varience */
                     tt-lorryStock.TolP    = tt-lorryStock.Excess -  tt-lorryStock.Short  
                .
        END.
    END.

    EMPTY TEMP-TABLE tt-print.
    
    FOR EACH tt-lorryStock NO-LOCK,
        EACH itms WHERE itms.itmID = tt-lorryStock.itmID NO-LOCK
        BY itms.SortId.
        cnt = cnt + 1.
            CREATE tt-print.
            tt-print.ID      = cnt                      .
            tt-print.VehID   = tt-lorryStock.VehID      .  
            tt-print.itmID   = tt-lorryStock.itmID      . 
            tt-print.itmName = tt-lorryStock.itmName    . 
            tt-print.SortId  = tt-lorryStock.SortId     .
            tt-print.weight  = tt-lorryStock.Weight     . 
            tt-print.BSC     = tt-lorryStock.BSC        . 
            tt-print.BSP     = tt-lorryStock.BSP        .
            tt-print.GRRD    = tt-lorryStock.GRRD       . 
            tt-print.GRST    = tt-lorryStock.GRST       .
            tt-print.LDC     = tt-lorryStock.LDC        . 
            tt-print.LDP     = tt-lorryStock.LDP        . 
            tt-print.ULC     = tt-lorryStock.ULC        . 
            tt-print.ULP     = tt-lorryStock.ULP        . 
            tt-print.RDC     = tt-lorryStock.RDC        . 
            tt-print.RDP     = tt-lorryStock.RDP        . 
            tt-print.billedP = tt-lorryStock.billedP    .
            tt-print.Excess  = tt-lorryStock.Excess     . 
            tt-print.Short   = tt-lorryStock.Short      . 
            tt-print.CrDate  = tt-lorryStock.CrDate     .
            tt-print.TolP    = tt-lorryStock.TolP       .
    END.
    
    EMPTY TEMP-TABLE tt-lorryStock.
    
    FOR EACH tt-print.
        CREATE tt-lorryStock.
            tt-lorryStock.ID      = tt-print.ID         .
            tt-lorryStock.VehID   = tt-print.VehID      .  
            tt-lorryStock.itmID   = tt-print.itmID      . 
            tt-lorryStock.itmName = tt-print.itmName    . 
            tt-lorryStock.SortId  = tt-print.SortId     .
            tt-lorryStock.weight  = tt-print.Weight     . 
            tt-lorryStock.BSC     = tt-print.BSC        . 
            tt-lorryStock.BSP     = tt-print.BSP        .
            tt-lorryStock.GRRD    = tt-print.GRRD       . 
            tt-lorryStock.GRST    = tt-print.GRST       .
            tt-lorryStock.LDC     = tt-print.LDC        . 
            tt-lorryStock.LDP     = tt-print.LDP        . 
            tt-lorryStock.ULC     = tt-print.ULC        . 
            tt-lorryStock.ULP     = tt-print.ULP        . 
            tt-lorryStock.RDC     = tt-print.RDC        . 
            tt-lorryStock.RDP     = tt-print.RDP        . 
            tt-lorryStock.billedP = tt-print.billedP    .
            tt-lorryStock.Excess  = tt-print.Excess     . 
            tt-lorryStock.Short   = tt-print.Short      . 
            tt-lorryStock.CrDate  = tt-print.CrDate     .
            tt-lorryStock.TolP    = tt-print.TolP       .
    END.


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

