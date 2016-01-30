&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
DEFINE VARIABLE calendr AS COM-HANDLE   NO-UNDO.
DEFINE VARIABLE tempDate AS DATE        NO-UNDO.
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE SHARED VARIABLE session_Window AS INT.
/* Local Variable Definitions ---                                       */

DEFINE TEMP-TABLE tt-itms LIKE itms.
DEFINE TEMP-TABLE tt-recipts LIKE recipts.
DEFINE TEMP-TABLE tt-bills LIKE bills.

DEFINE TEMP-TABLE tt-tols
    FIELD itm# AS INT
    FIELD tol AS INT
    .
DEFINE TEMP-TABLE tt-body
    FIELD bill# AS INT
    FIELD itm# AS INT
    FIELD qty AS INT
    .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS cmbVeh btn btnEmptyPrint btnClose 
&Scoped-Define DISPLAYED-OBJECTS cmbVeh 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlFrame-4 AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame-4 AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn 
     LABEL "Print" 
     SIZE 14 BY 1.

DEFINE BUTTON btnClose 
     LABEL "Close" 
     SIZE 14 BY 1.

DEFINE BUTTON btnEmptyPrint 
     LABEL "Print Empty" 
     SIZE 14 BY 1.

DEFINE VARIABLE cmbVeh AS INTEGER FORMAT ">>>>9":U INITIAL 0 
     LABEL "Vehicle" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEM-PAIRS "--Select Here--",0
     DROP-DOWN-LIST
     SIZE 30 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     cmbVeh AT ROW 1.54 COL 7.29 WIDGET-ID 84
     btn AT ROW 4.23 COL 3.29 WIDGET-ID 2
     btnEmptyPrint AT ROW 4.23 COL 18.43 WIDGET-ID 168
     btnClose AT ROW 4.23 COL 33.86 WIDGET-ID 166
     "Select Date" VIEW-AS TEXT
          SIZE 11 BY .85 AT ROW 2.88 COL 12.43 WIDGET-ID 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 47.72 BY 4.54
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
         TITLE              = "Daily Bills Summary"
         COLUMN             = 49.14
         ROW                = 13.31
         HEIGHT             = 4.54
         WIDTH              = 47.72
         MAX-HEIGHT         = 20.54
         MAX-WIDTH          = 124.29
         VIRTUAL-HEIGHT     = 20.54
         VIRTUAL-WIDTH      = 124.29
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
/* SETTINGS FOR COMBO-BOX cmbVeh IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame-4 ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 2.88
       COLUMN          = 24
       HEIGHT          = .81
       WIDTH           = 20
       WIDGET-ID       = 164
       HIDDEN          = no
       SENSITIVE       = yes.
/* CtrlFrame-4 OCXINFO:CREATE-CONTROL from: {20DD1B9E-87C4-11D1-8BE3-0000F8754DA1} type: DTPicker */
      CtrlFrame-4:MOVE-AFTER(cmbVeh:HANDLE IN FRAME DEFAULT-FRAME).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Daily Bills Summary */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. 
  IF THIS-PROCEDURE:PERSISTENT THEN  */ RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Daily Bills Summary */
DO:
  /* This event will close the window and terminate the procedure.  */
    APPLY "CLOSE":U TO THIS-PROCEDURE.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn C-Win
ON CHOOSE OF btn IN FRAME DEFAULT-FRAME /* Print */
DO:
    DEFINE VARIABLE tim AS INTEGER     NO-UNDO.

    IF cmbVeh = 0 THEN
    DO:
        MESSAGE "Select vehical first." VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN.
    END.

    tempDate = calendr:VALUE.
    FIND FIRST bills WHERE bills.bilDate = tempDate AND bills.vehNo = cmbVeh NO-LOCK NO-ERROR.
    IF AVAILABLE bills THEN
    DO:
       tim = TIME.
       RUN printer(tempDate,cmbVeh).
       MESSAGE "Query compleated in " + STRING(TIME - tim,"HH:MM:SS") VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.
    ELSE
    DO:
        FIND FIRST vehical WHERE ID = cmbVeh.
        MESSAGE "No Bills for " + string(tempDate) + " from vehical : " + vehical.veh# + "." VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RELEASE vehical.
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


&Scoped-define SELF-NAME btnEmptyPrint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnEmptyPrint C-Win
ON CHOOSE OF btnEmptyPrint IN FRAME DEFAULT-FRAME /* Print Empty */
DO:
    DEFINE VARIABLE tim AS INTEGER     NO-UNDO.
    tempDate = calendr:VALUE.

    IF cmbVeh = 0 THEN
    DO:
        MESSAGE "Select vehical first." VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN.
    END.

    FIND FIRST bills WHERE bills.bilDate = tempDate NO-LOCK NO-ERROR.
    IF AVAILABLE bills THEN
    DO:
        tim = TIME.
        RUN printEmptyFast(tempDate,cmbVeh).
        MESSAGE "Query compleated in " + STRING(TIME - tim,"HH:MM:SS") VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.
    ELSE
    DO:
        MESSAGE "No Bills for " + string(tempDate) + "." VIEW-AS ALERT-BOX WARNING BUTTONS OK.
    END.
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


&Scoped-define SELF-NAME CtrlFrame-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame-4 C-Win OCX.Change
PROCEDURE CtrlFrame-4.DTPicker.Change .
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

  RUN VehLoader.

  calendr = chCtrlFrame-4:DTPicker.
  calendr:VALUE = TODAY - 1.

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

OCXFile = SEARCH( "DailySummary.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chCtrlFrame-4 = CtrlFrame-4:COM-HANDLE
    UIB_S = chCtrlFrame-4:LoadControls( OCXFile, "CtrlFrame-4":U)
    CtrlFrame-4:NAME = "CtrlFrame-4":U
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "DailySummary.wrx":U SKIP(1)
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
  DISPLAY cmbVeh 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE cmbVeh btn btnEmptyPrint btnClose 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE printEmptyFast C-Win 
PROCEDURE printEmptyFast :
DEFINE VARIABLE tempTol AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER tempDate AS DATE        NO-UNDO.
DEFINE INPUT  PARAMETER tempVeh AS INTEGER     NO-UNDO.
DEFINE VARIABLE tempId AS INTEGER     NO-UNDO.

OUTPUT TO VALUE ("E:\ICS\bin\print\Daily_Bill_SummaryEmpty.txt").

RUN ttBind(tempDate,tempVeh).

    FOR EACH tt-bills BY tt-bills.bill#.
        FOR EACH tt-recipts WHERE tt-recipts.bill# = tt-bills.bill#.
            FIND FIRST tt-tols WHERE tt-tols.itm# = tt-recipts.item# NO-ERROR.
                IF AVAILABLE tt-tols THEN tt-tols.tol = tt-tols.tol + tt-recipts.pieses.
                IF NOT AVAILABLE tt-tols THEN 
                DO:
                    CREATE tt-tols.
                    ASSIGN tt-tols.itm# = tt-recipts.item# tt-tols.tol = tt-recipts.pieses.
                END.
            RELEASE tt-tols.
        END.
    END.

    tempId = 0.

    FOR EACH tt-itms by tt-itms.SortID.     
        tempId = tempId + 1.
        PUT UNFORMAT                                         
            string( tempId ) + "|" +                  
            string( tt-itms.itmName) + "|" +                 
            string( tt-itms.unitWeightKG) + "|" +            
            string( tt-itms.unitsPerCase) + "|||||||||||" .
        FIND FIRST tt-tols WHERE tt-tols.itm# = tt-itms.itmID NO-ERROR.
        IF AVAILABLE tt-tols THEN PUT UNFORMAT STRING(tt-tols.tol).
        ELSE PUT UNFORMAT "0".
        PUT UNFORMAT SKIP.
    END.

FIND FIRST vehical WHERE vehical.ID = tempVeh NO-ERROR.
    IF AVAILABLE vehical THEN 
    PUT UNFORMAT "|   Date : " + STRING(tempDate) + "   Veh : " + STRING(vehical.veh#) + " " + vehical.descrip .

OUTPUT CLOSE.
DOS SILENT START VALUE("E:\ICS\bin\print\Daily_Bill_SummaryEmpty.bat").
DOS SILENT START excel VALUE("E:\ICS\bin\print\Daily_Bill_SummaryEmpty.xls").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE printer C-Win 
PROCEDURE printer :
DEFINE VARIABLE tempTol AS INTEGER     NO-UNDO.
DEFINE INPUT PARAMETER tempDate AS DATE        NO-UNDO.
DEFINE INPUT PARAMETER tempVeh AS INTEGER     NO-UNDO.
DEFINE VARIABLE tempId AS INTEGER     NO-UNDO.


RUN ttBind(tempDate,tempVeh).

OUTPUT TO VALUE ("E:\ICS\bin\print\Daily_Bill_Summary.txt").

PUT UNFORMAT "ID|Product|Weight|Unit|".

    FOR EACH tt-bills BY tt-bills.bill#.
        PUT UNFORMAT string(tt-bills.BillNo) + "|".
        FOR EACH tt-recipts WHERE tt-recipts.bill# = tt-bills.bill#.
            FIND FIRST tt-body WHERE tt-body.bill# = tt-bills.bill# AND tt-body.itm# = tt-recipts.item# NO-ERROR.
                IF  AVAILABLE tt-body THEN 
                    tt-body.qty = tt-body.qty + tt-recipts.pieses.
                ELSE
                DO:
                CREATE tt-body.
                ASSIGN 
                   tt-body.bill# = tt-bills.bill#
                   tt-body.itm# = tt-recipts.item#
                   tt-body.qty  = tt-recipts.pieses
                    .
                END.
            RELEASE tt-body.

            FIND FIRST tt-tols WHERE tt-tols.itm# = tt-recipts.item# NO-ERROR.
                IF AVAILABLE tt-tols THEN 
                    tt-tols.tol = tt-tols.tol + tt-recipts.pieses.
                IF NOT AVAILABLE tt-tols THEN 
                DO:
                    CREATE tt-tols.
                    ASSIGN 
                        tt-tols.itm# = tt-recipts.item# 
                        tt-tols.tol = tt-recipts.pieses.
                END.
            RELEASE tt-tols.
        END.
    END.

    PUT UNFORMAT "Total|" SKIP.

    FOR EACH tt-tols WHERE tt-tols.tol > 0 BY tt-tols.itm#.
        FOR EACH tt-bills BY tt-bills.bill#.
            FIND FIRST tt-body WHERE tt-body.itm# = tt-tols.itm# AND tt-body.bill# = tt-bills.bill# NO-ERROR.
                IF NOT AVAILABLE tt-body THEN
                DO:
                CREATE tt-body.
                    tt-body.bill# = tt-bills.bill#.
                    tt-body.itm# = tt-tols.itm#.
                    tt-body.qty  = 0.
                END.
            RELEASE tt-body.
        END.
    END.
    
    tempId = 0.

    FOR EACH tt-itms by tt-itms.SortID.
        FIND FIRST tt-tols WHERE tt-tols.itm# = tt-itms.itmID NO-ERROR.
        IF AVAILABLE tt-tols AND tt-tols.tol > 0 THEN
        DO:
            tempId = tempId + 1.
            PUT UNFORMAT
                    string( tempId ) + "|" +
                    string( tt-itms.itmName) + "|" +
                    string( tt-itms.unitWeightKG) + "|" +
                    string( tt-itms.unitsPerCase) + "|".
            FOR EACH tt-body WHERE tt-body.itm# = tt-tols.itm# BY tt-body.bill#.
                IF tt-body.qty > 0 THEN PUT UNFORMAT string(tt-body.qty) + "|".
                ELSE PUT UNFORMAT "|".
            END.
            PUT UNFORMAT STRING(tt-tols.tol) SKIP.
        END.
    END.
    FIND FIRST vehical WHERE vehical.ID = tempVeh NO-ERROR.
    IF AVAILABLE vehical THEN 
    PUT UNFORMAT "|   Date : " + STRING(tempDate) + "   Veh : " + STRING(vehical.veh#) + " " + vehical.descrip .
OUTPUT CLOSE.

DOS SILENT START VALUE("E:\ICS\bin\print\Daily_Bill_Summary.bat").
DOS SILENT START excel VALUE("E:\ICS\bin\print\Daily_Bill_Summary.xlsm").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE printerEmpty C-Win 
PROCEDURE printerEmpty :
DEFINE VARIABLE tempTol AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER tempDate AS DATE        NO-UNDO.
DEFINE INPUT  PARAMETER tempVeh AS INTEGER     NO-UNDO.
DEFINE VARIABLE tempId AS INTEGER     NO-UNDO.

OUTPUT TO VALUE ("E:\ICS\bin\print\Daily_Bill_SummaryEmpty.txt").

tempId = 0.

FOR EACH itms by itms.SortID.
    tempId = tempId + 1.
    PUT UNFORMAT
        string( tempId ) + "|" +
        string( itms.itmName) + "|" +
        string( itms.unitWeightKG) + "|" +
        string( itms.unitsPerCase) + "|||||||||||" .

    tempTol = 0.

    FOR EACH bills WHERE  bills.bilDate = tempDate AND bills.vehNo = tempVeh BY bills.BillNo.
        FIND FIRST recipts WHERE recipts.bill# = bills.bill# AND recipts.item# = itms.itmID EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE recipts THEN
            DO:
               tempTol = tempTol + recipts.pieses.
            END.
    END.
    IF tempTol = 0 THEN PUT UNFORMAT "0" SKIP.
    IF tempTol > 0 THEN PUT UNFORMAT STRING(tempTol)  SKIP.
END.
FIND FIRST vehical WHERE vehical.ID = tempVeh NO-ERROR.
    IF AVAILABLE vehical THEN 
    PUT UNFORMAT SKIP "|    Date : " + STRING(tempDate) + "   Veh : " + STRING(vehical.veh#) + " " + vehical.descrip .
OUTPUT CLOSE.
DOS SILENT START VALUE("E:\ICS\bin\print\Daily_Bill_SummaryEmpty.bat").
DOS SILENT START excel VALUE("E:\ICS\bin\print\Daily_Bill_SummaryEmpty.xls").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ttBind C-Win 
PROCEDURE ttBind :
DEFINE INPUT  PARAMETER inpDate AS DATE     NO-UNDO.
DEFINE INPUT  PARAMETER inpVeh AS INTEGER     NO-UNDO.

EMPTY TEMP-TABLE tt-bills.
EMPTY TEMP-TABLE tt-recipts.
EMPTY TEMP-TABLE tt-itms.
EMPTY TEMP-TABLE tt-tols.
EMPTY TEMP-TABLE tt-body.


FOR EACH bills WHERE bills.bilDate = inpDate AND  bills.vehNo = inpVeh BY bills.bill#.
    CREATE tt-bills.
     tt-bills.bill#   = bills.bill#. 
     tt-bills.BillNo  = bills.BillNo. 
END.

FOR EACH recipts.
    CREATE tt-recipts.
     tt-recipts.bill#       =   recipts.bill#       .
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
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VehLoader C-Win 
PROCEDURE VehLoader :
FOR EACH vehical.
    cmbVeh:ADD-LAST(veh# + " - " + descrip,ID) IN FRAME DEFAULT-FRAME.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

