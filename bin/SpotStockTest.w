&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnPrint btnPrintEmpty btnClose 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnClose 
     LABEL "Close" 
     SIZE 15 BY 1.12.

DEFINE BUTTON btnPrint 
     LABEL "Print" 
     SIZE 15 BY 1.12.

DEFINE BUTTON btnPrintEmpty 
     LABEL "Print Empty" 
     SIZE 15 BY 1.12.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnPrint AT ROW 3.96 COL 5.57 WIDGET-ID 2
     btnPrintEmpty AT ROW 3.96 COL 20.86 WIDGET-ID 8
     btnClose AT ROW 3.96 COL 36.29 WIDGET-ID 6
     "Print Empty : Print the empty Spot Stock Test sheet." VIEW-AS TEXT
          SIZE 44.43 BY .77 AT ROW 2.69 COL 5.86 WIDGET-ID 10
     "Print             : Print the Spot Stock Test sheet with Data." VIEW-AS TEXT
          SIZE 46 BY .77 AT ROW 1.65 COL 5.86 WIDGET-ID 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 55.43 BY 4.46
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
         TITLE              = "Spot Stock Test"
         COLUMN             = 50.14
         ROW                = 11.04
         HEIGHT             = 4.38
         WIDTH              = 55.57
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 80
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
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Spot Stock Test */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Spot Stock Test */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
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
  RUN printTxt.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPrintEmpty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPrintEmpty C-Win
ON CHOOSE OF btnPrintEmpty IN FRAME DEFAULT-FRAME /* Print Empty */
DO:
  RUN printEmpty.
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
  ENABLE btnPrint btnPrintEmpty btnClose 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE printEmpty C-Win 
PROCEDURE printEmpty :
DEFINE VARIABLE id AS INTEGER     NO-UNDO INIT 0.
OUTPUT TO VALUE("E:\ICS\bin\print\Spot_Stock_Test_.txt").
    PUT UNFORMAT "No|Product|Weight|Van C|Van P|Stock C|Stock P|Total P|Buying Price|Amount|Selling Price|Amount|" SKIP. 
        FOR EACH itms WHERE (stockP + stockC + BSC + BSP) > 0 BY itms.SortID.
           PUT UNFORMAT STRING(id + 1) + "|".
           PUT UNFORMAT STRING( itmName ) + "|" .
           PUT UNFORMAT STRING( unitWeightKG ) + "|" SKIP.
           id = id + 1.
        END.
    PUT UNFORMAT "|Total|" SKIP.
  OUTPUT CLOSE.
  DOS SILENT START VALUE("E:\ICS\bin\print\Spot_Stock_Test_.bat").
  DOS SILENT START VALUE("E:\ICS\bin\print\Spot_Stock_Test_.xlsm").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE printTxt C-Win 
PROCEDURE printTxt :
DEFINE VARIABLE tempVanC        AS INTEGER     NO-UNDO.
DEFINE VARIABLE tempVanP        AS INTEGER     NO-UNDO.
DEFINE VARIABLE tempStockC      AS INTEGER     NO-UNDO.
DEFINE VARIABLE tempStockP      AS INTEGER     NO-UNDO.
DEFINE VARIABLE tempTotalP      AS INTEGER     NO-UNDO.
DEFINE VARIABLE tempAmountBuy   AS DECIMAL     NO-UNDO.
DEFINE VARIABLE tempAmountSell  AS DECIMAL     NO-UNDO.
DEFINE VARIABLE tempNo  AS INT     NO-UNDO INIT 0.

DEFINE VARIABLE tolVanC        AS INTEGER     NO-UNDO.
DEFINE VARIABLE tolVanP        AS INTEGER     NO-UNDO.
DEFINE VARIABLE tolStockC      AS INTEGER     NO-UNDO.
DEFINE VARIABLE tolStockP      AS INTEGER     NO-UNDO.
DEFINE VARIABLE tolTotalP      AS INTEGER     NO-UNDO.
DEFINE VARIABLE tolAmountBuy   AS INTEGER     NO-UNDO.
DEFINE VARIABLE tolAmountSell  AS INTEGER     NO-UNDO.

DEFINE VARIABLE lastBSDate AS DATE        NO-UNDO.


OUTPUT TO VALUE("E:\ICS\bin\print\Spot_Stock_Test.txt").

    PUT UNFORMAT "No|Product|Weight|Van C|Van P|Stock C|Stock P|Total P|Buying Price|Amount|Selling Price|Amount|" SKIP. 

        FOR EACH itms WHERE (stockP + stockC + BSC + BSP) > 0 BY itms.SortID.
            tempVanC       = 0                    .
            tempVanP       = 0    .
            FOR EACH vehical.
                FOR EACH lorryStock WHERE lorryStock.VehID = vehical.ID.
                    ACCUMULATE lorryStock.crDate (MAX).
                END.
                lastBSDate = ACCUM MAX lorryStock.crDate.

                FIND LAST lorryStock WHERE lorryStock.itmID = itms.itmID AND lorryStock.VehID = vehical.ID AND ics.lorryStock.crDate = lastBSDate NO-ERROR.
                    IF AVAILABLE lorryStock THEN
                    DO:
                        tempVanC = tempVanC + int(lorryStock.BSC).
                        tempVanP= tempVanP + int(lorryStock.BSP).
                    END.
                RELEASE lorryStock.
            END.

            tempStockC     = itms.stockC                 .
            tempStockP     = itms.stockP                 .
            tempTotalP     = tempStockP + tempVanP + ((tempVanC + tempStockC) * unitsPerCase )                .
            tempAmountBuy  = unitPriceB * tempTotalP.
            tempAmountSell = unitPriceS * tempTotalP.

           PUT UNFORMAT STRING( tempNo + 1) + "|".
           PUT UNFORMAT STRING( itms.itmName ) + "|" .
           PUT UNFORMAT STRING( itms.unitWeightKG ) + "|" .
           PUT UNFORMAT STRING( tempVanC) + "|" .
           PUT UNFORMAT STRING( tempVanP) + "|" .
           PUT UNFORMAT STRING( tempStockC) + "|" .
           PUT UNFORMAT STRING( tempStockP) + "|" .
           PUT UNFORMAT STRING( tempTotalP) + "|" .
           PUT UNFORMAT STRING( itms.unitPriceB) + "|" .
           PUT UNFORMAT STRING( tempAmountBuy) + "|" .
           PUT UNFORMAT STRING( itms.unitPriceS) + "|" .
           PUT UNFORMAT STRING( tempAmountSell) + "|" SKIP.

           tolVanC       = tolVanC       + tempVanC        .
           tolVanP       = tolVanP       + tempVanP        .
           tolStockC     = tolStockC     + tempStockC      .
           tolStockP     = tolStockP     + tempStockP      .
           tolTotalP     = tolTotalP     + tempTotalP      .
           tolAmountBuy  = tolAmountBuy  + tempAmountBuy   .
           tolAmountSell = tolAmountSell + tempAmountSell  .

           tempNo = tempNo + 1.
        END.

    PUT UNFORMAT "|Total||" 
        + STRING( tolVanC) + "|" 
        + STRING( tolVanP) + "|" 
        + STRING( tolStockC) + "|"
        + STRING( tolStockP) + "|"
        + STRING( tolTotalP) + "||"
        + STRING( tolAmountBuy) + "||"
        + STRING( tolAmountSell) + "|" SKIP.

  OUTPUT CLOSE.

  DOS SILENT START VALUE("E:\ICS\bin\print\Spot_Stock_Test.bat").
  DOS SILENT START VALUE("E:\ICS\bin\print\Spot_Stock_Test.xlsm").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

