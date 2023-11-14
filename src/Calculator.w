&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: Basic calculator
  Description: Use OO-ABL to operate the calculator.
  Author: Lucas Bicalho 
  Created: 2023-05-23 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

USING src.Calculator.

CREATE WIDGET-POOL.



/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE Calc         AS src.Calculator NO-UNDO.
DEFINE VARIABLE tempScreen   AS CHARACTER      NO-UNDO.
DEFINE VARIABLE dVal1        AS DECIMAL        NO-UNDO.
DEFINE VARIABLE dVal2        AS DECIMAL        NO-UNDO.
DEFINE VARIABLE cTemp        AS CHARACTER      NO-UNDO.
DEFINE VARIABLE cOp          AS CHARACTER      NO-UNDO.
DEFINE VARIABLE cTempHistory AS CHARACTER      NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME frMain

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 RECT-6 cHistory bt7 bt8 bt9 ~
btDivide bt4 bt5 bt6 btMultiply bt1 bt2 bt3 btSubtract btClear bt0 btVirg ~
btAdd btResult filScreen 
&Scoped-Define DISPLAYED-OBJECTS cHistory filScreen 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt0 
     LABEL "0" 
     SIZE 6 BY 1.52.

DEFINE BUTTON bt1 
     LABEL "1" 
     SIZE 6 BY 1.52.

DEFINE BUTTON bt2 
     LABEL "2" 
     SIZE 6 BY 1.52.

DEFINE BUTTON bt3 
     LABEL "3" 
     SIZE 6 BY 1.52.

DEFINE BUTTON bt4 
     LABEL "4" 
     SIZE 6 BY 1.52.

DEFINE BUTTON bt5 
     LABEL "5" 
     SIZE 6 BY 1.52.

DEFINE BUTTON bt6 
     LABEL "6" 
     SIZE 6 BY 1.52.

DEFINE BUTTON bt7 
     LABEL "7" 
     SIZE 6 BY 1.52.

DEFINE BUTTON bt8 
     LABEL "8" 
     SIZE 6 BY 1.52.

DEFINE BUTTON bt9 
     LABEL "9" 
     SIZE 6 BY 1.52.

DEFINE BUTTON btAdd 
     LABEL "+" 
     SIZE 9.4 BY 1.52.

DEFINE BUTTON btClear 
     LABEL "CE" 
     SIZE 6 BY 3.24.

DEFINE BUTTON btDivide 
     LABEL "÷" 
     SIZE 9.4 BY 1.52.

DEFINE BUTTON btMultiply 
     LABEL "×" 
     SIZE 9.4 BY 1.52.

DEFINE BUTTON btResult 
     LABEL "=" 
     SIZE 23.4 BY 1.52.

DEFINE BUTTON btSubtract 
     LABEL "-" 
     SIZE 9.4 BY 1.52.

DEFINE BUTTON btVirg 
     LABEL "," 
     SIZE 6 BY 1.52.

DEFINE VARIABLE cHistory AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 30 BY 10 NO-UNDO.

DEFINE VARIABLE filScreen AS CHARACTER FORMAT "X(22)":U 
      VIEW-AS TEXT 
     SIZE 30 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 33.4 BY 1.57
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 33.4 BY 8.81.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 33.4 BY 11.19.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME frMain
     cHistory AT ROW 1.95 COL 38 NO-LABEL WIDGET-ID 58
     bt7 AT ROW 3.86 COL 3 WIDGET-ID 24
     bt8 AT ROW 3.86 COL 10 WIDGET-ID 22
     bt9 AT ROW 3.86 COL 17 WIDGET-ID 20
     btDivide AT ROW 3.86 COL 24 WIDGET-ID 12
     bt4 AT ROW 5.52 COL 3 WIDGET-ID 28
     bt5 AT ROW 5.52 COL 10 WIDGET-ID 26
     bt6 AT ROW 5.52 COL 17 WIDGET-ID 30
     btMultiply AT ROW 5.52 COL 24 WIDGET-ID 2
     bt1 AT ROW 7.19 COL 3 WIDGET-ID 34
     bt2 AT ROW 7.19 COL 10 WIDGET-ID 32
     bt3 AT ROW 7.19 COL 17 WIDGET-ID 36
     btSubtract AT ROW 7.19 COL 24 WIDGET-ID 8
     btClear AT ROW 8.86 COL 3 WIDGET-ID 16
     bt0 AT ROW 8.86 COL 10 WIDGET-ID 38
     btVirg AT ROW 8.86 COL 17 WIDGET-ID 40
     btAdd AT ROW 8.86 COL 24 WIDGET-ID 10
     btResult AT ROW 10.57 COL 10 WIDGET-ID 14
     filScreen AT ROW 2.19 COL 3.4 NO-LABEL WIDGET-ID 44
     "History" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 1.05 COL 37.8 WIDGET-ID 56
     "Calculator LB-567" VIEW-AS TEXT
          SIZE 17.6 BY .62 AT ROW 1.19 COL 18.8 RIGHT-ALIGNED WIDGET-ID 46
     RECT-1 AT ROW 1.91 COL 1.6 WIDGET-ID 18
     RECT-2 AT ROW 3.62 COL 1.6 WIDGET-ID 42
     RECT-6 AT ROW 1.24 COL 36.2 WIDGET-ID 54
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 69.8 BY 11.62
         BGCOLOR 8  WIDGET-ID 100.


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
         TITLE              = "Calc"
         HEIGHT             = 11.62
         WIDTH              = 69.8
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 80
         RESIZE             = YES
         SCROLL-BARS        = NO
         STATUS-AREA        = NO
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = YES
         THREE-D            = YES
         MESSAGE-AREA       = NO
         SENSITIVE          = YES.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME frMain
   FRAME-NAME                                                           */
ASSIGN 
       cHistory:READ-ONLY IN FRAME frMain        = TRUE.

/* SETTINGS FOR FILL-IN filScreen IN FRAME frMain
   ALIGN-L                                                              */
/* SETTINGS FOR TEXT-LITERAL "Calculator LB-567"
          SIZE 17.6 BY .62 AT ROW 1.19 COL 18.8 RIGHT-ALIGNED           */

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Calc */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Calc */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME





&Scoped-define SELF-NAME bt0
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt0 C-Win
ON CHOOSE OF bt0 IN FRAME frMain /* 0 */
DO:
    ASSIGN 
        cTemp = cTemp + "0"
        filScreen:SCREEN-VALUE IN FRAME frMain = cTemp. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt1 C-Win
ON CHOOSE OF bt1 IN FRAME frMain /* 1 */
DO:
    ASSIGN 
        cTemp = cTemp + "1"
        filScreen:SCREEN-VALUE IN FRAME frMain = cTemp.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt2 C-Win
ON CHOOSE OF bt2 IN FRAME frMain /* 2 */
DO:
    ASSIGN 
        cTemp = cTemp + "2"
        filScreen:SCREEN-VALUE IN FRAME frMain = cTemp.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt3 C-Win
ON CHOOSE OF bt3 IN FRAME frMain /* 3 */
DO:
    ASSIGN 
        cTemp = cTemp + "3"
        filScreen:SCREEN-VALUE IN FRAME frMain = cTemp.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt4 C-Win
ON CHOOSE OF bt4 IN FRAME frMain /* 4 */
DO:
    ASSIGN 
        cTemp = cTemp + "4"
        filScreen:SCREEN-VALUE IN FRAME frMain = cTemp.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt5 C-Win
ON CHOOSE OF bt5 IN FRAME frMain /* 5 */
DO:
    ASSIGN 
        cTemp = cTemp + "5"
        filScreen:SCREEN-VALUE IN FRAME frMain = cTemp.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt6 C-Win
ON CHOOSE OF bt6 IN FRAME frMain /* 6 */
DO:
    ASSIGN 
        cTemp = cTemp + "6"
        filScreen:SCREEN-VALUE IN FRAME frMain = cTemp.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt7 C-Win
ON CHOOSE OF bt7 IN FRAME frMain /* 7 */
DO:
    ASSIGN 
        cTemp = cTemp + "7"
        filScreen:SCREEN-VALUE IN FRAME frMain = cTemp.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt8 C-Win
ON CHOOSE OF bt8 IN FRAME frMain /* 8 */
DO:
    ASSIGN 
        cTemp = cTemp + "8"
        filScreen:SCREEN-VALUE IN FRAME frMain = cTemp.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt9 C-Win
ON CHOOSE OF bt9 IN FRAME frMain /* 9 */
DO:
    ASSIGN 
        cTemp = cTemp + "9"
        filScreen:SCREEN-VALUE IN FRAME frMain = cTemp.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btAdd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btAdd C-Win
ON CHOOSE OF btAdd IN FRAME frMain /* + */
DO:
    ASSIGN filScreen. 
    cOp = "+". 
    RUN pStoreValues.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btClear
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btClear C-Win
ON CHOOSE OF btClear IN FRAME frMain /* CE */
DO:
    RUN pClear.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btDivide
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btDivide C-Win
ON CHOOSE OF btDivide IN FRAME frMain /* ÷ */
DO:
    ASSIGN filScreen.
    cOp = "/".
    RUN pStoreValues.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btMultiply
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btMultiply C-Win
ON CHOOSE OF btMultiply IN FRAME frMain /* × */
DO:
    ASSIGN filScreen.
    cOp = "*".
    RUN pStoreValues.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btResult
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btResult C-Win
ON CHOOSE OF btResult IN FRAME frMain /* = */
DO:
    ASSIGN filScreen.
    RUN pStoreValues.  
    ASSIGN cTempHistory = cTempHistory + STRING(dVal1) + " " + cOp + " " + STRING(dVal2) + " = " + CHR(10). 
    CASE cOp:
        WHEN "+" THEN ASSIGN tempScreen = Calc:Add(dVal1, dVal2).
        WHEN "-" THEN ASSIGN tempScreen = Calc:Subtract(dVal1, dVal2).
        WHEN "*" THEN ASSIGN tempScreen = Calc:Multiply(dVal1, dVal2).
        WHEN "/" THEN ASSIGN tempScreen = Calc:Divide(dVal1, dVal2).
        OTHERWISE MESSAGE "Invalid Operation!"
                  VIEW-AS ALERT-BOX ERROR TITLE "Error!".
    END CASE.
    
    ASSIGN 
        tempScreen   = (IF SUBSTRING (tempScreen,1,1) = "." THEN "0" + tempScreen ELSE tempScreen)
        filScreen:SCREEN-VALUE IN FRAME frMain = tempScreen //STRING(, ">>>,>>>,>>9.99")
        cTempHistory = cTempHistory + tempScreen + CHR(10) + CHR(10)
        tempScreen   = ""
        dVal1        = 0
        dVal2        = 0.
    
    cHistory:SCREEN-VALUE IN FRAME frMain = cTempHistory.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btSubtract
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btSubtract C-Win
ON CHOOSE OF btSubtract IN FRAME frMain /* - */
DO:
    ASSIGN filScreen. 
    cOp = "-". 
    RUN pStoreValues.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btVirg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btVirg C-Win
ON CHOOSE OF btVirg IN FRAME frMain /* , */
DO:
    IF NUM-ENTRIES (cTemp, ".") < 2 THEN
    ASSIGN 
        cTemp = cTemp + "."
        filScreen:SCREEN-VALUE IN FRAME frMain = cTemp.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */
Calc = NEW Calculator().


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
  DISPLAY cHistory filScreen 
      WITH FRAME frMain IN WINDOW C-Win.
  ENABLE RECT-1 RECT-2 RECT-6 cHistory bt7 bt8 bt9 btDivide bt4 bt5 bt6 
         btMultiply bt1 bt2 bt3 btSubtract btClear bt0 btVirg btAdd btResult 
         filScreen 
      WITH FRAME frMain IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-frMain}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pClear C-Win 
PROCEDURE pClear :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    ASSIGN tempScreen = ""
           filScreen:SCREEN-VALUE IN FRAME frMain  = ""
           dVal1      = 0
           dVal2      = 0.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pStoreValues C-Win 
PROCEDURE pStoreValues :
/*------------------------------------------------------------------------------
 Purpose: Store values in the temp variables
 Notes:
------------------------------------------------------------------------------*/
    IF dVal1 = 0 AND dVal2 = 0 THEN DO: 
        ASSIGN 
            dVal1 = DECIMAL(cTemp)
            cTemp = "".
    END.
    ELSE IF dVal1 > 0 AND dVal2 = 0 THEN DO:
        ASSIGN 
            dVal2 = DECIMAL(cTemp)
            cTemp = "".
    END. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

