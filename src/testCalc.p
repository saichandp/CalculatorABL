
/*------------------------------------------------------------------------
    File        : testCalc.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : lucas.santos
    Created     : Fri May 19 13:13:02 BRT 2023
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

USING src.Calculator.

DEFINE VARIABLE Calc AS Calculator NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

Calc = NEW Calculator().

OUTPUT TO "D:\workspace\Calculator-OOABL\out\testCalc.out".

MESSAGE "Testing Calculator.cls" SKIP(2).
MESSAGE ">>> Test Add(5, 1)" SKIP.
MESSAGE ">>> Result: "Calc:Add(5, 1) SKIP(2).

MESSAGE ">>> Test Subtract(5, 1)" SKIP.
MESSAGE ">>> Result: "Calc:Subtract(5, 1) SKIP(2).

MESSAGE ">>> Test Multiply(5, 1)" SKIP.
MESSAGE ">>> Result: "Calc:Multiply(5, 1) SKIP(2).

MESSAGE ">>> Test Divide(5, 0)" SKIP.
MESSAGE ">>> Result: "Calc:Divide(5, 0) SKIP(2).

MESSAGE ">>> Test Divide(5, 2)" SKIP.
MESSAGE ">>> Result: "Calc:Divide(5, 2) SKIP(2).

OUTPUT CLOSE.

CATCH sErr AS Progress.Lang.Error :
    MESSAGE sErr:GetMessage(0).
    
END CATCH.