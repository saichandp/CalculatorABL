 
 /*------------------------------------------------------------------------
    File        : Calculator
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : lucas.santos
    Created     : Fri May 19 13:03:41 BRT 2023
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS src.Calculator: 
    
    DEFINE PUBLIC PROPERTY dResult AS CHARACTER NO-UNDO 
    GET.
    SET. 
    
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    CONSTRUCTOR PUBLIC Calculator (  ):
        SUPER ().
        
        THIS-OBJECT:Clear().
        
    END CONSTRUCTOR.


    /*------------------------------------------------------------------------------
     Purpose: Perform addition
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC CHARACTER Add(INPUT d1 AS DECIMAL,
                                INPUT d2 AS DECIMAL):
        dResult = STRING(d1 + d2).
        RETURN dResult.        
    END METHOD.    
    
    /*------------------------------------------------------------------------------
     Purpose: Perform subtraction
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC CHARACTER Subtract(INPUT d1 AS DECIMAL,
                                   INPUT d2 AS DECIMAL):
        dResult = STRING(d1 - d2).
        RETURN dResult.        
    END METHOD.    
    
    /*------------------------------------------------------------------------------
     Purpose: Perform multiplication
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC CHARACTER Multiply(INPUT d1 AS DECIMAL,
                                   INPUT d2 AS DECIMAL):
        dResult = STRING(d1 * d2).
        RETURN dResult.        
    END METHOD.   
     
    /*------------------------------------------------------------------------------
     Purpose: Perform Division
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC CHARACTER Divide(INPUT d1 AS DECIMAL,
                                 INPUT d2 AS DECIMAL):
        IF d2 = 0 THEN DO: 
            MESSAGE "Error! Division by zero is undetermined!"
            VIEW-AS ALERT-BOX ERROR TITLE "Invalid operation!".
            dResult = "ERROR DIVISION BY ZERO".
        END.
        ELSE dResult = STRING(d1 / d2).
        RETURN dResult.        
    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose: Clear numbers
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC VOID Clear(  ):
        dResult = "".
    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose: Gets the result
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC CHARACTER getResult(  ):
        RETURN dResult.        
    END METHOD.    
    
END CLASS.