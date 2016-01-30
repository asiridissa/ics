PROPATH = "E:\ICS\bin," + PROPATH.
CONNECT -db E:\ICS\db\ics -1 NO-ERROR.

DEFINE NEW GLOBAL SHARED VARIABLE session_User AS CHARACTER INIT "users".
DEFINE NEW GLOBAL SHARED VARIABLE session_UserType AS CHARACTER INIT "Accountant".
DEFINE VARIABLE err AS INT   NO-UNDO.

IF ERROR-STATUS:ERROR THEN
DO:
    DO err = 1 TO ERROR-STATUS:NUM-MESSAGES:
        IF ERROR-STATUS:GET-NUMBER(err) = 263 THEN
            MESSAGE "Program already running !" SKIP VIEW-AS ALERT-BOX ERROR BUTTONS OK.
        IF ERROR-STATUS:GET-NUMBER(err) <> 263 THEN
            MESSAGE "Serious error occured. Contact Your sytem Administrator !" SKIP
                    "Error status : " ERROR-STATUS:GET-MESSAGE(err) 
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    END.
    QUIT.
END.
ELSE
DO:
    RUN value("Tree.r").
    DISCONNECT ics.
    QUIT.
END.
