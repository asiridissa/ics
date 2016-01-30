DEFINE NEW GLOBAL SHARED VARIABLE session_UsersName AS CHARACTER.
DEFINE NEW GLOBAL SHARED VARIABLE session_User AS CHARACTER.
DEFINE INPUT  PARAMETER logData AS CHARACTER   NO-UNDO.
                  
CREATE syslog.
ASSIGN
    syslog.logDetails = logData
    syslog.timeStamp  = DATETIME(TODAY,MTIME)
    syslog.userName   = session_UsersName
    syslog.usersName   = session_User
    .
