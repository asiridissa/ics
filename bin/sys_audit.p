FOR EACH syslog BY syslog.timeStamp DESC.
    OUTPUT TO VALUE("E:\ICS\bin\print\sys_audit.txt").
    put unformat(  string(syslog.timeStamp)  ) + "|".
        put unformat(  syslog.userName   ) + "|".
        put unformat(  syslog.usersName  ) + "|" .
        put unformat(  syslog.logDetails ) + "|" SKIP.
    OUTPUT CLOSE.
END.

DOS SILENT START notepad VALUE("E:\ICS\bin\print\sys_audit.txt").
