@echo off
setlocal
:PROMPT
SET /P AREYOUSURE=Content in the server will be override. Are you sure [Y/N] ?&
IF /I "%AREYOUSURE%" NEQ "Y" GOTO END
echo Commit will proceed...&
set PATH=C:\Program Files\WinRar;%PATH%
e:
:cd ics_vendol\db
rar a db.rar *.* -x*.bat -x*.exe
echo This will take few seconds..
svn cleanup
svn commit -m "%username%"

echo Commit completed...&
pause

:END
endlocal



