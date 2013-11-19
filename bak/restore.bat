@echo off
del E:\ICS\db /f /s /q
xcopy E:\ICS\bak\245657958486156 E:\ICS\db /s /q /g /h /r /y
@echo off
exit