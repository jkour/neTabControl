@echo off
rem 1:The directory to execute msbuild (project directory)
set runDir=%1

rem 2:The full path of rsvars.bat
set rsvarsDir=%2

rem 3:The project file
set projFile=%3

rem 4:The compile platform
set currPlatform=%4

rem 5:BPL output
set BPLDir=%5
echo %BPLDir%

rem 6:DCU output
set DCUDir=%6
echo %DCUDir%


call %rsvarsDir%
set FullProjPath=%runDir%\%projFile%
echo %FullProjPath%

set FullDCUPathDebug=%DCUDir%\%currPlatform%\Debug
echo FullDCUPathDebug

set FullDCUPathRelease=%DCUDir%\%currPlatform%\Release
echo FullDCUPathRelease

%FrameworkDir%\msbuild.exe %FullProjPath% /p:platform=%currPlatform% /p:config=Debug /p:DCC_BPLOutput=%BPLDir% /p:DCC_DCUOutput=%FullDCUPathDebug%
%FrameworkDir%\msbuild.exe %FullProjPath% /p:platform=%currPlatform% /p:config=Release /p:DCC_BPLOutput=%BPLDir% /p:DCC_DCUOutput=%FullDCUPathRelease%
