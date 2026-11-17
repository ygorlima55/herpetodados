@echo off
rem Acha o Python 3 (lancador py, python no PATH ou instalacao do usuario) e o deixa em PY.
set "PY="
if defined HERP_PYTHON if exist "%HERP_PYTHON%" set "PY=%HERP_PYTHON%" & goto :eof
for /f "delims=" %%P in ('py -3 -c "import sys;print(sys.executable)" 2^>nul') do set "PY=%%P"
if defined PY goto :eof
for /f "delims=" %%P in ('python -c "import sys;print(sys.version_info[0]>=3 and sys.executable or '')" 2^>nul') do set "PY=%%P"
if defined PY goto :eof
for /d %%D in ("%LOCALAPPDATA%\Programs\Python\Python3*" "%ProgramFiles%\Python3*") do if exist "%%~D\python.exe" set "PY=%%~D\python.exe"
