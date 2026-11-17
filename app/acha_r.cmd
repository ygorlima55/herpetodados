@echo off
rem Acha o Rscript.exe mais novo instalado (Arquivos de Programas ou pasta do usuario) e o deixa em RSCRIPT.
set "RSCRIPT="
if defined HERP_RSCRIPT if exist "%HERP_RSCRIPT%" set "RSCRIPT=%HERP_RSCRIPT%" & goto :eof
for /d %%D in ("%ProgramFiles%\R\R-*" "%LOCALAPPDATA%\Programs\R\R-*") do (
  if exist "%%~D\bin\x64\Rscript.exe" set "RSCRIPT=%%~D\bin\x64\Rscript.exe"
)
if not defined RSCRIPT for %%X in (Rscript.exe) do if not "%%~$PATH:X"=="" set "RSCRIPT=%%~$PATH:X"
