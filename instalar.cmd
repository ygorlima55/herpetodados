@echo off
chcp 65001 >nul
title Herpetodados - instalacao
setlocal
set "AQUI=%~dp0"
echo.
echo  HERPETODADOS - instalacao
echo  ==========================
echo  Precisa de: R (4.3 ou mais novo), Python 3 e os pacotes do R (baixados agora, ~150 MB).
echo  Nada e enviado para fora do seu computador; as planilhas ficam na pasta "levantamentos".
echo.

rem ---- 1. R ----
echo [ 10%%] Procurando o R...
call "%AQUI%app\acha_r.cmd"
if defined RSCRIPT (
  echo [ok] R encontrado: %RSCRIPT%
) else (
  echo [falta] R nao encontrado.
  call :oferece_winget "RProject.R" "R" "https://cran.r-project.org/bin/windows/base/"
  call "%AQUI%app\acha_r.cmd"
  if not defined RSCRIPT ( echo Instale o R e rode este instalador de novo. & pause & exit /b 1 )
)

rem ---- 2. Python ----
echo [ 30%%] Procurando o Python...
call "%AQUI%app\acha_python.cmd"
if defined PY (
  echo [ok] Python encontrado: %PY%
) else (
  echo [falta] Python 3 nao encontrado.
  call :oferece_winget "Python.Python.3.13" "Python" "https://www.python.org/downloads/"
  call "%AQUI%app\acha_python.cmd"
  if not defined PY ( echo Instale o Python e rode este instalador de novo. & pause & exit /b 1 )
)

rem ---- 3. pacotes do R ----
echo.
echo [ 50%%] Instalando os pacotes do R na pasta r-pacotes (so na primeira vez; leva alguns minutos)...
"%RSCRIPT%" "%AQUI%R\instalar_pacotes.R"
if errorlevel 1 ( echo. & echo Os pacotes nao instalaram. Confira a internet e rode de novo. & pause & exit /b 1 )

echo.
echo [100%%] Tudo pronto. Para usar: dois cliques em "abrir_herpetodados.cmd".
echo Para testar, solte a planilha da pasta "exemplo" na pagina que abrir.
pause
exit /b 0

:oferece_winget
where winget >nul 2>nul
if errorlevel 1 (
  echo   Baixe e instale o %~2 por este endereco: %~3
  start "" "%~3"
  pause
  exit /b 0
)
set "RESP="
set /p "RESP=  Instalar o %~2 agora pelo winget (gerenciador de programas do Windows)? [S/N] "
if /i "%RESP%"=="S" (
  winget install --id %~1 -e --accept-package-agreements --accept-source-agreements
) else (
  echo   Baixe e instale o %~2 por este endereco: %~3
  start "" "%~3"
  pause
)
exit /b 0
