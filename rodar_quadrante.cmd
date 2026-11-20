@echo off
chcp 65001 >nul
title Quadrante
setlocal
set "AQUI=%~dp0"
call "%AQUI%app\acha_r.cmd"
if not defined RSCRIPT (
  echo Nao achei o R. Rode primeiro o instalar.cmd.
  pause & exit /b 1
)
set "PLANILHA=%~1"
if "%PLANILHA%"=="" (
  echo.
  echo  QUADRANTE - analises de comunidade no R
  echo  Arraste a planilha .xlsx para cima deste arquivo, ou cole o caminho abaixo.
  echo.
  set /p "PLANILHA=Planilha: "
)
set "PLANILHA=%PLANILHA:"=%"
if not exist "%PLANILHA%" (
  echo Arquivo nao encontrado: "%PLANILHA%"
  pause & exit /b 1
)
echo.
echo Rodando as analises (leva alguns minutos)...
"%RSCRIPT%" "%AQUI%R\quadrante.R" "%PLANILHA%"
if errorlevel 1 (
  echo.
  echo Deu erro. A mensagem acima diz onde.
  pause & exit /b 1
)
for %%F in ("%PLANILHA%") do set "SAIDA=%%~dpFQuadrante_%%~nF"
echo.
echo Pronto. Abrindo o painel: "%SAIDA%\painel.html"
start "" "%SAIDA%\painel.html"
timeout /t 5 >nul
