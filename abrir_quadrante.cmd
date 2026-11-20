@echo off
chcp 65001 >nul
title Quadrante (feche esta janela para encerrar)
set "AQUI=%~dp0"
call "%AQUI%app\acha_python.cmd"
if not defined PY (
  echo Nao achei o Python. Rode primeiro o instalar.cmd.
  pause & exit /b 1
)
if not exist "%AQUI%r-pacotes\vegan" if not exist "C:\DadosBio\envs\R-herpetodados\vegan" (
  echo Os pacotes do R ainda nao foram instalados. Rode primeiro o instalar.cmd.
  pause & exit /b 1
)
echo.
echo  QUADRANTE
echo  O programa abre na janela do Quadrante. Ao fechar aquela janela,
echo  o programa encerra sozinho (cerca de 1 minuto depois).
echo.
"%PY%" -I "%AQUI%app\servidor.py"
if errorlevel 3 if not errorlevel 4 exit /b 0
if not errorlevel 1 exit /b 0
pause
