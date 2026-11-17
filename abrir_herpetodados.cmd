@echo off
chcp 65001 >nul
title Herpetodados (feche esta janela para encerrar)
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
echo  HERPETODADOS
echo  O programa abre no navegador. Deixe esta janela aberta enquanto usar;
echo  para encerrar, feche esta janela.
echo.
"%PY%" -I "%AQUI%app\servidor.py"
pause
