@echo off
REM ---------------------------------------------------------------------------
REM  Kelp Quest - launcher
REM
REM  Double-click this file. On the first run it builds a private Python
REM  environment and installs what it needs, which takes several minutes
REM  (torch is a large download); after that it starts straight away.
REM
REM  You need Python 3.10 or newer installed. Nothing else -- no conda
REM  environment, no manual pip, no PATH setup.
REM ---------------------------------------------------------------------------
setlocal EnableExtensions
cd /d "%~dp0"

REM The environment lives outside the repo on purpose. This checkout sits in a
REM OneDrive folder, and a virtualenv there would be thousands of files for the
REM sync client to chew through forever.
set "ENV_ROOT=%LOCALAPPDATA%\KelpQuest"
set "VENV=%ENV_ROOT%\venv"
set "VPY=%VENV%\Scripts\python.exe"
set "VPYW=%VENV%\Scripts\pythonw.exe"

REM ---- 1. a working environment already? --------------------------------
if exist "%VPY%" (
  "%VPY%" -c "import kelpquest.gui.app" >nul 2>&1
  if not errorlevel 1 goto run
)

REM ---- 2. find a Python that actually works -----------------------------
REM Candidates are tested, not trusted. A partial install still leaves a
REM python.exe on disk that cannot find its own standard library, and picking
REM it produces a baffling error about _tkinter rather than an obvious "this
REM Python is broken". What is on PATH is tried first.
set "SYS_PY="
for /f "delims=" %%P in ('where python 2^>nul') do call :probe "%%~P"
for /f "delims=" %%P in ('where python3 2^>nul') do call :probe "%%~P"
call :probe "%LOCALAPPDATA%\Programs\Python\Python313\python.exe"
call :probe "%LOCALAPPDATA%\Programs\Python\Python312\python.exe"
call :probe "%LOCALAPPDATA%\Programs\Python\Python311\python.exe"
call :probe "%LOCALAPPDATA%\anaconda3\python.exe"
call :probe "%USERPROFILE%\anaconda3\python.exe"
call :probe "C:\Program Files\Python313\python.exe"
call :probe "C:\Program Files\Python312\python.exe"
call :probe "C:\Program Files\Python311\python.exe"

if not defined SYS_PY (
  echo.
  echo Could not find a working Python 3.10 or newer with tkinter.
  echo.
  echo Install one from https://www.python.org/downloads/ and tick both
  echo   [x] tcl/tk and IDLE
  echo   [x] Add python.exe to PATH
  echo then run this file again.
  echo.
  pause
  exit /b 1
)

REM ---- 3. build the environment ------------------------------------------
echo.
echo First run: setting up a private Python environment.
echo Using: %SYS_PY%
echo Into : %VENV%
echo.
echo This takes several minutes and happens only once.
echo.
if not exist "%ENV_ROOT%" mkdir "%ENV_ROOT%"
if not exist "%VPY%" (
  "%SYS_PY%" -m venv "%VENV%"
  if errorlevel 1 goto envfail
)
"%VPY%" -m pip install --upgrade pip --quiet --disable-pip-version-check

REM torch first, from the CPU wheel index. Letting ultralytics pull it from
REM PyPI instead fetches the CUDA build -- several gigabytes, on laptops with
REM no GPU to use it.
"%VPY%" -c "import torch" >nul 2>&1
if errorlevel 1 (
  echo Installing PyTorch ^(CPU build^)...
  "%VPY%" -m pip install torch torchvision --index-url https://download.pytorch.org/whl/cpu --quiet --disable-pip-version-check
  if errorlevel 1 goto envfail
)

echo Installing Kelp Quest...
"%VPY%" -m pip install -r requirements-app.txt --quiet --disable-pip-version-check
if errorlevel 1 goto envfail
echo.
echo Setup complete.
echo.

REM ---- 4. run -------------------------------------------------------------
:run
set "PYTHONPATH=%~dp0"
"%VPYW%" -m kelpquest
if errorlevel 1 (
  echo.
  echo Kelp Quest exited with an error. Running again with the console visible:
  echo.
  set "KELPQUEST_DEBUG=1"
  "%VPY%" -m kelpquest
  echo.
  pause
)
endlocal
exit /b

REM ---------------------------------------------------------------------------
REM  :probe "<path to python.exe>"
REM  Accepts the first candidate that is 3.10+ and can import tkinter.
REM ---------------------------------------------------------------------------
:probe
if defined SYS_PY exit /b
if "%~1"=="" exit /b
if not exist "%~1" exit /b
"%~1" -c "import sys, tkinter, venv; sys.exit(0 if sys.version_info >= (3, 10) else 1)" >nul 2>&1
if errorlevel 1 exit /b
set "SYS_PY=%~1"
exit /b

:envfail
echo.
echo Could not build the environment. The output above says why.
echo.
echo A common cause is no network access the first time this is run --
echo the packages have to be downloaded once.
echo.
pause
exit /b 1
