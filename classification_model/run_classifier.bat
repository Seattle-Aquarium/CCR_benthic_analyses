@echo off
REM ---------------------------------------------------------------
REM  CCR Benthic Classifier - launcher
REM  Double-click this file to open the app.
REM  Needs the `rov_env` conda environment (see requirements.txt).
REM ---------------------------------------------------------------
setlocal
cd /d "%~dp0"

REM Prefer rov_env, which is where ultralytics and torch actually live.
set "PY="
for %%P in (
  "%LOCALAPPDATA%\anaconda3\envs\rov_env\pythonw.exe"
  "%USERPROFILE%\anaconda3\envs\rov_env\pythonw.exe"
  "%LOCALAPPDATA%\miniconda3\envs\rov_env\pythonw.exe"
) do if not defined PY if exist %%P set "PY=%%~P"

if not defined PY (
  for /f "delims=" %%P in ('where pythonw 2^>nul') do if not defined PY set "PY=%%P"
)
if not defined PY (
  echo Could not find Python. Activate the rov_env environment and run:
  echo     python -m classifier
  pause
  exit /b 1
)

"%PY%" -m classifier
if errorlevel 1 (
  echo.
  echo The app exited with an error. Running again with the console visible:
  echo.
  "%PY:pythonw.exe=python.exe%" -m classifier
  pause
)
endlocal
