#!/bin/bash
echo Starting...
if test "$OS" = "Windows_NT"
then
  # use .Net
  echo .NET build...
  .paket/paket.bootstrapper.exe
  exit_code=$?
  if [ $exit_code -ne 0 ]; then
    echo Command failed:  paket.bootstrapper.exe
  	exit $exit_code
  fi

  .paket/paket.exe restore
  exit_code=$?
  if [ $exit_code -ne 0 ]; then
    echo Command failed: paket.exe restore
  	exit $exit_code
  fi
  
  [ ! -e build.fsx ] && .paket/paket.exe update
  [ ! -e build.fsx ] && packages/FAKE/tools/FAKE.exe init.fsx
  packages/FAKE/tools/FAKE.exe $@ --fsiargs -d:MONO build.fsx 
else
  # use mono
  echo mono build...
  mono .paket/paket.bootstrapper.exe
  exit_code=$?
  if [ $exit_code -ne 0 ]; then
    echo Command failed: paket.bootstrapper.exe
  	exit $exit_code
  fi

  mono .paket/paket.exe restore
  exit_code=$?
  if [ $exit_code -ne 0 ]; then
    echo Command failed: paket.exe restore
  	exit $exit_code
  fi

  [ ! -e build.fsx ] && mono .paket/paket.exe update
  [ ! -e build.fsx ] && mono packages/FAKE/tools/FAKE.exe init.fsx
  mono packages/FAKE/tools/FAKE.exe $@ --fsiargs -d:MONO build.fsx 
fi
