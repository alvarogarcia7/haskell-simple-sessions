#!/bin/bash

set -e
set -o pipefail


projects=`ls -dp * | grep /`

for project in $projects; do
  cd $project
  if [[ -e "Makefile" ]]; then
      if [[ -e ".projectignore" ]]; then
        continue
      fi
    echo "Executing project $project"
    make test
  else
      echo "No makefile in $project. Skipping."
  fi
  cd ..
done


