#/usr/bin/env bash

exceptions=["math_module.epos"]
for file in *.epos; do
  if [[ ! " ${exceptions[@]} " =~ " ${file} " ]]; then
    echo "Testing ${file}"
    result=$(../result/bin/epos -r ${file})
    echo "${file}:: ${result}" >>test-results.txt
  fi
done
