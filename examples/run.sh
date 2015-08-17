#!/bin/bash
main='../src/main'
exit_code=0

cd `dirname $0`

echo "building..."
cd ../src/
omake

echo "testing..."
cd ../examples/

for file in success/*.java; do
  $main "$file"
  if [[ $? != 0 ]]; then
    printf "\e[0;31mERROR: $file\e[0m\n"
    exit_code=1
  fi
done

for file in failure/*.java; do
  $main "$file"
  if [[ $? != 1 ]]; then
    printf "\e[0;31mERROR: $file\e[0m\n"
    exit_code=1
  fi
done

exit $exit_code
