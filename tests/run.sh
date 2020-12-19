#!/bin/bash -e

echo "> Compiling LAM..."
cargo run -- --version

echo "> Compiling test suite..."
erlc *.erl
erlc -S *.erl

echo "> Generating .expect files..."
for erl in $(find . -name "*.erl"); do
  escript $erl > $erl.expect
done

echo "> Running tests..."
for erl in $(find ./ -name "*.erl"); do
  module_name=$(echo $erl | sed 's|./||g' | sed 's/.erl//g')
  echo -n "  * ${module_name}..."
  bin_name="./${module_name}.exe"
  cargo run -- build \
    "${module_name}.beam" \
    --output $bin_name \
    --target native \
    --entrypoint $module_name
  $bin_name > $erl.actual

  diff=$(
    diff $erl.actual $erl.expect >/dev/null;
    echo $?
  )
  if [[ $diff -eq 0 ]]; then
    rm -f $erl.actual
    echo "OK"
  else
    echo $erl >> ._to_update
    echo "ERR"
    echo "Diff: "
    diff --color $erl.actual $erl.expect
    echo ""
    echo ""
  fi
done
