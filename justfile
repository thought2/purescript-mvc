purs_args := "--stash --censor-lib --censor-codes=ImplicitQualifiedImport"
cfg_test := "--config test.dhall"

build-strict:
    spago build --purs-args "--strict {{purs_args}}"

build:
    spago build --purs-args "{{purs_args}}"

test-strict:
    spago {{cfg_test}} test --purs-args "--strict {{purs_args}}"

test:
    spago {{cfg_test}} test --purs-args "{{purs_args}}"

clean:
    rm -rf .spago output .psa-stash

ide:
    spago {{cfg_test}} test --purs-args "{{purs_args}} --json-errors"

ci: clean build-strict test-strict

gen-readme:
    echo "" > README.md
    yarn run --silent purs-to-md --input-purs samples/01_Sample.Component1.purs --output-md - >> README.md
    yarn run --silent purs-to-md --input-purs samples/02_Sample.Component2.purs --output-md - >> README.md
    yarn run --silent purs-to-md --input-purs samples/03_Sample.Component3.purs --output-md - >> README.md
    yarn run --silent purs-to-md --input-purs samples/04_Sample.Record.Manually.purs --output-md - >> README.md
    yarn run --silent purs-to-md --input-purs samples/05_Sample.Record.Generically.purs --output-md - >> README.md

    yarn run md-magic

run:
    yarn run parcel static/index.html

format:
    purs-tidy format-in-place "src/**/*.purs"
    purs-tidy format-in-place "test/**/*.purs"
    purs-tidy format-in-place "samples/**/*.purs"


check-format:
    purs-tidy check "src/**/*.purs"
    purs-tidy check "test/**/*.purs"
    purs-tidy check "samples/**/*.purs"

gen-gifs:
    rm -rf assets/gif
    node run.js
    for file in assets/gif/*.mp4; do \
      output="${file%.*}.gif" ; \
      ffmpeg -i $file  -y -loop 0 $output ; \
    done
    rm -rf assets/gif/*.mp4