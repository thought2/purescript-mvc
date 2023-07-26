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
    yarn run --silent purs-to-md --input-purs samples/06_Sample.Variant.Manually.purs --output-md - >> README.md
    yarn run --silent purs-to-md --input-purs samples/07_Sample.Variant.Generically.purs --output-md - >> README.md

    yarn run md-magic

gen-purs-docs:
    FILE=.spago/package.json; echo '{}' > $FILE; spago docs; echo '{"type": "module"}' > $FILE

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

gen-imgs:
    rm -rf assets/img
    mkdir -p assets/img
    node scripts/run-puppeteer.js
    
preview-md:
    grip