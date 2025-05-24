build:
    dune build

test:
    dune runtest

clean:
    dune clean

buildc: clean build 

testc: clean test

demo: buildc
    dune exec demo
