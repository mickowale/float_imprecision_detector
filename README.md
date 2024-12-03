# PreciseDetector: Floating Point Imprecision and Division by Zero Detector using LLVM OCaml binding.

**NOTE**: This project was completed as part of a course project at KAIST (Korean Advanced Institute of Science and Technology.)

## Description:
* The tool is designed to help catch floating point imprecision propagation and division by zero. Specifically, it detects if 32-bit float operations are too lossy compared to 64-bit float operations based on a tolerable amount of passed in as a parameter. You can freely adjust the threshold float value (i.e. the maximum tolerable deviation of the 32 bit float value from a 64 bit value) to see how precise the program under test is.

* **Why**: Floating point numbers are notoriously known for propagating imprecision which can easily occure as a result of multiplication, division and most notoriously exponentiation operations. Many applications will have to identify which representation to use based on their own preferences as there are tradeoffs to the options. 32-bit floats would save memory (and usually speed) but are less precise than others (double/64-bit, long-double/80-bit). Hence, finding an automated way to locate imprecision propagation would help identify if allocating the extra bits is worth it for your program.
* See slides under `./doc` for design choices and details about implementation.

## Dependencies:
  -  OCaml 4.12 or later
  - [LLVM OCaml](https://opam.ocaml.org/packages/llvm/)
  - [MLGMPIDL library](https://ocaml.org/p/mlgmpidl/1.2.5/doc/index.html) for arbitrary precision floating point computation. GMP specifically. 
  
## Run:
  - Run `make` in the main project directory
  - `./analyzer [threshold float] [ LLVM IR file ]`

### Running Examples:
  - Run `make` in the test directory
  - cd ..
  - `./anayzer [threshold float] test/example[n].ll`
