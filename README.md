# Floating Point Imprecision and Division by Zero Identifier

Name: Michael Tegegn
St. ID: 20170897

Description:
  The tool is designed to help catch floating point imprecision propagation and division by zero. This tool will catch floating point imprecision of 32-bit floating point numbers as compared to their 64-bit counter parts. You can freely adjust the threshold float value (i.e. the maximum tolerable deviation of the 32 bit float value from a 64 bit value) to see how precise the program under test is.
  
Dependencies:
  - The tool uses the MLGMPIDL library for arbitrary precision floating point computation. GMP specifically. 
  
Run commands:
  - Run make in the main project directory
  - ./analyzer [threshold float] [ LLVM IR file ]

Test available files from the test directory:
  - Run make in the test directory
  - cd ..
  - ./anayzer [threshold float] test/example[n].ll
