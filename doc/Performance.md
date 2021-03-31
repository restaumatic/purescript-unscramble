# Performance

## Benchmarking

`bench/Micro.purs` contains a microbenchmark exercising decoding various structures.
This is done using synthetic datatypes, to test how the operations behave with regards to the size of the datatype. For example `R3` is a record with 3 fields, `R10` is a record with 10 fields, `Enum10` is an enum with 10 constructors, and so on.

Decoding is performed using multiple libraries (currently `unscramble`, `argonaut`, `foreign-generic` and `simple-json`) for comparison.

Use `npm run bench-micro` to run it using Node.js.

You can pass additional command line arguments to filter the cases executed. For example, `npm run bench-micro Enum10 Enum30` will run only benchmarks related to Enum decoding.
