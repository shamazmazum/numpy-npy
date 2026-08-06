[![CI tests](https://github.com/shamazmazum/numpy-npy/actions/workflows/test.yml/badge.svg)](https://github.com/shamazmazum/numpy-npy/actions/workflows/test.yml)
# numpy-npy

This library allows Lisp programs to read and write arrays in the Numpy
file format.

The API is deliberately simple and consists of two functions: `load-array`
and `store-array`.

This library is forked from marcoheisig/numpy-file-format and has the following
improvements:

* Special floating point values like NaN and ±∞ are supported.
* Endianness conversion is supported.
* Undocumented multibyte dtype format `b1` is used to read bit arrays (numpy
  uses it instead of `?`).

## Benchmarks

### Reading

```lisp
(time
 (let ((foo (numpy-npy:load-array "~/test/test.npy")))
   (values
    (array-dimensions foo)
    (array-element-type foo))))
```

With `float64` type:

~~~~
Evaluation took:
  15.503 seconds of real time
  15.504563 seconds of total run time (12.898722 user, 2.605841 system)
  [ Real times consist of 0.105 seconds GC time, and 15.398 seconds non-GC time. ]
  [ Run times consist of 0.105 seconds GC time, and 15.400 seconds non-GC time. ]
  100.01% CPU
  58,912,245,538 processor cycles
  23,999,991,824 bytes consed

(1000 1000 1000)
DOUBLE-FLOAT
~~~~

With `uint32` type:

~~~~
Evaluation took:
  6.897 seconds of real time
  6.897962 seconds of total run time (1.212562 user, 5.685400 system)
  [ Real times consist of 0.092 seconds GC time, and 6.805 seconds non-GC time. ]
  [ Run times consist of 0.092 seconds GC time, and 6.806 seconds non-GC time. ]
  100.01% CPU
  26,211,563,962 processor cycles
  4,000,000,016 bytes consed
  
(1000 1000 1000)
(UNSIGNED-BYTE 32)
~~~~

### Writing

```lisp
(time
 (let ((array (make-array '(1000 1000 1000) :element-type 'TYPE)))
   (numpy-npy:store-array array "~/test/test.npy")
   t))
```

With `TYPE = double-float`:

~~~~
Evaluation took:
  17.420 seconds of real time
  17.422503 seconds of total run time (14.801434 user, 2.621069 system)
  [ Real times consist of 0.254 seconds GC time, and 17.166 seconds non-GC time. ]
  [ Run times consist of 0.254 seconds GC time, and 17.169 seconds non-GC time. ]
  100.02% CPU
  66,195,554,602 processor cycles
  23,999,991,696 bytes consed
  
T
~~~~

With `TYPE = (unsigned-byte 32)`:

~~~~
Evaluation took:
  2.338 seconds of real time
  2.338344 seconds of total run time (1.023899 user, 1.314445 system)
  [ Real times consist of 0.251 seconds GC time, and 2.087 seconds non-GC time. ]
  [ Run times consist of 0.251 seconds GC time, and 2.088 seconds non-GC time. ]
  100.00% CPU
  8,887,564,412 processor cycles
  4,000,000,016 bytes consed
  
T
~~~~
