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
