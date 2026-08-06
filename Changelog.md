# Changelog

## 0.4

* Improved speed of all I/O operations.
* Now `npy-error` and `npy-warning` are signaled in the exceptional situations
  instead of `simple-error` and `simple-warning`.

## 0.3

* Fortran order is supported in LOAD-ARRAY (though it's not very efficient).

## 0.2

* Reading and writing of bit arrays and octet arrays (signed and unsigned) are
  much faster now.
