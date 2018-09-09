.onAttach=function(libname, pkgname)
{
  packageStartupMessage("
LABELLED 2.0.0: BREAKING CHANGE

Following version 2.0.0 of `haven`, `labelled()` and `labelled_spss()` now produce objects with class 'haven_labelled' and 'haven_labelled_spss', due to conflict between the previous 'labelled' class and the 'labelled' class used by `Hmisc`.

A new function `update_labelled()` could be used to convert data imported with an older version of `haven`/`labelled` to the new classes.
")
}
