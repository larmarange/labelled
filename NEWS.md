# labelled dev

* `to_factor()`, `to_character()` and `to_labelled.factor()` now preserves variable label.

# labelled 0.2.0

* Following evolution of `haven`, `labelled` deosn't support missing values anymore (cf. https://github.com/hadley/haven/commit/4b12ff9d51ddb9e7486966b85e0bcff44992904d)

* New function `to_character()` (cf. https://github.com/larmarange/labelled/commit/3d32852587bb707d06627e56407eed1c9d5a49de)

* `to_factor()` could now be applied to a data.frame (cf. https://github.com/larmarange/labelled/commit/ce1d750681fe0c9bcd767cb83a8d72ed4c5fc5fb)

* If `data.table` is available, labelled attribute are now changed by _reference_ (cf. https://github.com/larmarange/labelled/commit/c8b163f706122844d798e6625779e8a65e5bbf41)

* `zap_labels()` added as a synonym of `remove_labels()`
