NEWS
============

v0.2.7 2021-12-06
------------------

* Documentation fixes for `TokenStream`
* Fix bug in `read_while()` and `read()` when first token matches
* Refactor `read_while()`, `read_until()` to use `rle()` for an easier way of getting
  the logic correct.


v0.2.6 2021-09-16
------------------

* Add `...` args to `lex()` which are passed through to `stringi::stri_match_all()`


v0.2.5 2021-09-01
------------------

* Increase test coverage to 1005 of the code
* Include a base R variation of the R6 TokenStream by using environments
  directly.


v0.2.4 2021-08-30
------------------

* Switch to `stringi` instead of `stringr` as it is a lighter dependency


v0.2.3 2020-12-12
------------------

* Bug-fixing of 'consume_until'
* Improved print statement for TokenStream
* renamed `regex` to `re` to avoid clash with `stringr`


v0.2.2 2020-12-10
------------------

* Stricter checks on 'regex_idx' validity


v0.2.1 2020-12-08
------------------

* Bug fixes
* Added initial `PBRT` vignette


v0.2.0 2020-12-05
------------------

* Initial release
* Major update + API changes from the now defunct minilexer
