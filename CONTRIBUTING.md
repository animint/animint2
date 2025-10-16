## Communication in issues

Thanks for your interest in contributing to the animint project!
Please file an issue and tag

* `@animint/developers` for questions/feedback about code.
* `@animint/translation` for translations of documentation to non-English languages, like French.

## Avoid AI code generation

In your PRs, please avoid including code from Github Copilot or other AI code generation software. These tools create several problems, because generated code is often

* irrelevant / incorrect.
* too complex / repetitive / confusing.
* copied from other projects which may have incompatible licenses.

## Easy to review PRs

To make each PR easy to review, please limit each PR to one minimal set of changes.
Before requesting review, please click "Files changed" tab to preview what your PR will look like to a reviewer.
Make sure there are only changes that are relevant (typically the new feature code, and new test case code).

## Testing

The first commit/push in any branch/PR should ideally be only a new test case which fails using the current master code.
Only after that test fails, resulting in red X in the PR, then please push the new code which will make the test pass.

If your first push in the branch was not a test case that fails, but you pushed a test case later, then it is difficult for reviewers to verify that your test case is sufficient for testing the changes/fixes to the code. To make your PR easier to review, please demonstrate that the test case is sufficient, by following these steps:

* First push a commit which reverts the changes/fixes to the code (but keeps the test cases which should fail, resulting in a red X).
* Then push a commit which restores the changes/fixes to the code (which should result in a passing build with green check).

See [our chromote documentation](https://github.com/animint/animint2/wiki/Chromote-testing-documentation) about how to setup remote-controlled web browser testing on your local machine.
There are no visual regression tests that need to be checked before merging a PR, but it is a good idea to look at what is rendered in the remote-controlled web browser window when testing on your local machine.

## Style

* avoid line breaks in function definitions and `test_that` blocks.
* if there are repeated blocks of similar code in test cases, please create a function. Recent examples include `get_element_bbox()` and `mouseMoved()` in [helper-functions.R](https://github.com/animint/animint2/blob/master/tests/testthat/helper-functions.R).
* use `sprintf("some %s strings",value)` instead of `paste("some",value,"strings")`, see [Writing good messages in potools developrs vignette](https://cran.r-project.org/web/packages/potools/vignettes/developers.html).
* for internal functions, please do not write documentation, and please avoid code for checking types of arguments.
* Whenever possible, JavaScript code should use D3 data-bind instead of basic for loop.
* only use `return()` in R code for early exit (avoid using at end of function for normal return).
