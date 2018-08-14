# Contributor Guidelines

Thanks for contributing! Open source projects can only exist with support from contributors like you.

## General guidelines

- Changes to the compiler source should have accompanying tests:
  - For standalone functionality, it makes sense to have unit tests; Kit uses [hspec](http://hackage.haskell.org/package/hspec) for this. See existing tests in the "tests/" directory for examples. Hspec tests in the tests/ directory are automatically discovered.
  - For testing the compiler end-to-end, Kit has functional tests in "tests/compile-src". Any kit file placed here will be automatically compiled and run, and if you place a similarly named ".stdout" file next to the kit file, the stdout of your program will be compared to this file. Failure to compile, unsuccessful exit from the program, or stdout mismatch will fail the test. Please make sure your test is deterministic and doesn't rely on things like network IO or random numbers.
  - To run your unit/functional tests, execute `stack run` from the root directory of the repo.

- Haskell code should be formatted with [brittany](https://github.com/lspitzner/brittany). No exceptions - code style is one of the least interesting things to discuss!

## Reporting an issue

- Please open an issue on GitHub and fill in all relevant information from the issue template.

## Submitting a pull request

- Make sure your commits have clear and descriptive messages.
- Create a fork of the repository in GitHub.
- Push your new code to a branch in your fork, and make a pull request. Please give enough context so we can tell why this change should be merged, and an example of the problem it's fixing if any.
- Please watch the pull request and be responsive to questions or feedback so we can merge as soon as it's ready.
- Your commits may be squashed on merging to keep things simple, and maintainers may make minor changes like style cleanup and squash them into your commit.

## Making a feature request

TODO
