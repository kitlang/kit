`kit` is the build tool for the Kit programming language; it invokes `kitc` or `kvm` to build your project.


Building
--------

If you already have `kit`, it can rebuild itself by running `kit` in the project directory. Use `kit build`; be warned that if you use `kit` or `kit run` Kit will be stuck in a rebuilding loop indefinitely.

If not, you can bootstrap `kit` with: `kitc kit -o ./kit`


Usage
-----

`kit` looks for a project file called kit.yaml, which describes your project's configuration. See the kit.yaml in this directory for an example.

kit.yaml should specify one or more targets:

```yaml
targets:
  release:
    # ...
```

Each target section can specify any valid kit.yaml options, which will only take effect when building the given target. If no target is specified, `kit` will default to the first.

To invoke `kit` with a specific target, use `kit TARGET` or `kit (build|run) TARGET`. e.g. if you have a `test` target, run `kit test` to build and run the test entry point.
