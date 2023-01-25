This is the provided git repository for the WACC compilers lab. You should work
in this repository regularly committing and pushing your work back to GitLab.

# Provided files/directories
test line.
## project

The project directory contains the `build.properties` and `plugins.sbt` files
used by SBT:

* `build.properties`: sets which version of SBT will be used for the project
* `plugins.sbt`: adds plugins to SBT, by default you probably want `sbt-assembly`,
                 which can be used to compile "Fat Runnable Jars", and then I've
                 added `sbt-git` as well, which is handy in that it allows you
                 to run `git` commands directly in a running `sbt` session. Using
                 a _fat jar_ is useful, as it doesn't require any classpath magic when
                 running the program -- all dependencies are packaged within the jar.

## src/main

The src/main directory is where you code for your compiler should go, and just
contains a README and a stub hello world file with a simple calculator inside.

## src/test
The src/test directory is where you should put the code for your tests, which
can be ran via `sbt test`. The suggested framework is `scalatest`, the dependency
for which has already been included.

## build.sbt
The `build.sbt` is the definition of your project's build requirements. By default,
this template has added the latest stable versions of both `scalatest` and `parsley`
to the build: you should check **regularly** to see if your `parsley` needs updating
during the course of WACC!

## compile

The compile script can be edited to change the frontend interface to your WACC
compiler. You are free to change the language used in this script, but do not
change its name.

## Makefile

Your Makefile should be edited so that running 'make' in the root directory
builds your WACC compiler. Currently running 'make' will call the `sbt compile`
and then `sbt assembly` commands, which will build a `wacc-42-compiler.jar`
in the root directory of the project.
