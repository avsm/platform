## craml: A CRAM-testing framework for testing shell scripts

CRAM is a is functional testing framework for command line
applications. `craml` is freely inspired by the [Python
tool](https://bitheap.org/cram/), which was itself based on
Mercurial's [unified test
format](https://www.selenic.com/blog/?p=663).

`craml` is released as a single binary (called `craml`) and
supports the following syntax:

- Lines beginning with two spaces, a dollar sign, and a space are
  *commands* and will be run in the shell.
- Multi-lines commands end by `\` and continue with two spaces and
  a `>` sign on the next line:
  ```
  $ <line1> \
  > <line2> \
  > <line3>
  ```
- Lines beginning with two spaces are considered command *output*.
- Command outputs can contains *ellipsis*: `...`. These will
  match any possible outputs (on zero, one or multiple lines).
- Lines starting by `<--` are command *pre-conditions*; they will
  determine the conditions where the command is run. Currently, only
  non-deterministic modes are supported as pre-conditions (see below).
- Lines starting by `-->` are command *post-conditions*. Currently,
  only exit codes are supported as post-conditions (see below).
- Anything else is a comment. It is not possible to put comments
  in the middle of an output as it is not clear what should be done
  to them when the output changes.

To run the tests described in a `<file>`, use `craml <file>`. This will
run all the commands in sequence and will generated `<file>.corrected`
if one of the output do not match the expected command outputs.

### Non-deterministic Outputs

`craml` supports non-deterministic outputs:

```
<-- non-deterministic
  $ <command>
  <output>
```

In that case, `craml <file>` will run the command but will not
generate `<file>.corrected` if the new output differs from the one
described in the file. Use `craml --non-deterministic <file>` to come
back to the default behaviour.

### Non-deterministic Commands

`craml` supports non-deterministic outputs:

```
<-- non-deterministic [skip]
  $ <command>
  <output>
```

In that case, `craml <file>` will *not* run the command. Use `craml
--non-deterministic <file>` to come back to the default behaviour.

### Exit Codes

`craml` tests exit codes:

```
  $ <command>
  <output>
--> exit 10
```

If `<command>` does not exit with code 10, then `craml <file>` will
generate `<file>.corrected` with the right exit code. Note that `@@
exit 0` will not be displayed.
