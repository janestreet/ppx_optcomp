## 113.24.00

- Change the way optcomp resolve filenames in #import directives

  Do the same as cpp, i.e. for relative filenames, consider they are
  relative to the directory of the file being parsed. This doesn't
  matter internally as build commands are always executed from the
  current directory, but it matters for the public release as everything
  is executed from the root.
