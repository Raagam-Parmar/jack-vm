1. This program can handle multiple VM files.
2. It can handle multiline and inline comments.
3. It is necessary for the VM files to have a `Sys.init' function otherwise the hack output may not work as expected.
4. To run this program, do the following:
    1. dune build
    2. dune exec jackvm (your arguments here, file path to your .vm files)
The name of the first file in the argument will be the name of the hack output file.