# Known Limitations:
1. It can only handle comments properly yet, though it may handle inline comments `// ...' properly
2. The input file must not begin with a new line, your code must begin immediately in the input file
3. It is not a REPL, you must specify the input file՚s full path to read the file
4. The location of the output file can not be changed, it is hardcoded (for now) to `output.txt' inside the dune project directory
5. The AST used currently is not scalable, and all c_instructions are hard coded

There may be some other limitations which I may be unaware of

# How to Run:
1. Depends on menhir
2. After downloading and installing the needed libraries, 
	$ dune exec assembler
3. Provide the full path to your input file, otherwise press [ENTER] to use the default input file
4. If the provided path is not readhable by the system, the assembler will read the default input file
5. The output binary will be stored `output.txt' file in the dune proj directory
