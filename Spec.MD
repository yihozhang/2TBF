# Specification of 2-Tape Brainfuck (2TBF) Language

Different from Brainfuck, besides the normal tape which we call `Arr`, 2TBF has one more tape called `Stk` that the language can operate on. Each tape has infinite many cells. Each cell on the tape `Arr` is an integer that can be marked with a finite set of symbols, while each cell on the tape `Stk` consist only of plain integers. Note here integers is finite following the definition of finite alphabet of a Turing Machine. 2TBF is allowed to freely move pointers on the first tape, while the second tape acts like a stack, where users can only push to and pop the top. The following is a quick reference for operations allowed in 2TBF:

`+ num?`: pop out two numbers from the stack, push the sum of the two numbers if `num` is not specified. Otherwise add `num` to the value at the top of `Stk`.

`-`: pop out two numbers from the stack, push the difference between the second number and the first number if `num` is not specified. Otherwise minus `num` from the value at the top of `Stk`.

`u num?`: push `num` to the stack. If `num` is not specified, then push the value the `Arr` pointer currently points to.

`o num?`: pop the number from the stack, save the value at the cell that the `Arr` pointer currently points to. If num is specified and stands for a false value (i.e., 0), then the value popped is thrown away.

`> num?`: move the `Arr` pointer to the right for `num` cells, where `num` is the value at the top of `Stk` if not specified.

`< num?`: move the `Arr` pointer to the left for `num` cells, where `num` is the value at the top of `Stk` if not specified.

`r`: read one character from the input to the stack at `Arr`.

`w`: write one character to the output from the stack at `Arr`.

`R`: read one number from the input to the stack at `Arr`.

`W`: write one number to the output from the stack at `Arr`.

`! syb`: place a `syb` mark at the current cell at `Arr`. `syb` should be one digit in hex, i.e., from 0 to f.

`? syb`: check if `syb` is marked at the current cell at `Arr`. Push 0 to the stack if yes and 1 otherwise.
