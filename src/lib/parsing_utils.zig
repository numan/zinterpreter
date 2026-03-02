pub const Precedence = enum(u4) {
    lowest,
    assign, // =
    equals, // ==
    less_greater, // <, >
    sum, // +
    product, // *
    prefix, // -X or !X
    call, // myFunction(X)
    index, // array[index]
};
