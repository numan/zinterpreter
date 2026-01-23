pub const Precedence = enum {
    lowest,
    equals, // ==
    less_greater, // <, >
    sum, // +
    product, // *
    prefix, // -X or !X
    call, // myFunction(X)
};
