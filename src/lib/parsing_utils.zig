pub const Precedence = enum(u3) {
    lowest,
    equals, // ==
    less_greater, // <, >
    sum, // +
    product, // *
    prefix, // -X or !X
    call, // myFunction(X)
};
