pub const Precedence = enum {
    Lowest,
    Equals, // ==
    LessGreater, // <, >
    Sum, // +
    Product, // *
    Prefix, // -X or !X
    Call, // myFunction(X)
};
