UnaryOp(  -> int
    Indirection,
    Cast( -> int* 
        BinOp( -> long + long -> long
            UnaryOp( -> struct*
                Address, -> 
                Identifier( -> struct
                    Identifier(
                        "instance",
                    ),
                ),
            ),
            Plus,
            Constant( -> long
                Integer(
                    Integer {
                        base: Decimal,
                        number: "4",
                        suffix: IntegerSuffix {
                            size: Long,
                            unsigned: false,
                            imaginary: false,
                        },
                    },
                ),
            ),
        ),
        Pointer(
            Int,
        ),
    ),
),