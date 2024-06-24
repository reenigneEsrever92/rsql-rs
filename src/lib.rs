use nom::{
    branch::alt,
    bytes::{complete::tag, streaming::take_until},
    character::{
        complete::{alphanumeric1, char, digit1, multispace0},
        streaming::alpha1,
    },
    combinator::{map, recognize},
    multi::many0_count,
    sequence::{pair, preceded, tuple},
    IResult,
};

pub use nom::{
    error::{Error, ErrorKind},
    Err,
};

#[derive(Debug, PartialEq)]
pub struct Rsql<'a> {
    pub expression: Expression<'a>,
}

#[derive(Debug, PartialEq)]
pub enum Expression<'a> {
    Or(Box<Expression<'a>>, Box<Expression<'a>>),
    And(Box<Expression<'a>>, Box<Expression<'a>>),
    Stmt(Stmt<'a>),
}

#[derive(Debug, PartialEq)]
pub struct Stmt<'a> {
    pub operand: &'a str,
    pub operator: Operator,
    pub value: Value<'a>,
}

#[derive(Debug, PartialEq)]
pub enum Operator {
    Eq,
    Neq,
    Gt,
    Gte,
    Lt,
    Lte,
    In,
    Out,
}

#[derive(Debug, PartialEq)]
pub enum Value<'a> {
    String(&'a str),
    Number(u64),
}

pub fn parse_rsql(input: &str) -> Result<Rsql, Err<Error<&str>>> {
    let (_, expression) = expression(input)?;
    Ok(Rsql { expression })
}

fn expression(input: &str) -> IResult<&str, Expression> {
    or_expression(input)
}

fn stmt_expression(input: &str) -> IResult<&str, Expression> {
    alt((map(stmt, Expression::Stmt), preceded(multispace0, group)))(input)
}

fn or_expression(input: &str) -> IResult<&str, Expression> {
    alt((
        map(
            tuple((and_expression, preceded(multispace0, char(',')), expression)),
            |(e1, _, e2)| Expression::Or(Box::new(e1), Box::new(e2)),
        ),
        and_expression,
    ))(input)
}

fn and_expression(input: &str) -> IResult<&str, Expression> {
    alt((
        map(
            tuple((
                stmt_expression,
                preceded(multispace0, char(';')),
                and_expression,
            )),
            |(e1, _, e2)| Expression::And(Box::new(e1), Box::new(e2)),
        ),
        stmt_expression,
    ))(input)
}

fn group(input: &str) -> IResult<&str, Expression> {
    map(
        tuple((char('('), expression, char(')'))),
        |(_, expression, _)| expression,
    )(input)
}

fn stmt(input: &str) -> IResult<&str, Stmt<'_>> {
    map(
        tuple((identifier, operator, val)),
        |(operand, operator, value)| Stmt {
            operand,
            operator,
            value,
        },
    )(input)
}

fn operator(input: &str) -> IResult<&str, Operator> {
    preceded(
        multispace0,
        alt((
            map(tag("=="), |_| Operator::Eq),
            map(tag("!="), |_| Operator::Neq),
            map(tag(">="), |_| Operator::Gte),
            map(tag(">"), |_| Operator::Gt),
            map(tag("<="), |_| Operator::Lte),
            map(tag("<"), |_| Operator::Lt),
            map(tag("=in="), |_| Operator::In),
            map(tag("=out="), |_| Operator::Out),
        )),
    )(input)
}

fn val<'a>(input: &'a str) -> IResult<&'a str, Value> {
    return alt((
        map(quoted, |string: &'a str| Value::String(string)),
        map(preceded(multispace0, digit1), |number: &'a str| {
            Value::Number(number.parse::<u64>().unwrap())
        }),
    ))(input);
}

fn quoted(input: &str) -> IResult<&str, &str> {
    map(
        preceded(
            multispace0,
            alt((
                tuple((tag("\""), take_until("\""), tag("\""))),
                tuple((tag("'"), take_until("'"), tag("'"))),
            )),
        ),
        |(_, string, _)| string,
    )(input)
}

fn identifier(input: &str) -> IResult<&str, &str> {
    preceded(
        multispace0,
        recognize(pair(
            alt((alpha1, tag("_"))),
            many0_count(alt((alphanumeric1, tag("_")))),
        )),
    )(input)
}

#[cfg(test)]
mod test {
    use crate::{expression, quoted, stmt, stmt_expression, Expression, Operator, Stmt, Value};

    #[test]
    fn test_string() {
        let (_, result) = quoted(r#""test""#).unwrap();
        assert_eq!(result, "test");
    }

    #[test]
    fn test_stmt() {
        let (_, result) = stmt("test==5").unwrap();
        assert_eq!(
            result,
            Stmt {
                operand: "test",
                operator: Operator::Eq,
                value: Value::Number(5)
            }
        );

        let (_, result) = stmt("test==\"start\"").unwrap();
        assert_eq!(
            result,
            Stmt {
                operand: "test",
                operator: Operator::Eq,
                value: Value::String("start")
            }
        );
    }

    #[test]
    fn test_expression() {
        let (_, result) = expression("test==5, test==6").unwrap();
        assert_eq!(
            result,
            Expression::Or(
                Box::new(Expression::Stmt(Stmt {
                    operand: "test",
                    operator: Operator::Eq,
                    value: Value::Number(5)
                })),
                Box::new(Expression::Stmt(Stmt {
                    operand: "test",
                    operator: Operator::Eq,
                    value: Value::Number(6)
                })),
            )
        );

        let (_, result) = expression("   test==5 ;test==6").unwrap();
        assert_eq!(
            result,
            Expression::And(
                Box::new(Expression::Stmt(Stmt {
                    operand: "test",
                    operator: Operator::Eq,
                    value: Value::Number(5)
                })),
                Box::new(Expression::Stmt(Stmt {
                    operand: "test",
                    operator: Operator::Eq,
                    value: Value::Number(6)
                })),
            )
        );

        let (_, result) = expression("test==5,test==6;test==7").unwrap();
        assert_eq!(
            result,
            Expression::Or(
                Box::new(Expression::Stmt(Stmt {
                    operand: "test",
                    operator: Operator::Eq,
                    value: Value::Number(5)
                })),
                Box::new(Expression::And(
                    Box::new(Expression::Stmt(Stmt {
                        operand: "test",
                        operator: Operator::Eq,
                        value: Value::Number(6)
                    })),
                    Box::new(Expression::Stmt(Stmt {
                        operand: "test",
                        operator: Operator::Eq,
                        value: Value::Number(7)
                    })),
                )),
            )
        );

        let (_, result) = expression("(test==5 ,test==6)").unwrap();
        assert_eq!(
            result,
            Expression::Or(
                Box::new(Expression::Stmt(Stmt {
                    operand: "test",
                    operator: Operator::Eq,
                    value: Value::Number(5)
                })),
                Box::new(Expression::Stmt(Stmt {
                    operand: "test",
                    operator: Operator::Eq,
                    value: Value::Number(6)
                })),
            )
        );

        let (_, result) = expression("(test==5,test==6);test==7").unwrap();
        assert_eq!(
            result,
            Expression::And(
                Box::new(Expression::Or(
                    Box::new(Expression::Stmt(Stmt {
                        operand: "test",
                        operator: Operator::Eq,
                        value: Value::Number(5)
                    })),
                    Box::new(Expression::Stmt(Stmt {
                        operand: "test",
                        operator: Operator::Eq,
                        value: Value::Number(6)
                    })),
                )),
                Box::new(Expression::Stmt(Stmt {
                    operand: "test",
                    operator: Operator::Eq,
                    value: Value::Number(7)
                }))
            )
        );

        let (_, result) = expression("test==5, (test==6,test==7)").unwrap();
        assert_eq!(
            result,
            Expression::Or(
                Box::new(Expression::Stmt(Stmt {
                    operand: "test",
                    operator: Operator::Eq,
                    value: Value::Number(5)
                })),
                Box::new(Expression::Or(
                    Box::new(Expression::Stmt(Stmt {
                        operand: "test",
                        operator: Operator::Eq,
                        value: Value::Number(6)
                    })),
                    Box::new(Expression::Stmt(Stmt {
                        operand: "test",
                        operator: Operator::Eq,
                        value: Value::Number(7)
                    })),
                )),
            )
        );

        let (_, result) = expression("test==5;test==6,test==7").unwrap();
        assert_eq!(
            result,
            Expression::Or(
                Box::new(Expression::And(
                    Box::new(Expression::Stmt(Stmt {
                        operand: "test",
                        operator: Operator::Eq,
                        value: Value::Number(5)
                    })),
                    Box::new(Expression::Stmt(Stmt {
                        operand: "test",
                        operator: Operator::Eq,
                        value: Value::Number(6)
                    })),
                )),
                Box::new(Expression::Stmt(Stmt {
                    operand: "test",
                    operator: Operator::Eq,
                    value: Value::Number(7)
                })),
            )
        );

        let (_, result) = expression("test==5;test==6;test==7").unwrap();
        assert_eq!(
            result,
            Expression::And(
                Box::new(Expression::Stmt(Stmt {
                    operand: "test",
                    operator: Operator::Eq,
                    value: Value::Number(5)
                })),
                Box::new(Expression::And(
                    Box::new(Expression::Stmt(Stmt {
                        operand: "test",
                        operator: Operator::Eq,
                        value: Value::Number(6)
                    })),
                    Box::new(Expression::Stmt(Stmt {
                        operand: "test",
                        operator: Operator::Eq,
                        value: Value::Number(7)
                    })),
                )),
            )
        );

        let (_, result) = expression("test==5,test==6,test==7;test==8;test==9").unwrap();
        assert_eq!(
            result,
            Expression::Or(
                Box::new(Expression::Stmt(Stmt {
                    operand: "test",
                    operator: Operator::Eq,
                    value: Value::Number(5)
                })),
                Box::new(Expression::Or(
                    Box::new(Expression::Stmt(Stmt {
                        operand: "test",
                        operator: Operator::Eq,
                        value: Value::Number(6)
                    })),
                    Box::new(Expression::And(
                        Box::new(Expression::Stmt(Stmt {
                            operand: "test",
                            operator: Operator::Eq,
                            value: Value::Number(7)
                        })),
                        Box::new(Expression::And(
                            Box::new(Expression::Stmt(Stmt {
                                operand: "test",
                                operator: Operator::Eq,
                                value: Value::Number(8)
                            })),
                            Box::new(Expression::Stmt(Stmt {
                                operand: "test",
                                operator: Operator::Eq,
                                value: Value::Number(9)
                            })),
                        )),
                    )),
                ))
            )
        );

        let (_, result) = expression("(test==5,test==6,test==7);test==8;test==9").unwrap();
        assert_eq!(
            result,
            Expression::And(
                Box::new(Expression::Or(
                    Box::new(Expression::Stmt(Stmt {
                        operand: "test",
                        operator: Operator::Eq,
                        value: Value::Number(5)
                    })),
                    Box::new(Expression::Or(
                        Box::new(Expression::Stmt(Stmt {
                            operand: "test",
                            operator: Operator::Eq,
                            value: Value::Number(6)
                        })),
                        Box::new(Expression::Stmt(Stmt {
                            operand: "test",
                            operator: Operator::Eq,
                            value: Value::Number(7)
                        }))
                    )),
                )),
                Box::new(Expression::And(
                    Box::new(Expression::Stmt(Stmt {
                        operand: "test",
                        operator: Operator::Eq,
                        value: Value::Number(8)
                    })),
                    Box::new(Expression::Stmt(Stmt {
                        operand: "test",
                        operator: Operator::Eq,
                        value: Value::Number(9)
                    })),
                ))
            )
        );

        let (_, result) = expression(r#"test== "5sda""#).unwrap();
        assert_eq!(
            result,
            Expression::Stmt(Stmt {
                operand: "test",
                operator: Operator::Eq,
                value: Value::String("5sda")
            }),
        );
    }

    #[test]
    fn test_parse() {
        quoted("'test'").unwrap();
        stmt_expression("file_size>=5").unwrap();
        expression(
            "(mime_type==\"image/webp\",mime_type==\"image/jpeg\",file_size>=5);name=='test_name'",
        )
        .unwrap();
    }
}
