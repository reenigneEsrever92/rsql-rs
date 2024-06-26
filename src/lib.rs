use nom::{
    branch::alt,
    bytes::{complete::tag, streaming::take_until},
    character::{
        complete::{alphanumeric1, char, digit1, multispace0},
        streaming::alpha1,
    },
    combinator::{map, recognize},
    error::{ContextError, ParseError},
    multi::many0_count,
    sequence::{pair, preceded, tuple},
    IResult,
};

pub use nom::{
    error::{Error, ErrorKind},
    Err,
};
use nom_supreme::{error::ErrorTree, final_parser::final_parser};

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

pub fn parse_rsql(input: &str) -> Result<Rsql, ErrorTree<&str>> {
    let res = final_parser(expression::<ErrorTree<&str>>)(input)?;
    Ok(Rsql { expression: res })
}

fn expression<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Expression, E> {
    or_expression(input)
}

fn stmt_expression<'a, E>(input: &'a str) -> IResult<&'a str, Expression, E>
where
    E: ParseError<&'a str> + ContextError<&'a str>,
{
    preceded(multispace0, alt((map(stmt, Expression::Stmt), group)))(input)
}

fn or_expression<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Expression, E> {
    alt((
        map(
            tuple((and_expression, preceded(multispace0, char(',')), expression)),
            |(e1, _, e2)| Expression::Or(Box::new(e1), Box::new(e2)),
        ),
        and_expression,
    ))(input)
}

fn and_expression<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Expression, E> {
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

fn group<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Expression, E> {
    map(
        tuple((char('('), expression, char(')'))),
        |(_, expression, _)| expression,
    )(input)
}

fn stmt<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Stmt<'a>, E> {
    map(
        tuple((identifier, operator, val)),
        |(operand, operator, value)| Stmt {
            operand,
            operator,
            value,
        },
    )(input)
}

fn identifier<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, &'a str, E> {
    preceded(
        multispace0,
        recognize(pair(
            alt((alpha1, tag("_"))),
            many0_count(alt((alphanumeric1, tag("_")))),
        )),
    )(input)
}

fn operator<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Operator, E> {
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

fn val<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Value<'a>, E> {
    return alt((
        map(quoted, |string: &'a str| Value::String(string)),
        map(preceded(multispace0, digit1), |number: &'a str| {
            Value::<'a>::Number(number.parse::<u64>().unwrap())
        }),
    ))(input);
}

fn quoted<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, &'a str, E> {
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

#[cfg(test)]
mod test {
    use nom::{
        error::{convert_error, ErrorKind, VerboseError},
        Finish,
    };
    use nom_supreme::error::ErrorTree;

    use crate::{expression, quoted, stmt, Expression, Operator, Stmt, Value};

    #[test]
    fn test_expected_stmt() {
        let expression = expression::<ErrorTree<&str>>(r#""test"=="test""#);
        println!("expression: {expression:?}");
    }

    #[test]
    fn test_stmt() {
        let (_, result) = stmt::<(&str, ErrorKind)>("test==5").unwrap();
        assert_eq!(
            result,
            Stmt {
                operand: "test",
                operator: Operator::Eq,
                value: Value::Number(5)
            }
        );

        let (_, result) = stmt::<(&str, ErrorKind)>("test==\"start\"").unwrap();
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
    fn test_and_expression() {
        let (_, result) = expression::<(&str, ErrorKind)>("   test==5 ;test==6").unwrap();
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
    }

    #[test]
    fn test_or_and_expression() {
        let (_, result) = expression::<(&str, ErrorKind)>("test==5,test==6;test==7").unwrap();
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
    }

    #[test]
    fn test_and_or_expression() {
        let (_, result) = expression::<(&str, ErrorKind)>("test==5;test==6,test==7").unwrap();
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
    }

    #[test]
    fn test_and_and_and_expression() {
        let (_, result) = expression::<(&str, ErrorKind)>("test==5;test==6;test==7").unwrap();
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
    }

    #[test]
    fn test_group() {
        let (_, result) = expression::<(&str, ErrorKind)>("(test==5,test==6);test==7").unwrap();
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

        let (_, result) = expression::<(&str, ErrorKind)>("test==5, (test==6,test==7)").unwrap();
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
    }

    #[test]
    fn test_long_expression() {
        let (_, result) =
            expression::<(&str, ErrorKind)>("test==5,test==6,test==7;test==8;test==9").unwrap();
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
    }

    #[test]
    fn test_long_group_expression() {
        let (_, result) =
            expression::<(&str, ErrorKind)>("(test==5,test==6,test==7);test==8;test==9").unwrap();
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
    }

    #[test]
    fn test_or_expression() {
        let (_, result) = expression::<(&str, ErrorKind)>("test==5, test==6").unwrap();
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

        let (_, result) = expression::<(&str, ErrorKind)>("(test==5 ,test==6)").unwrap();
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
    }

    #[test]
    fn test_string() {
        quoted::<(&str, ErrorKind)>("'test'").unwrap();
        quoted::<(&str, ErrorKind)>(r#""test""#).unwrap();
    }
}
