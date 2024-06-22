use nom::{
    branch::alt,
    bytes::complete::{escaped, tag},
    character::complete::{alphanumeric1, char, digit1, multispace0, one_of},
    combinator::{cut, map},
    error::ParseError,
    sequence::{delimited, preceded, terminated, tuple},
    IResult,
};

#[derive(Debug, PartialEq)]
pub struct Rsql<'a> {
    expression: Expression<'a>,
}

#[derive(Debug, PartialEq)]
pub enum Expression<'a> {
    Group(Box<Expression<'a>>),
    Or(Box<Expression<'a>>, Box<Expression<'a>>),
    And(Box<Expression<'a>>, Box<Expression<'a>>),
    Stmt(Stmt<'a>),
}

#[derive(Debug, PartialEq)]
pub struct Stmt<'a> {
    operand: &'a str,
    operator: Operator,
    value: Value<'a>,
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

pub fn parse_rsql(input: &str) -> Result<Rsql, nom::Err<nom::error::Error<&str>>> {
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
            tuple((
                and_expression,
                preceded(multispace0, char(',')),
                and_expression,
            )),
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
                stmt_expression,
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
        tuple((string, operator, value)),
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
            map(tag(">"), |_| Operator::Gt),
            map(tag(">="), |_| Operator::Gte),
            map(tag("<"), |_| Operator::Lt),
            map(tag("<="), |_| Operator::Lte),
            map(tag("=in="), |_| Operator::In),
            map(tag("=out="), |_| Operator::Out),
        )),
    )(input)
}

fn value<'a>(input: &'a str) -> IResult<&'a str, Value> {
    return alt((
        map(quoted, |string: &'a str| Value::String(string)),
        map(preceded(multispace0, digit1), |number: &'a str| {
            Value::Number(number.parse::<u64>().unwrap())
        }),
    ))(input);
}

fn quoted(i: &str) -> IResult<&str, &str> {
    preceded(
        multispace0,
        preceded(tag("\""), cut(terminated(string, tag("\"")))),
    )(i)
}

fn string<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, &'a str, E> {
    preceded(multispace0, escaped(alphanumeric1, '\\', one_of("\"n\\")))(i)
}

#[cfg(test)]
mod test {
    use crate::{expression, quoted, stmt, Expression, Operator, Stmt, Value};

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

        let (_, result) = expression("test==5 ,   test==6;test==7  ").unwrap();
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
}
