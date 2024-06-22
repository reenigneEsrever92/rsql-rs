use nom::{
    branch::alt,
    bytes::complete::{escaped, tag},
    character::complete::{alphanumeric1, char, digit1, one_of},
    combinator::{cut, map},
    error::ParseError,
    sequence::{preceded, terminated, tuple},
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
    StringBetween(&'a str),
    StringStart(&'a str),
    StringEnd(&'a str),
    Number(u64),
}

pub fn parse_query(input: &str) -> Result<Rsql, nom::Err<nom::error::Error<&str>>> {
    Ok(Rsql {
        expression: expression(input)?.1,
    })
}

fn expression(input: &str) -> IResult<&str, Expression> {
    and_expression(input)
}

fn stmt_expression(input: &str) -> IResult<&str, Expression> {
    map(stmt, Expression::Stmt)(input)
}

fn or_expression(input: &str) -> IResult<&str, Expression> {
    alt((
        map(
            tuple((stmt_expression, char(','), expression)),
            |(e1, _, e2)| Expression::Or(Box::new(e1), Box::new(e2)),
        ),
        stmt_expression,
    ))(input)
}

fn and_expression(input: &str) -> IResult<&str, Expression> {
    alt((
        map(
            tuple((or_expression, char(';'), expression)),
            |(e1, _, e2)| Expression::And(Box::new(e1), Box::new(e2)),
        ),
        or_expression,
    ))(input)
}

#[allow(dead_code)]
fn group<'a>() -> IResult<&'a str, Expression<'a>> {
    todo!()
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
    alt((
        map(tag("=="), |_| Operator::Eq),
        map(tag("!="), |_| Operator::Neq),
        map(tag(">"), |_| Operator::Gt),
        map(tag(">="), |_| Operator::Gte),
        map(tag("<"), |_| Operator::Lt),
        map(tag("<="), |_| Operator::Lte),
        map(tag("=in="), |_| Operator::In),
        map(tag("=out="), |_| Operator::Out),
    ))(input)
}

fn value<'a>(input: &'a str) -> IResult<&'a str, Value> {
    return alt((
        map(string_between, |string: &'a str| {
            Value::StringBetween(string)
        }),
        map(string_start, |string: &'a str| Value::StringStart(string)),
        map(string_end, |string: &'a str| Value::StringEnd(string)),
        map(quoted, |string: &'a str| Value::String(string)),
        map(digit1, |number: &'a str| {
            Value::Number(number.parse::<u64>().unwrap())
        }),
    ))(input);
}

fn quoted(i: &str) -> IResult<&str, &str> {
    preceded(char('\"'), cut(terminated(string, char('\"'))))(i)
}

fn string_between(i: &str) -> IResult<&str, &str> {
    preceded(tag("\"*"), cut(terminated(string, tag("\"*"))))(i)
}

fn string_start(i: &str) -> IResult<&str, &str> {
    preceded(char('\"'), cut(terminated(string, tag("*\""))))(i)
}

fn string_end(i: &str) -> IResult<&str, &str> {
    preceded(tag("\"*"), cut(terminated(string, char('\"'))))(i)
}

fn string<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, &'a str, E> {
    escaped(alphanumeric1, '\\', one_of("\"n\\"))(i)
}

#[cfg(test)]
mod test {
    use crate::{expression, stmt, Expression, Operator, Stmt, Value};

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

        let (_, result) = stmt("test==\"start*\"").unwrap();
        assert_eq!(
            result,
            Stmt {
                operand: "test",
                operator: Operator::Eq,
                value: Value::StringStart("start")
            }
        );
    }

    #[test]
    fn test_expression() {
        let (_, result) = expression("test==5,test==6").unwrap();
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

        let (_, result) = expression("test==5,test==6;test==7").unwrap();
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
                })),
            )
        );
    }
}
