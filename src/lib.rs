use nom::{
    branch::alt,
    bytes::{
        complete::{tag, take_till, take_while_m_n},
        streaming::{take_until, take_while},
    },
    character::{is_alphanumeric, streaming::alphanumeric1},
    combinator::map_res,
    error::ParseError,
    multi::many_till,
    sequence::Tuple,
    Compare, IResult, InputLength, InputTake, Parser,
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
    Number(u32),
}

pub fn parse_query(input: &str) -> Result<Rsql, nom::Err<nom::error::Error<&str>>> {
    Ok(Rsql {
        expression: parse_expression(input)?.1,
    })
}

fn parse_expression(input: &str) -> IResult<&str, Expression> {
    if let Ok((input, tag)) = tag::<_, _, nom::error::Error<&str>>("(")(input) {
        let (tag, input) = take_until(")")(input)?;
        parse_expression(input)
    } else if let (input, (operator, operand)) = take_text_till(alt((
        tag("=="),
        tag(">"),
        tag(">="),
        tag("<"),
        tag("<="),
        tag("!="),
        tag("=in="),
        tag("=out="),
    )))(input)?
    {
        todo!()
    } else {
        todo!()
    }
}

fn take_text_till(
    mut until: impl FnMut(&str) -> IResult<&str, &str>,
) -> impl FnMut(&str) -> IResult<&str, (&str, &str)> {
    move |input: &str| {
        for (current_index, _) in input.char_indices() {
            match until(&input[current_index..]) {
                Ok((new_input, result)) => {
                    let leading_text = &input[0..current_index];
                    return Ok((new_input, (result, leading_text)));
                }
                Err(err) => {}
            }
        }
        Err(nom::Err::Error(nom::error::Error {
            input,
            code: nom::error::ErrorKind::Fail,
        }))
    }
}

pub fn operator<T, Input, Error: ParseError<Input>>(
    tag: T,
) -> impl Fn(Input) -> IResult<Input, Input, Error>
where
    Input: InputTake + Compare<T>,
    T: InputLength + Clone,
{
    move |i: Input| todo!()
}

#[test]
fn parse_color() {
    assert_eq!(
        parse_query("#2F14DF"),
        Ok((
            "",
            Color {
                red: 47,
                green: 20,
                blue: 223,
            }
        ))
    );
}
