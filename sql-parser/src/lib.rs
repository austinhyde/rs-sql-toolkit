use nom::branch::alt;
use nom::bytes::complete::{is_not, take_while1};
use nom::bytes::complete::{tag, tag_no_case};
use nom::character::complete::{digit1, multispace0, multispace1};
use nom::combinator::{map, opt, value};
use nom::multi::{fold_many0, separated_list};
use nom::number::complete::float;
use nom::sequence::{delimited, pair, preceded, terminated, tuple};
use nom::IResult;

use sql_models::expr::*;
use sql_models::select::*;

type SResult<'a, T> = IResult<&'a str, T>;

pub fn select(input: &str) -> Result<Query, String> {
    let (_, (_, fields, table, condition, order, limit)) = tuple((
        terminated(tag_no_case("select"), multispace1),
        field_list,
        preceded(multispace1, from_clause),
        opt(preceded(multispace1, where_clause)),
        opt(preceded(multispace1, order_clause)),
        opt(preceded(multispace1, limit_clause)),
    ))(input)
    .map_err(|e| e.to_string())?;

    Ok(Query {
        fields,
        table,
        condition,
        order,
        limit,
    })
}

fn field_list(input: &str) -> SResult<Vec<FieldExpr>> {
    separated_list(delimited(multispace0, tag(","), multispace0), field_expr)(input)
}

fn field_expr(input: &str) -> SResult<FieldExpr> {
    alt((value(FieldExpr::All, tag("*")), map(expr, FieldExpr::Expr)))(input)
}

fn from_clause(input: &str) -> SResult<String> {
    preceded(terminated(tag_no_case("from"), multispace1), identifier)(input)
}

fn where_clause(input: &str) -> SResult<Expr> {
    preceded(terminated(tag_no_case("where"), multispace1), expr)(input)
}

fn order_clause(input: &str) -> SResult<(String, Option<SortDir>)> {
    pair(
        preceded(terminated(tag_no_case("order by"), multispace1), fieldref),
        opt(preceded(
            multispace1,
            alt((
                value(SortDir::Asc, tag_no_case("asc")),
                value(SortDir::Desc, tag_no_case("desc")),
            )),
        )),
    )(input)
}

fn limit_clause(input: &str) -> SResult<usize> {
    preceded(
        terminated(tag_no_case("limit"), multispace1),
        map(digit1, |ds: &str| ds.parse().unwrap()),
    )(input)
}

fn expr(input: &str) -> SResult<Expr> {
    pratt_expr(input, 0)
}

// Implements a Pratt expression parser with precedence rules
// http://journal.stuffwithstuff.com/2011/03/19/pratt-parsers-expression-parsing-made-easy/
fn pratt_expr(input: &str, precedence: u8) -> SResult<Expr> {
    // parse the first expression
    let (mut input, mut lhs) = pratt_prefix_expr(input)?;

    while let Ok((rest, op)) = delimited(multispace0, binop, multispace0)(input) {
        if precedence >= op.precedence() {
            break;
        }

        let (rest, e) = pratt_bin_expr(rest, lhs, op)?;
        input = rest;
        lhs = e;
    }

    Ok((input, lhs))
}

fn pratt_prefix_expr(input: &str) -> SResult<Expr> {
    alt((
        map(literal_str, Expr::StrLit),
        map(literal_num, Expr::NumLit),
        map(literal_bool, Expr::BoolLit),
        map(fieldref, Expr::FieldRef),
        map(unary_operation, Expr::UnOp),
    ))(input)
}

fn pratt_bin_expr(input: &str, lhs: Expr, op: BinOp) -> SResult<Expr> {
    let (input, rhs) = pratt_expr(input, op.precedence())?;
    Ok((input, Expr::BinOp(BinaryOperation::new(lhs, op, rhs))))
}

fn unary_operation(input: &str) -> SResult<UnaryOperation> {
    map(pair(unop, expr), UnaryOperation::new_tuple)(input)
}

fn unop(input: &str) -> SResult<UnOp> {
    value(UnOp::Not, terminated(tag_no_case("not"), multispace1))(input)
}

fn binop(input: &str) -> SResult<BinOp> {
    let eq = alt((tag("="), tag("==")));
    let ne = alt((tag("!="), tag("<>")));
    alt((
        value(BinOp::And, terminated(tag_no_case("and"), multispace1)),
        value(BinOp::Or, terminated(tag_no_case("or"), multispace1)),
        value(BinOp::Lt, tag("<")),
        value(BinOp::Le, tag("<=")),
        value(BinOp::Gt, tag(">")),
        value(BinOp::Ge, tag(">=")),
        value(BinOp::Eq, eq),
        value(BinOp::Ne, ne),
    ))(input)
}

fn fieldref(input: &str) -> SResult<String> {
    identifier(input)
}

fn identifier(input: &str) -> SResult<String> {
    map(
        take_while1(|c: char| c.is_alphanumeric() || c == '_'),
        str::to_string,
    )(input)
}

fn literal_str(input: &str) -> SResult<String> {
    delimited(
        tag("'"),
        fold_many0(
            // TODO: account for escapes
            is_not("'"),
            String::new(),
            |mut acc, c| {
                acc.push_str(c);
                acc
            },
        ),
        tag("'"),
    )(input)
}

fn literal_num(input: &str) -> SResult<f32> {
    float(input)
}

fn literal_bool(input: &str) -> SResult<bool> {
    alt((
        value(true, tag_no_case("true")),
        value(false, tag_no_case("false")),
    ))(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn v<T>(r: SResult<T>) -> T {
        let (rest, val) = r.unwrap();
        assert_eq!("", rest);
        val
    }

    #[test]
    fn test_literal_bool() {
        assert_eq!(true, v(literal_bool("true")));
        assert_eq!(false, v(literal_bool("false")));
    }

    #[test]
    fn test_literal_num() {
        assert_eq!(1.23, v(literal_num("1.23")));
        assert_eq!(-10.0, v(literal_num("-10")));
    }

    #[test]
    fn test_literal_str() {
        assert_eq!("hello world!", v(literal_str("'hello world!'")));
    }

    #[test]
    fn test_fieldref() {
        assert_eq!("some_col", v(fieldref("some_col")));
    }

    #[test]
    fn test_unop() {
        assert_eq!(
            v(unary_operation("not true")),
            UnaryOperation::new(UnOp::Not, Expr::BoolLit(true))
        );
    }

    #[test]
    fn test_expr_simple() {
        assert_eq!(
            v(expr("a < b")),
            Expr::BinOp(BinaryOperation::new(
                Expr::FieldRef("a".to_string()),
                BinOp::Lt,
                Expr::FieldRef("b".to_string())
            )),
        )
    }

    #[test]
    fn test_expr_nested() {
        assert_eq!(
            v(expr("a < b AND c = d")),
            Expr::BinOp(BinaryOperation::new(
                Expr::BinOp(BinaryOperation::new(
                    Expr::FieldRef("a".to_string()),
                    BinOp::Lt,
                    Expr::FieldRef("b".to_string())
                )),
                BinOp::And,
                Expr::BinOp(BinaryOperation::new(
                    Expr::FieldRef("c".to_string()),
                    BinOp::Eq,
                    Expr::FieldRef("d".to_string())
                )),
            )),
        );
    }

    #[test]
    fn test_limit_clause() {
        assert_eq!(v(limit_clause("limit 10")), 10);
    }

    #[test]
    fn test_order_clause() {
        assert_eq!(
            v(order_clause("order by col desc")),
            ("col".to_string(), Some(SortDir::Desc))
        );
    }

    #[test]
    fn test_where_clause() {
        assert_eq!(
            v(where_clause("where x = y and z < 3")),
            Expr::BinOp(BinaryOperation {
                op: BinOp::And,
                lhs: Box::new(Expr::BinOp(BinaryOperation {
                    op: BinOp::Eq,
                    lhs: Box::new(Expr::FieldRef("x".to_string())),
                    rhs: Box::new(Expr::FieldRef("y".to_string())),
                })),
                rhs: Box::new(Expr::BinOp(BinaryOperation {
                    op: BinOp::Lt,
                    lhs: Box::new(Expr::FieldRef("z".to_string())),
                    rhs: Box::new(Expr::NumLit(3.0)),
                })),
            })
        );
    }

    #[test]
    fn test_select() {
        let query =
            "select foo, bar, 7, * from example where x > 3 and y != 'hello' order by foo limit 4";
        let actual = select(query).unwrap();
        let expected = Query {
            fields: vec![
                FieldExpr::Expr(Expr::FieldRef("foo".to_string())),
                FieldExpr::Expr(Expr::FieldRef("bar".to_string())),
                FieldExpr::Expr(Expr::NumLit(7.0)),
                FieldExpr::All,
            ],
            table: "example".to_string(),
            condition: Some(Expr::BinOp(BinaryOperation::new(
                Expr::BinOp(BinaryOperation::new(
                    Expr::FieldRef("x".to_string()),
                    BinOp::Gt,
                    Expr::NumLit(3.0),
                )),
                BinOp::And,
                Expr::BinOp(BinaryOperation::new(
                    Expr::FieldRef("y".to_string()),
                    BinOp::Ne,
                    Expr::StrLit("hello".to_string()),
                )),
            ))),
            order: Some(("foo".to_string(), None)),
            limit: Some(4),
        };
        assert_eq!(expected, actual, "Expected [{}] got [{}]", expected, actual);
        println!("Input: {}", query);
        println!("Parsed: {}", actual);
    }
}
