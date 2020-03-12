use super::expr::Expr;
use std::fmt::Display;

#[derive(Debug, PartialEq)]
pub struct Query {
    pub fields: Vec<FieldExpr>,
    pub table: String,
    pub condition: Option<Expr>,
    pub order: Option<(String, Option<SortDir>)>,
    pub limit: Option<usize>,
}

// test
impl Display for Query {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "SELECT {} FROM {}",
            self.fields
                .iter()
                .map(|f| f.to_string())
                .collect::<Vec<_>>()
                .join(", "),
            self.table,
        )?;
        if let Some(expr) = &self.condition {
            write!(f, " WHERE {}", expr)?;
        }
        if let Some((col, maybe_dir)) = &self.order {
            write!(f, " ORDER BY {}", col)?;
            if let Some(dir) = maybe_dir {
                write!(f, " {}", dir)?;
            }
        }
        if let Some(n) = self.limit {
            write!(f, " LIMIT {}", n)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum FieldExpr {
    All,
    Expr(Expr),
}

impl Display for FieldExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::All => write!(f, "*"),
            Self::Expr(e) => write!(f, "{}", e),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum SortDir {
    Asc,
    Desc,
}
impl Display for SortDir {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                SortDir::Asc => "ASC",
                SortDir::Desc => "DESC",
            }
        )
    }
}
