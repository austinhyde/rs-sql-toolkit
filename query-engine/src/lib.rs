use sql_models::select::Query;

type QueryResult = Result<(), String>;

pub fn execute<T: DataStore>(_query: Query, _datastore: T) -> QueryResult {
    Ok(())
}

pub trait DataStore {}
