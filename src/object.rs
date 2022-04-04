type ObjectType = String;

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    NULL,
}

impl Object {
    pub fn inspect(&self) -> String {
        match self {
            Object::Integer(x) => x.to_string(),
            Object::Boolean(x) => x.to_string(),
            Object::NULL => "null".to_string(),
        }
    }

    pub fn object_type(&self) -> ObjectType {
        match self {
            Object::Integer(_) => "INTEGER".to_string(),
            Object::Boolean(_) => "BOOLEAN".to_string(),
            Object::NULL => "NULL".to_string(),
        }
    }
}
