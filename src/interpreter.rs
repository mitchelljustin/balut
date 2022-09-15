use std::collections::HashMap;

use crate::ast::Node;
use crate::interpreter::ErrorKind::{ArityError, ClassNotFound, ObjectNotFound, PropertyNotFound, TypeError, Unknown};
use crate::parser::parse_source;
use crate::types::Integer;

pub enum Receiver {
    Class,
    Instance,
}

type MethodFn = dyn Fn(&mut Interpreter, ObjectId, &[ObjectId]) -> ObjectResult;

fn swag(_: &mut Interpreter, _: ObjectId, _: &[ObjectId]) -> ObjectResult {
    Ok(1)
}

pub enum MethodBody {
    Native(Box<MethodFn>),
    Node(Node),
}

macro native_method($func:expr) {
$crate::interpreter::MethodBody::Native(Box::new($func))
}

pub struct Method {
    receiver: Receiver,
    name: String,
    params: Vec<Node>,
    body: MethodBody,
}

#[derive(Debug, Clone)]
pub enum Value {
    Nil,
    String(String),
    Integer(Integer),
}

impl Value {
    pub fn class_name(&self) -> &'static str {
        match self {
            Value::Nil => "Nil",
            Value::String(_) => "String",
            Value::Integer(_) => "Integer",
        }
    }
}

pub struct Object {
    id: ObjectId,
    class: ObjectId,
    methods: HashMap<ObjectId, Method>,
    properties: HashMap<ObjectId, ObjectId>,
    rust_val: Value,
}


impl Object {
    fn new(id: ObjectId, class: ObjectId) -> Self {
        Self {
            id,
            class,
            methods: Default::default(),
            properties: Default::default(),
            rust_val: Value::Nil,
        }
    }

    fn add_method(&mut self, name: ObjectId, method: Method) {
        self.methods.insert(name, method);
    }

    fn set_property(&mut self, key: ObjectId, value: ObjectId) {
        self.properties.insert(key, value);
    }

    fn get_property(&self, key: ObjectId) -> Option<ObjectId> {
        self.properties.get(&key).copied()
    }

    pub fn expect_string(&self) -> Result<&String, ErrorKind> {
        let Value::String(ref string) = self.rust_val else {
            return Err(TypeError {
                expected: "String".to_string(),
                actual: self.rust_val.class_name().to_string(),
            });
        };
        Ok(string)
    }
}

type ObjectId = i64;

#[derive(Debug)]
pub enum ErrorKind {
    ObjectNotFound(ObjectId),
    ClassNotFound(String),
    PropertyNotFound(String),
    TypeError { expected: String, actual: String },
    ArityError { method_name: String, expected: usize, actual: usize },
    Unknown,
}

pub struct Interpreter {
    objects: Vec<Object>,
    class_by_name: HashMap<String, ObjectId>,
    strings: HashMap<String, ObjectId>,
    globals: HashMap<String, ObjectId>,
    nil: ObjectId,
}

type ObjectResult = Result<ObjectId, ErrorKind>;
type EmptyResult = Result<(), ErrorKind>;

const CLASS_CLASS: ObjectId = 0;

fn ident_list(idents: &[&str]) -> Vec<Node> {
    idents
        .into_iter()
        .map(|name| Node::Ident { name: name.to_string() })
        .collect()
}

impl Interpreter {
    fn new() -> Self {
        Self {
            objects: Default::default(),
            class_by_name: Default::default(),
            strings: Default::default(),
            globals: Default::default(),
            nil: -1,
        }
    }

    fn create_object(&mut self, class: ObjectId) -> ObjectId {
        let id = self.objects.len() as ObjectId;
        let object = Object::new(id, class);
        self.objects.push(object);
        id
    }

    fn init(&mut self) -> Result<(), ErrorKind> {
        self.init_class_class()?;
        self.init_object_class()?;
        self.init_string_class()?;
        self.init_integer_class()?;
        self.init_nil()?;
        Ok(())
    }

    fn init_class_class(&mut self) -> EmptyResult {
        self.create_class("Class".to_string())?;
        Ok(())
    }

    fn init_integer_class(&mut self) -> EmptyResult {
        self.create_class("Integer".to_string())?;
        Ok(())
    }

    fn init_string_class(&mut self) -> EmptyResult {
        self.create_class("String".to_string())?;
        Ok(())
    }

    fn init_nil(&mut self) -> EmptyResult {
        let class = self.create_class("Nil".to_string())?;
        let nil = self.create_object(class);
        self.define_global("nil".to_string(), nil);
        self.nil = nil;
        Ok(())
    }

    fn define_method(&mut self,
                     target: ObjectId,
                     name: &str,
                     receiver: Receiver,
                     params: Vec<Node>,
                     body: MethodBody,
    ) -> EmptyResult {
        let name_obj = self.create_string(name.to_string())?;
        self.get_object_mut(target)?
            .add_method(name_obj, Method {
                name: name.to_string(),
                receiver,
                params,
                body,
            });
        Ok(())
    }

    fn init_object_class(&mut self) -> EmptyResult {
        let class = self.create_class("Object".to_string())?;

        self.define_method(
            class,
            "property_get",
            Receiver::Instance,
            ident_list(&["name"]),
            MethodBody::Native(Box::new(|ctx, this, args| {
                let &[name] = args else { return Err(Unknown); };
                let this = ctx.get_object(this)?;
                let name = ctx.get_object(name)?;
                let name_string = name.expect_string()?;
                let value = this.get_property(name.id).ok_or_else(|| PropertyNotFound(name_string.clone()))?;
                Ok(value)
            })))?;

        self.define_method(
            class,
            "property_set",
            Receiver::Instance,
            ident_list(&["name", "value"]),
            MethodBody::Native(Box::new(|ctx, this, args| {
                let &[name, value] = args else { return Err(Unknown); };
                ctx.get_object(name)?.expect_string()?;
                ctx.get_object(value)?;
                ctx.get_object_mut(this)?.set_property(name, value);
                Ok(ctx.nil)
            })))?;


        Ok(())
    }

    fn call(&mut self) {}

    fn create_string(&mut self, string: String) -> ObjectResult {
        let id = match self.strings.get(&string) {
            Some(&id) => id,
            None => {
                let id = self.create_value_object(Value::String(string.clone()))?;
                self.strings.insert(string, id);
                id
            }
        };
        Ok(id)
    }

    fn create_value_object(&mut self, rust_val: Value) -> ObjectResult {
        let class = self.lookup_class(rust_val.class_name())?;
        let id = self.create_object(class);
        self.get_object_mut(id)?.rust_val = rust_val;
        Ok(id)
    }

    fn define_global(&mut self, name: String, value: ObjectId) {
        self.globals.insert(name, value);
    }

    fn set_property(&mut self, target: ObjectId, key: String, value: ObjectId) -> ObjectResult {
        let key_obj = self.create_string(key)?;
        self.get_object_mut(target)?.set_property(key_obj, value);
        Ok(value)
    }

    fn create_class(&mut self, name: String) -> ObjectResult {
        let class = self.create_object(CLASS_CLASS);
        self.class_by_name.insert(name.clone(), class);
        let name_obj = self.create_string(name.clone())?;
        self.set_property(class, "name".to_string(), name_obj)?;
        Ok(class)
    }

    fn get_object(&self, id: ObjectId) -> Result<&Object, ErrorKind> {
        self.objects.get(id as usize).ok_or_else(|| ObjectNotFound(id))
    }

    fn get_object_mut(&mut self, id: ObjectId) -> Result<&mut Object, ErrorKind> {
        self.objects.get_mut(id as usize).ok_or_else(|| ObjectNotFound(id))
    }

    fn get_class(&self, name: &str) -> Result<&Object, ErrorKind> {
        self.get_object(self.lookup_class(name)?)
    }

    fn get_class_mut(&mut self, name: &str) -> Result<&mut Object, ErrorKind> {
        self.get_object_mut(self.lookup_class(name)?)
    }

    fn lookup_class(&self, name: &str) -> ObjectResult {
        self.class_by_name
            .get(name)
            .cloned()
            .ok_or_else(|| ClassNotFound(name.to_string()))
    }
}