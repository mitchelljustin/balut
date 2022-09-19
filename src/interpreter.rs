use std::collections::HashMap;
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};

use crate::ast::{Literal, Node};
use crate::interpreter::ErrorKind::{ArityError, FacetNotFound, MethodNotFound, ObjectNotFound, PropertyNotFound, TypeError, Unimplemented, Unknown, VariableNotFound};
use crate::types::Int;

#[derive(Debug, PartialEq, Eq)]
pub enum Receiver {
    Module,
    Instance,
}

type MethodFn = fn(&mut Interpreter, ObjectId, &[ObjectId]) -> ObjectResult;

pub enum MethodBody {
    Internal(MethodFn),
    User(Node),
}

impl Debug for MethodBody {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            MethodBody::Internal(_) => write!(f, "Internal"),
            MethodBody::User(node) => write!(f, "User({node})")
        }
    }
}

#[derive(Debug)]
pub struct Method {
    name: String,
    params: Vec<Node>,
    receiver: Receiver,
    body: MethodBody,
}

#[derive(Debug, Default)]
pub struct Facet {
    methods: HashMap<ObjectId, ObjectId>,
}

#[derive(Debug)]
pub struct Nil {}

#[derive(Debug)]
pub enum Value {
    Nil(Nil),
    Method(Method),
    Facet(Facet),
    String(String),
    Int(Int),
    Array(Vec<ObjectId>),
}

impl Value {
    pub fn facet_name(&self) -> &'static str {
        match self {
            Value::Nil(_) => "Nil",
            Value::String(_) => "String",
            Value::Int(_) => "Int",
            Value::Method(_) => "Method",
            Value::Facet(_) => "Facet",
            Value::Array(_) => "Array",
        }
    }

    pub fn nil() -> Self {
        Value::Nil(Nil {})
    }
}

impl Default for Value {
    fn default() -> Self {
        Self::nil()
    }
}

pub struct Object {
    id: ObjectId,
    facets: Vec<ObjectId>,
    properties: HashMap<ObjectId, ObjectId>,
    value: Value,
}

macro define_expects($($method:ident, $method_mut:ident -> $discriminant:ident($ty:ty) )+) {
$(
        fn $method(&self) -> Result<&$ty, ErrorKind> {
            match self.value {
                Value::$discriminant(ref value) => Ok(value),
                _ => Err(TypeError {
                    expected: stringify!($discriminant).to_string(),
                    actual: self.value.facet_name().to_string(),
                })
            }
        }
        fn $method_mut(&mut self) -> Result<&mut $ty, ErrorKind> {
            match self.value {
                Value::$discriminant(ref mut value) => Ok(value),
                _ => Err(TypeError {
                    expected: stringify!($discriminant).to_string(),
                    actual: self.value.facet_name().to_string(),
                })
            }
        }
    )+
}

impl Object {
    fn new(id: ObjectId) -> Self {
        Self {
            id,
            facets: Default::default(),
            properties: Default::default(),
            value: Default::default(),
        }
    }

    fn add_facet(&mut self, facet_id: ObjectId) {
        self.facets.push(facet_id);
    }

    fn set_property(&mut self, key_id: ObjectId, value_id: ObjectId) {
        self.properties.insert(key_id, value_id);
    }

    fn get_property(&self, key_id: ObjectId) -> Option<ObjectId> {
        self.properties.get(&key_id).copied()
    }

    define_expects! {
        expect_nil, expect_nil_mut_DO_NOT_USE -> Nil(Nil)
        expect_int, expect_int_mut -> Int(Int)
        expect_string, expect_string_mut -> String(String)
        expect_array, expect_array_mut -> Array(Vec<ObjectId>)
        expect_facet, expect_facet_mut -> Facet(Facet)
        expect_method, expect_method_mut -> Method(Method)
    }
}

type ObjectId = i64;

#[derive(Debug)]
pub enum ErrorKind {
    ObjectNotFound(ObjectId),
    FacetNotFound(String),
    PropertyNotFound(String),
    VariableNotFound(String),
    MethodNotFound { name: String, target_id: ObjectId },
    TypeError { expected: String, actual: String },
    ArityError { method_name: String, expected: usize, actual: usize },
    Unimplemented(Node),
    Unknown,
}

#[derive(Debug)]
pub enum ScopeKind {
    Global,
    Method,
    Facet,
}

#[derive(Debug)]
pub struct Scope {
    name: String,
    binding: HashMap<ObjectId, ObjectId>,
}

impl Scope {
    fn new(name: String) -> Self {
        Self {
            name,
            binding: Default::default(),
        }
    }
}


impl Display for ErrorKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl Error for ErrorKind {}

pub struct Interpreter {
    objects: Vec<Object>,
    facet_by_name: HashMap<String, ObjectId>,
    strings: HashMap<String, ObjectId>,
    scope_stack: Vec<Scope>,
    nil: ObjectId,
}

type ObjectResult = Result<ObjectId, ErrorKind>;
type EmptyResult = Result<(), ErrorKind>;


fn ident_list(idents: &[&str]) -> Vec<Node> {
    idents
        .into_iter()
        .map(|name| Node::Ident { name: name.to_string() })
        .collect()
}

fn object_property_get(ctx: &mut Interpreter, this_id: ObjectId, args: &[ObjectId]) -> ObjectResult {
    let &[name_id] = args else { return Err(Unknown); };
    let this = ctx.get_object(this_id)?;
    let name_string = ctx.get_object(name_id)?.expect_string()?;
    let value = this.get_property(name_id).ok_or_else(|| PropertyNotFound(name_string.clone()))?;
    Ok(value)
}

fn object_property_set(ctx: &mut Interpreter, this_id: ObjectId, args: &[ObjectId]) -> ObjectResult {
    let &[name_id, value_id] = args else { return Err(Unknown); };
    ctx.get_object(name_id)?.expect_string()?;
    ctx.get_object(value_id)?;
    ctx.get_object_mut(this_id)?.set_property(name_id, value_id);
    Ok(value_id)
}

fn object_facets(ctx: &mut Interpreter, this_id: ObjectId, args: &[ObjectId]) -> ObjectResult {
    let &[] = args else { return Err(Unknown); };
    let this = ctx.get_object(this_id)?;
    let facets = this.facets.clone();
    ctx.create_object_with_value(Value::Array(facets))
}


impl Interpreter {
    pub fn new() -> Self {
        Self {
            objects: Default::default(),
            facet_by_name: Default::default(),
            strings: Default::default(),
            scope_stack: vec![Scope::new("global".to_string())],
            nil: -1,
        }
    }

    fn scope_mut(&mut self) -> &mut Scope {
        self.scope_stack.first_mut().unwrap()
    }

    fn global_mut(&mut self) -> &mut Scope {
        self.scope_stack.last_mut().unwrap()
    }

    fn push_scope(&mut self, name: String) {
        self.scope_stack.insert(0, Scope::new(name));
    }

    fn pop_scope(&mut self) {
        self.scope_stack.remove(0);
    }

    fn create_object(&mut self) -> ObjectId {
        let id = self.objects.len() as ObjectId;
        let object = Object::new(id);
        self.objects.push(object);
        id
    }

    fn create_object_of_facet(&mut self, facet_name: &str) -> ObjectResult {
        let id = self.create_object();
        self.add_facet_with_name(id, facet_name)?;
        Ok(id)
    }

    fn set_variable(&mut self, var_name_id: ObjectId, value_id: ObjectId) -> ObjectResult {
        self.get_object(var_name_id)?.expect_string()?;
        self.get_object(value_id)?;
        self.scope_mut().binding.insert(var_name_id, value_id);
        Ok(value_id)
    }

    fn get_variable(&mut self, var_name_id: ObjectId) -> ObjectResult {
        let var_name = self.get_object(var_name_id)?.expect_string()?;
        for Scope { binding, .. } in self.scope_stack.iter() {
            if let Some(&value_id) = binding.get(&var_name_id) {
                return Ok(value_id);
            }
        }

        Err(VariableNotFound(var_name.clone()))
    }

    fn add_facet(&mut self, target_id: ObjectId, facet_id: ObjectId) -> EmptyResult {
        self.get_object_mut(target_id)?.add_facet(facet_id);
        Ok(())
    }

    fn add_facet_with_name(&mut self, target_id: ObjectId, facet_name: &str) -> EmptyResult {
        let facet_id = self.lookup_facet(facet_name)?;
        self.add_facet(target_id, facet_id)
    }

    fn add_method(&mut self, facet_id: ObjectId, method: Method) -> EmptyResult {
        let name = self.string(&method.name)?;
        let method_obj = self.create_object_with_value(Value::Method(method))?;
        let facet = self.get_object_mut(facet_id)?.expect_facet_mut()?;
        facet.methods.insert(name, method_obj);
        Ok(())
    }

    pub fn init(&mut self) -> Result<(), ErrorKind> {
        self.init_object_facet()?;
        self.init_string_facet()?;
        self.init_int_facet()?;
        self.init_method_facet()?;
        self.init_facet_facet()?;
        self.init_nil()?;
        Ok(())
    }

    fn init_int_facet(&mut self) -> EmptyResult {
        self.create_facet("Int")?;
        Ok(())
    }

    fn init_string_facet(&mut self) -> EmptyResult {
        self.create_facet("String")?;
        Ok(())
    }

    fn init_method_facet(&mut self) -> EmptyResult {
        self.create_facet("Method")?;
        Ok(())
    }

    fn init_facet_facet(&mut self) -> EmptyResult {
        self.create_facet("Facet")?;
        Ok(())
    }

    fn init_nil(&mut self) -> EmptyResult {
        self.create_facet("Nil")?;
        self.nil = self.create_object_of_facet("Nil")?;
        Ok(())
    }

    fn init_object_facet(&mut self) -> EmptyResult {
        let facet_id = self.create_bare_facet("Object")?;

        self.add_method(
            facet_id,
            Method {
                name: "property_get".to_string(),
                receiver: Receiver::Instance,
                params: ident_list(&["name"]),
                body: MethodBody::Internal(object_property_get),
            },
        )?;

        self.add_method(
            facet_id,
            Method {
                name: "property_set".to_string(),
                receiver: Receiver::Instance,
                params: ident_list(&["name", "value"]),
                body: MethodBody::Internal(object_property_set),
            },
        )?;

        self.add_method(
            facet_id,
            Method {
                name: "facets".to_string(),
                receiver: Receiver::Instance,
                params: ident_list(&[]),
                body: MethodBody::Internal(object_facets),
            },
        )?;

        Ok(())
    }

    pub fn eval(&mut self, node: &Node) -> ObjectResult {
        match node {
            Node::Literal { value } => match value {
                Literal::String(string) => self.string(string),
                Literal::Int(int) => self.create_object_with_value(Value::Int(*int)),
            },
            Node::Nil => Ok(self.nil),
            Node::Phrase { .. } => Err(Unimplemented(node.clone())),
            Node::Grouping { .. } => Err(Unimplemented(node.clone())),
            Node::Binary { .. } => Err(Unimplemented(node.clone())),
            Node::Assignment { .. } => Err(Unimplemented(node.clone())),
            Node::Unary { .. } => Err(Unimplemented(node.clone())),
            Node::Access { .. } => Err(Unimplemented(node.clone())),
            Node::Sequence { .. } => Err(Unimplemented(node.clone())),
            Node::Path { .. } => Err(Unimplemented(node.clone())),
            Node::Nomen { .. } => Err(Unimplemented(node.clone())),
            Node::Ident { .. } => Err(Unimplemented(node.clone())),
        }
    }

    fn string(&mut self, string: &str) -> ObjectResult {
        Ok(
            match self.strings.get(string) {
                Some(&id) => id,
                None => {
                    let id = self.create_object_with_value(Value::String(string.to_string()))?;
                    self.strings.insert(string.to_string(), id);
                    id
                }
            }
        )
    }

    fn create_object_with_value(&mut self, value: Value) -> ObjectResult {
        let id = self.create_object_of_facet(value.facet_name())?;
        self.get_object_mut(id)?.value = value;
        Ok(id)
    }

    fn set_property(&mut self, target_id: ObjectId, key_string: &str, value_id: ObjectId) -> ObjectResult {
        let key_id = self.string(&key_string)?;
        self.get_object_mut(target_id)?.set_property(key_id, value_id);
        Ok(value_id)
    }

    fn create_bare_facet(&mut self, facet_name: &str) -> ObjectResult {
        let facet_id = self.create_object();
        self.facet_by_name.insert(facet_name.to_string(), facet_id);
        let name_string = self.string(&facet_name)?;
        self.set_property(
            facet_id,
            "name",
            name_string,
        )?;
        Ok(facet_id)
    }

    fn create_facet(&mut self, facet_name: &str) -> ObjectResult {
        let facet_id = self.create_bare_facet(facet_name)?;
        self.add_facet_with_name(facet_id, "Object")?;
        Ok(facet_id)
    }

    fn call_method(&mut self, target_id: ObjectId, receiver: Receiver, method_name_id: ObjectId, args: &[ObjectId]) -> ObjectResult {
        let target = self.get_object(target_id)?;
        let method_name = self.get_object(method_name_id)?.expect_string()?;
        for &facet_id in target.facets.iter() {
            let facet = self.get_object(facet_id)?.expect_facet()?;
            let Some(&method_id) = facet.methods.get(&method_name_id) else {
                continue;
            };
            let method = self.get_object(method_id)?.expect_method()?;
            if method.receiver != receiver {
                continue;
            }
            let method_body = match method.body {
                MethodBody::Internal(method_fn) => {
                    return method_fn(self, target_id, args);
                },
                MethodBody::User(ref node) => node,
            };
            if method.params.len() != args.len() {
                return Err(ArityError {
                    expected: method.params.len(),
                    actual: args.len(),
                    method_name: method_name.clone(),
                });
            }
            self.push_scope(method_name.clone());
            self.set_variable(self.string("self")?, target_id)?;
            for (param, &arg) in method.params.iter().zip(args) {
                let Node::Ident { name } = param else {
                    return Err(Unknown);
                };
                self.set_variable(self.string(name)?, arg)?;
            }
            self.eval(method_body)?;
            self.pop_scope();
        }
        Err(MethodNotFound { name: method_name.clone(), target_id })
    }


    fn get_object(&self, id: ObjectId) -> Result<&Object, ErrorKind> {
        self.objects.get(id as usize).ok_or_else(|| ObjectNotFound(id))
    }

    fn get_object_mut(&mut self, id: ObjectId) -> Result<&mut Object, ErrorKind> {
        self.objects.get_mut(id as usize).ok_or_else(|| ObjectNotFound(id))
    }

    fn lookup_facet(&self, name: &str) -> ObjectResult {
        self.facet_by_name
            .get(name)
            .cloned()
            .ok_or_else(|| FacetNotFound(name.to_string()))
    }
}