#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Document {
    pub definitions: Vec<Definition>,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum Definition {
    Operation(OperationDefinition),
    Fragment(FragmentDefinition),
    Schema(SchemaDefinition),
    SchemaExtension(SchemaExtension),
    ObjectType(ObjectTypeDefinition),
    ObjectTypeExtension(ObjectTypeExtension),
    ScalarType(ScalarTypeDefinition),
    ScalarTypeExtension(ScalarTypeExtension),
    InterfaceType(InterfaceTypeDefinition),
    InterfaceTypeExtension(InterfaceTypeExtension),
    UnionType(UnionTypeDefinition),
    UnionTypeExtension(UnionTypeExtension),
    EnumType(EnumTypeDefinition),
    EnumTypeExtension(EnumTypeExtension),
    InputObjectType(InputObjectTypeDefinition),
    InputObjectTypeExtension(InputObjectTypeExtension),
    Directive(DirectiveDefinition),
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct ScalarTypeDefinition {
    pub name: String,
    pub description: Option<String>,
    pub directives: Vec<Directive>,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct ScalarTypeExtension {
    pub name: String,
    pub directives: Vec<Directive>,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct InterfaceTypeDefinition {
    pub name: String,
    pub fields: Vec<FieldDefinition>,
    pub description: Option<String>,
    pub directives: Vec<Directive>,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct InterfaceTypeExtension {
    pub name: String,
    pub fields: Vec<FieldDefinition>,
    pub directives: Vec<Directive>,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct UnionTypeDefinition {
    pub name: String,
    pub description: Option<String>,
    pub directives: Vec<Directive>,
    pub types: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct UnionTypeExtension {
    pub name: String,
    pub directives: Vec<Directive>,
    pub types: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct EnumTypeDefinition {
    pub name: String,
    pub directives: Vec<Directive>,
    pub description: Option<String>,
    pub values: Vec<EnumValueDefinition>,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct EnumTypeExtension {
    pub name: String,
    pub directives: Vec<Directive>,
    pub values: Vec<EnumValueDefinition>,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct EnumValueDefinition {
    pub name: String,
    pub directives: Vec<Directive>,
    pub description: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct InputObjectTypeDefinition {
    pub name: String,
    pub directives: Vec<Directive>,
    pub description: Option<String>,
    pub fields: Vec<InputValueDefinition>,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct InputObjectTypeExtension {
    pub name: String,
    pub directives: Vec<Directive>,
    pub fields: Vec<InputValueDefinition>,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct DirectiveDefinition {
    pub name: String,
    pub description: Option<String>,
    pub locations: Vec<DirectiveLocation>,
    pub arguments: Vec<InputValueDefinition>,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum DirectiveLocation {
    QUERY,
    MUTATION,
    SUBSCRIPTION,
    FIELD,
    FRAGMENT_DEFINITION,
    FRAGMENT_SPREAD,
    INLINE_FRAGMENT,
    VARIABLE_DEFINITION,
    SCHEMA,
    SCALAR,
    OBJECT,
    FIELD_DEFINITION,
    ARGUMENT_DEFINITION,
    INTERFACE,
    UNION,
    ENUM,
    ENUM_VALUE,
    INPUT_OBJECT,
    INPUT_FIELD_DEFINITION,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct ObjectTypeDefinition {
    pub name: String,
    pub interfaces: Vec<String>,
    pub fields: Vec<FieldDefinition>,
    pub description: Option<String>,
    pub directives: Vec<Directive>,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct ObjectTypeExtension {
    pub name: String,
    pub interfaces: Vec<String>,
    pub fields: Vec<FieldDefinition>,
    pub directives: Vec<Directive>,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct FieldDefinition {
    pub name: String,
    pub type_reference: TypeReference,
    pub arguments: Vec<InputValueDefinition>,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct InputValueDefinition {
    pub name: String,
    pub type_reference: TypeReference,
    pub default_value: Option<Value>,
    pub description: Option<String>,
    pub directives: Vec<Directive>,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct SchemaDefinition {
    pub directives: Vec<Directive>,
    pub operation_type_definitions: Vec<OperationTypeDefinition>,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct SchemaExtension {
    pub directives: Vec<Directive>,
    pub operation_type_definitions: Vec<OperationTypeDefinition>,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct OperationTypeDefinition {
    pub operation: OperationType,
    pub type_name: String,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct OperationDefinition {
    pub operation_type: OperationType,
    pub name: Option<String>,
    pub selection_set: SelectionSet,
    pub variable_definitions: Vec<VariableDefinition>,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct VariableDefinition {
    pub name: String,
    pub type_reference: TypeReference,
    pub default_value: Option<Value>,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum TypeReference {
    NamedType(String),
    ListType(Box<TypeReference>),
    NonNullType(Box<TypeReference>),
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum Value {
    Variable(String),
    IntValue(String),
    FloatValue(String),
    StringValue(String),
    BooleanValue(bool),
    NullValue,
    EnumValue(String),
    ListValue(Box<Vec<Value>>),
    ObjectValue(Box<Vec<ObjectField>>),
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct ObjectField {
    pub name: String,
    pub value: Value,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct SelectionSet {
    pub selections: Vec<Selection>,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum Selection {
    Field(Field),
    FragmentSpread(FragmentSpread),
    InlineFragment(InlineFragment),
}
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Field {
    pub name: String,
    pub alias: Option<String>,
    pub selection_set: Option<SelectionSet>,
    pub directives: Vec<Directive>,
    pub arguments: Vec<Argument>
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct FragmentSpread {
    pub name: String,
}
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct InlineFragment {
    pub type_condition: Option<String>,
    pub selection_set: SelectionSet,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum OperationType {
    Query,
    Mutation,
    Subscription,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct FragmentDefinition {
    pub name: String,
    pub type_condition: String,
    pub selection_set: SelectionSet,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Directive {
    pub name: String,
    pub arguments: Vec<Argument>,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Argument {
    pub name: String,
    pub value: Value,
}
