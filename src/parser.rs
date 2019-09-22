use crate::ast::*;
use crate::lexer::*;
use Option::*;
use Result::*;
use TokenKind::*;
use TypeReference::*;


#[derive(Debug)]
pub struct ParsingError {
    pub message: String,
}
impl ParsingError {
    pub fn new(message_str: &str) -> ParsingError {
        ParsingError {
            message: String::from(message_str),
        }
    }
}

pub fn parse(lexer: &mut Lexer) -> Result<Document, ParsingError> {
    document(lexer)
}

fn document(lexer: &mut Lexer) -> Result<Document, ParsingError> {
    let definitions = many(
        lexer,
        TokenKind::SOF,
        parse_definition,
        TokenKind::END_OF_INPUT,
    )?;
    Ok(Document { definitions })
}

fn parse_definition(lexer: &mut Lexer) -> Result<Definition, ParsingError> {
    if peek(lexer, NAME) {
        match lexer.current_token_value().as_ref() {
            "query" | "mutation" | "subscription" | "fragment" => {
                return parse_executable_definition(lexer)
            }
            "schema" | "scalar" | "type" | "interface" | "union" | "enum" | "input"
            | "directive" => return parse_type_system_definition(lexer),
            "extend" => return parse_type_system_extension(lexer),
            value => return Err(ParsingError::new(&format!("unexpected string '{}'", value))),
        }
    } else if peek(lexer, BRACE_L) {
        return parse_executable_definition(lexer);
    } else if peek_description(lexer) {
        return parse_type_system_definition(lexer);
    }
    Err(ParsingError::new("unexpected token"))
}

fn parse_type_system_definition(lexer: &mut Lexer) -> Result<Definition, ParsingError> {
    let (token_kind, token_value) = if peek_description(lexer) {
        let lookahead_token = lookahead_lexer(lexer)?;
        (lookahead_token.kind, lookahead_token.get_value())
    } else {
        (lexer.current_token().kind, lexer.current_token_value())
    };
    if token_kind != NAME {
        return unexpected_token(token_kind, NAME);
    }
    match token_value.as_ref() {
        "schema" => return parse_schema_definition(lexer),
        "scalar" => return parse_scalar_type_definition(lexer),
        "type" => return parse_object_type_definition(lexer),
        "interface" => return parse_interface_type_definition(lexer),
        "union" => return parse_union_type_definition(lexer),
        "enum" => return parse_enum_type_definition(lexer),
        "input" => return parse_input_object_type_definition(lexer),
        "directive" => return parse_directive_definition(lexer),
        value => {
            return Err(ParsingError::new(&format!(
                "unexpected {} for type system definition",
                value
            )))
        }
    }
}

fn parse_directives(lexer: &mut Lexer, is_const: bool) -> Result<Vec<Directive>, ParsingError> {
    let mut result = Vec::new();
    loop {
        if !peek(lexer, AT) {
            break;
        }
        result.push(parse_directive(lexer, is_const)?);
    }
    Ok(result)
}
fn parse_directive(lexer: &mut Lexer, is_const: bool) -> Result<Directive, ParsingError> {
    let name = parse_name(lexer)?;
    let arguments = parse_arguments(lexer, is_const)?;
    Ok(Directive { name, arguments })
}

fn parse_arguments(lexer: &mut Lexer, is_const: bool) -> Result<Vec<Argument>, ParsingError> {
    let parse_fn = if is_const {
        parse_const_argument
    } else {
        parse_argument
    };
    if peek(lexer, PAREN_L) {
        many(lexer, PAREN_L, parse_fn, PAREN_R)
    } else {
        Ok(Vec::new())
    }
}

fn parse_argument(lexer: &mut Lexer) -> Result<Argument, ParsingError> {
    let name = parse_name(lexer)?;
    expect_token(lexer, COLON)?;
    let value = parse_value_value(lexer)?;
    Ok(Argument { name, value })
}

fn parse_const_argument(lexer: &mut Lexer) -> Result<Argument, ParsingError> {
    let name = parse_name(lexer)?;
    expect_token(lexer, COLON)?;
    let value = parse_const_value(lexer)?;
    Ok(Argument { name, value })
}

fn parse_schema_definition(lexer: &mut Lexer) -> Result<Definition, ParsingError> {
    expect_keyword(lexer, "schema")?;
    let directives = parse_directives(lexer, true)?;
    let operation_type_definitions =
        many(lexer, BRACE_L, parse_operation_type_definition, BRACE_R)?;
    Ok(Definition::Schema(SchemaDefinition {
        directives,
        operation_type_definitions,
    }))
}
fn parse_operation_type_definition(
    lexer: &mut Lexer,
) -> Result<OperationTypeDefinition, ParsingError> {
    let operation = parse_operation_type(lexer)?;
    expect_token(lexer, COLON)?;
    let type_name = parse_name(lexer)?;
    Ok(OperationTypeDefinition {
        operation,
        type_name,
    })
}
fn parse_scalar_type_definition(lexer: &mut Lexer) -> Result<Definition, ParsingError> {
    let description = parse_description(lexer)?;
    expect_keyword(lexer, "scalar")?;
    let name = parse_name(lexer)?;
    let directives = parse_directives(lexer, true)?;
    Ok(Definition::ScalarType(ScalarTypeDefinition {
        name,
        description,
        directives,
    }))
}
fn parse_object_type_definition(lexer: &mut Lexer) -> Result<Definition, ParsingError> {
    expect_keyword(lexer, "type")?;
    let description = parse_description(lexer)?;
    let name = parse_name(lexer)?;
    let interfaces = parse_implements_interfaces(lexer)?;
    let directives = parse_directives(lexer, true)?;
    let fields = parse_field_definitions(lexer)?;
    Ok(Definition::ObjectType(ObjectTypeDefinition {
        name,
        interfaces,
        fields,
        description,
        directives,
    }))
}

fn parse_field_definitions(lexer: &mut Lexer) -> Result<Vec<FieldDefinition>, ParsingError> {
    many(lexer, BRACE_L, parse_field_definition, BRACE_R)
}

fn parse_field_definition(lexer: &mut Lexer) -> Result<FieldDefinition, ParsingError> {
    let name = parse_name(lexer)?;
    let arguments = parse_argument_definitions(lexer)?;
    expect_token(lexer, COLON)?;
    let type_reference = parse_type_reference(lexer)?;
    Ok(FieldDefinition {
        name,
        type_reference,
        arguments,
    })
}
fn parse_argument_definitions(
    lexer: &mut Lexer,
) -> Result<Vec<InputValueDefinition>, ParsingError> {
    if !peek(lexer, PAREN_L) {
        return Ok(Vec::new());
    }
    many(lexer, PAREN_L, parse_input_value_definition, PAREN_R)
}

fn parse_input_value_definition(lexer: &mut Lexer) -> Result<InputValueDefinition, ParsingError> {
    let description = parse_description(lexer)?;
    let name = parse_name(lexer)?;
    expect_token(lexer, COLON)?;
    let type_reference = parse_type_reference(lexer)?;
    let default_value = match expect_optional_token(lexer, EQUALS)? {
        Some(_) => Some(parse_const_value(lexer)?),
        None => None,
    };
    let directives = parse_directives(lexer, true)?;
    Ok(InputValueDefinition {
        name,
        type_reference,
        default_value,
        description,
        directives,
    })
}

fn parse_implements_interfaces(lexer: &mut Lexer) -> Result<Vec<String>, ParsingError> {
    if !expect_optional_keyword(lexer, "implements")? {
        return Ok(Vec::new());
    }
    let mut result = Vec::new();
    expect_optional_token(lexer, AMP)?;
    loop {
        result.push(parse_name(lexer)?);
        if expect_optional_token(lexer, AMP)?.is_none() {
            break;
        }
    }
    Ok(result)
}
fn parse_interface_type_definition(lexer: &mut Lexer) -> Result<Definition, ParsingError> {
    expect_keyword(lexer, "interface")?;
    let description = parse_description(lexer)?;
    let name = parse_name(lexer)?;
    let directives = parse_directives(lexer, true)?;
    let fields = parse_field_definitions(lexer)?;
    Ok(Definition::InterfaceType(InterfaceTypeDefinition {
        description,
        name,
        fields,
        directives,
    }))
}
fn parse_union_type_definition(lexer: &mut Lexer) -> Result<Definition, ParsingError> {
    let description = parse_description(lexer)?;
    expect_keyword(lexer, "union")?;
    let name = parse_name(lexer)?;
    let directives = parse_directives(lexer, true)?;
    let types = parse_union_type_members(lexer)?;
    Ok(Definition::UnionType(UnionTypeDefinition {
        name,
        description,
        directives,
        types,
    }))
}

fn parse_union_type_members(lexer: &mut Lexer) -> Result<Vec<String>, ParsingError> {
    let mut result = Vec::new();
    if expect_optional_token(lexer, EQUALS)?.is_some() {
        expect_optional_token(lexer, PIPE)?;
        loop {
            result.push(parse_name(lexer)?);
            if expect_optional_token(lexer, PIPE)?.is_none() {
                break;
            }
        }
    };
    Ok(result)
}
fn parse_enum_type_definition(lexer: &mut Lexer) -> Result<Definition, ParsingError> {
    let description = parse_description(lexer)?;
    expect_keyword(lexer, "enum")?;
    let name = parse_name(lexer)?;
    let directives = parse_directives(lexer, true)?;
    let values = parse_enum_values_definition(lexer)?;
    Ok(Definition::EnumType(EnumTypeDefinition {
        name,
        directives,
        description,
        values,
    }))
}
fn parse_enum_values_definition(
    lexer: &mut Lexer,
) -> Result<Vec<EnumValueDefinition>, ParsingError> {
    if peek(lexer, BRACE_L) {
        many(lexer, BRACE_L, parse_enum_value_definition, BRACE_R)
    } else {
        Ok(Vec::new())
    }
}
fn parse_enum_value_definition(lexer: &mut Lexer) -> Result<EnumValueDefinition, ParsingError> {
    let description = parse_description(lexer)?;
    let name = parse_name(lexer)?;
    let directives = parse_directives(lexer, true)?;
    Ok(EnumValueDefinition {
        name,
        description,
        directives,
    })
}
fn parse_input_object_type_definition(lexer: &mut Lexer) -> Result<Definition, ParsingError> {
    let description = parse_description(lexer)?;
    expect_keyword(lexer, "input")?;
    let name = parse_name(lexer)?;
    let directives = parse_directives(lexer, true)?;
    let fields = parse_input_fields_definition(lexer)?;
    Ok(Definition::InputObjectType(InputObjectTypeDefinition {
        description,
        name,
        directives,
        fields,
    }))
}

fn parse_input_fields_definition(
    lexer: &mut Lexer,
) -> Result<Vec<InputValueDefinition>, ParsingError> {
    if peek(lexer, BRACE_L) {
        many(lexer, BRACE_L, parse_input_value_definition, BRACE_R)
    } else {
        Ok(Vec::new())
    }
}

fn parse_directive_definition(lexer: &mut Lexer) -> Result<Definition, ParsingError> {
    println!("parse directive def");
    let description = parse_description(lexer)?;
    expect_keyword(lexer, "directive")?;
    expect_token(lexer, AT)?;
    let name = parse_name(lexer)?;
    let arguments = parse_argument_definitions(lexer)?;
    expect_keyword(lexer, "on")?;
    let locations = parse_directive_locations(lexer)?;
    Ok(Definition::Directive(DirectiveDefinition {
        description,
        name,
        arguments,
        locations,
    }))
}

fn parse_directive_locations(lexer: &mut Lexer) -> Result<Vec<DirectiveLocation>, ParsingError> {
    expect_optional_token(lexer, PIPE)?;
    let mut result = Vec::new();
    loop {
        result.push(parse_directive_location(lexer)?);
        if expect_optional_token(lexer, PIPE)?.is_none() {
            break;
        }
    }
    Ok(result)
}
fn parse_directive_location(lexer: &mut Lexer) -> Result<DirectiveLocation, ParsingError> {
    let name = parse_name(lexer)?;
    match name.as_ref() {
        "QUERY" => Ok(DirectiveLocation::QUERY),
        "MUTATION" => Ok(DirectiveLocation::MUTATION),
        "SUBSCRIPTION" => Ok(DirectiveLocation::SUBSCRIPTION),
        "FIELD" => Ok(DirectiveLocation::FIELD),
        "FRAGMENT_DEFINITION" => Ok(DirectiveLocation::FRAGMENT_DEFINITION),
        "FRAGMENT_SPREAD" => Ok(DirectiveLocation::FRAGMENT_SPREAD),
        "INLINE_FRAGMENT" => Ok(DirectiveLocation::INLINE_FRAGMENT),
        "VARIABLE_DEFINITION" => Ok(DirectiveLocation::VARIABLE_DEFINITION),
        "SCHEMA" => Ok(DirectiveLocation::SCHEMA),
        "SCALAR" => Ok(DirectiveLocation::SCALAR),
        "OBJECT" => Ok(DirectiveLocation::OBJECT),
        "FIELD_DEFINITION" => Ok(DirectiveLocation::FIELD_DEFINITION),
        "ARGUMENT_DEFINITION" => Ok(DirectiveLocation::ARGUMENT_DEFINITION),
        "INTERFACE" => Ok(DirectiveLocation::INTERFACE),
        "UNION" => Ok(DirectiveLocation::UNION),
        "ENUM" => Ok(DirectiveLocation::ENUM),
        "ENUM_VALUE" => Ok(DirectiveLocation::ENUM_VALUE),
        "INPUT_OBJECT" => Ok(DirectiveLocation::INPUT_OBJECT),
        "INPUT_FIELD_DEFINITION" => Ok(DirectiveLocation::INPUT_FIELD_DEFINITION),
        _ => Err(ParsingError::new("unexpected directive location")),
    }
}

fn unexpected_token<T>(actual: TokenKind, expected: TokenKind) -> Result<T, ParsingError> {
    Err(ParsingError::new(&format!(
        "Unexecpted {:?}, but expected {:?}",
        actual, expected
    )))
}

fn parse_description(lexer: &mut Lexer) -> Result<Option<String>, ParsingError> {
    if peek_description(lexer) {
        Ok(Some(get_string(parse_string_literal(lexer)?)))
    } else {
        Ok(None)
    }
}
fn get_string(string_value: Value) -> String {
    match string_value {
        Value::StringValue(value) => value,
        _ => panic!("shoud not happen"),
    }
}

fn parse_type_system_extension(lexer: &mut Lexer) -> Result<Definition, ParsingError> {
    let token = lookahead_lexer(lexer)?;
    if token.kind != NAME {
        return unexpected_token(token.kind, NAME);
    }

    match token.value.as_ref().unwrap().as_ref() {
        "schema" => {
            return parse_schema_extension(lexer);
        }
        "scalar" => {
            return parse_scalar_type_extension(lexer);
        }
        "type" => {
            return parse_object_type_extension(lexer);
        }
        "interface" => {
            return parse_interface_type_extension(lexer);
        }
        "union" => {
            return parse_union_type_extension(lexer);
        }
        "enum" => {
            return parse_enum_type_extension(lexer);
        }
        "input" => {
            return parse_input_object_type_extension(lexer);
        }
        _ => return Err(ParsingError::new("unexpected")),
    }
}
fn parse_schema_extension(lexer: &mut Lexer) -> Result<Definition, ParsingError> {
    expect_keyword(lexer, "extend")?;
    expect_keyword(lexer, "schema")?;
    let directives = parse_directives(lexer, true)?;
    let operation_type_definitions = if peek(lexer, BRACE_L) {
        many(lexer, BRACE_L, parse_operation_type_definition, BRACE_R)?
    } else {
        Vec::new()
    };
    if directives.len() == 0 && operation_type_definitions.len() == 0 {
        return Err(ParsingError::new("unexpected"));
    };
    Ok(Definition::SchemaExtension(SchemaExtension {
        directives,
        operation_type_definitions,
    }))
}

fn parse_scalar_type_extension(lexer: &mut Lexer) -> Result<Definition, ParsingError> {
    expect_keyword(lexer, "extend")?;
    expect_keyword(lexer, "scalar")?;
    let name = parse_name(lexer)?;
    let directives = parse_directives(lexer, true)?;
    if directives.len() == 0 {
        return Err(ParsingError::new("'extend scalar' requires directives"));
    };
    Ok(Definition::ScalarTypeExtension(ScalarTypeExtension {
        directives,
        name,
    }))
}
fn parse_object_type_extension(lexer: &mut Lexer) -> Result<Definition, ParsingError> {
    expect_keyword(lexer, "extend")?;
    expect_keyword(lexer, "type")?;
    let name = parse_name(lexer)?;
    let interfaces = parse_implements_interfaces(lexer)?;
    let directives = parse_directives(lexer, true)?;
    let fields = parse_field_definitions(lexer)?;
    if interfaces.len() == 0 && directives.len() == 0 && fields.len() == 0 {
        return Err(ParsingError::new("unexpected"));
    };
    Ok(Definition::ObjectTypeExtension(ObjectTypeExtension {
        directives,
        name,
        interfaces,
        fields,
    }))
}

fn parse_interface_type_extension(lexer: &mut Lexer) -> Result<Definition, ParsingError> {
    expect_keyword(lexer, "extend")?;
    expect_keyword(lexer, "interface")?;
    let name = parse_name(lexer)?;
    let directives = parse_directives(lexer, true)?;
    let fields = parse_field_definitions(lexer)?;
    if directives.len() == 0 && fields.len() == 0 {
        return Err(ParsingError::new("unexpected"));
    };
    Ok(Definition::InterfaceTypeExtension(InterfaceTypeExtension {
        directives,
        name,
        fields,
    }))
}

fn parse_union_type_extension(lexer: &mut Lexer) -> Result<Definition, ParsingError> {
    expect_keyword(lexer, "extend")?;
    expect_keyword(lexer, "union")?;
    let name = parse_name(lexer)?;
    let directives = parse_directives(lexer, true)?;
    let types = parse_union_type_members(lexer)?;
    if directives.len() == 0 && types.len() == 0 {
        return Err(ParsingError::new("unexpected"));
    };
    Ok(Definition::UnionTypeExtension(UnionTypeExtension {
        directives,
        name,
        types,
    }))
}

fn parse_enum_type_extension(lexer: &mut Lexer) -> Result<Definition, ParsingError> {
    expect_keyword(lexer, "extend")?;
    expect_keyword(lexer, "enum")?;
    let name = parse_name(lexer)?;
    let directives = parse_directives(lexer, true)?;
    let values = parse_enum_values_definition(lexer)?;
    if directives.len() == 0 && values.len() == 0 {
        return Err(ParsingError::new("unexpected"));
    };
    Ok(Definition::EnumTypeExtension(EnumTypeExtension {
        directives,
        name,
        values,
    }))
}
fn parse_input_object_type_extension(lexer: &mut Lexer) -> Result<Definition, ParsingError> {
    expect_keyword(lexer, "extend")?;
    expect_keyword(lexer, "input")?;
    let name = parse_name(lexer)?;
    let directives = parse_directives(lexer, true)?;
    let fields = parse_input_fields_definition(lexer)?;
    if directives.len() == 0 && fields.len() == 0 {
        return Err(ParsingError::new("unexpected"));
    };
    Ok(Definition::InputObjectTypeExtension(
        InputObjectTypeExtension {
            directives,
            name,
            fields,
        },
    ))
}

fn parse_executable_definition(lexer: &mut Lexer) -> Result<Definition, ParsingError> {
    if peek(lexer, NAME) {
        let name_token = lexer.tokens.last().unwrap();
        let name_value = name_token.value.as_ref().unwrap().as_ref();
        return match name_value {
            "query" | "mutation" | "subscription" => parse_operation_definition(lexer),
            "fragment" => parse_fragment_definition(lexer),
            _ => Err(ParsingError::new(&format!(
                "Illegal operation name {}",
                name_value
            ))),
        };
    } else if peek(lexer, BRACE_L) {
        return parse_operation_definition(lexer);
    }
    Err(ParsingError::new("unexpected"))
}

fn parse_fragment_definition(lexer: &mut Lexer) -> Result<Definition, ParsingError> {
    expect_keyword(lexer, "fragment")?;
    let name = parse_fragment_name(lexer)?;
    expect_keyword(lexer, "on")?;
    let type_condition = parse_name(lexer)?;
    let selection_set = parse_selection_set(lexer)?;
    Ok(Definition::Fragment(FragmentDefinition {
        name,
        type_condition,
        selection_set,
    }))
}

fn parse_operation_definition(lexer: &mut Lexer) -> Result<Definition, ParsingError> {
    if peek(lexer, BRACE_L) {
        let selection_set = parse_selection_set(lexer)?;
        return Ok(Definition::Operation(OperationDefinition {
            operation_type: OperationType::Query,
            name: None,
            selection_set,
            variable_definitions: Vec::new(),
        }));
    }
    let operation_type = parse_operation_type(lexer)?;
    let name = match peek(lexer, NAME) {
        true => Some(parse_name(lexer)?),
        false => None,
    };
    let variable_definitions = parse_variable_definitions(lexer)?;
    let selection_set = parse_selection_set(lexer)?;
    Ok(Definition::Operation(OperationDefinition {
        operation_type,
        name,
        selection_set,
        variable_definitions,
    }))
}

fn parse_variable_definitions(lexer: &mut Lexer) -> Result<Vec<VariableDefinition>, ParsingError> {
    match peek(lexer, PAREN_L) {
        true => many(lexer, PAREN_L, parse_variable_definition, PAREN_R),
        false => Ok(vec![]),
    }
}
fn parse_variable_definition(lexer: &mut Lexer) -> Result<VariableDefinition, ParsingError> {
    let name = parse_variable(lexer)?;
    expect_token(lexer, COLON)?;
    let type_reference = parse_type_reference(lexer)?;
    let default_value = match expect_optional_token(lexer, EQUALS)? {
        Some(_) => Some(parse_value_literal(lexer, true)?),
        None => None,
    };
    Ok(VariableDefinition {
        name,
        type_reference,
        default_value,
    })
}

fn parse_value_literal(lexer: &mut Lexer, is_const: bool) -> Result<Value, ParsingError> {
    let token = lexer.current_token();
    let token_value = lexer.current_token_value_safe();
    match token.kind {
        BRACKET_L => parse_list(lexer, is_const),
        BRACE_L => parse_object(lexer, is_const),
        INT => {
            advance_lexer(lexer)?;
            Ok(Value::IntValue(token_value.unwrap()))
        }
        FLOAT => {
            advance_lexer(lexer)?;
            Ok(Value::FloatValue(token_value.unwrap()))
        }
        STRING | BLOCK_STRING => parse_string_literal(lexer),
        NAME => {
            let name_value = token_value.unwrap();
            if name_value == "true" || name_value == "false" {
                advance_lexer(lexer)?;
                Ok(Value::BooleanValue(name_value == "true"))
            } else if name_value == "null" {
                advance_lexer(lexer)?;
                Ok(Value::NullValue)
            } else {
                advance_lexer(lexer)?;
                Ok(Value::EnumValue(name_value))
            }
        }
        DOLLAR => {
            if !is_const {
                Ok(Value::Variable(parse_variable(lexer)?))
            } else {
                Err(ParsingError::new(
                    "variable not allowed in const expression",
                ))
            }
        }
        _ => Err(ParsingError::new("ERROR")),
    }
}

fn parse_const_value(lexer: &mut Lexer) -> Result<Value, ParsingError> {
    parse_value_literal(lexer, true)
}

fn parse_value_value(lexer: &mut Lexer) -> Result<Value, ParsingError> {
    parse_value_literal(lexer, false)
}

fn parse_string_literal(lexer: &mut Lexer) -> Result<Value, ParsingError> {
    let value = lexer.current_token_value();
    advance_lexer(lexer)?;
    Ok(Value::StringValue(value))
}

fn parse_list(lexer: &mut Lexer, is_const: bool) -> Result<Value, ParsingError> {
    let parse_fn = if is_const {
        parse_const_value
    } else {
        parse_value_value
    };
    let values = any(lexer, BRACKET_L, parse_fn, BRACKET_R)?;
    Ok(Value::ListValue(Box::new(values)))
}
fn parse_object(lexer: &mut Lexer, is_const: bool) -> Result<Value, ParsingError> {
    let parse_fn = if is_const {
        parse_object_field_const
    } else {
        parse_object_field_value
    };
    let fields = any(lexer, BRACE_L, parse_fn, BRACE_R)?;
    Ok(Value::ObjectValue(Box::new(fields)))
}

fn parse_object_field_const(lexer: &mut Lexer) -> Result<ObjectField, ParsingError> {
    parse_object_field(lexer, true)
}
fn parse_object_field_value(lexer: &mut Lexer) -> Result<ObjectField, ParsingError> {
    parse_object_field(lexer, false)
}

fn parse_object_field(lexer: &mut Lexer, is_const: bool) -> Result<ObjectField, ParsingError> {
    let name = parse_name(lexer)?;
    expect_token(lexer, COLON)?;
    Ok(ObjectField {
        name,
        value: parse_value_literal(lexer, is_const)?,
    })
}
fn parse_variable(lexer: &mut Lexer) -> Result<String, ParsingError> {
    expect_token(lexer, DOLLAR)?;
    parse_name(lexer)
}

fn parse_type_reference(lexer: &mut Lexer) -> Result<TypeReference, ParsingError> {
    let mut _type: TypeReference;
    if expect_optional_token(lexer, BRACKET_L)?.is_some() {
        _type = parse_type_reference(lexer)?;
        expect_token(lexer, BRACKET_R)?;
        _type = TypeReference::ListType(Box::new(_type));
    } else {
        _type = TypeReference::NamedType(parse_name(lexer)?);
    }
    if expect_optional_token(lexer, BANG)?.is_some() {
        _type = TypeReference::NonNullType(Box::new(_type));
    }
    return Ok(_type);
}

fn parse_operation_type(lexer: &mut Lexer) -> Result<OperationType, ParsingError> {
    let name = expect_token(lexer, NAME)?;
    let name_value = name.value.as_ref();
    match name_value.unwrap().as_ref() {
        "query" => Ok(OperationType::Query),
        "mutation" => Ok(OperationType::Mutation),
        "subscription" => Ok(OperationType::Subscription),
        _ => Err(ParsingError::new("not yet")),
    }
}
fn parse_selection_set(lexer: &mut Lexer) -> Result<SelectionSet, ParsingError> {
    let selections = many(lexer, BRACE_L, parse_selection, BRACE_R)?;
    Ok(SelectionSet { selections })
}

fn parse_selection(lexer: &mut Lexer) -> Result<Selection, ParsingError> {
    if peek(lexer, SPREAD) {
        return parse_fragment_selection(lexer);
    }
    Ok(Selection::Field(parse_field(lexer)?))
}

fn parse_fragment_selection(lexer: &mut Lexer) -> Result<Selection, ParsingError> {
    expect_token(lexer, SPREAD)?;
    let has_type_condition = expect_optional_keyword(lexer, "on")?;
    if !has_type_condition && peek(lexer, NAME) {
        let name = parse_fragment_name(lexer)?;
        return Ok(Selection::FragmentSpread(FragmentSpread { name }));
    }
    let type_condition = if has_type_condition {
        Some(parse_name(lexer)?)
    } else {
        None
    };
    let selection_set = parse_selection_set(lexer)?;
    Ok(Selection::InlineFragment(InlineFragment {
        type_condition,
        selection_set,
    }))
}

fn parse_fragment_name(lexer: &mut Lexer) -> Result<String, ParsingError> {
    if lexer.current_token_value() == "on" {
        return Err(ParsingError::new("Unexpected 'on'"));
    }
    return parse_name(lexer);
}

fn parse_field(lexer: &mut Lexer) -> Result<Field, ParsingError> {
    let name_or_alias = parse_name(lexer)?;
    let name: String;
    let mut alias: Option<String> = None;
    let colon = expect_optional_token(lexer, COLON)?;
    if colon.is_some() {
        alias = Some(name_or_alias);
        name = parse_name(lexer)?;
    } else {
        name = name_or_alias;
    }
    let arguments = parse_arguments(lexer, false)?;
    let directives = parse_directives(lexer, false)?;
    let mut selection_set: Option<SelectionSet> = None;
    if peek(lexer, BRACE_L) {
        selection_set = Some(parse_selection_set(lexer)?);
    }
    Ok(Field {
        name,
        alias,
        selection_set,
        arguments,
        directives,
    })
}

fn parse_name(lexer: &mut Lexer) -> Result<String, ParsingError> {
    let name_token = expect_token(lexer, NAME)?;
    Ok(name_token.value.as_ref().unwrap().clone())
}

fn peek(lexer: &mut Lexer, kind: TokenKind) -> bool {
    lexer.current_token().kind == kind
}
fn peek_description(lexer: &mut Lexer) -> bool {
    peek(lexer, STRING) || peek(lexer, BLOCK_STRING)
}

fn any<T>(
    lexer: &mut Lexer,
    open_kind: TokenKind,
    parse_fn: fn(&mut Lexer) -> Result<T, ParsingError>,
    close_king: TokenKind,
) -> Result<Vec<T>, ParsingError> {
    expect_token(lexer, open_kind)?;
    let mut result: Vec<T> = Vec::new();
    while expect_optional_token(lexer, close_king)?.is_none() {
        result.push(parse_fn(lexer)?);
    }
    Result::Ok(result)
}

fn many<T>(
    lexer: &mut Lexer,
    open_kind: TokenKind,
    parse_fn: fn(&mut Lexer) -> Result<T, ParsingError>,
    close_king: TokenKind,
) -> Result<Vec<T>, ParsingError> {
    expect_token(lexer, open_kind)?;
    let mut result: Vec<T> = Vec::new();
    result.push(parse_fn(lexer)?);
    while expect_optional_token(lexer, close_king)?.is_none() {
        result.push(parse_fn(lexer)?);
    }
    Result::Ok(result)
}

fn expect_keyword<'a>(lexer: &mut Lexer, value: &str) -> Result<(), ParsingError> {
    let token = lexer.current_token();
    if token.kind == TokenKind::NAME && token.value.as_ref().unwrap() == value {
        advance_lexer(lexer)?;
        return Ok(());
    }
    Err(ParsingError::new(&format!("expect {}", value)))
}

fn expect_optional_keyword(lexer: &mut Lexer, value: &str) -> Result<bool, ParsingError> {
    let token = lexer.current_token();
    if token.kind == TokenKind::NAME && token.value.as_ref().unwrap() == value {
        advance_lexer(lexer)?;
        return Ok(true);
    }
    Ok(false)
}

fn expect_token(lexer: &mut Lexer, kind: TokenKind) -> Result<&Token, ParsingError> {
    let last_token = lexer.current_token();
    let last_kind = last_token.kind;
    if last_kind == kind {
        advance_lexer(lexer)?;
        Result::Ok(lexer.prev_token())
    } else {
        Result::Err(ParsingError {
            message: format!("Expected {:?}, but got {:?}", kind, last_kind),
        })
    }
}

fn expect_optional_token(
    lexer: &mut Lexer,
    kind: TokenKind,
) -> Result<Option<&Token>, ParsingError> {
    let current_token = lexer.current_token();
    if current_token.kind == kind {
        advance_lexer(lexer)?;
        Ok(Option::Some(lexer.prev_token()))
    } else {
        Ok(Option::None)
    }
}

fn advance_lexer(lexer: &mut Lexer) -> Result<&Token, ParsingError> {
    match lexer.advance() {
        Ok(token) => Ok(token),
        Err(lexer_error) => Err(ParsingError::new(&format!(
            "Lexer error: {}",
            lexer_error.message
        ))),
    }
}
fn lookahead_lexer(lexer: &mut Lexer) -> Result<Token, ParsingError> {
    match lexer.lookahead() {
        Ok(token) => Ok(token),
        Err(lexer_error) => Err(ParsingError::new(&format!(
            "Lexer error: {}",
            lexer_error.message
        ))),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! enum_field {
        ($enum:ident $field:ident $exp:expr) => {
            if let $enum::$field(value) = $exp {
                value
            } else {
                panic!("unexpected {}",);
            }
        };
    }

    #[test]
    fn parse_simple_query() {
        let mut lexer = Lexer::new("{foo}");
        let result = parse(&mut lexer);
        assert!(result.is_ok());
        let field = Field {
            name: String::from("foo"),
            alias: None,
            selection_set: None,
            directives: Vec::new(),
            arguments: Vec::new(),
        };
        let selection_set = SelectionSet {
            selections: vec![Selection::Field(field)],
        };

        let document = result.unwrap();
        let definition = Definition::Operation(OperationDefinition {
            name: None,
            operation_type: OperationType::Query,
            variable_definitions: Vec::new(),
            selection_set,
        });
        let expected = Document {
            definitions: vec![definition],
        };
        assert_eq!(document, expected);
    }
    #[test]
    fn parse_field_alias() {
        let mut lexer = Lexer::new("{alias: foo}");
        let result = parse(&mut lexer);
        let definition = &result.unwrap().definitions[0];
        let operation_definition = enum_field!(Definition Operation definition);
        let field = enum_field!(Selection Field &operation_definition.selection_set.selections[0]);
        assert_eq!(field.alias.as_ref().unwrap(), "alias");
    }

    #[test]
    fn parse_field_with_args() {
        let mut lexer = Lexer::new(r#"{foo(arg:"hello")}"#);
        let result = parse(&mut lexer);
        let definition = &result.unwrap().definitions[0];
        let operation_definition = enum_field!(Definition Operation definition);
        let field = enum_field!(Selection Field &operation_definition.selection_set.selections[0]);
        let argument = &field.arguments[0];
        assert_eq!(argument.name, "arg");
        assert_eq!(argument.value, Value::StringValue(String::from("hello")));
    }

    #[test]
    fn parse_fragment_definition() {
        let mut lexer = Lexer::new("fragment Foo on Bar { field }");
        let result = parse(&mut lexer);
        let definition = &result.unwrap().definitions[0];
        let fragment_definition = enum_field!(Definition Fragment definition);
        assert_eq!(fragment_definition.name, "Foo");
        assert_eq!(fragment_definition.type_condition, "Bar");
        let selection = &fragment_definition.selection_set.selections[0];
        let field = enum_field!(Selection Field selection);
        assert_eq!(field.name, "field");
    }

    #[test]
    fn parse_object_definition() {
        let mut lexer = Lexer::new("type Foo{bar: String}");
        let result = parse(&mut lexer);
        let definition = &result.unwrap().definitions[0];
        let object_type_definition = enum_field!(Definition ObjectType definition);
        assert_eq!(object_type_definition.name, "Foo");
        let field_definiton = &object_type_definition.fields[0];
        assert_eq!(field_definiton.name, "bar");
        assert_eq!(
            field_definiton.type_reference,
            TypeReference::NamedType(String::from("String"))
        );
    }
        #[test]
    fn parse_object_extension() {
        let mut lexer = Lexer::new("extend type Foo{bar: String}");
        let result = parse(&mut lexer);
        let definition = &result.unwrap().definitions[0];
        let object_type_extension = enum_field!(Definition ObjectTypeExtension definition);
        assert_eq!(object_type_extension.name, "Foo");
        let field_definiton = &object_type_extension.fields[0];
        assert_eq!(field_definiton.name, "bar");
        assert_eq!(
            field_definiton.type_reference,
            TypeReference::NamedType(String::from("String"))
        );
    }


    #[test]
    fn parse_schema_definition() {
        let mut lexer = Lexer::new("schema { query: MyQuery}");
        let result = parse(&mut lexer);
        let definition = &result.unwrap().definitions[0];
        let schema_definition = enum_field!(Definition Schema definition);
        let op_type_def = &schema_definition.operation_type_definitions[0];
        assert_eq!(op_type_def.operation, OperationType::Query);
        assert_eq!(op_type_def.type_name, "MyQuery");
    }

    #[test]
    fn parse_schema_extension() {
        let mut lexer = Lexer::new("extend schema { mutation: MyMutation}");
        let result = parse(&mut lexer);
        let definition = &result.unwrap().definitions[0];
        let schema_extension = enum_field!(Definition SchemaExtension definition);
        let op_type_def = &schema_extension.operation_type_definitions[0];
        assert_eq!(op_type_def.operation, OperationType::Mutation);
        assert_eq!(op_type_def.type_name, "MyMutation");
    }
    #[test]
    fn parse_interface_definition() {
        let mut lexer = Lexer::new("interface MyI { field : Int }");
        let result = parse(&mut lexer);
        let definition = &result.unwrap().definitions[0];
        let interface_definition = enum_field!(Definition InterfaceType definition);
        assert_eq!(interface_definition.name, "MyI");
        let field_definiton = &interface_definition.fields[0];
        assert_eq!(field_definiton.name, "field");
        assert_eq!(
            field_definiton.type_reference,
            TypeReference::NamedType(String::from("Int"))
        );
    }

    #[test]
    fn parse_interface_extension() {
        let mut lexer = Lexer::new("extend interface MyI { field : Int }");
        let result = parse(&mut lexer);
        let definition = &result.unwrap().definitions[0];
        let interface_extension = enum_field!(Definition InterfaceTypeExtension definition);
        assert_eq!(interface_extension.name, "MyI");
        let field_definiton = &interface_extension.fields[0];
        assert_eq!(field_definiton.name, "field");
        assert_eq!(
            field_definiton.type_reference,
            TypeReference::NamedType(String::from("Int"))
        );
    }

    #[test]
    fn parse_enum_definition() {
        let mut lexer = Lexer::new("enum MyEnum { FOO, BAR }");
        let result = parse(&mut lexer);
        let definition = &result.unwrap().definitions[0];
        let enum_definition = enum_field!(Definition EnumType definition);
        assert_eq!(enum_definition.name, "MyEnum");
        let value1 = &enum_definition.values[0];
        let value2 = &enum_definition.values[1];
        assert_eq!(value1.name, "FOO");
        assert_eq!(value2.name, "BAR");
    }

    #[test]
    fn parse_enum_extension() {
        let mut lexer = Lexer::new("extend enum MyEnum { FOO, BAR }");
        let result = parse(&mut lexer);
        let definition = &result.unwrap().definitions[0];
        let enum_extension = enum_field!(Definition EnumTypeExtension definition);
        assert_eq!(enum_extension.name, "MyEnum");
        let value1 = &enum_extension.values[0];
        let value2 = &enum_extension.values[1];
        assert_eq!(value1.name, "FOO");
        assert_eq!(value2.name, "BAR");
    }

    #[test]
    fn parse_union_definition() {
        let mut lexer = Lexer::new("union MyUnion = A | B | C");
        let result = parse(&mut lexer);
        let definition = &result.unwrap().definitions[0];
        let union_definition = enum_field!(Definition UnionType definition);
        assert_eq!(union_definition.name, "MyUnion");
        let type1 = &union_definition.types[0];
        let type2 = &union_definition.types[1];
        let type3 = &union_definition.types[2];
        assert_eq!(type1, "A");
        assert_eq!(type2, "B");
        assert_eq!(type3, "C");
    }

    #[test]
    fn parse_union_extension() {
        let mut lexer = Lexer::new("extend union MyUnion = A");
        let result = parse(&mut lexer);
        let definition = &result.unwrap().definitions[0];
        let union_extension = enum_field!(Definition UnionTypeExtension definition);
        assert_eq!(union_extension.name, "MyUnion");
        let type1 = &union_extension.types[0];
        assert_eq!(type1, "A");
    }

    #[test]
    fn parse_directive_definition() {
        let mut lexer = Lexer::new("directive @MyDirective on FIELD_DEFINITION");
        let result = parse(&mut lexer);
        let definition = &result.unwrap().definitions[0];
        let directive_definition = enum_field!(Definition Directive definition);
        assert_eq!(directive_definition.name, "MyDirective");
        let location = directive_definition.locations[0];
        assert_eq!(location, DirectiveLocation::FIELD_DEFINITION);
    }

    #[test]
    fn parse_input_definition() {
        let mut lexer = Lexer::new("input MyInput {field: [Bool!]!}");
        let result = parse(&mut lexer);
        let definition = &result.unwrap().definitions[0];
        let input_definition = enum_field!(Definition InputObjectType definition);
        assert_eq!(input_definition.name, "MyInput");
        let field = &input_definition.fields[0];
        assert_eq!(field.name, "field");
        let non_null_bool = NonNullType(Box::new(NamedType(String::from("Bool"))));
        let type_ref = NonNullType(Box::new(ListType(Box::new(non_null_bool))));
        assert_eq!(field.type_reference, type_ref);
    }

    #[test]
    fn parse_input_extension() {
        let mut lexer = Lexer::new("extend input MyInput {field: Bool!}");
        let result = parse(&mut lexer);
        let definition = &result.unwrap().definitions[0];
        let input_extension = enum_field!(Definition InputObjectTypeExtension definition);
        assert_eq!(input_extension.name, "MyInput");
        let field = &input_extension.fields[0];
        assert_eq!(field.name, "field");
        let type_ref = NonNullType(Box::new(NamedType(String::from("Bool"))));
        assert_eq!(field.type_reference, type_ref);
    }

    #[test]
    fn parse_scalar_definition() {
        let mut lexer = Lexer::new("scalar MyScalar");
        let result = parse(&mut lexer);
        let definition = &result.unwrap().definitions[0];
        let scalar_type = enum_field!(Definition ScalarType definition);
        assert_eq!(scalar_type.name, "MyScalar");
    }

    // #[test]
    // fn parse_scalar_extension() {
    //     let mut lexer = Lexer::new("extend scalar MyScalar @MyDirective");
    //     let result = parse(&mut lexer);
    //     let definition = &result.unwrap().definitions[0];
    //     let scalar_type = enum_field!(Definition ScalarTypeExtension definition);
    //     assert_eq!(scalar_type.name, "MyScalar");
    //     let directive = Directive {
    //         name: String::from("MyDirective"),
    //         arguments: Vec::new()
    //     };
    //     assert_eq!(scalar_type.directives, vec![directive]);
    // }

    #[test]
    fn parse_two_level_query() {
        let mut lexer = Lexer::new("{foo{bar}}");
        let result = parse(&mut lexer);
        assert!(result.is_ok());
        let bar = Field {
            name: String::from("bar"),
            alias: None,
            selection_set: None,
            directives: Vec::new(),
            arguments: Vec::new(),
        };
        let selection_set_foo = SelectionSet {
            selections: vec![Selection::Field(bar)],
        };
        let foo = Field {
            name: String::from("foo"),
            alias: None,
            selection_set: Some(selection_set_foo),
            directives: Vec::new(),
            arguments: Vec::new(),
        };
        let selection_set_operation = SelectionSet {
            selections: vec![Selection::Field(foo)],
        };

        let document = result.unwrap();
        let definition = Definition::Operation(OperationDefinition {
            operation_type: OperationType::Query,
            selection_set: selection_set_operation,
            variable_definitions: Vec::new(),
            name: None,
        });
        let expected = Document {
            definitions: vec![definition],
        };
        assert_eq!(document, expected);
    }
    #[test]
    fn parse_operation_name() {
        let mut lexer = Lexer::new("query myQuery {foo}");
        let result = parse(&mut lexer);
        assert!(result.is_ok());
        let document = result.unwrap();
        if let Definition::Operation(operation_definition) = &document.definitions[0] {
            assert_eq!(operation_definition.operation_type, OperationType::Query);
            assert_eq!(operation_definition.name.as_ref().unwrap(), "myQuery");
        } else {
            assert!(false);
        }
    }

    #[test]
    fn parse_variable_definitions() {
        let mut lexer = Lexer::new(r#"query myQuery($var1: String = "hello") {foo}"#);
        let result = parse(&mut lexer);
        assert!(result.is_ok());
        let document = result.unwrap();
        if let Definition::Operation(operation_definition) = &document.definitions[0] {
            assert_eq!(operation_definition.variable_definitions.len(), 1);
            let var_def = &operation_definition.variable_definitions[0];
            assert_eq!(var_def.name, "var1");
            assert_eq!(
                var_def.type_reference,
                TypeReference::NamedType(String::from("String"))
            );
            assert_eq!(
                var_def.default_value.as_ref().unwrap(),
                &Value::StringValue(String::from("hello"))
            );
        } else {
            assert!(false);
        }
    }

    #[test]
    fn parse_variable_definitions_object_default_value() {
        let mut lexer = Lexer::new(r#"query myQuery($var1: InputObject = {field: "hello"}) {foo}"#);
        let result = parse(&mut lexer);
        assert!(result.is_ok());
        let document = result.unwrap();
        if let Definition::Operation(operation_definition) = &document.definitions[0] {
            assert_eq!(operation_definition.variable_definitions.len(), 1);
            let var_def = &operation_definition.variable_definitions[0];
            let default_value = var_def.default_value.as_ref().unwrap();
            let object_field = ObjectField {
                name: String::from("field"),
                value: Value::StringValue(String::from("hello")),
            };
            assert_eq!(
                default_value,
                &Value::ObjectValue(Box::new(vec![object_field]))
            );
        } else {
            assert!(false);
        }
    }
}
