const { parse } = require('../pkg/graphql_wasm_parser');
const assert = require('assert');

const ast = parse(`type Foo{field1: String, field2: Int}`);
// console.log(JSON.stringify(ast));

const objectDef = ast.definitions[0];
assert.equal(objectDef.ObjectType.name, "Foo");
assert.equal(objectDef.ObjectType.fields.length, 2);
assert.equal(objectDef.ObjectType.fields[0].name, "field1");
assert.equal(objectDef.ObjectType.fields[1].name, "field2");

console.log("Tests successful");