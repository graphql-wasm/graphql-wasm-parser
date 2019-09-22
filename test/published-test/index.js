const { parse } = require('graphql-wasm-parser');

const ast = parse("{foo}");
console.log(JSON.stringify(ast));