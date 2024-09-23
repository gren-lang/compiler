#!/usr/bin/env node

const compiler = require("./compiler.js");
const compilerInstance = compiler.Gren.Main.init({});

compilerInstance.ports.completeStaticBuild.subscribe(function(output) {
  console.log("Make static build out of " + output);
});
