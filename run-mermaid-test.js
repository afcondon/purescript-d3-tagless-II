// Simple test runner that shows the Mermaid AST output
import { simpleBarChartAST, nestedSelectionAST, updatePatternAST, runMermaidAST } from './output/Test.MermaidASTExample/index.js';

console.log("=== Simple Bar Chart AST ===");
runMermaidAST(simpleBarChartAST)().then(result => {
  console.log(result);
  console.log("");
  
  console.log("=== Nested Selection AST ===");
  return runMermaidAST(nestedSelectionAST)();
}).then(result => {
  console.log(result);
  console.log("");
  
  console.log("=== Update Pattern AST ===");
  return runMermaidAST(updatePatternAST)();
}).then(result => {
  console.log(result);
}).catch(err => {
  console.error("Error:", err.message);
});
