const d3 = require('d3-hierarchy');

// Simple test tree
const data = {
  name: "root",
  children: [
    { name: "A", children: [{ name: "A1" }, { name: "A2" }] },
    { name: "B" },
    { name: "C", children: [{ name: "C1" }, { name: "C2" }, { name: "C3" }] }
  ]
};

const root = d3.hierarchy(data);
const treeLayout = d3.tree().size([100, 100]);
treeLayout(root);

console.log("Root:", root.data.name, "x=", root.x.toFixed(2), "y=", root.y.toFixed(2));
root.children.forEach(child => {
  console.log("  Child:", child.data.name, "x=", child.x.toFixed(2), "y=", child.y.toFixed(2));
  if (child.children) {
    child.children.forEach(grandchild => {
      console.log("    Grandchild:", grandchild.data.name, "x=", grandchild.x.toFixed(2), "y=", grandchild.y.toFixed(2));
    });
  }
});
