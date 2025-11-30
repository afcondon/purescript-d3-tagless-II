// D3 dependencies: d3-shape
import { line } from "d3-shape";

export function createLineGenerator_(config) {
  return () => {
    return line()
      .x(d => config.xScale(d.x))
      .y(d => config.yScale(d.y));
  };
}

export function generateLinePath_(generator) {
  return (data) => generator(data);
}
