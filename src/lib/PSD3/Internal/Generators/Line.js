import * as d3 from "d3";

export function createLineGenerator(config) {
  return () => {
    return d3.line()
      .x(d => config.xScale(d.x))
      .y(d => config.yScale(d.y));
  };
}

export function generateLinePath(generator) {
  return (data) => generator(data);
}
