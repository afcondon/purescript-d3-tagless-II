// FFI for SVG interpreter
export const unsafeParseNum = (s) => parseFloat(s);
export const unsafeParseBool = (s) => s === "true";
export const unsafeShowField = (name) => (record) => String(record[name]);
