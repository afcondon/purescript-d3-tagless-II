// FFI for Explorer - string manipulation utilities

export const charAtFFI = (idx) => (str) => {
  return idx < str.length ? str.charAt(idx) : "";
};

export const substringFFI = (start) => (end) => (str) => {
  return str.substring(start, end);
};
