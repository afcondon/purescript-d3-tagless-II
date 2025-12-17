// FFI for Test.Golden.Util

export const parseNumber = (str) => {
  const n = parseFloat(str);
  if (isNaN(n)) {
    return null;
  }
  return n;
};
