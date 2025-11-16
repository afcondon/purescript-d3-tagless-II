export function filterImpl(predicate) {
  return function(array) {
    return array.filter(predicate);
  };
}
