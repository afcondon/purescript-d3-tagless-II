// FFI for App.purs
export function formatWithCommas(str) {
  return str.replace(/\B(?=(\d{3})+(?!\d))/g, ",");
}
