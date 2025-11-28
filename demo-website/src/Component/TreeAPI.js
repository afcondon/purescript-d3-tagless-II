// FFI for TreeAPI component

export const clearViz = () => {
  const viz = document.getElementById('viz');
  if (viz) {
    viz.innerHTML = '';
  }
};
