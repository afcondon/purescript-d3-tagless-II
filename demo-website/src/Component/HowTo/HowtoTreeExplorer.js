export const parseFloat = str => {
  const n = Number.parseFloat(str);
  return isNaN(n) ? null : n;
};

export const setInnerHTML = element => html => () => {
  element.innerHTML = html;
};

export const floor = Math.floor;
