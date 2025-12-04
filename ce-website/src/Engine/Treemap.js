// FFI for Engine.Treemap

export const unsafeFloor = x => Math.floor(x);

export const clearElement = element => () => {
  while (element.firstChild) {
    element.removeChild(element.firstChild);
  }
};

export const setupWatermarkHover = () => {
  // Set up hover interactions for watermark labels
  // When hovering over a rect, make its corresponding label fully opaque
  const rects = document.querySelectorAll('.watermark-package');
  const labels = document.querySelectorAll('.watermark-package-label');

  // Both arrays should have same length and same order (from same data binding)
  rects.forEach((rect, index) => {
    const label = labels[index];
    if (!label) return;

    rect.addEventListener('mouseenter', () => {
      label.style.opacity = '1.0';
      label.style.transition = 'opacity 0.2s ease';
    });

    rect.addEventListener('mouseleave', () => {
      label.style.opacity = '0.3';
      label.style.transition = 'opacity 0.2s ease';
    });
  });
};
