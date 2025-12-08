// FFI for Engine.Treemap

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

    // Store package name on rect for sync lookup
    const pkgName = label.textContent;
    rect.setAttribute('data-package', pkgName);
    label.setAttribute('data-package', pkgName);

    rect.addEventListener('mouseenter', () => {
      label.style.opacity = '1.0';
      label.style.transition = 'opacity 0.2s ease';
      rect.style.fill = 'rgba(255, 255, 255, 0.15)';  // Show subtle fill on hover
      rect.style.stroke = 'rgba(255, 255, 255, 0.8)';
      rect.style.strokeWidth = '2';
      rect.style.transition = 'fill 0.2s ease, stroke 0.2s ease, stroke-width 0.2s ease';
    });

    rect.addEventListener('mouseleave', () => {
      label.style.opacity = '0.3';
      label.style.transition = 'opacity 0.2s ease';
      rect.style.fill = 'rgba(255, 255, 255, 0)';  // Back to invisible
      rect.style.stroke = '';
      rect.style.strokeWidth = '';
    });
  });
};

// Highlight a specific package in the treemap watermark
export const highlightWatermarkPackage_ = (packageName) => () => {
  const rects = document.querySelectorAll('.watermark-package');
  const labels = document.querySelectorAll('.watermark-package-label');

  rects.forEach((rect) => {
    const pkgName = rect.getAttribute('data-package');
    if (pkgName === packageName) {
      rect.style.fill = 'rgba(255, 255, 255, 0.15)';  // Show subtle fill
      rect.style.stroke = 'rgba(255, 255, 255, 0.9)';
      rect.style.strokeWidth = '2.5';
      rect.style.transition = 'fill 0.15s ease, stroke 0.15s ease, stroke-width 0.15s ease';
    }
  });

  labels.forEach((label) => {
    const pkgName = label.getAttribute('data-package');
    if (pkgName === packageName) {
      label.style.opacity = '1.0';
      label.style.transition = 'opacity 0.15s ease';
    }
  });
};

// Clear all watermark package highlights
export const clearWatermarkHighlight_ = () => () => {
  const rects = document.querySelectorAll('.watermark-package');
  const labels = document.querySelectorAll('.watermark-package-label');

  rects.forEach((rect) => {
    rect.style.fill = 'rgba(255, 255, 255, 0)';  // Back to invisible
    rect.style.stroke = '';
    rect.style.strokeWidth = '';
  });

  labels.forEach((label) => {
    label.style.opacity = '0.3';
    label.style.transition = 'opacity 0.15s ease';
  });
};
