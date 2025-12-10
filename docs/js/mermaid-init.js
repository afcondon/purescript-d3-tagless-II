// Mermaid diagram initialization
(function() {
  // Load Mermaid from CDN
  const script = document.createElement('script');
  script.src = 'https://cdn.jsdelivr.net/npm/mermaid@10/dist/mermaid.min.js';

  script.onload = function() {
    console.log('Mermaid library loaded successfully');

    // Initialize mermaid with custom theme
    window.mermaid.initialize({
      startOnLoad: false,
      theme: 'neutral',
      themeVariables: {
        primaryColor: '#f5e6d3',
        primaryTextColor: '#2c1810',
        primaryBorderColor: '#8b7355',
        lineColor: '#8b7355',
        secondaryColor: '#d4c4b0',
        tertiaryColor: '#e8dcc6',
        background: '#faf8f5',
        mainBkg: '#f5e6d3',
        secondBkg: '#e8dcc6',
        tertiaryBkg: '#d4c4b0',
        border1: '#8b7355',
        border2: '#a0896f',
        fontFamily: '"Georgia", "Times New Roman", serif',
        fontSize: '14px'
      },
      flowchart: {
        curve: 'basis',
        padding: 20,
        nodeSpacing: 50,
        rankSpacing: 50,
        htmlLabels: true
      },
      securityLevel: 'loose'
    });

    console.log('Mermaid initialized and ready');
  };

  document.head.appendChild(script);
})();