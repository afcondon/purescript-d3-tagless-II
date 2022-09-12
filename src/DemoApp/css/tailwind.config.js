module.exports = {
  purge: [],
  darkMode: false, // or 'media' or 'class'
  theme: {
    screens: {
      sm: '480px',
      md: '768px',
      lg: '976px',
      xl: '1440px',
      xxl: '2000px',
    },
    extend: {
      colors: {
        'black-10': '#00091A',
        'black-20': '#242A33',
        'black-modal-a90': 'rgba(0,9,26,0.9)',
        'grey-50': '#5C6573',
        'grey-50-a20': 'rgba(102,113,128,0.2)',
        'grey-50-a30': 'rgba(102,113,128,0.3)',
        'grey-50-a80': 'rgba(102,113,128,0.8)',
        'grey-50-a90': 'rgba(102,113,128,0.9)',
        'grey-70': 'rgb(143,158,179)',
        'grey-70-a30': 'rgba(143,158,179,0.3)',
        'grey-70-a40': 'rgba(143,158,179,0.4)',
        'grey-80': '#C2C6CC',
        'grey-90': '#E1E3E6',
        'grey-95': '#F0F1F2',
        'grey-97': '#F7F7F7',
        'blue-65': '#008AA6',
        'blue-75': '#009FBF',
        'blue-82': '#00ABD1',
        'blue-88': '#00BBE0',
        'red': '#FF5471',
        'yellow': '#FFC859',
        'green': '#66C7AF',
        'steel-75': '#A8B2BF',
        'steel-85': '#BFCAD9',
        'steel-100': '#E6F0FF'
      },
      spacing: {
        '0': '0',
        '5': '1.25rem',
        '7': '1.75rem',
        '9': '2.25rem',
        '10': '2.5rem',
        '12': '3rem',
        '14': '3.5rem',
        '16': '4rem',
        '20': '5rem',
        '30': '7.5rem',
        '40': '10rem',
        '50': '12.5rem',
        '60': '15rem',
        '80': '20rem',
        '90': '22.5rem',
        '120': '30rem',
        '160': '40rem',
      },
      zIndex: {
        '60': '60',
        '-10': '-10',
      },
    },
  },
  variants: {
    extend: {},
  },
  plugins: [],
}