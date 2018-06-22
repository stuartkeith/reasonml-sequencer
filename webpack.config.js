const path = require('path');

const outputDirProduction = path.join(__dirname, "build/");
const isProduction = process.env.NODE_ENV === 'production';

module.exports = {
  entry: './src/Index.bs.js',
  mode: isProduction ? 'production' : 'development',
  output: {
    path: isProduction ? outputDirProduction : undefined,
    filename: 'index.js',
  },
};
