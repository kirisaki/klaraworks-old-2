const path = require('path')

module.exports = {
  mode: 'development',
  entry: {
    main: [
      './client/index.js'
    ]
  },

  output: {
    path: path.resolve(__dirname, 'dist'),
    filename: '[name].js',
  },

  module: {
    rules: [{
      test: /\.elm$/,
      exclude: [/elm-stuff/, /node_modules/],
      use: {
	loader: 'elm-webpack-loader',
	options: {
	  cwd: path.resolve(__dirname, 'client')
	}
      }
    },{
      test: /\.s[ca]ss$/,
      exclude: /node_modules/,
      use: ['style-loader', 'css-loader', 'sass-loader'],
    },{
      test:/\.html$/,
      exclude: /node_modules/,
      use: 'file-loader?name=[name].[ext]',
    },{
      test: /\.(jpg|png|svg)$/,
      use: [
        {
          loader: 'file-loader',
          options: {
            name: './img/[name].[ext]'
          },
        }],
    }],
    noParse: /\.elm$/,
  },
}
