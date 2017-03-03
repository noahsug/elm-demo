var HtmlWebpackPlugin = require('html-webpack-plugin')
var path = require('path')
var webpack = require('webpack')
var merge = require('webpack-merge')

var entryPath = path.join(__dirname, 'src/static/index.js')
var outputPath = path.join(__dirname, 'dist')

var TARGET_ENV = process.env.npm_lifecycle_event === 'build' ? 'production' : 'development'
var outputFilename = TARGET_ENV === 'production' ? '[name]-[hash].js' : '[name].js'

var commonConfig = {
  output: {
    path: outputPath,
    filename: path.join('static/js/', outputFilename),
  },

  resolve: {
    extensions: ['', '.js', '.elm']
  },

  module: {
    noParse: /\.elm$/,
    loaders: [
      {
        test: /\.css$/,
        loader: 'style!css'
      }
    ]
  },

  plugins: [
    new HtmlWebpackPlugin({
      template: 'src/static/index.html',
      inject: 'body',
      filename: 'index.html'
    })
  ],
}

if (TARGET_ENV === 'development') {
  module.exports = merge(commonConfig, {
    entry: [
      'webpack-dev-server/client?http://localhost:8000',
      entryPath
    ],

    devServer: {
      historyApiFallback: true,
    },

    module: {
      loaders: [
        {
          test:    /\.elm$/,
          exclude: [/elm-stuff/, /node_modules/],
          loader:  'elm-hot!elm-webpack?verbose=true&warn=true&debug=true'
        },
      ]
    }
  })
}

if (TARGET_ENV === 'production') {
  module.exports = merge(commonConfig, {
    entry: entryPath,

    module: {
      loaders: [
        {
          test: /\.elm$/,
          exclude: [/elm-stuff/, /node_modules/],
          loader: 'elm-webpack'
        },
      ]
    },

    plugins: [
      new webpack.optimize.OccurenceOrderPlugin(),
      new webpack.optimize.UglifyJsPlugin({
        minimize: true,
        compressor: { warnings: false }
      })
    ]
  })
}
