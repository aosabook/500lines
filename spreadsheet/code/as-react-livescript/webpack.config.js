var webpack = require('webpack');
module.exports = {
    entry: [
        'webpack-dev-server/client?http://localhost:8080',
        'webpack/hot/dev-server',
        './main.ls'
    ],
    output: {
        path: __dirname,
        filename: 'main.js',
        publicPath: '/'
    },
    plugins: [
        new webpack.HotModuleReplacementPlugin()
    ],
    module: {
        loaders: [
            { test: /main\.ls$/, loaders: [ 'react-hot', 'livescript' ] },
            { test: /worker\.ls$/, loaders: [ 'worker', 'livescript' ] }
        ]
    },
}
