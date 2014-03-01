const marked = require('marked');

module.exports = {

  processHtml: function(wikiMarkup){
    return marked(wikiMarkup);
  },

  getSeparator: function(path){
    //if path already ends in /, return empty string, otherwise return /
    return /\/$/.test(path) ? '': '/';
  }

}
