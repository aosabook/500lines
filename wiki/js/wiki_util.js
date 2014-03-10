var marked = require('marked');

module.exports = {

  processHtml: function(wikiMarkup){
    return marked(wikiMarkup, {sanitize: true});
  }
};
